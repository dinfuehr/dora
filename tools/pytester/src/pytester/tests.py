from __future__ import annotations

import itertools
import os
import re
import shlex
import sys
import tomllib
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Optional, List, Sequence, Tuple

from .config import (
    ALL_CONFIGS,
    ARCH,
    BOOTS_CONFIG,
    CANNON_CONFIG,
    Config,
    OS_NAME,
    PACKAGE_TESTS_DIR,
    REPO_ROOT,
    TESTS_DIR,
)
from .options import RunnerOptions


FILECHECK_DIRECTIVE_RE = re.compile(r"//\s*CHECK(?:-[A-Z0-9_]+)?:")


ERROR_NAME_TO_CODE = {
    "div0": 101,
    "assert": 102,
    "array": 103,
    "nil": 104,
    "cast": 105,
    "oom": 106,
    "stack-overflow": 107,
    "overflow": 109,
    "shift": 110,
}


def create_platform_context() -> Dict[str, object]:
    linux = OS_NAME == "linux"
    macos = OS_NAME == "macos"
    windows = OS_NAME == "windows"
    unix = linux or macos
    x64 = ARCH == "x64"
    arm64 = ARCH == "arm64"
    return {
        "arch": ARCH,
        "os": OS_NAME,
        "linux": linux,
        "macos": macos,
        "windows": windows,
        "unix": unix,
        "x64": x64,
        "arm64": arm64,
    }


PLATFORM_CONTEXT = create_platform_context()
CONFIG_NAMES = {config.name for config in ALL_CONFIGS}


def evaluate_platform_expression(expression: str) -> bool:
    try:
        return bool(eval(expression, {"__builtins__": {}}, PLATFORM_CONTEXT))
    except Exception as exc:  # noqa: BLE001
        raise RuntimeError(
            f"invalid platform expression '{expression}': {exc}"
        ) from exc


@dataclass
class TestExpectation:
    fail: bool = False
    position: Optional[str] = None
    code: Optional[object] = None
    message: Optional[str] = None
    stdout: Optional[str] = None
    stderr: Optional[str] = None
    filecheck_path: Optional[Path] = None


@dataclass
class ConfigCondition:
    kind: str
    config_name: str

    def enabled_for(self, config: Config) -> bool:
        matches = config.name == self.config_name
        if self.kind == "if":
            return matches
        assert self.kind == "unless"
        return not matches


@dataclass
class ConditionalArguments:
    arguments: List[str]
    condition: Optional[ConfigCondition] = None

    def enabled_for(self, config: Config) -> bool:
        return self.condition is None or self.condition.enabled_for(config)


class TestCase:
    def __init__(self, relative_path: str) -> None:
        self.file = relative_path
        self.test_file = relative_path
        self.compile_args = []
        self.runtime_args = []
        self.args = []
        self._compile_args: List[ConditionalArguments] = []
        self._runtime_args: List[ConditionalArguments] = []
        self._args: List[ConditionalArguments] = []
        self.expectation = TestExpectation()
        self.timeout: Optional[int] = None
        self.configs: List[Config] = []
        self.package_dir: Optional[str] = None
        self.package_name: Optional[str] = None
        self.requires_boots = False
        self._flaky = False
        self._ignore = False

    def get_timeout(self, options: "RunnerOptions") -> int:
        if options.forced_timeout is not None:
            return options.forced_timeout
        if self.timeout is not None:
            return self.timeout
        return 60

    def set_ignore(self) -> None:
        self._ignore = True

    def ignore(self) -> bool:
        return self._ignore

    def set_flaky(self) -> None:
        self._flaky = True

    def flaky(self) -> bool:
        return self._flaky

    def is_package(self) -> bool:
        return self.package_dir is not None

    def for_config(self, config: Config) -> "TestCase":
        test_case = TestCase(self.file)
        test_case.test_file = self.test_file
        test_case.compile_args = arguments_for_config(self._compile_args, config)
        test_case.runtime_args = arguments_for_config(self._runtime_args, config)
        test_case.args = arguments_for_config(self._args, config)
        test_case.expectation = self.expectation
        test_case.timeout = self.timeout
        test_case.configs = [config]
        test_case.package_dir = self.package_dir
        test_case.package_name = self.package_name
        test_case.requires_boots = self.requires_boots
        test_case._flaky = self._flaky
        test_case._ignore = self._ignore
        return test_case


def arguments_for_config(
    entries: Sequence[ConditionalArguments], config: Config
) -> List[str]:
    return list(
        itertools.chain.from_iterable(
            entry.arguments for entry in entries if entry.enabled_for(config)
        )
    )


def load_test_files(options: RunnerOptions) -> List[str]:
    if options.files:
        result: List[str] = []
        for entry in options.files:
            path = Path(entry)
            if path.is_dir():
                result.extend(str(file) for file in test_files_in_dir(path))
            elif path.is_file():
                result.append(str(path))
            else:
                print(f"{entry} is not a file or directory.")
                sys.exit(1)
        return result

    return [str(file) for file in default_test_files()]


def read_cmdline(text: str) -> List[str]:
    args: List[str] = []
    in_quote = False
    escaped = False
    current: List[str] = []

    for char in text:
        if escaped:
            if char == "n":
                current.append("\n")
            elif char == "t":
                current.append("\t")
            else:
                raise ValueError(f"unknown escape sequence \\{char}")
            escaped = False
            continue
        if char == "\\" and in_quote:
            escaped = True
            continue
        if char == '"':
            if in_quote:
                args.append("".join(current))
                current = []
                in_quote = False
            elif not current:
                in_quote = True
            else:
                current.append(char)
            continue
        if char.isspace() and not in_quote:
            if current:
                args.append("".join(current))
                current = []
            continue
        current.append(char)

    if current:
        args.append("".join(current))

    return args


def default_test_files() -> List[Path]:
    files = list(TESTS_DIR.rglob("*.dora"))
    files.extend(package_test_manifests_in(PACKAGE_TESTS_DIR))
    return sorted(files)


def test_files_in_dir(path: Path) -> List[Path]:
    def is_package_source_file(file: Path) -> bool:
        return any((parent / "dora-package.toml").is_file() for parent in file.parents)

    files = [file for file in path.rglob("*.dora") if not is_package_source_file(file)]
    files.extend(package_test_manifests_in(path))
    return sorted(files)


def package_test_manifests_in(path: Path) -> List[Path]:
    manifest = path / "dora-package.toml"
    if manifest.is_file():
        return [manifest]

    return sorted(path.glob("*/dora-package.toml"))


def read_conditional_arguments(
    arguments: List[str], file_path: str, line: str
) -> ConditionalArguments:
    condition = None
    if (
        len(arguments) >= 2
        and arguments[-2] in {"if", "unless"}
        and arguments[-1] in CONFIG_NAMES
    ):
        config_name = arguments[-1]
        condition = ConfigCondition(arguments[-2], config_name)
        arguments = arguments[:-2]

    args = list(itertools.chain.from_iterable(shlex.split(s) for s in arguments))
    return ConditionalArguments(args, condition)


def parse_test_files(
    options: RunnerOptions, files: Sequence[str]
) -> List[Tuple[TestCase, Config]]:
    tests: List[Tuple[TestCase, Config]] = []
    for file_path in files:
        tests.extend(parse_test_file(options, file_path))
    return tests


def parse_test_file(
    options: RunnerOptions, file_path: str
) -> List[Tuple[TestCase, Config]]:
    if Path(file_path).name == "dora-package.toml":
        return parse_package_test_file(options, file_path)
    return parse_dora_test_file(options, file_path)


def parse_dora_test_file(
    options: RunnerOptions, file_path: str
) -> List[Tuple[TestCase, Config]]:
    absolute_path = Path(file_path).resolve()
    test_case = TestCase(relative_path_for(absolute_path))
    test_dir = absolute_path.parent
    has_filecheck = False

    file_on_disk = REPO_ROOT / test_case.file

    with open(file_on_disk, "r", encoding="utf-8") as handle:
        for raw_line in handle:
            if not has_filecheck and FILECHECK_DIRECTIVE_RE.search(raw_line):
                has_filecheck = True
            line = raw_line.strip()
            directive = read_dora_directive(line)
            if directive is None:
                continue
            if not directive:
                continue
            arguments = read_cmdline(directive)
            if not arguments:
                continue
            parse_dora_directive(test_case, arguments, file_path, line)

    if has_filecheck:
        test_case.expectation.filecheck_path = file_on_disk

    read_output_files(test_case, file_on_disk)

    configs = dora_test_configs(options, test_dir)
    if test_case.requires_boots:
        configs = [c for c in configs if c is BOOTS_CONFIG]
    test_case.configs = configs

    return [(test_case.for_config(config), config) for config in configs]


def parse_package_test_file(
    options: RunnerOptions, file_path: str
) -> List[Tuple[TestCase, Config]]:
    absolute_path = Path(file_path).resolve()
    test_case = TestCase(relative_path_for(absolute_path))
    test_dir = absolute_path.parent
    file_on_disk = REPO_ROOT / test_case.file

    test_case.package_dir = relative_path_for(test_dir)
    test_case.package_name = read_package_name(file_on_disk)

    with open(file_on_disk, "r", encoding="utf-8") as handle:
        for raw_line in handle:
            line = raw_line.strip()
            directive = read_package_directive(line)
            if directive is None:
                continue
            if not directive:
                continue
            arguments = read_cmdline(directive)
            if not arguments:
                continue
            parse_package_directive(test_case, arguments, file_path, line)

    read_output_files(test_case, file_on_disk)

    configs = package_test_configs(options)
    test_case.configs = configs
    return [(test_case.for_config(config), config) for config in configs]


def parse_dora_directive(
    test_case: TestCase, arguments: List[str], file_path: str, line: str
) -> None:
    keyword = arguments[0]
    if keyword == "error":
        parse_error_directive(test_case, arguments, file_path, line)
    elif keyword == "platform":
        parse_platform_directive(test_case, arguments)
    elif keyword == "file":
        test_case.test_file = arguments[1]
    elif keyword == "ignore":
        test_case.set_ignore()
    elif keyword == "args":
        test_case._args.append(
            read_conditional_arguments(arguments[1:], file_path, line)
        )
    elif keyword == "compile-args":
        test_case._compile_args.append(
            read_conditional_arguments(arguments[1:], file_path, line)
        )
    elif keyword == "runtime-args":
        test_case._runtime_args.append(
            read_conditional_arguments(arguments[1:], file_path, line)
        )
    elif keyword == "boots":
        test_case.requires_boots = True
    elif keyword == "timeout":
        test_case.timeout = int(arguments[1])
    elif keyword == "flaky":
        test_case.set_flaky()
    else:
        raise ValueError(f"unknown expectation in {file_path}: {line}")


def parse_package_directive(
    test_case: TestCase, arguments: List[str], file_path: str, line: str
) -> None:
    keyword = arguments[0]
    if keyword == "error":
        parse_error_directive(test_case, arguments, file_path, line)
    elif keyword == "platform":
        parse_platform_directive(test_case, arguments)
    elif keyword == "ignore":
        test_case.set_ignore()
    elif keyword == "args":
        test_case._args.append(
            read_conditional_arguments(arguments[1:], file_path, line)
        )
    elif keyword == "runtime-args":
        test_case._runtime_args.append(
            read_conditional_arguments(arguments[1:], file_path, line)
        )
    elif keyword == "timeout":
        test_case.timeout = int(arguments[1])
    elif keyword == "flaky":
        test_case.set_flaky()
    else:
        raise ValueError(f"unknown package expectation in {file_path}: {line}")


def parse_error_directive(
    test_case: TestCase, arguments: List[str], file_path: str, line: str
) -> None:
    test_case.expectation.fail = True
    if len(arguments) == 1:
        return

    detail = arguments[1]
    if detail == "code":
        test_case.expectation.code = int(arguments[2])
    else:
        test_case.expectation.code = ERROR_NAME_TO_CODE.get(detail)
        if test_case.expectation.code is None:
            raise ValueError(f"unknown error expectation in {file_path}: {line}")


def parse_platform_directive(test_case: TestCase, arguments: List[str]) -> None:
    supported = evaluate_platform_expression(arguments[1])
    if not supported:
        test_case.set_ignore()


def dora_test_configs(options: RunnerOptions, test_dir: Path) -> List[Config]:
    if options.force_config is not None:
        return [options.force_config]

    configs = []
    for config in ALL_CONFIGS:
        if config.enabled_for(test_dir):
            configs.append(config)
    if options.select_config is not None:
        if options.select_config in configs:
            return [options.select_config]
        elif options.select_config is CANNON_CONFIG:
            if options.select_config.enabled_for(test_dir):
                return [options.select_config]
            else:
                return []
        else:
            return []

    return configs


def package_test_configs(options: RunnerOptions) -> List[Config]:
    # Package tests invoke `dora build`, which currently uses the boots compiler.
    requested_config = options.force_config or options.select_config
    if requested_config is not None and requested_config is not BOOTS_CONFIG:
        return []
    return [BOOTS_CONFIG]


def relative_path_for(path: Path) -> str:
    try:
        return str(path.resolve().relative_to(REPO_ROOT))
    except ValueError:
        return os.path.relpath(path.resolve(), REPO_ROOT)


def read_package_name(path: Path) -> str:
    with path.open("rb") as handle:
        manifest = tomllib.load(handle)

    package = manifest.get("package")
    if not isinstance(package, dict):
        raise ValueError(f"missing [package] table in {path}")

    name = package.get("name")
    if not isinstance(name, str):
        raise ValueError(f"missing package.name in {path}")

    return name


def read_output_files(test_case: TestCase, file_on_disk: Path) -> None:
    stdout_path = file_on_disk.with_suffix(".stdout")
    if stdout_path.exists():
        test_case.expectation.stdout = stdout_path.read_text(encoding="utf-8")

    stderr_path = file_on_disk.with_suffix(".stderr")
    if stderr_path.exists():
        test_case.expectation.stderr = stderr_path.read_text(encoding="utf-8")


def read_dora_directive(line: str) -> Optional[str]:
    if not line.startswith("//="):
        return None
    return line[3:].strip()


def read_package_directive(line: str) -> Optional[str]:
    if not line.startswith("#="):
        return None
    return line[2:].strip()
