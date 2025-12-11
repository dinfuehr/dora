from __future__ import annotations

import itertools
import os
import platform
import re
import shlex
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Optional, List, Sequence, Tuple

from .config import ALL_CONFIGS, Config, REPO_ROOT, TESTS_DIR
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
}


def detect_architecture() -> Optional[str]:
    machine = platform.machine().lower()
    if machine in {"x86_64", "amd64"}:
        return "x64"
    if machine in {"arm64", "aarch64"}:
        return "arm64"
    raise RuntimeError(f"unknown architecture {machine}")


def detect_os() -> str:
    system = platform.system().lower()
    if system == "linux":
        return "linux"
    if system == "darwin":
        return "macos"
    if system == "windows":
        return "windows"
    if os.name == "nt":
        return "windows"
    raise RuntimeError(f"unknown operating system {system}")


ARCH = detect_architecture()
OS_NAME = detect_os()


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


class TestCase:
    def __init__(self, relative_path: str) -> None:
        self.file = relative_path
        self.test_file = relative_path
        self.vm_args = []
        self.args = []
        self.expectation = TestExpectation()
        self.timeout: Optional[int] = None
        self.configs: List[Config] = []
        self.enable_boots = False
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


def load_test_files(options: RunnerOptions) -> List[str]:
    if options.files:
        result: List[str] = []
        for entry in options.files:
            path = Path(entry)
            if path.is_dir():
                for file in sorted(path.rglob("*.dora")):
                    result.append(str(file))
            elif path.is_file():
                result.append(str(path))
            else:
                print(f"{entry} is not a file or directory.")
                sys.exit(1)
        return result
    return sorted(str(path) for path in TESTS_DIR.rglob("*.dora"))


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
    absolute_path = Path(file_path).resolve()
    try:
        relative_path = str(absolute_path.relative_to(REPO_ROOT))
    except ValueError:
        relative_path = os.path.relpath(absolute_path, REPO_ROOT)

    test_case = TestCase(relative_path)
    test_dir = absolute_path.parent
    has_filecheck = False

    if options.force_config is not None:
        test_case.configs.append(options.force_config)
    else:
        for config in ALL_CONFIGS:
            if config.enabled_for(test_dir):
                test_case.configs.append(config)
        if options.select_config is not None:
            if options.select_config in test_case.configs:
                test_case.configs = [options.select_config]
            else:
                test_case.set_ignore()

    file_on_disk = REPO_ROOT / test_case.file

    with open(file_on_disk, "r", encoding="utf-8") as handle:
        for raw_line in handle:
            if not has_filecheck and FILECHECK_DIRECTIVE_RE.search(raw_line):
                has_filecheck = True
            line = raw_line.strip()
            if not line.startswith("//="):
                continue
            directive = line[3:].strip()
            if not directive:
                continue
            arguments = read_cmdline(directive)
            if not arguments:
                continue
            keyword = arguments[0]
            if keyword == "error":
                test_case.expectation.fail = True
                if len(arguments) == 1:
                    continue
                detail = arguments[1]
                if detail == "code":
                    test_case.expectation.code = int(arguments[2])
                else:
                    test_case.expectation.code = ERROR_NAME_TO_CODE.get(detail)
                    if test_case.expectation.code is None:
                        raise ValueError(
                            f"unknown error expectation in {file_path}: {line}"
                        )
            elif keyword == "platform":
                supported = evaluate_platform_expression(arguments[1])
                if not supported:
                    test_case.set_ignore()
            elif keyword == "file":
                test_case.test_file = arguments[1]
            elif keyword == "ignore":
                test_case.set_ignore()
            elif keyword == "stdout":
                if len(arguments) > 1 and arguments[1] == "file":
                    stdout_path = file_on_disk.with_suffix(".stdout")
                    test_case.expectation.stdout = stdout_path.read_text(
                        encoding="utf-8"
                    )
                elif len(arguments) > 1:
                    test_case.expectation.stdout = arguments[1]
            elif keyword == "stderr":
                if len(arguments) > 1:
                    test_case.expectation.stderr = arguments[1]
            elif keyword == "args":
                args = list(
                    itertools.chain.from_iterable(shlex.split(s) for s in arguments[1:])
                )
                test_case.args.extend(args)
            elif keyword == "vm-args":
                vm_args = list(
                    itertools.chain.from_iterable(shlex.split(s) for s in arguments[1:])
                )
                test_case.vm_args.extend(vm_args)
            elif keyword == "boots":
                test_case.enable_boots = True
            elif keyword == "timeout":
                test_case.timeout = int(arguments[1])
            elif keyword == "flaky":
                test_case.set_flaky()
            else:
                raise ValueError(f"unknown expectation in {file_path}: {line}")

    if has_filecheck:
        test_case.expectation.filecheck_path = file_on_disk

    return [(test_case, config) for config in test_case.configs]
