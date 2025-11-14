#!/usr/bin/env python3
from __future__ import annotations

import argparse
import os
import platform
import random
import re
import signal
import subprocess
import sys
import threading
import time
import shlex
from collections import OrderedDict
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, List, Optional, Sequence, Tuple, Set

SCRIPT_DIR = Path(__file__).resolve().parent
REPO_ROOT = SCRIPT_DIR.parent
TESTS_DIR = REPO_ROOT / "tests"

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


@dataclass
class Config:
    name: str
    flags: str
    directories: object
    enable_boots: bool = False

    def enabled_for(self, test_dir: Path) -> bool:
        if self.directories is True:
            return True
        for entry in self.directories:
            if test_dir == TESTS_DIR / entry:
                return True
        return False

@dataclass
class TestExpectation:
    fail: bool = False
    position: Optional[str] = None
    code: Optional[object] = None
    message: Optional[str] = None
    stdout: Optional[str] = None
    stderr: Optional[str] = None


class TestCase:
    def __init__(self, relative_path: str) -> None:
        self.file = relative_path
        self.test_file = relative_path
        self.vm_args = ""
        self.args = ""
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


@dataclass
class TestResult:
    test_case: TestCase
    config: Config
    status: str
    message: Optional[str] = None
    stdout: str = ""
    stderr: str = ""
    cmdline: str = ""
    cargo_cmd: str = ""
    attempt: int = 1

    @classmethod
    def error(
        cls,
        test_case: TestCase,
        config: Config,
        message: str,
        stdout: str,
        stderr: str,
        cmdline: str,
        cargo_cmd: str,
        attempt: int,
    ) -> "TestResult":
        return cls(
            test_case=test_case,
            config=config,
            status="failed",
            message=message,
            stdout=stdout,
            stderr=stderr,
            cmdline=cmdline,
            cargo_cmd=cargo_cmd,
            attempt=attempt,
        )

    @classmethod
    def success(cls, test_case: TestCase, config: Config) -> "TestResult":
        return cls(test_case=test_case, config=config, status="passed")

    @classmethod
    def ignore(cls, test_case: TestCase, config: Config) -> "TestResult":
        return cls(test_case=test_case, config=config, status="ignore")

    def is_success(self) -> bool:
        return self.status == "passed"


@dataclass
class ProcessResult:
    pid: Optional[int] = None
    status: Optional[object] = None
    stdout: str = ""
    stderr: str = ""
    timeout: bool = False


DEFAULT_CONFIG = Config("default", "", True)
ALWAYS_BOOTS_CONFIG = Config("always_boots", "--always-boots", True, enable_boots=True)
ALL_CONFIGS = [DEFAULT_CONFIG, ALWAYS_BOOTS_CONFIG]


@dataclass
class RunnerOptions:
    target: str = "debug"
    capture: bool = True
    stress: bool = False
    stress_timeout: Optional[int] = None
    processors: Optional[int] = None
    forced_timeout: Optional[int] = None
    cargo_target: Optional[str] = None
    files: List[str] = field(default_factory=list)
    exit_after_n_failures: Optional[int] = None
    env_overrides: Dict[str, str] = field(default_factory=dict)
    verbose: bool = False
    check_only: bool = False
    extra_args: Optional[str] = None
    force_config: Optional[Config] = None
    select_config: Optional[Config] = None
    print_tests: bool = False


class ThreadSynchronization:
    def __init__(self) -> None:
        self.mutex = threading.Lock()
        self._cancel_event = threading.Event()

    def cancel(self) -> None:
        self._cancel_event.set()

    def cancelled(self) -> bool:
        return self._cancel_event.is_set()

    def sleep_until_timeout_or_cancelled(self, timeout: int) -> None:
        self._cancel_event.wait(timeout)
        self._cancel_event.set()


class ProcessManager:
    def __init__(self) -> None:
        self._lock = threading.Lock()
        self._processes: Set[subprocess.Popen[str]] = set()

    def register(self, proc: subprocess.Popen[str]) -> None:
        with self._lock:
            self._processes.add(proc)

    def unregister(self, proc: subprocess.Popen[str]) -> None:
        with self._lock:
            self._processes.discard(proc)

    def cancel_all(self) -> None:
        with self._lock:
            processes = list(self._processes)
            self._processes.clear()
        for proc in processes:
            if proc.poll() is None:
                try:
                    proc.kill()
                except OSError:
                    pass


class StatusDisplay:
    def __init__(self, enabled: bool, start_time: float) -> None:
        self.enabled = enabled
        self.start_time = start_time
        self.active = False
        self.line_count = 0

    def render(
        self,
        passed: int,
        failed: int,
        ignored: int,
        running_tests: List[str],
    ) -> None:
        if not self.enabled:
            return
        self._clear()
        sorted_tests = sorted(running_tests)
        duration = self._format_duration()
        lines = [
            f"{passed} tests passed; {ignored} tests ignored; {failed} tests failed; {len(running_tests)} tests running; running for {duration}."
        ]
        if sorted_tests:
            lines.extend(sorted_tests)
        else:
            lines.append("none")
        for index, line in enumerate(lines):
            prefix = "\r" if index == 0 else ""
            newline = "\n" if index < len(lines) - 1 else ""
            sys.stdout.write(prefix + line + newline)
        sys.stdout.flush()
        self.active = True
        self.line_count = len(lines)

    def suspend(self) -> None:
        self._clear()

    def finish(self) -> None:
        if not self.enabled:
            return
        self._clear()

    def _clear(self) -> None:
        if not self.enabled:
            return
        if not self.active or self.line_count == 0:
            return
        sys.stdout.write("\r")
        for index in range(self.line_count):
            sys.stdout.write("\x1b[2K")
            if index < self.line_count - 1:
                sys.stdout.write("\x1b[1A\r")
        sys.stdout.flush()
        self.active = False
        self.line_count = 0

    def _format_duration(self) -> str:
        elapsed = max(0.0, time.time() - self.start_time)
        return f"{elapsed:.1f} seconds"


def binary_path(options: RunnerOptions) -> str:
    directory = options.target
    extension = ".exe" if os.name == "nt" else ""
    target_dir = f"target/{options.cargo_target}" if options.cargo_target else "target"
    return f"{target_dir}/{directory}/dora{extension}"


def interpret_status(code: Optional[int]) -> Optional[object]:
    if code is None:
        return None
    if code >= 0:
        return code
    signal_code = -code
    try:
        signal_name = signal.Signals(signal_code).name
    except ValueError:
        signal_name = "unknown"
    return f"signal {signal_name}/{signal_code}"


def spawn_with_timeout(
    env_overrides: Dict[str, str],
    cmd: Sequence[str],
    timeout: int,
    process_manager: ProcessManager,
) -> ProcessResult:
    env = os.environ.copy()
    env.update(env_overrides)
    result = ProcessResult()
    with subprocess.Popen(
        list(cmd),
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        env=env,
    ) as proc:
        process_manager.register(proc)
        result.pid = proc.pid
        try:
            stdout, stderr = proc.communicate(timeout=timeout)
            result.stdout = stdout or ""
            result.stderr = stderr or ""
            result.status = interpret_status(proc.returncode)
        except subprocess.TimeoutExpired:
            result.timeout = True
            proc.kill()
            stdout, stderr = proc.communicate()
            result.stdout = stdout or ""
            result.stderr = stderr or ""
            result.status = "TIMEOUT"
        finally:
            process_manager.unregister(proc)
    return result


def num_from_shell(cmd: Sequence[str]) -> int:
    try:
        output = subprocess.check_output(cmd, text=True)
    except (subprocess.CalledProcessError, FileNotFoundError):
        return 0
    try:
        return int(output.strip())
    except ValueError:
        return 0


def query_number_processors() -> int:
    system = OS_NAME
    if system == "linux":
        num = num_from_shell(["nproc", "--all"])
        if num > 0:
            return num
        try:
            with open("/proc/cpuinfo", "r", encoding="utf-8") as handler:
                num = sum(1 for line in handler if line.startswith("processor"))
                if num > 0:
                    return num
        except OSError:
            pass
    elif system == "macos":
        num = num_from_shell(["sysctl", "-n", "hw.ncpu"])
        if num > 0:
            return num
    elif system == "windows":
        try:
            num = int(os.environ.get("NUMBER_OF_PROCESSORS", "0"))
            if num > 0:
                return num
        except ValueError:
            pass
    return 1


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


def evaluate_platform_expression(expression: str) -> bool:
    try:
        return bool(eval(expression, {"__builtins__": {}}, PLATFORM_CONTEXT))
    except Exception as exc:  # noqa: BLE001
        raise RuntimeError(f"invalid platform expression '{expression}': {exc}") from exc


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


def parse_test_files(options: RunnerOptions, files: Sequence[str]) -> List[Tuple[TestCase, Config]]:
    tests: List[Tuple[TestCase, Config]] = []
    for file_path in files:
        tests.extend(parse_test_file(options, file_path))
    return tests


def parse_test_file(options: RunnerOptions, file_path: str) -> List[Tuple[TestCase, Config]]:
    absolute_path = Path(file_path).resolve()
    try:
        relative_path = str(absolute_path.relative_to(REPO_ROOT))
    except ValueError:
        relative_path = os.path.relpath(absolute_path, REPO_ROOT)

    test_case = TestCase(relative_path)
    test_dir = absolute_path.parent

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
                        raise ValueError(f"unknown error expectation in {file_path}: {line}")
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
                    test_case.expectation.stdout = stdout_path.read_text(encoding="utf-8")
                elif len(arguments) > 1:
                    test_case.expectation.stdout = arguments[1]
            elif keyword == "stderr":
                if len(arguments) > 1:
                    test_case.expectation.stderr = arguments[1]
            elif keyword == "args":
                test_case.args = " ".join(arguments[1:])
            elif keyword == "vm-args":
                addition = " ".join(arguments[1:])
                if test_case.vm_args:
                    test_case.vm_args += " "
                test_case.vm_args += addition
            elif keyword == "boots":
                test_case.enable_boots = True
            elif keyword == "timeout":
                test_case.timeout = int(arguments[1])
            elif keyword == "flaky":
                test_case.set_flaky()
            else:
                raise ValueError(f"unknown expectation in {file_path}: {line}")

    return [(test_case, config) for config in test_case.configs]


def canonicalize(output: str) -> str:
    return output.replace("\\", "/")


ERROR_IN_RE = re.compile(r"^error in (.+) at (\d+:\d+): (.+)$")
ERROR_AT_RE = re.compile(r"^error at (\d+:\d+): (.+)$")


def read_error_message(content: str) -> Tuple[Optional[str], Optional[str]]:
    position: Optional[str] = None
    message: Optional[str] = None
    for line in content.splitlines():
        stripped = line.strip()
        if stripped in {"1 error found.", "error during parsing."}:
            return position, message
        match = ERROR_IN_RE.match(stripped)
        if match:
            position = match.group(2)
            message = match.group(3)
            continue
        match = ERROR_AT_RE.match(stripped)
        if match:
            position = match.group(1)
            message = match.group(2)
    return position, message


def print_output(output: str) -> None:
    if not output:
        return
    max_output_length = 8 * 1024
    print(output[:max_output_length], end="")
    if len(output) > max_output_length:
        print("OUTPUT TOO LONG AND WAS CUT OFF...")


def print_run_details(
    test_case: TestCase,
    stdout: str,
    stderr: str,
    cmdline: str,
    cargo_cmd: str,
    attempt: int,
) -> None:
    print("#==== STDOUT")
    print_output(stdout)
    print("#==== STDERR")
    print_output(stderr)
    if test_case.flaky():
        print(f"RUN {attempt} of flaky test.")
    print(f"RUN: {cmdline}")
    print(f"RUN: {cargo_cmd}")
    sys.stdout.flush()


def check_process_result(test_case: TestCase, result: ProcessResult, options: RunnerOptions) -> bool | str:
    timeout_message = f"test timed out after {test_case.get_timeout(options)} seconds"
    if result.timeout:
        return timeout_message
    if options.check_only:
        if result.status != 0:
            return "semantic check failed"
        return True
    if test_case.expectation.fail:
        position, message = read_error_message(result.stderr)
        if result.status == 0:
            return "expected failure (test exited with 0)"
        if (
            test_case.expectation.code is not None
            and result.status != test_case.expectation.code
        ):
            return (
                f"expected failure ({test_case.expectation.code} expected but test returned"
                f" {result.status})"
            )
        if (
            test_case.expectation.position is not None
            and position != test_case.expectation.position
        ):
            return (
                "position does not match"
                f" ({position!r} != {test_case.expectation.position!r})"
            )
        if (
            test_case.expectation.message is not None
            and message != test_case.expectation.message
        ):
            return (
                "message does not match"
                f" ({message!r} != {test_case.expectation.message!r})"
            )
    elif result.status != 0:
        return f"expected success (0 expected but test returned {result.status})"

    if (
        test_case.expectation.stdout is not None
        and test_case.expectation.stdout != canonicalize(result.stdout)
    ):
        return "stdout does not match"
    if (
        test_case.expectation.stderr is not None
        and test_case.expectation.stderr != canonicalize(result.stderr)
    ):
        return "stderr does not match"
    return True


def print_result(test_case: TestCase, config: Config, test_result: TestResult) -> None:
    if test_result.status == "ignore":
        print(f"{test_case.file} ... ignore")
        return
    print(f"{test_case.file}.{config.name}... ", end="")
    if test_result.status == "passed":
        print("ok")
    elif test_result.status == "failed":
        suffix = f" ({test_result.message})" if test_result.message else ""
        print(f"failed{suffix}")
    else:
        raise RuntimeError(f"unknown status {test_result.status}")


def test_name(num: int) -> str:
    return "test" if num == 1 else "tests"


def next_test(
    worklist: List[Tuple[TestCase, Config]],
    sync: ThreadSynchronization,
    options: RunnerOptions,
) -> Optional[Tuple[TestCase, Config]]:
    if sync.cancelled():
        return None
    if not worklist:
        return None
    if options.stress:
        return random.choice(worklist)
    return worklist.pop()


def run_test(
    options: RunnerOptions,
    test_case: TestCase,
    config: Config,
    mutex: threading.Lock,
    attempt: int,
    process_manager: ProcessManager,
    stop_event: threading.Event,
) -> TestResult:
    if test_case.ignore():
        return TestResult.ignore(test_case, config)
    cmd_parts: List[str] = [binary_path(options)]

    def extend_with(text: str) -> None:
        if text:
            cmd_parts.extend(shlex.split(text))

    if config.flags:
        extend_with(config.flags)
    if test_case.enable_boots or config.enable_boots:
        cmd_parts.append("--boots")
    if options.check_only:
        cmd_parts.append("--check")
    if test_case.vm_args:
        extend_with(test_case.vm_args)
    if options.extra_args:
        extend_with(options.extra_args)
    cmd_parts.append(test_case.test_file)
    if test_case.args:
        extend_with(test_case.args)

    quoted_cmd = " ".join(shlex.quote(part) for part in cmd_parts)
    cargo_args = " ".join(shlex.quote(part) for part in cmd_parts[1:])
    cargo_cmd = f"cargo run -p dora -- {cargo_args}"
    if options.verbose:
        print(quoted_cmd)
    process_result = spawn_with_timeout(
        options.env_overrides,
        cmd_parts,
        test_case.get_timeout(options),
        process_manager,
    )
    evaluation = check_process_result(test_case, process_result, options)
    if evaluation is True:
        result = TestResult.success(test_case, config)
    else:
        result = TestResult.error(
            test_case,
            config,
            str(evaluation),
            process_result.stdout,
            process_result.stderr,
            quoted_cmd,
            cargo_cmd,
            attempt,
        )
    result.stdout = process_result.stdout
    result.stderr = process_result.stderr
    result.cmdline = quoted_cmd
    result.cargo_cmd = cargo_cmd
    result.attempt = attempt
    return result


def run_tests(options: RunnerOptions) -> bool:
    test_files = load_test_files(options)
    worklist = parse_test_files(options, test_files)
    random.shuffle(worklist)
    if options.stress and not worklist:
        print("--stress needs at least one test.")
        return False
    binary = binary_path(options)
    if not Path(binary).is_file():
        print(f"no executable {binary} found")
        return False

    threads: List[threading.Thread] = []
    synchronization = ThreadSynchronization()
    mutex = synchronization.mutex
    faillist: List[TestResult] = []
    passed = 0
    failed = 0
    ignored = 0
    start_time = time.time()
    process_manager = ProcessManager()
    stop_event = threading.Event()
    status_display = StatusDisplay(
        not options.print_tests, start_time
    )
    running_tests: "OrderedDict[str, str]" = OrderedDict()
    status_display.render(passed, failed, ignored, list(running_tests.values()))

    def request_stop() -> None:
        if stop_event.is_set():
            return
        stop_event.set()
        synchronization.cancel()
        process_manager.cancel_all()

    computed_processors = query_number_processors()
    if options.processors is not None:
        num_threads = max(1, options.processors)
    elif options.stress:
        num_threads = max(1, computed_processors * 2)
    else:
        num_threads = max(1, computed_processors)

    def worker() -> None:
        nonlocal passed, failed, ignored
        while True:
            if stop_event.is_set():
                break
            with mutex:
                test_with_config = next_test(worklist, synchronization, options)
                if test_with_config:
                    test_case, config = test_with_config
                    test_id = f"{test_case.file}.{config.name}"
                    running_tests[test_id] = test_id
                    status_display.render(
                        passed, failed, ignored, list(running_tests.values())
                    )
            if not test_with_config:
                break
            test_case, config = test_with_config
            test_id = f"{test_case.file}.{config.name}"
            attempt = 1
            while True:
                result = run_test(
                    options,
                    test_case,
                    config,
                    mutex,
                    attempt,
                    process_manager,
                    stop_event,
                )
                if (
                    result.status == "failed"
                    and test_case.flaky()
                    and attempt < 3
                    and not stop_event.is_set()
                ):
                    with mutex:
                        print(f"{test_case.file} ... failed - try again")
                    attempt += 1
                    continue
                break

            need_output = (not options.capture) or (result.status == "failed")

            with mutex:
                running_tests.pop(test_id, None)
                status_display.suspend()
                if stop_event.is_set():
                    break
                if result.status == "ignore":
                    ignored += 1
                elif result.status == "passed":
                    passed += 1
                elif result.status == "failed":
                    failed += 1
                    faillist.append(result)
                    if (
                        options.exit_after_n_failures is not None
                        and len(faillist) >= options.exit_after_n_failures
                    ):
                        synchronization.cancel()
                else:
                    raise RuntimeError(f"unknown status {result.status}")
                if (not need_output) and options.print_tests:
                    print_result(test_case, config, result)
                    sys.stdout.flush()
                status_display.render(
                    passed, failed, ignored, list(running_tests.values())
                )

            if stop_event.is_set():
                break

            if need_output:
                print_run_details(
                    test_case,
                    result.stdout,
                    result.stderr,
                    result.cmdline,
                    result.cargo_cmd,
                    result.attempt,
                )
                with mutex:
                    if options.print_tests:
                        print_result(test_case, config, result)
                        sys.stdout.flush()
                    status_display.render(
                        passed, failed, ignored, list(running_tests.values())
                    )

    try:
        for _ in range(num_threads):
            thread = threading.Thread(target=worker)
            thread.start()
            threads.append(thread)

        if options.stress and options.stress_timeout:
            synchronization.sleep_until_timeout_or_cancelled(options.stress_timeout)

        for thread in threads:
            thread.join()
    except KeyboardInterrupt:
        print("Interrupted. Stopping tests...")
        request_stop()
        for thread in threads:
            thread.join()
        status_display.finish()
        return False

    if stop_event.is_set():
        status_display.finish()
        return False

    status_display.finish()

    if faillist:
        print("\nFailed Tests:")
        for failed_result in faillist:
            print(f"= {failed_result.test_case.file}.{failed_result.config.name}")
            print_run_details(
                failed_result.test_case,
                failed_result.stdout,
                failed_result.stderr,
                failed_result.cmdline,
                failed_result.cargo_cmd,
                failed_result.attempt,
            )
            print()

    passed_text = f"{passed} {test_name(passed)} passed"
    failed_text = f"{failed} {test_name(failed)} failed"
    ignored_text = f"{ignored} {test_name(ignored)} ignored"
    print(f"{passed_text}; {ignored_text}; {failed_text}")
    if not options.stress:
        duration = time.time() - start_time
        print(f"Ran in {duration:.1f} seconds.")
    return failed == 0


def lookup_config(name: str) -> Config:
    for config in ALL_CONFIGS:
        if config.name == name:
            return config
    raise ValueError(f"unknown config {name}")


def _determine_select_config(argv: Sequence[str]) -> Optional[Tuple[str, Optional[str]]]:
    idx = 0
    selection: Optional[Tuple[str, Optional[str]]] = None
    while idx < len(argv):
        token = argv[idx]
        if token == "--select-config":
            if idx + 1 >= len(argv):
                raise ValueError("missing argument for --select-config")
            selection = ("select", argv[idx + 1])
            idx += 2
            continue
        if token.startswith("--select-config="):
            selection = ("select", token.split("=", 1)[1])
            idx += 1
            continue
        if token == "--default":
            selection = ("default", None)
            idx += 1
            continue
        if token == "--always-boots":
            selection = ("always", None)
            idx += 1
            continue
        idx += 1
    return selection


def process_arguments(argv: Sequence[str]) -> RunnerOptions:

    parser = argparse.ArgumentParser(description="Run Dora language tests")
    parser.add_argument("files", nargs="*", help="Specific test files or directories to run")
    parser.add_argument("-j", dest="processors", type=int, metavar="N", help="Number of worker threads")
    parser.add_argument("--timeout", dest="forced_timeout", type=int, metavar="SECONDS")
    parser.add_argument(
        "--stress",
        nargs="?",
        const=60,
        type=int,
        metavar="SECONDS",
        help="Stress mode duration in seconds (default 60)"
    )
    parser.add_argument(
        "--exit-after-n-failures",
        dest="exit_after_n_failures",
        type=int,
        metavar="N"
    )
    parser.add_argument("--target", dest="cargo_target", metavar="NAME")
    parser.add_argument("--env", dest="env", action="append", default=[], metavar="NAME=VALUE")
    parser.add_argument("--force-config", dest="force_config")
    parser.add_argument("--select-config", dest="select_config")
    parser.add_argument("--default", action="store_true", dest="default_config")
    parser.add_argument("--always-boots", action="store_true", dest="always_boots")
    parser.add_argument("--release", action="store_true")
    parser.add_argument("--extra-args", dest="extra_args")
    parser.add_argument("--capture", action=argparse.BooleanOptionalAction, default=True)
    parser.add_argument("--verbose", action="store_true")
    parser.add_argument("--check", action="store_true")
    parser.add_argument("--print-tests", action=argparse.BooleanOptionalAction, default=None)

    args = parser.parse_args(list(argv))

    files = list(args.files)
    processors = max(1, args.processors) if args.processors is not None else None
    forced_timeout = args.forced_timeout

    if args.stress is not None:
        stress = True
        stress_timeout = args.stress if args.stress >= 1 else 60
    else:
        stress = False
        stress_timeout = None

    env_overrides: Dict[str, str] = {}
    for assignment in args.env:
        if "=" not in assignment:
            parser.error("--env requires NAME=VALUE")
        name, value = assignment.split("=", 1)
        env_overrides[name] = value

    if args.force_config:
        force_config = lookup_config(args.force_config)
    else:
        force_config = None

    try:
        selection = _determine_select_config(list(argv))
    except ValueError as exc:  # pragma: no cover
        parser.error(str(exc))

    if selection is None:
        if args.select_config:
            selection = ("select", args.select_config)
        elif args.default_config:
            selection = ("default", None)
        elif args.always_boots:
            selection = ("always", None)

    if selection is None:
        select_config = None
    else:
        kind, value = selection
        if kind == "select":
            select_config = lookup_config(value)
        elif kind == "default":
            select_config = DEFAULT_CONFIG
        elif kind == "always":
            select_config = ALWAYS_BOOTS_CONFIG
        else:  # pragma: no cover
            select_config = None

    target = "release" if args.release else "debug"

    if args.print_tests is None:
        print_tests = not sys.stdout.isatty()
    else:
        print_tests = args.print_tests

    return RunnerOptions(
        target=target,
        capture=args.capture,
        stress=stress,
        stress_timeout=stress_timeout,
        processors=processors,
        forced_timeout=forced_timeout,
        cargo_target=args.cargo_target,
        files=files,
        exit_after_n_failures=args.exit_after_n_failures,
        env_overrides=env_overrides,
        verbose=args.verbose,
        check_only=args.check,
        extra_args=args.extra_args,
        force_config=force_config,
        select_config=select_config,
        print_tests=print_tests,
    )


def main(argv: Sequence[str]) -> int:
    options = process_arguments(list(argv))
    success = run_tests(options)
    return 0 if success else 1


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
