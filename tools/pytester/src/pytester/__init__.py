#!/usr/bin/env python3
from __future__ import annotations

import multiprocessing
import os
import random
import re
import signal
import subprocess
import sys
import threading
import time
import shlex
from collections import OrderedDict
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Optional, Sequence, Tuple, Set
from .config import Config
from .filecheck import run_filecheck
from .options import RunnerOptions
from .tests import TestCase, parse_test_files, load_test_files
from .cli import ensure_running_from_repo_root, process_arguments


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
    def __init__(self, enabled: bool, start_time: float, total_tests: int) -> None:
        self.enabled = enabled
        self.start_time = start_time
        self.active = False
        self.line_count = 0
        self.total_tests = total_tests

    def render(
        self,
        passed: int,
        failed: int,
        ignored: int,
        running_tests: List[Tuple[str, float]],
    ) -> None:
        if not self.enabled:
            return
        self._clear()
        slow_tests = []
        now = time.time()

        threshold = 7.0

        for name, start in running_tests:
            duration = now - start
            if duration >= threshold:
                slow_tests.append((name, duration))

        sorted_tests = [
            f"{name} ({duration:.1f}s)" for name, duration in sorted(slow_tests)
        ]
        duration = self._format_duration()
        lines = [
            f"{self.total_tests} total; {passed} passed; {ignored} ignored; {failed} failed; {len(running_tests)} running; running for {duration}."
        ]
        if sorted_tests:
            lines.append(f"Slow tests (>{threshold:.1f}s):")
            lines.extend(sorted_tests)
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


def check_process_result(
    test_case: TestCase, result: ProcessResult, options: RunnerOptions
) -> bool | str:
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

    if config.flags:
        cmd_parts.extend(config.flags)
    if test_case.enable_boots or config.enable_boots:
        cmd_parts.append("--boots")
    if options.check_only:
        cmd_parts.append("--check")
    if test_case.vm_args:
        cmd_parts.extend(test_case.vm_args)
    if options.extra_args:
        cmd_parts.extend(options.extra_args)
    cmd_parts.append(test_case.test_file)
    if test_case.args:
        cmd_parts.extend(test_case.args)

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
        filecheck_error: Optional[str] = None
        if test_case.expectation.filecheck_path is not None:
            filecheck_error = run_filecheck(
                test_case.expectation.filecheck_path, process_result.stdout
            )

        if filecheck_error is None:
            result = TestResult.success(test_case, config)
        else:
            stderr_output = process_result.stderr
            if stderr_output:
                stderr_output = stderr_output.rstrip("\n") + "\n"
            stderr_output += filecheck_error + "\n"
            result = TestResult.error(
                test_case,
                config,
                "filecheck failed",
                process_result.stdout,
                stderr_output,
                quoted_cmd,
                cargo_cmd,
                attempt,
            )
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
    if result.status == "passed":
        result.stdout = process_result.stdout
        result.stderr = process_result.stderr
    result.cmdline = quoted_cmd
    result.cargo_cmd = cargo_cmd
    result.attempt = attempt
    return result


def run_tests(options: RunnerOptions) -> bool:
    test_files = load_test_files(options)
    worklist = parse_test_files(options, test_files)
    total_tests = len(worklist)
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
    status_display = StatusDisplay(not options.print_tests, start_time, total_tests)
    running_tests: "OrderedDict[str, float]" = OrderedDict()
    status_display.render(passed, failed, ignored, list(running_tests.items()))

    def request_stop() -> None:
        if stop_event.is_set():
            return
        stop_event.set()
        synchronization.cancel()
        process_manager.cancel_all()

    try:
        computed_processors = multiprocessing.cpu_count()
    except NotImplementedError:
        computed_processors = 1
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
                    running_tests[test_id] = time.time()
                    status_display.render(
                        passed, failed, ignored, list(running_tests.items())
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
                    passed, failed, ignored, list(running_tests.items())
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
                        passed, failed, ignored, list(running_tests.items())
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
        request_stop()
        for thread in threads:
            thread.join()
        print("Interrupted. Stopping tests...")

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

    if stop_event.is_set():
        return False

    return failed == 0


def main(argv: Sequence[str]) -> int:
    ensure_running_from_repo_root()
    options = process_arguments(list(argv))
    success = run_tests(options)
    return 0 if success else 1


def cli() -> None:
    """Entry point for uv's `[project.scripts]`."""
    raise SystemExit(main(sys.argv[1:]))


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
