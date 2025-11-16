import argparse
import sys
from pathlib import Path
from typing import Dict, Sequence

from .config import REPO_ROOT, Config, ALL_CONFIGS, DEFAULT_CONFIG
from .options import RunnerOptions


def ensure_running_from_repo_root() -> None:
    current_dir = Path.cwd().resolve()
    if current_dir != REPO_ROOT:
        raise SystemExit(
            f"pytester must be run from the repository root: expected {REPO_ROOT}, got {current_dir}"
        )


def lookup_config(name: str) -> Config:
    for config in ALL_CONFIGS:
        if config.name == name:
            return config
    raise ValueError(f"unknown config {name}")


def process_arguments(argv: Sequence[str]) -> RunnerOptions:
    parser = argparse.ArgumentParser(description="Run Dora language tests")
    config_choices = [config.name for config in ALL_CONFIGS] + ["all"]
    parser.add_argument(
        "files", nargs="*", help="Specific test files or directories to run"
    )
    parser.add_argument(
        "-j", dest="processors", type=int, metavar="N", help="Number of worker threads"
    )
    parser.add_argument("--timeout", dest="forced_timeout", type=int, metavar="SECONDS")
    parser.add_argument(
        "--stress",
        nargs="?",
        const=60,
        type=int,
        metavar="SECONDS",
        help="Stress mode duration in seconds (default 60)",
    )
    parser.add_argument(
        "--exit-after-n-failures", dest="exit_after_n_failures", type=int, metavar="N"
    )
    parser.add_argument("--target", dest="cargo_target", metavar="NAME")
    parser.add_argument(
        "--env", dest="env", action="append", default=[], metavar="NAME=VALUE"
    )
    parser.add_argument("--force-config", dest="force_config")
    parser.add_argument(
        "--config",
        dest="config",
        choices=config_choices,
        help="Restrict tests to a specific configuration (default: all)",
    )
    parser.add_argument("--release", action="store_true")
    parser.add_argument("--extra-args", dest="extra_args")
    parser.add_argument(
        "--capture", action=argparse.BooleanOptionalAction, default=True
    )
    parser.add_argument("-v", "--verbose", action="store_true")
    parser.add_argument("--check", action="store_true")
    parser.add_argument(
        "--print-tests", action=argparse.BooleanOptionalAction, default=None
    )

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

    select_config = DEFAULT_CONFIG
    force_config = None

    if args.force_config:
        force_config = lookup_config(args.force_config)
    elif args.config:
        if args.config == "all":
            select_config = None
        else:
            select_config = lookup_config(args.config)

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
