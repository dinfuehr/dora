from __future__ import annotations

import os
import platform
from dataclasses import dataclass, field
from pathlib import Path
from typing import List

SCRIPT_DIR = Path(__file__).resolve().parent
REPO_ROOT = SCRIPT_DIR.parent.parent.parent.parent.resolve()
TESTS_DIR = REPO_ROOT / "test" / "rt"


@dataclass
class Config:
    name: str
    flags: List[str] = field(default_factory=list)
    include_dirs: List[Path] = field(default_factory=list)
    exclude_dirs: List[Path] = field(default_factory=list)

    def __post_init__(self) -> None:
        self.include_dirs = [self._normalize_path(entry) for entry in self.include_dirs]
        self.exclude_dirs = [self._normalize_path(entry) for entry in self.exclude_dirs]

    def enabled_for(self, test_dir: Path) -> bool:
        normalized_dir = test_dir.resolve()
        if any(self._is_within(normalized_dir, entry) for entry in self.exclude_dirs):
            return False
        if not self.include_dirs:
            return True
        return any(
            self._is_within(normalized_dir, entry) for entry in self.include_dirs
        )

    @staticmethod
    def _normalize_path(path: Path | str) -> Path:
        candidate = Path(path)
        if candidate.is_absolute():
            return candidate.resolve()
        return (TESTS_DIR / candidate).resolve()

    @staticmethod
    def _is_within(test_dir: Path, candidate: Path) -> bool:
        try:
            test_dir.relative_to(candidate)
            return True
        except ValueError:
            return False


def detect_architecture() -> str:
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


def supports_aot() -> bool:
    if OS_NAME == "linux":
        return ARCH in ("x64", "arm64")
    if OS_NAME == "macos":
        return ARCH == "arm64"
    return False


AOT_CONFIG = Config("default")
DEFAULT_CONFIG = AOT_CONFIG
CANNON_CONFIG = Config("cannon")
AOT_SUPPORTED = supports_aot()
ALL_CONFIGS = []
if AOT_SUPPORTED:
    ALL_CONFIGS.extend([DEFAULT_CONFIG, CANNON_CONFIG])
NAMED_CONFIGS = [DEFAULT_CONFIG, CANNON_CONFIG]
