from __future__ import annotations

from dataclasses import dataclass, field
from pathlib import Path
from typing import List

SCRIPT_DIR = Path(__file__).resolve().parent
REPO_ROOT = SCRIPT_DIR.parent.parent.parent.parent.resolve()
TESTS_DIR = REPO_ROOT / "tests"


@dataclass
class Config:
    name: str
    flags: List[str] = field(default_factory=list)
    include_dirs: List[Path] = field(default_factory=list)
    exclude_dirs: List[Path] = field(default_factory=list)
    enable_boots: bool = False

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


DEFAULT_CONFIG = Config("default")
ALWAYS_BOOTS_CONFIG = Config(
    "always_boots", flags=["--always-boots"], exclude_dirs=["opt"], enable_boots=True
)
ALL_CONFIGS = [DEFAULT_CONFIG, ALWAYS_BOOTS_CONFIG]
