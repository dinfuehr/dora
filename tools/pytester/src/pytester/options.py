from dataclasses import dataclass, field
from typing import Dict, List, Optional
from .config import Config


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
