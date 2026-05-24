from pytester import env_with_dora_flags
from pytester.cli import process_arguments


def test_env_with_dora_flags_appends_runtime_args(monkeypatch):
    monkeypatch.setenv("DORA_FLAGS", "--gc-verbose")

    env, dora_flags = env_with_dora_flags(
        {"DORA_FLAGS": "--gc-stress"}, ["--gc-verify", "--max-heap-size=32M"]
    )

    assert env["DORA_FLAGS"] == "--gc-stress --gc-verify --max-heap-size=32M"
    assert dora_flags == "--gc-stress --gc-verify --max-heap-size=32M"


def test_all_config_selects_all_configs():
    options = process_arguments(["--config", "all"])

    assert options.select_config is None
