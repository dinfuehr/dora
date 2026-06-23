from pytester import env_with_dora_flags
from pytester.cli import process_arguments
from pytester.config import (
    BOOTS_CONFIG,
    CANNON_CONFIG,
)


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


def test_no_config_selects_all_configs():
    options = process_arguments([])

    assert options.select_config is None


def test_boots_config_selects_boots():
    options = process_arguments(["--config", "boots"])

    assert options.select_config is BOOTS_CONFIG


def test_cannon_config_selects_cannon():
    options = process_arguments(["--config", "cannon"])

    assert options.select_config is CANNON_CONFIG


def test_aot_flag_selects_boots():
    options = process_arguments(["--aot"])

    assert options.select_config is BOOTS_CONFIG
