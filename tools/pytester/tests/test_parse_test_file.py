from pytester.config import AOT_CONFIG, CANNON_CONFIG, DEFAULT_CONFIG
from pytester.options import RunnerOptions
from pytester.tests import parse_test_file


def test_stdout_file_detected(tmp_path, monkeypatch):
    monkeypatch.setattr("pytester.tests.REPO_ROOT", tmp_path)

    dora_file = tmp_path / "hello.dora"
    dora_file.write_text("fn main() {}\n")
    stdout_file = tmp_path / "hello.stdout"
    stdout_file.write_text("Hello, world!\n")

    options = RunnerOptions(force_config=DEFAULT_CONFIG)
    results = parse_test_file(options, str(dora_file))

    assert len(results) == 1
    test_case, _config = results[0]
    assert test_case.expectation.stdout == "Hello, world!\n"


def test_stderr_file_detected(tmp_path, monkeypatch):
    monkeypatch.setattr("pytester.tests.REPO_ROOT", tmp_path)

    dora_file = tmp_path / "crash.dora"
    dora_file.write_text("//= error div0\nfn main() {}\n")
    stderr_file = tmp_path / "crash.stderr"
    stderr_file.write_text("division by 0\n")

    options = RunnerOptions(force_config=DEFAULT_CONFIG)
    results = parse_test_file(options, str(dora_file))

    assert len(results) == 1
    test_case, _config = results[0]
    assert test_case.expectation.stderr == "division by 0\n"


def test_no_stdout_or_stderr_without_files(tmp_path, monkeypatch):
    monkeypatch.setattr("pytester.tests.REPO_ROOT", tmp_path)

    dora_file = tmp_path / "plain.dora"
    dora_file.write_text("fn main() {}\n")

    options = RunnerOptions(force_config=DEFAULT_CONFIG)
    results = parse_test_file(options, str(dora_file))

    assert len(results) == 1
    test_case, _config = results[0]
    assert test_case.expectation.stdout is None
    assert test_case.expectation.stderr is None


def test_compile_and_runtime_args_detected(tmp_path, monkeypatch):
    monkeypatch.setattr("pytester.tests.REPO_ROOT", tmp_path)

    dora_file = tmp_path / "args.dora"
    dora_file.write_text(
        "\n".join(
            [
                '//= compile-args "--gc=swiper --boots"',
                '//= runtime-args "--gc-verify --max-heap-size=32M"',
                "fn main() {}",
            ]
        )
        + "\n"
    )

    options = RunnerOptions(force_config=DEFAULT_CONFIG)
    results = parse_test_file(options, str(dora_file))

    assert len(results) == 1
    test_case, _config = results[0]
    assert test_case.compile_args == ["--gc=swiper", "--boots"]
    assert test_case.runtime_args == ["--gc-verify", "--max-heap-size=32M"]


def test_aot_conditional_args_apply_to_cannon(tmp_path, monkeypatch):
    monkeypatch.setattr("pytester.tests.REPO_ROOT", tmp_path)

    dora_file = tmp_path / "args.dora"
    dora_file.write_text(
        "\n".join(
            [
                '//= runtime-args "--jit-only" unless aot',
                "fn main() {}",
            ]
        )
        + "\n"
    )

    options = RunnerOptions(force_config=CANNON_CONFIG)
    results = parse_test_file(options, str(dora_file))

    assert len(results) == 1
    test_case, _config = results[0]
    assert test_case.runtime_args == []


def test_all_configs_uses_defined_configs(tmp_path, monkeypatch):
    monkeypatch.setattr("pytester.tests.REPO_ROOT", tmp_path)
    monkeypatch.setattr("pytester.tests.ALL_CONFIGS", [DEFAULT_CONFIG, AOT_CONFIG])

    dora_file = tmp_path / "hello.dora"
    dora_file.write_text("fn main() {}\n")

    options = RunnerOptions(select_config=None)
    results = parse_test_file(options, str(dora_file))

    configs = [config for _test_case, config in results]
    assert DEFAULT_CONFIG in configs
    assert AOT_CONFIG in configs
    assert CANNON_CONFIG not in configs
