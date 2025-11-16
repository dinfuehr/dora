from pytester.filecheck import _transform_check_file


def test_transform_strips_percent_p_on_posix():
    result = _transform_check_file("before %p(foo/bar) after", "posix")
    assert result == "before foo/bar after"


def test_transform_converts_slashes_on_windows():
    result = _transform_check_file("prefix %p(foo/bar) suffix", "nt")
    assert result == "prefix foo\\bar suffix"


def test_transform_leaves_non_matches_untouched():
    content = "no macros here"
    assert _transform_check_file(content, "posix") == content
