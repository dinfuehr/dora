from __future__ import annotations

import io
import os
import re
import tempfile
from pathlib import Path
from typing import Match, Optional, Tuple

from filecheck.finput import FInput
from filecheck.matcher import Matcher
from filecheck.options import DumpInputKind, Options
from filecheck.parser import Parser

P_PERCENT_RE = re.compile(r"%p\(([^)]*)\)")


def _transform_check_file(content: str, os_name: str) -> str:
    def replacer(match: Match[str]) -> str:
        inner = match.group(1)
        if os_name == "nt":
            inner = inner.replace("/", "\\")
        return inner

    return P_PERCENT_RE.sub(replacer, content)


def _prepare_check_file(check_file: Path) -> Tuple[Path, Optional[Path]]:
    content = check_file.read_text(encoding="utf-8")
    transformed = _transform_check_file(content, os.name)
    tmp = tempfile.NamedTemporaryFile("w", delete=False, encoding="utf-8")
    try:
        tmp.write(transformed)
        tmp.flush()
    finally:
        tmp.close()
    return Path(tmp.name), Path(tmp.name)


def run_filecheck(check_file: Path, stdout: str) -> Optional[str]:
    prepared_check_file, temporary_check_file = _prepare_check_file(check_file)
    try:
        opts = Options(
            match_filename=str(prepared_check_file),
            input_file="<stdin>",
            allow_empty=True,
            check_prefixes="CHECK",
            dump_input=DumpInputKind.NEVER,
        )
        parser = Parser.from_opts(opts)
        finput = FInput("<stdin>", FInput.canonicalize_line_ends(stdout))
        matcher = Matcher(opts, finput, parser)
        buffer = io.StringIO()
        matcher.stderr = buffer
        try:
            exit_code = matcher.run()
        finally:
            parser.input.close()
    finally:
        if temporary_check_file is not None:
            try:
                temporary_check_file.unlink()
            except OSError:
                pass

    if exit_code == 0:
        return None

    details = buffer.getvalue().strip()
    if not details:
        details = "FileCheck reported an unknown error."
    return f"FileCheck failed for {check_file}:\n{details}"
