from __future__ import annotations

import importlib.resources
import os
import subprocess
from pathlib import Path

import pytest

from fvs2py.enums import FvsVariant


@pytest.mark.parametrize("variant", FvsVariant)
def test_fvs_build(variant: FvsVariant, tmp_path: Path):
    """
    Test to confirm each FVS regional variant has been built successfully.

    Creates a keyfile in a temporary directory and runs it, confirming that:
        1. The FVS executable exists
        2. The keyfile for testing the FVS variant exists.
        3. The execution of FVS generated an *.out file
        4. The *.out file didn't include a warning
        5. The *.out file didn't include an error
        6. The return code from executing FVS was 0 (successful)

    Removes the FVS *.out file as well.

    Args:
      variant (FvsVariant): an FVS variant code that is a member of the FvsVariant enum
      tmp_path (Generator[Path]): a Pytest automatically-inserted "fixture", generates a
        temporary path
    """
    fvs = f"/usr/local/bin/FVS{variant.lower()}"

    keyfile = f"{tmp_path}/{variant}_buildtest.key"
    keyfile_content = (
        importlib.resources.files("fvs2py.tests.keyfiles")
        .joinpath(f"{variant}.key")
        .read_text()
    )

    with open(keyfile, "w") as f:
        f.write(keyfile_content)
    outfile = f"{tmp_path}/{variant}_buildtest.out"
    proc = subprocess.run([fvs, f"--keywordfile={keyfile}"])

    assert os.path.exists(keyfile)
    assert os.path.exists(outfile)
    with open(outfile) as f:
        outdata = f.read()
    assert "WARNING:" not in outdata
    assert "ERROR:" not in outdata
    assert proc.returncode == 0
