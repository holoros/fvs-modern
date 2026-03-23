import pytest

from fvs2py._core import FvsCore
from fvs2py.constants import NEEDED_ROUTINES


@pytest.mark.usefixtures("mock_valid_fvs_dll")
def test_valid_cdll_load():
    fvs = FvsCore("/not/a/real/dir/FVSxx.so")

    assert fvs.variant == "XX"
    for routine in NEEDED_ROUTINES:
        assert hasattr(fvs, f"_{routine}")


@pytest.mark.usefixtures("mock_invalid_fvs_dll")
def test_missing_routines():
    msg = " ".join(
        [
            ", ".join([*NEEDED_ROUTINES[1:]]),
            "are needed routines that are not available in library, "
            "(maybe they weren't exported when library was built)",
        ]
    )
    with pytest.raises(ImportError) as excinfo:
        FvsCore("/not/a/real/dir/FVSxx.so")

    assert excinfo.type is ImportError
    assert str(excinfo.value) == msg


@pytest.mark.usefixtures("mock_another_invalid_fvs_dll")
def test_routine_not_callable():
    msg = " ".join(
        [
            ", ".join(NEEDED_ROUTINES),
            "are needed routines that are not available in library, "
            "(maybe they weren't exported when library was built)",
        ]
    )
    with pytest.raises(ImportError) as excinfo:
        FvsCore("/not/a/real/dir/FVSxx.so")

    assert excinfo.type is ImportError
    assert str(excinfo.value) == msg


@pytest.mark.usefixtures("mock_valid_reformatted_fvs_dll")
def test_cdll_load_reformatted_routines():
    fvs = FvsCore("/not/a/real/dir/FVSyz.so")

    assert fvs.variant == "YZ"
    for routine in NEEDED_ROUTINES:
        assert hasattr(fvs, f"_{routine}")
