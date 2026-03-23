import importlib.resources
import os

import numpy as np
import pandas as pd
import pytest

from fvs2py._base import FVS
from fvs2py.constants import (
    FVS_ITRNCD_FINISHED_ALL_STANDS,
    FVS_ITRNCD_GOOD_RUNNING_STATE,
    FVS_ITRNCD_NOT_STARTED,
    MGMT_ID_COLUMN_NAME,
    SPECIES_ATTRS,
    SPECIES_COLUMN_NAMES,
    STAND_ID_COLUMN_NAME,
    STR_MAXSPECIES,
    STR_NCYCLES,
    STR_NPLOTS,
    STR_NTREES,
    SUMMARY_COLS,
)
from fvs2py.enums import FvsVariant

TEST_DLL = "/usr/local/lib/FVSso.so"
TEST_KEYFILE_PATH = importlib.resources.files("fvs2py.tests.keyfiles").joinpath(
    "SO.key"
)
FVS_RESTART_CODE_DONE_RUNNING_STAND = 100
FVS_RESTART_CODE_INITIALIZED = 0
FVS_STOP_POINT_CODE_AFTER_FIRST_EVMON = 2

FVS_EXIT_CODE_INPUT_DATA_ERROR = 1
FVS_EXIT_CODE_KEYWORD_ERROR = 2
FVS_EXIT_CODE_NO_ERROR = 0

FVSSO_START_DIMS = {
    "ntrees": 0,
    "ncycles": 0,
    "nplots": 0,
    "maxtrees": 3000,
    "maxspecies": 33,
    "maxplots": 500,
    "maxcycles": 40,
}
FVSOP_START_DIMS = {
    "ntrees": 0,
    "ncycles": 0,
    "nplots": 0,
    "maxtrees": 2000,
    "maxspecies": 39,
    "maxplots": 500,
    "maxcycles": 40,
}
SO_KEYFILE_STAND_IDS = {"stand_id": "12345", "stand_cn": "", "mgmt_id": "NONE"}


@pytest.fixture
def fvs():  # setup and teardown for FVS loaded from DLL/SO
    # Setup code executed before each test function
    fvs = FVS(TEST_DLL)
    yield fvs
    # Teardown code executed after each test function
    fvs._unload_fvs()


@pytest.fixture
def keyfile(tmp_path):
    keyfile_path = tmp_path / "test_keyfile.key"
    database_path = tmp_path / "FVSOut.db"

    # need to make sure keyfile puts FVSOut.db in temp path so it's torn down
    # even though TEST_KEYFILE doesn't include command to generate a DB, if
    # FVS encounters an error or warning, it will generate one anyways
    keyfile_content = TEST_KEYFILE_PATH.read_text().replace(
        "STDIDENT", f"DATABASE\nDSNOUT\n{database_path}\nEND\nSTDIDENT"
    )

    with open(keyfile_path, "w") as f:
        f.write(keyfile_content)

    return keyfile_path


def test_load_keyfile(fvs, keyfile):
    """Checks that keyfile attributes get populated and itrncd updates."""
    assert fvs.itrncd == FVS_ITRNCD_NOT_STARTED
    assert fvs.keyfile is None
    assert fvs.keyfile_path is None

    fvs.load_keyfile(keyfile)
    assert fvs.itrncd == FVS_ITRNCD_GOOD_RUNNING_STATE
    assert fvs.keyfile_path == keyfile
    assert fvs.keyfile == keyfile.read_text()


def test_stop_points(fvs):
    """Checks that stop point code and year works."""
    assert fvs.stop_point_code is None
    assert fvs.stop_point_year is None

    fvs.set_stop_point_codes(-1, 0)
    assert fvs.stop_point_code == -1
    assert fvs.stop_point_year == 0


def test_stop_point_year_without_stop_point_code(fvs):
    assert fvs.stop_point_code is None
    assert fvs.stop_point_year is None
    match_msg = (
        "Must specify stop_point_year if also specifying stop_point_code"
    )
    with pytest.raises(ValueError, match=match_msg):
        fvs.set_stop_point_codes(None, 0)


@pytest.mark.parametrize("stop_point_code", range(-2, 10))
def test_invalid_stop_point_code(fvs, stop_point_code):
    assert fvs.stop_point_code is None
    assert fvs.stop_point_year is None
    match_msg = "Invalid value for stop_point_code"
    if stop_point_code not in range(-1, 8):
        with pytest.raises(ValueError, match=match_msg):
            fvs.set_stop_point_codes(stop_point_code, 0)
    else:
        fvs.set_stop_point_codes(stop_point_code, 0)
        assert fvs.stop_point_code == stop_point_code
        assert fvs.stop_point_year == 0


def test_run_with_keyfile_succeeds(fvs, keyfile):
    assert fvs.itrncd == FVS_ITRNCD_NOT_STARTED

    fvs.load_keyfile(keyfile)
    assert fvs.restart_code == FVS_RESTART_CODE_INITIALIZED
    fvs.run()
    assert fvs.restart_code == FVS_RESTART_CODE_DONE_RUNNING_STAND
    assert fvs.itrncd == FVS_ITRNCD_GOOD_RUNNING_STATE
    fvs.run()
    assert fvs.itrncd == FVS_ITRNCD_FINISHED_ALL_STANDS

    assert os.path.exists(f"{keyfile.parent}/test_keyfile.out")


def test_run_without_keyfile_raises(fvs):
    with pytest.raises(AttributeError, match="No keyfile loaded yet."):
        fvs.run()


def test_restart_codes_match_stop_point_codes(fvs, keyfile):
    assert fvs.itrncd == FVS_ITRNCD_NOT_STARTED
    assert fvs.stop_point_code is None
    assert fvs.stop_point_year is None
    assert fvs.restart_code == FVS_RESTART_CODE_INITIALIZED

    fvs.load_keyfile(keyfile)
    assert fvs.restart_code == FVS_RESTART_CODE_INITIALIZED
    fvs.run(
        stop_point_code=FVS_STOP_POINT_CODE_AFTER_FIRST_EVMON,
        stop_point_year=2010,
    )
    assert fvs.restart_code == FVS_STOP_POINT_CODE_AFTER_FIRST_EVMON
    assert fvs.itrncd == FVS_ITRNCD_GOOD_RUNNING_STATE
    fvs.run()
    assert fvs.restart_code == FVS_RESTART_CODE_DONE_RUNNING_STAND
    assert fvs.itrncd == FVS_ITRNCD_GOOD_RUNNING_STATE
    fvs.run()
    assert fvs.itrncd == FVS_ITRNCD_FINISHED_ALL_STANDS
    assert os.path.exists(f"{keyfile.parent}/test_keyfile.out")


def test_exit_code_valid_run(fvs, keyfile):
    assert fvs.exit_code == 0
    fvs.load_keyfile(keyfile)
    fvs.run()
    assert fvs.exit_code == 0


def test_exit_code_keyword_error(fvs, keyfile):
    assert fvs.exit_code == FVS_EXIT_CODE_NO_ERROR
    keyfile_content = keyfile.read_text()

    with open(keyfile, "w") as f:
        f.write(keyfile_content.replace("NUMCYCLE        10.0", "NUMCYCLE"))

    fvs.load_keyfile(keyfile)
    fvs.run()
    assert fvs.exit_code == FVS_EXIT_CODE_KEYWORD_ERROR


def test_exit_code_input_data_error(fvs, keyfile):
    assert fvs.exit_code == FVS_EXIT_CODE_NO_ERROR
    keyfile_content = keyfile.read_text()
    with open(keyfile, "w") as f:
        f.write(keyfile_content.replace("CDS612", "ABCDEF"))
    fvs.load_keyfile(keyfile)
    fvs.run()
    assert fvs.exit_code == FVS_EXIT_CODE_INPUT_DATA_ERROR


def test_dims(keyfile):
    fvs = FVS(TEST_DLL)
    assert fvs.dims == FVSSO_START_DIMS

    fvs.load_keyfile(keyfile)
    fvs.run(
        -1
    )  # run until first possible stop point (just load trees + keyfile params)
    assert fvs.dims[STR_NTREES] == 2
    assert fvs.dims[STR_NCYCLES] == 10
    assert fvs.dims[STR_NPLOTS] == 1
    fvs._unload_fvs()

    fvs = FVS(TEST_DLL.replace("FVSso", "FVSop"))
    assert fvs.dims == FVSOP_START_DIMS
    fvs._unload_fvs()


def test_stand_ids_without_load(fvs, keyfile):
    with pytest.raises(AttributeError, match="Keyfile not loaded yet."):
        fvs.stand_ids

    fvs.load_keyfile(keyfile)

    with pytest.raises(
        RuntimeError, match="No inventory data loaded yet. Call `run` method."
    ):
        fvs.stand_ids

    fvs.run(-1)
    assert fvs.stand_ids == SO_KEYFILE_STAND_IDS


def test_stand_ids_rename(fvs, keyfile):
    keyfile_content = keyfile.read_text()

    NEW_MGMT_ID = "WHAT"
    NEW_STAND_ID = "6789"
    with open(keyfile, "w") as f:
        f.write(
            keyfile_content.replace(
                "PROCESS", f"MGMTID\n{NEW_MGMT_ID}\nPROCESS"
            ).replace("12345", NEW_STAND_ID)
        )
    fvs.load_keyfile(keyfile)
    fvs.run(-1)
    assert fvs.stand_ids[MGMT_ID_COLUMN_NAME] == NEW_MGMT_ID
    assert fvs.stand_ids[STAND_ID_COLUMN_NAME] == NEW_STAND_ID


def test_summary(fvs, keyfile):
    fvs.load_keyfile(keyfile)
    pre_summary = fvs.summary
    assert pre_summary is None

    fvs.run()
    dims = fvs.dims
    ncycles = dims[STR_NCYCLES]
    post_summary = fvs.summary

    assert isinstance(post_summary, pd.DataFrame)
    assert post_summary.shape == (ncycles + 1, len(SUMMARY_COLS))
    assert set(post_summary.columns) == set(SUMMARY_COLS)
    assert (post_summary.dtypes == np.intc).all()


@pytest.mark.parametrize("variant", FvsVariant)
def test_species_codes(variant):
    fvs = FVS(f"/usr/local/lib/FVS{variant.lower()}.so")
    dims = fvs.dims
    max_species = dims[STR_MAXSPECIES]

    assert set(fvs.species_codes.columns) == set(SPECIES_COLUMN_NAMES)
    assert fvs.species_codes.shape == (max_species, len(SPECIES_COLUMN_NAMES))
    fvs._unload_fvs()


@pytest.mark.parametrize("variant", FvsVariant)
def test_species_attrs(variant, keyfile, recwarn):
    fvs = FVS(f"/usr/local/lib/FVS{variant.lower()}.so")

    # warning should be raised if run hasn't been started
    fvs.species_attrs
    assert len(recwarn) == 1
    w = recwarn.pop(UserWarning)
    assert str(w.message) == "No species attributes initialized yet."

    fvs.load_keyfile(keyfile)
    fvs.run(-1)
    # No warning once run has been started
    fvs.species_attrs
    assert len(recwarn) == 0

    dims = fvs.dims
    max_species = dims[STR_MAXSPECIES]
    assert set(fvs.species_attrs.columns) == set(SPECIES_ATTRS)
    assert fvs.species_attrs.shape == (max_species, len(SPECIES_ATTRS))
    fvs._unload_fvs()


def test_species_attr_get_set(fvs, keyfile):
    fvs.load_keyfile(keyfile)
    fvs.run(-1)

    for attr in SPECIES_ATTRS:
        before = fvs.get_species_attr(attr)
        fvs.set_species_attr(attr, before + 1.0)
        after = fvs.get_species_attr(attr)
        assert np.isclose(after, (before + 1.0)).all()


def test_reload_fvs(fvs, keyfile):
    fvs.load_keyfile(keyfile)
    fvs.run()
    assert fvs.itrncd == FVS_ITRNCD_GOOD_RUNNING_STATE
    fvs._reload_fvs()
    assert fvs.itrncd == FVS_ITRNCD_NOT_STARTED


def test_load_keyfile_after_run(fvs, keyfile):
    fvs.load_keyfile(keyfile)
    fvs.run()
    assert fvs.itrncd == FVS_ITRNCD_GOOD_RUNNING_STATE
    assert fvs.summary is not None  # results of first run exist
    fvs.run()  # conclude the run
    assert fvs.itrncd == FVS_ITRNCD_FINISHED_ALL_STANDS
    assert fvs.summary is not None  # results of first run exist
    fvs.load_keyfile(keyfile)
    assert fvs.itrncd == FVS_ITRNCD_GOOD_RUNNING_STATE
    assert fvs.summary is None  # results of first run were cleared


def test_decorator_requires_fvs_library_following_unload(fvs):
    fvs._unload_fvs()
    with pytest.raises(
        RuntimeError, match="FVS library not loaded, unable to call run."
    ):
        fvs.run()


def test_fvs_methods_require_fvs_library(fvs):
    fvs._unload_fvs()
    with pytest.raises(
        RuntimeError,
        match="FVS library not loaded, unable to access species_attrs property.",
    ):
        fvs.species_attrs


def test_keyfile_reload_warning(fvs, keyfile, recwarn):
    # check that warning is raised if keyfile is loaded after the run
    fvs.load_keyfile(keyfile)
    fvs.run()
    assert fvs.itrncd == FVS_ITRNCD_GOOD_RUNNING_STATE
    fvs.load_keyfile(keyfile)
    assert len(recwarn) == 1
    w = recwarn.pop(UserWarning)
    assert (
        str(w.message)
        == "FVS had not completed the previous simulation. Outputs from that simulation may be incomplete."
    )
