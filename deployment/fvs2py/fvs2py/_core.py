from __future__ import annotations

import ctypes as ct
import logging
import os
from pathlib import Path

from fvs2py.common import load_dll, unload_dll
from fvs2py.constants import NEEDED_ROUTINES


class FvsCore:
    """Base class for FVS API wrapper."""

    def __init__(self, lib_path: str | os.PathLike):
        """Loads FVS shared library and checks to ensure needed routines exist.

        Args:
          lib_path : path to FVS library
        """
        self.lib_path: Path = Path(os.path.abspath(lib_path))
        self._lib: ct.CDLL | None = None
        self.variant: str = (
            os.path.basename(self.lib_path)
            .split(".")[0]
            .split("FVS")[-1]
            .upper()
        )

        # Declare function attributes with type annotations for mypy
        # These are ctypes foreign function pointers loaded from the shared library
        self._fvs: ct._FuncPointer
        self._fvsAddActivity: ct._FuncPointer
        self._fvsAddTrees: ct._FuncPointer
        self._fvsDimSizes: ct._FuncPointer
        self._fvsEvmonAttr: ct._FuncPointer
        self._fvsFFEAttrs: ct._FuncPointer
        self._fvsGetRestartCode: ct._FuncPointer
        self._fvsGetRtnCode: ct._FuncPointer
        self._fvsGetICCode: ct._FuncPointer
        self._fvsSVSDimSizes: ct._FuncPointer
        self._fvsSetStoppointCodes: ct._FuncPointer
        self._fvsSetCmdLine: ct._FuncPointer
        self._fvsSVSObjData: ct._FuncPointer
        self._fvsSpeciesAttr: ct._FuncPointer
        self._fvsSpeciesCode: ct._FuncPointer
        self._fvsStandID: ct._FuncPointer
        self._fvsSummary: ct._FuncPointer
        self._fvsTreeAttr: ct._FuncPointer
        self._fvsUnitConversion: ct._FuncPointer

        self._load_fvs()
        assert self._lib is not None  # to satisfy type checker

        # check for needed routines that are missing
        missing = []

        for routine in NEEDED_ROUTINES:
            if hasattr(self._lib, routine) and callable(
                getattr(self._lib, routine)
            ):
                logging.debug(f"Found {routine} as expected.")
                # anticipate subroutine name changes depending upon compiler and OS
                # unix pattern on fortran functions
            elif hasattr(self._lib, f"{routine.lower()}_") and callable(
                getattr(self._lib, f"{routine.lower()}_")
            ):
                logging.debug(f"Found {routine} renamed as {routine.lower()}_.")
            else:
                missing.append(routine)

        if len(missing) > 0:
            msg = " ".join(
                [
                    ", ".join(missing),
                    "are needed routines that are not available in library, "
                    "(maybe they weren't exported when library was built)",
                ]
            )
            raise ImportError(msg)
        try:
            self._fvs = self._lib.fvs_
            self._fvsAddActivity = self._lib.fvsaddactivity_
            self._fvsAddTrees = self._lib.fvsaddtrees_
            self._fvsDimSizes = self._lib.fvsdimsizes_
            self._fvsEvmonAttr = self._lib.fvsevmonattr_
            self._fvsFFEAttrs = self._lib.fvsffeattrs_
            self._fvsGetRestartCode = self._lib.fvsgetrestartcode_
            self._fvsGetRtnCode = self._lib.fvsgetrtncode_
            self._fvsGetICCode = self._lib.fvsgeticcode_
            self._fvsSVSDimSizes = self._lib.fvssvsdimsizes_
            self._fvsSetStoppointCodes = self._lib.fvssetstoppointcodes_
            self._fvsSetCmdLine = self._lib.fvssetcmdline_
            self._fvsSVSObjData = self._lib.fvssvsobjdata_
            self._fvsSpeciesAttr = self._lib.fvsspeciesattr_
            self._fvsSpeciesCode = self._lib.fvsspeciescode_
            self._fvsStandID = self._lib.fvsstandid_
            self._fvsSummary = self._lib.fvssummary_
            self._fvsTreeAttr = self._lib.fvstreeattr_
            self._fvsUnitConversion = self._lib.fvsunitconversion_
        except AttributeError:
            self._fvs = self._lib.fvs
            self._fvsAddActivity = self._lib.fvsAddActivity
            self._fvsAddTrees = self._lib.fvsAddTrees
            self._fvsDimSizes = self._lib.fvsDimSizes
            self._fvsEvmonAttr = self._lib.fvsEvmonAttr
            self._fvsFFEAttrs = self._lib.fvsFFEAttrs
            self._fvsGetRestartCode = self._lib.fvsGetRestartCode
            self._fvsGetRtnCode = self._lib.fvsGetRtnCode
            self._fvsGetICCode = self._lib.fvsGetICCode
            self._fvsSVSDimSizes = self._lib.fvsSVSDimSizes
            self._fvsSetStoppointCodes = self._lib.fvsSetStoppointCodes
            self._fvsSetCmdLine = self._lib.fvsSetCmdLine
            self._fvsSVSObjData = self._lib.fvsSVSObjData
            self._fvsSpeciesAttr = self._lib.fvsSpeciesAttr
            self._fvsSpeciesCode = self._lib.fvsSpeciesCode
            self._fvsStandID = self._lib.fvsStandID
            self._fvsSummary = self._lib.fvsSummary
            self._fvsTreeAttr = self._lib.fvsTreeAttr
            self._fvsUnitConversion = self._lib.fvsUnitConversion

        return

    def _load_fvs(self) -> None:
        if self._lib:
            logging.debug("Unloading existing library.")
            self._unload_fvs()
        self._lib = load_dll(self.lib_path)
        return

    def _unload_fvs(self) -> None:
        if self._lib:
            unload_dll(self._lib)
            self._lib = None
        else:
            logging.debug("No library to unload.")
        return
