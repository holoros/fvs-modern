"""Helper functions for generating the FVS Stand Stock table."""

from __future__ import annotations

import warnings
from collections.abc import Sequence

import numpy as np
import pandas as pd

from microfvs.enums import FvsOutputTableName

ALL_DIAMETER_CLASSES = "All"
CASE_ID_COLUMN_NAME = "caseid"
DIAMETER_CLASS_COLUMN_NAME = "dbh_class"
FORESTERS_CONSTANT = (
    0.005454154  # used to translate DBH in inches, to basal area ft2
)
HARVESTED_BASAL_AREA_COLUMN_NAME = "hrvba"
HARVESTED_BOARD_FEET_COLUMN_NAME = "hrvbdft"
HARVESTED_MERCH_CUBIC_FEET_COLUMN_NAME = "hrvmcuft"
HARVESTED_TOTAL_CUBIC_FEET_COLUMN_NAME = "hrvtcuft"
HARVESTED_TREES_PER_ACRE_COLUMN_NAME = "hrvtpa"
LIVE_BASAL_AREA_COLUMN_NAME = "liveba"
LIVE_BOARD_FEET_COLUMN_NAME = "livebdft"
LIVE_MERCH_CUBIC_FEET_COLUMN_NAME = "livemcuft"
LIVE_TOTAL_CUBIC_FEET_COLUMN_NAME = "livetcuft"
LIVE_TREES_PER_ACRE_COLUMN_NAME = "livetpa"
MORTALITY_BASAL_AREA_COLUMN_NAME = "mrtba"
MORTALITY_BOARD_FEET_COLUMN_NAME = "mrtbdft"
MORTALITY_MERCH_CUBIC_FEET_COLUMN_NAME = "mrtmcuft"
MORTALITY_TOTAL_CUBIC_FEET_COLUMN_NAME = "mrttcuft"
MORTALITY_TREES_PER_ACRE_COLUMN_NAME = "mrttpa"
RAW_BASAL_AREA_COLUMN_NAME = "ba"
RAW_BOARD_FEET_COLUMN_NAME = "bdft"
RAW_DIAMETER_COLUMN_NAME = "dbh"
RAW_MERCH_CUBIC_FEET_COLUMN_NAME = "mcuft"
RAW_MORTALITY_COLUMN_NAME = "mortpa"
RAW_TOTAL_CUBIC_FEET_COLUMN_NAME = "tcuft"
RAW_TREES_PER_ACRE_COLUMN_NAME = "tpa"
RESIDUAL_BASAL_AREA_COLUMN_NAME = "rsdba"
RESIDUAL_BOARD_FEET_COLUMN_NAME = "rsdbdft"
RESIDUAL_MERCH_CUBIC_FEET_COLUMN_NAME = "rsdmcuft"
RESIDUAL_TOTAL_CUBIC_FEET_COLUMN_NAME = "rsdtcuft"
RESIDUAL_TREES_PER_ACRE_COLUMN_NAME = "rsdtpa"
SPECIES_FIA_COLUMN_NAME = "speciesfia"
SPECIES_FVS_COLUMN_NAME = "speciesfvs"
SPECIES_PLANTS_COLUMNS_NAME = "speciesplants"
STAND_ID_COLUMN_NAME = "standid"
YEAR_COLUMN_NAME = "year"


CUTLIST_CALCULATION_COLUMNS = [
    HARVESTED_TREES_PER_ACRE_COLUMN_NAME,
    HARVESTED_BASAL_AREA_COLUMN_NAME,
    HARVESTED_TOTAL_CUBIC_FEET_COLUMN_NAME,
    HARVESTED_MERCH_CUBIC_FEET_COLUMN_NAME,
    HARVESTED_BOARD_FEET_COLUMN_NAME,
]

RESIDUAL_CALCULATION_COLUMNS = [
    RESIDUAL_TREES_PER_ACRE_COLUMN_NAME,
    RESIDUAL_BASAL_AREA_COLUMN_NAME,
    RESIDUAL_TOTAL_CUBIC_FEET_COLUMN_NAME,
    RESIDUAL_MERCH_CUBIC_FEET_COLUMN_NAME,
    RESIDUAL_BOARD_FEET_COLUMN_NAME,
]

TREELIST_CALCULATION_COLUMNS = [
    LIVE_TREES_PER_ACRE_COLUMN_NAME,
    MORTALITY_TREES_PER_ACRE_COLUMN_NAME,
    LIVE_BASAL_AREA_COLUMN_NAME,
    MORTALITY_BASAL_AREA_COLUMN_NAME,
    LIVE_TOTAL_CUBIC_FEET_COLUMN_NAME,
    MORTALITY_TOTAL_CUBIC_FEET_COLUMN_NAME,
    LIVE_MERCH_CUBIC_FEET_COLUMN_NAME,
    MORTALITY_MERCH_CUBIC_FEET_COLUMN_NAME,
    LIVE_BOARD_FEET_COLUMN_NAME,
    MORTALITY_BOARD_FEET_COLUMN_NAME,
]

STAND_STOCK_GROUPING_COLUMNS = [
    CASE_ID_COLUMN_NAME,
    SPECIES_FIA_COLUMN_NAME,
    SPECIES_FVS_COLUMN_NAME,
    SPECIES_PLANTS_COLUMNS_NAME,
    STAND_ID_COLUMN_NAME,
    YEAR_COLUMN_NAME,
]

STAND_STOCK_INDEX_COLUMNS = [
    CASE_ID_COLUMN_NAME,
    DIAMETER_CLASS_COLUMN_NAME,
    SPECIES_FIA_COLUMN_NAME,
    SPECIES_FVS_COLUMN_NAME,
    SPECIES_PLANTS_COLUMNS_NAME,
    STAND_ID_COLUMN_NAME,
    YEAR_COLUMN_NAME,
]

ALL_STAND_STOCK_COLUMNS = (
    CUTLIST_CALCULATION_COLUMNS
    + RESIDUAL_CALCULATION_COLUMNS
    + STAND_STOCK_INDEX_COLUMNS
    + TREELIST_CALCULATION_COLUMNS
)


class FvsStandStockWarning(UserWarning):
    """Warning issued when FVS Stand Stock Table is being created."""


def _make_diameter_class_bins(
    bin_width: int | float, last_bin_start: int | float
) -> np.array:
    """Makes diameter class bins.

    Creates an array of bin edges starting from 0 to np.inf, with last
    bin spanning fromlast_bin_start to np.inf.

    Args:
        bin_width (int | float): width of bins for all bins except last
            bin; last bin will be wider.
        last_bin_start (int | float): lower edge of last bin

    Returns:
        array of bin edges from zero to infinity
    """
    return np.append(
        np.arange(0, last_bin_start + bin_width, bin_width), np.inf
    )


def _has_tree_data(fvs_data: dict[str, pd.DataFrame]) -> bool:
    """Checks whether dict of FVS output dataframes has treelists in it.

    Args:
      fvs_data (dict[str, pd.DataFrame]): dictionary of dataframes
        scraped from FVS output database.

    Returns:
      bool: True if a treelist output table was found, False otherwise.
    """
    return (
        FvsOutputTableName.FVS_TREELIST in fvs_data
        or FvsOutputTableName.FVS_CUTLIST in fvs_data
        or FvsOutputTableName.FVS_AFTER_TREATMENT_TREELIST in fvs_data
    )


def _add_all_stocking_and_combine(
    stand_stock: pd.DataFrame, calculation_columns: Sequence[str]
) -> pd.DataFrame:
    """Computes stocking for all diameter classes and appends to table.

    Args:
      stand_stock (pd.DataFrame): dataframe of stand and stock data for
        each observed diameter class in a treelist created by
        _make_treelist_stand_stock, _make_cutlist_stand_stock, or
        _make_residual_stand_stock.
      calculation_columns (Sequence[str]): names of columns in the
        stand stock table for which sums will be calculated.

    Returns:
      stand stock table with row added with total stocking for all
      diameter classes.
    """
    all_dbh_classes = (
        stand_stock.groupby(by=STAND_STOCK_GROUPING_COLUMNS)[
            calculation_columns
        ]
        .sum()
        .reset_index()
    )
    all_dbh_classes[DIAMETER_CLASS_COLUMN_NAME] = ALL_DIAMETER_CLASSES

    # combine all and specific dbh classes
    return pd.concat(
        [
            all_dbh_classes.set_index(STAND_STOCK_INDEX_COLUMNS),
            stand_stock.groupby(by=STAND_STOCK_INDEX_COLUMNS)[
                calculation_columns
            ].sum(),
        ]
    )


def _make_treelist_stand_stock(
    tree_df: pd.DataFrame, bins: Sequence[int | float]
) -> pd.DataFrame:
    """Makes the Stand Stock table for the FVS_Treelist output table.

    Args:
        tree_df (pd.DataFrame): dataframe from FVS_Treelist output table
        bins (Sequence[int | float]): diameter class bins

    Returns:
        Pandas DataFrame of FVS Stand Stock table for treelist columns.
    """
    stand_stock = tree_df.copy()

    stand_stock[DIAMETER_CLASS_COLUMN_NAME] = pd.cut(
        stand_stock.dbh,
        bins,
        include_lowest=True,
        right=False,
        labels=bins[:-1],
    ).astype(int)

    stand_stock[RAW_BASAL_AREA_COLUMN_NAME] = (
        stand_stock[RAW_DIAMETER_COLUMN_NAME] ** 2 * FORESTERS_CONSTANT
    )
    stand_stock[LIVE_TREES_PER_ACRE_COLUMN_NAME] = stand_stock[
        RAW_TREES_PER_ACRE_COLUMN_NAME
    ]
    stand_stock[MORTALITY_TREES_PER_ACRE_COLUMN_NAME] = stand_stock[
        RAW_MORTALITY_COLUMN_NAME
    ]
    stand_stock[LIVE_BASAL_AREA_COLUMN_NAME] = (
        stand_stock[LIVE_TREES_PER_ACRE_COLUMN_NAME]
        * stand_stock[RAW_BASAL_AREA_COLUMN_NAME]
    )
    stand_stock[MORTALITY_BASAL_AREA_COLUMN_NAME] = (
        stand_stock[MORTALITY_TREES_PER_ACRE_COLUMN_NAME]
        * stand_stock[RAW_BASAL_AREA_COLUMN_NAME]
    )
    stand_stock[LIVE_TOTAL_CUBIC_FEET_COLUMN_NAME] = (
        stand_stock[LIVE_TREES_PER_ACRE_COLUMN_NAME]
        * stand_stock[RAW_TOTAL_CUBIC_FEET_COLUMN_NAME]
    )
    stand_stock[MORTALITY_TOTAL_CUBIC_FEET_COLUMN_NAME] = (
        stand_stock[MORTALITY_TREES_PER_ACRE_COLUMN_NAME]
        * stand_stock[RAW_TOTAL_CUBIC_FEET_COLUMN_NAME]
    )
    stand_stock[LIVE_MERCH_CUBIC_FEET_COLUMN_NAME] = (
        stand_stock[LIVE_TREES_PER_ACRE_COLUMN_NAME]
        * stand_stock[RAW_MERCH_CUBIC_FEET_COLUMN_NAME]
    )
    stand_stock[MORTALITY_MERCH_CUBIC_FEET_COLUMN_NAME] = (
        stand_stock[MORTALITY_TREES_PER_ACRE_COLUMN_NAME]
        * stand_stock[RAW_MERCH_CUBIC_FEET_COLUMN_NAME]
    )
    stand_stock[LIVE_BOARD_FEET_COLUMN_NAME] = (
        stand_stock[LIVE_TREES_PER_ACRE_COLUMN_NAME]
        * stand_stock[RAW_BOARD_FEET_COLUMN_NAME]
    )
    stand_stock[MORTALITY_BOARD_FEET_COLUMN_NAME] = (
        stand_stock[MORTALITY_TREES_PER_ACRE_COLUMN_NAME]
        * stand_stock[RAW_BOARD_FEET_COLUMN_NAME]
    )

    return _add_all_stocking_and_combine(
        stand_stock, TREELIST_CALCULATION_COLUMNS
    )


def _make_cutlist_stand_stock(
    tree_df: pd.DataFrame, bins: Sequence[int | float]
) -> pd.DataFrame:
    """Makes the Stand Stock table for the FVS_Cutlist output table.

    Args:
        tree_df (pd.DataFrame): dataframe from FVS_Cutlist output table
        bins (Sequence[int | float]): diameter class bins

    Returns:
        Pandas DataFrame of FVS Stand Stock table for cutlist columns.
    """
    stand_stock = tree_df.copy()

    stand_stock[DIAMETER_CLASS_COLUMN_NAME] = pd.cut(
        stand_stock.dbh,
        bins,
        include_lowest=True,
        right=False,
        labels=bins[:-1],
    ).astype(int)

    stand_stock[RAW_BASAL_AREA_COLUMN_NAME] = (
        stand_stock[RAW_DIAMETER_COLUMN_NAME] ** 2 * FORESTERS_CONSTANT
    )
    stand_stock[HARVESTED_TREES_PER_ACRE_COLUMN_NAME] = stand_stock[
        RAW_TREES_PER_ACRE_COLUMN_NAME
    ]
    stand_stock[HARVESTED_BASAL_AREA_COLUMN_NAME] = (
        stand_stock[HARVESTED_TREES_PER_ACRE_COLUMN_NAME]
        * stand_stock[RAW_BASAL_AREA_COLUMN_NAME]
    )
    stand_stock[HARVESTED_TOTAL_CUBIC_FEET_COLUMN_NAME] = (
        stand_stock[HARVESTED_TREES_PER_ACRE_COLUMN_NAME]
        * stand_stock[RAW_TOTAL_CUBIC_FEET_COLUMN_NAME]
    )
    stand_stock[HARVESTED_MERCH_CUBIC_FEET_COLUMN_NAME] = (
        stand_stock[HARVESTED_TREES_PER_ACRE_COLUMN_NAME]
        * stand_stock[RAW_MERCH_CUBIC_FEET_COLUMN_NAME]
    )
    stand_stock[HARVESTED_BOARD_FEET_COLUMN_NAME] = (
        stand_stock[HARVESTED_TREES_PER_ACRE_COLUMN_NAME]
        * stand_stock[RAW_BOARD_FEET_COLUMN_NAME]
    )

    return _add_all_stocking_and_combine(
        stand_stock, CUTLIST_CALCULATION_COLUMNS
    )


def _make_residual_stand_stock(
    tree_df: pd.DataFrame, bins: Sequence[int | float]
) -> pd.DataFrame:
    """Makes the Stand Stock table for the FVS_ATRTList output table.

    Args:
        tree_df (pd.DataFrame): dataframe from FVS_ATRTList output table
        bins (Sequence[int | float]): diameter class bins

    Returns:
        Pandas DataFrame of FVS Stand Stock table for residual columns.
    """
    stand_stock = tree_df.copy()

    stand_stock[DIAMETER_CLASS_COLUMN_NAME] = pd.cut(
        stand_stock.dbh,
        bins,
        include_lowest=True,
        right=False,
        labels=bins[:-1],
    ).astype(int)

    stand_stock[RAW_BASAL_AREA_COLUMN_NAME] = (
        stand_stock[RAW_DIAMETER_COLUMN_NAME] ** 2 * FORESTERS_CONSTANT
    )
    stand_stock[RESIDUAL_TREES_PER_ACRE_COLUMN_NAME] = stand_stock[
        RAW_TREES_PER_ACRE_COLUMN_NAME
    ]
    stand_stock[RESIDUAL_BASAL_AREA_COLUMN_NAME] = (
        stand_stock[RESIDUAL_TREES_PER_ACRE_COLUMN_NAME]
        * stand_stock[RAW_BASAL_AREA_COLUMN_NAME]
    )
    stand_stock[RESIDUAL_TOTAL_CUBIC_FEET_COLUMN_NAME] = (
        stand_stock[RESIDUAL_TREES_PER_ACRE_COLUMN_NAME]
        * stand_stock[RAW_TOTAL_CUBIC_FEET_COLUMN_NAME]
    )
    stand_stock[RESIDUAL_MERCH_CUBIC_FEET_COLUMN_NAME] = (
        stand_stock[RESIDUAL_TREES_PER_ACRE_COLUMN_NAME]
        * stand_stock[RAW_MERCH_CUBIC_FEET_COLUMN_NAME]
    )
    stand_stock[RESIDUAL_BOARD_FEET_COLUMN_NAME] = (
        stand_stock[RESIDUAL_TREES_PER_ACRE_COLUMN_NAME]
        * stand_stock[RAW_BOARD_FEET_COLUMN_NAME]
    )

    return _add_all_stocking_and_combine(
        stand_stock, RESIDUAL_CALCULATION_COLUMNS
    )


def make_stand_stock_table(
    fvs_data: dict[str, pd.DataFrame], dbh_class: int, large_dbh: int
) -> pd.DataFrame:
    """Makes the FVS Stand Stock Table by summarizing Treelist Tables.

    FVS Stand Stock Table contains trees per acre, basal area (sqft/ac),
    total cubic foot volume (cuft/ac), merchantable cubic foot volume
    (cuft/ac), and boardfoot volume (bdft/ac) by diameter class and
    species, for before-thinning live trees, harvested trees, mortality
    trees during the cycle, and residual after-thinning live trees.

    For estimates to be generated for live and mortality trees, the
    underlying FVS database must have a FVS_Treelist table. Similarly,
    to calculate values for harvested trees, the output database must
    include a FVS_Cutlist table. And for residual after-thinning trees,
    the output database must include a FVS_ATRTList table. If none of
    these tables are found, a warning will be raised and the StdStk
    table created will be empty. If any one of the these underlying
    tables are found in the FVS output database, the StdStk table will
    be populated, and any columns which cannot be calculated due to the
    absence of an underlying tree list in the FVS output database will
    be left as nulls. For example, if FVS_Treelist is found, but
    FVS_Cutlist is not, columns for harvested tree values in StdStk will
    be null.

    Args:
        fvs_data (dict[str, pd.DataFrame]): dictionary of Pandas
            dataframes, expected to include one of: "fvs_treelist",
            "fvs_cutlist", or "fvs_atrtlist"
        dbh_class (int): The width of each DBH class (in inches) used.
        large_dbh (int): The largest DBH beyond which all DBH classes
            will be lumped together.

    Returns:
        Pandas DataFrame of FVS Stand Stock table

    Raises:
        FvsStandStockWarning: if no tree tables are found in fvs_data.
    """
    if not _has_tree_data(fvs_data):
        warnings.warn(
            "No tree lists in provided data, FVS_StdStk will be empty.",
            FvsStandStockWarning,
        )
        return pd.DataFrame(
            columns=ALL_STAND_STOCK_COLUMNS, dtype=int
        )  # blank stand stock df

    bins = _make_diameter_class_bins(dbh_class, large_dbh)

    if FvsOutputTableName.FVS_TREELIST in fvs_data:
        treelist_stand_stock = _make_treelist_stand_stock(
            fvs_data[FvsOutputTableName.FVS_TREELIST], bins
        )
    else:
        treelist_stand_stock = (
            pd.DataFrame(  # blank stand_stock df with indexes only
                columns=STAND_STOCK_INDEX_COLUMNS, dtype=int
            ).set_index(STAND_STOCK_INDEX_COLUMNS)
        )
        treelist_stand_stock[TREELIST_CALCULATION_COLUMNS] = (
            None  # add treelist cols
        )

    if FvsOutputTableName.FVS_CUTLIST in fvs_data:
        cutlist_stand_stock = _make_cutlist_stand_stock(
            fvs_data[FvsOutputTableName.FVS_CUTLIST], bins
        )
    else:
        cutlist_stand_stock = (
            pd.DataFrame(  # blank stand_stock df with indexes only
                columns=STAND_STOCK_INDEX_COLUMNS, dtype=int
            ).set_index(STAND_STOCK_INDEX_COLUMNS)
        )
        cutlist_stand_stock[CUTLIST_CALCULATION_COLUMNS] = (
            None  # add cutlist cols
        )

    if FvsOutputTableName.FVS_AFTER_TREATMENT_TREELIST in fvs_data:
        residual_stand_stock = _make_residual_stand_stock(
            fvs_data[FvsOutputTableName.FVS_AFTER_TREATMENT_TREELIST], bins
        )
    else:
        residual_stand_stock = (
            pd.DataFrame(  # blank stand_stock df with indexes only
                columns=STAND_STOCK_INDEX_COLUMNS, dtype=int
            ).set_index(STAND_STOCK_INDEX_COLUMNS)
        )
        residual_stand_stock[RESIDUAL_CALCULATION_COLUMNS] = (
            None  # add residual cols
        )

    return (
        pd.concat(
            [treelist_stand_stock, cutlist_stand_stock, residual_stand_stock],
            axis=1,
        )
        .sort_index(
            level=[
                STAND_ID_COLUMN_NAME,
                YEAR_COLUMN_NAME,
                SPECIES_FVS_COLUMN_NAME,
                DIAMETER_CLASS_COLUMN_NAME,
            ]
        )
        .reset_index()
    )
