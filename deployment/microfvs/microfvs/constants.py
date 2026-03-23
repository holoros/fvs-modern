FVS_DATABASE_NAME = "FVS_Data.db"
EMPTY_STRING = ""
LINE_ENDING_STRING = "\n"
STR_PROCESS = "PROCESS"
STR_STAND_ID = "stand_id"
STR_STOP = "STOP"
KCP_SUFFIX = ".kcp"
CONTENT_TYPE = "Content-Type"
STAND_ID_COLUMN_NAME = "stand_id"
FVS_VARIANT_COLUMN_NAME = "variant"


# fvs output file constants
FVS_OUTFILE_ERROR_STRING = "ERROR:"
FVS_OUTFILE_WARNING_STRING = "WARNING:"
FVS_OUTFILE_WARNING_PREFIX = "*"
FVS_OUTFILE_WARNING_PREFIX3 = "***"
FVS_OUTFILE_FIRE_FUELS_PREFIX = "FFE MODEL"

# column names in FVS tables
FUEL_LITTER_COLUMN_NAME = "fuel_litter"
HEIGHT_COLUMN_NAME = "height"
INT64_DTYPE = "Int64"
LOCATION_COLUMN_NAME = "location"
SITE_SPECIES_COLUMN_NAME = "site_species"
TREE_HISTORY_COLUMN_NAME = "history"

# treeinit and standinit records for testing
TEST_TREEINIT_RECORDS = [
    {
        "stand_id": "061608985105",
        "plot_id": 1,
        "tree_id": 304,
        "tree_count": 6.018046,
        "history": 1.0,
        "species": 815,
        "diameter": 10.6,
        "ht": 48.0,
        "crratio": 30.0,
    },
    {
        "stand_id": "061608985105",
        "plot_id": 1,
        "tree_cn": "449500089489998",
        "tree_id": 305,
        "tree_count": 6.018046,
        "history": 1,
        "species": 815,
        "diameter": 9.4,
        "ht": 52.0,
        "crratio": 40.0,
    },
    {
        "stand_id": "061608985105",
        "plot_id": 1,
        "tree_id": 306,
        "tree_count": 6.018046,
        "history": 1.0,
        "species": 815,
        "diameter": 6.7,
        "ht": 25.0,
        "crratio": 40.0,
    },
]
TEST_STANDINIT_RECORDS = [
    {
        "stand_id": "061608985105",
        "variant": "CA",
        "inv_year": 2016,
        "latitude": 41.034483,
        "longitude": -121.65028,
        "location": 514.0,
        "habitat": 46,
        "age": 115,
        "aspect": 158.0,
        "slope": 10.0,
        "elevft": 2700.0,
        "basal_area_factor": 0.0,
        "inv_plot_size": 1.0,
        "brk_dbh": 999.0,
        "num_plots": 1.0,
        "site_species": 122,
        "site_index": 32.0,
    },
    {
        "stand_id": "0026200707030412520079",
        "variant": "LS",
        "inv_year": 2007.0,
        "latitude": 42.6369,
        "longitude": -83.5445,
        "location": 924.0,
        "age": 82,
        "aspect": 230.0,
        "slope": 10.0,
        "elevft": 960.0,
        "basal_area_factor": -24.0,
        "inv_plot_size": 300.0,
        "brk_dbh": 5.0,
        "num_plots": 4.0,
        "site_species": 837.0,
        "site_index": 63.0,
        "state": 26.0,
        "county": 125.0,
    },
]

TEST_FVS_WARNINGS_AND_ERRORS = """
********   FVS14 WARNING:  HABITAT/PLANT ASSOCIATION/ECOREGION CODE WAS NOT RECOGNIZED; HABITAT/PLANT ASSOCIATION/ECOREGION SET TO DEFAULT CODE.
...
 *** FFE MODEL WARNING: NO INITIAL BASAL AREA
 *** COVER TYPE SET TO RED OAK
...
********   FVS01 ERROR:  INVALID KEYWORD WAS SPECIFIED.  RECORDS READ=  54
"""

RESULT_TESTING_KEYFILE_CONTENT = """
STDIDENT
12345
DATABASE
DSNOUT
FVS_Data.db
ATRTLIDB           2         2
BURNREDB           2
CALBSTDB           2
CARBREDB           2
COMPUTDB
CUTLIDB            2         2
DWDVLDB            2
ECONRPTS           2         2
FUELSOUT           2
MORTREDB           2         2
POTFIRDB           2
SNAGOUDB           2         2
SNAGSUDB           2
STRCLSDB           2
SUMMARY            2
TREELIDB           2         2
END

CALBSTAT
TREELIST           0
CUTLIST            0
ATRTLIST           0
STRCLASS           0
FMIN
CARBREPT
CARBCUT
CANFPROF
FUELOUT
DWDVLOUT
MORTREPT
POTFIRE
SNAGSUM
SNAGOUT
END

STDINFO          610         1         1
DESIGN            -1         1
INVYEAR       1990.0
NUMCYCLE        10.0
TREEFMT
(I4,I4,F6.0,I1,A3,F4.1,F3.1,2F3.0,F4.1,I1,
6I2,2I1,I2,2I3,2I1,F3.0)
TREEDATA        15.0
0101 100    501  1 51
0101 101    501  2 41
-999
PROCESS
STOP
""".strip()


# constants for the generation of random treees
DIAMETER_MAXIMUM = 36
DIAMETER_MINIMUM = 0.1
GROWING_STOCK_THRESHOLD = 5.0
HEIGHT_B1 = 5.563
HEIGHT_B2 = -16.475
LOG_LENGTH = 16
MAX_MORTALITY_TREES_PER_ACRE = 5
MAX_TREES_PER_ACRE = 20
MERCH_HT_PROP = 0.9
MERCH_TOP_DIAMETER = 4
MERCH_VOL_PROP = 0.85
RANDOM_SPECIES = [
    {"speciesfvs": "DF", "speciesplants": "PSME", "speciesfia": "202"},
    {"speciesfvs": "PP", "speciesplants": "PIPO", "speciesfia": "122"},
    {"speciesfvs": "WF", "speciesplants": "ABCO", "speciesfia": "15"},
]
RANDOM_YEARS = [2000, 2005, 2010]
SAWLOG_DIAMETER_THRESHOLD = 9.0
STANDARD_HEIGHT = 4.5
