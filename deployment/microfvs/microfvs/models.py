from __future__ import annotations

import importlib.resources
import os
import subprocess
import warnings
from collections.abc import Sequence
from functools import cached_property
from pathlib import Path

import numpy as np
import pandas as pd
from jinja2 import Template
from pydantic import (
    AliasChoices,
    BaseModel,
    ConfigDict,
    Field,
    computed_field,
    field_validator,
)

from microfvs.constants import (
    DIAMETER_MAXIMUM,
    DIAMETER_MINIMUM,
    EMPTY_STRING,
    FVS_OUTFILE_ERROR_STRING,
    FVS_OUTFILE_FIRE_FUELS_PREFIX,
    FVS_OUTFILE_WARNING_PREFIX,
    FVS_OUTFILE_WARNING_PREFIX3,
    FVS_OUTFILE_WARNING_STRING,
    GROWING_STOCK_THRESHOLD,
    HEIGHT_B1,
    HEIGHT_B2,
    LINE_ENDING_STRING,
    LOG_LENGTH,
    MAX_MORTALITY_TREES_PER_ACRE,
    MAX_TREES_PER_ACRE,
    MERCH_HT_PROP,
    MERCH_TOP_DIAMETER,
    MERCH_VOL_PROP,
    RANDOM_SPECIES,
    RANDOM_YEARS,
    SAWLOG_DIAMETER_THRESHOLD,
    STANDARD_HEIGHT,
)
from microfvs.enums import (
    FvsEventType,
    FvsKeyfileTemplate,
    FvsOutputTableName,
    FvsVariant,
)
from microfvs.utils.sqlite_scraper import FvsSqliteScraper


class FvsEvent(BaseModel):
    """A class representing an event that can be triggered in FVS."""

    name: str
    content: str


class FvsEventLibrary:
    """A class for looking up FVS Events (treatments, disturbances)."""

    @cached_property
    def treatments(self) -> dict[str, FvsEvent]:
        """Get a dict of all known treatments."""
        with importlib.resources.as_file(
            importlib.resources.files(
                "microfvs.keyword_components.treatments"
            ).joinpath("")
        ) as path:
            return {
                x.stem: FvsEvent(name=x.stem, content=x.read_text())
                for x in path.rglob("*.kcp")
            }

    @cached_property
    def usfs_treatments(self) -> dict[str, FvsEvent]:
        """Get a dict of all known USFS treatments."""
        with importlib.resources.as_file(
            importlib.resources.files(
                "microfvs.keyword_components.treatments.usfs"
            ).joinpath("")
        ) as path:
            return {
                x.stem: FvsEvent(name=x.stem, content=x.read_text())
                for x in path.rglob("*.kcp")
            }

    @cached_property
    def disturbances(self) -> dict[str, FvsEvent]:
        """Get a dict of all known disturbancess."""
        with importlib.resources.as_file(
            importlib.resources.files(
                "microfvs.keyword_components.disturbances"
            ).joinpath("")
        ) as path:
            return {
                x.stem: FvsEvent(name=x.stem, content=x.read_text())
                for x in path.rglob("*.kcp")
            }

    def lookup(self, event_type: FvsEventType, event_key: str) -> FvsEvent:
        """Look up an event from the library.

        Args:
            event_type (FvsEventType): type of event to look up
            event_key (str | FvsEvent): the name of the event to look up
        """
        if event_type == FvsEventType.TREATMENT.value:
            if event_key not in self.treatments:
                msg = f"{event_key} is not a recognized treatment code."
                raise IndexError(msg)
            return self.treatments[event_key]

        if event_type == FvsEventType.DISTURBANCE.value:
            if event_key not in self.disturbances:
                msg = f"{event_key} is not a recognized disturbance code."
                raise IndexError(msg)
            return self.disturbances[event_key]

        msg = f"{event_type} is not a recognized FvsEventType"
        raise ValueError(msg)


class FvsKeyfileTemplateParams(BaseModel):
    """Parameters injected into Keyfile template for a simulation."""

    variant: FvsVariant
    stand_id: str
    num_cycles: int = 1
    cycle_length: int = 5
    treatments: list[FvsEvent] = [
        FvsEvent(name="NONE", content="*** NO TREATMENT ***")
    ]
    disturbances: list[FvsEvent] = [
        FvsEvent(name="NONE", content="*** NO DISTURBANCE ***")
    ]

    model_config = ConfigDict(
        extra="allow",
        use_enum_values=True,
    )

    @computed_field
    def treatment_name(self) -> str:
        """A name for the treatment(s) to be simulated."""
        return "+".join([t.name for t in self.treatments])

    @computed_field
    def disturbance_name(self) -> str:
        """A name for the disturbances(s) to be simulated."""
        return "+".join([d.name for d in self.disturbances])

    @property
    def expected_fields(self) -> dict:
        """Fields expected (not required) to exist and their values.

        These are injected into a keyfile in an initial step before
        extra fields are injected.
        """
        return {
            key: value
            for key, value in self.model_dump().items()
            if key not in self.model_extra
        }

    @property
    def extra_fields(self) -> dict:
        """Extra fields to be defined and their values.

        These are injected into a keyfile after expected fields are
        injected.
        """
        return self.model_extra

    @field_validator("treatments", mode="before")
    @classmethod
    def check_treatment(
        cls, value: str | FvsEvent | list[str] | list[FvsEvent]
    ) -> list[FvsEvent] | None:
        """Converts treatment(s) into list of FvsEvent."""
        library = FvsEventLibrary()
        if isinstance(value, str):
            return [
                library.lookup(
                    event_type=FvsEventType.TREATMENT, event_key=value
                )
            ]
        if isinstance(value, Sequence):
            return [
                library.lookup(event_type=FvsEventType.TREATMENT, event_key=v)
                if isinstance(v, str)
                else v
                for v in value
            ]
        return [FvsEvent(name="GROW", content="*** No Treatment ***")]

    @field_validator("disturbances", mode="before")
    @classmethod
    def check_disturbance(
        cls, value: str | FvsEvent | list[str] | list[FvsEvent]
    ) -> list[FvsEvent] | None:
        """Converts disturbance(s) into list of FvsEvent."""
        library = FvsEventLibrary()
        if isinstance(value, str):
            return [
                library.lookup(
                    event_type=FvsEventType.DISTURBANCE, event_key=value
                )
            ]
        if isinstance(value, Sequence):
            return [
                library.lookup(event_type=FvsEventType.DISTURBANCE, event_key=v)
                if isinstance(v, str)
                else v
                for v in value
            ]
        return [FvsEvent(name="UNDISTURBED", content="*** No Disturbance ***")]


class FvsStandStockParams(BaseModel):
    """Params for Stand and Stock Tables when scraping FVS results."""

    add_stand_stock: bool = True
    dbh_class: int = 4
    large_dbh: int = 48


class FvsKeyfile(BaseModel):
    """A model documenting a keyfile for running FVS on a single stand.

    Args:
        template (str): Optional Jinja template for the FVS Keyfile,
            defaults to FvsKeyfileTemplate.DEFAULT
        params (FvsKeyfileTemplateParams): Parameters to be injected
            into the keyfile template.

    Computed Attributes:
        name (str): concatenation of
            {fvs_variant}_{stand_id}_{treatment_name}_{disturbance_name}.
        stand_id (str): Stand identifier.
        fvs_variant (FvsVariant): The regional variant of FVS.
        content (str): the full text of the FVS Keyfile.
    """

    template: str = FvsKeyfileTemplate.DEFAULT
    params: FvsKeyfileTemplateParams

    @computed_field
    def name(self) -> str:
        """Name of FVS Keyfile."""
        return "_".join(
            [
                self.params.variant,
                self.params.stand_id,
                self.params.treatment_name,
                self.params.disturbance_name,
            ]
        )

    @computed_field
    def stand_id(self) -> str:
        """Stand ID for stand being run in Keyfile."""
        return str(self.params.stand_id)

    @computed_field
    def fvs_variant(self) -> str:
        """Name of FVS Variant."""
        return self.params.variant

    @computed_field
    def treatment_name(self) -> str:
        """Name of treatment(s) applied."""
        return self.params.treatment_name

    @computed_field
    def disturbance_name(self) -> str:
        """Name of disturbance(s) applied."""
        return self.params.disturbance_name

    @computed_field
    def content(self) -> str:
        """Content of the FVS Keyfile.

        The template is rendered in two distinct steps. First, expected
        fields are injected into the template. Second, any extra fields
        are injected.This allows the content of expected fields to
        include placeholders that are filled when the extra fields are
        injected.
        """
        rendered = Template(self.template).render(**self.params.expected_fields)

        if len(self.params.extra_fields) > 0:
            rendered = Template(rendered).render(**self.params.extra_fields)

        return rendered


class FvsTreeInitRecord(BaseModel):
    """A model of an individual tree record used as input to FVS.

    Defined based on the fields to be formatted according to the TREEFMT
    FVS keyword, as defined in the FVS Keyword Guide:
    https://www.fs.usda.gov/fmsc/ftp/fvs/docs/gtr/keyword.pdf

    """

    stand_id: str
    plot_id: int | None = None
    tree_id: int | str | None = None
    tree_count: float | None = None
    species: int | str
    diameter: float = Field(
        gt=0, validation_alias=AliasChoices("dbh", "diameter")
    )
    history: int | None = None
    ht: float | None = Field(
        default=None, gt=0, validation_alias=AliasChoices("height", "ht")
    )
    crratio: int | None = Field(
        default=None,
        ge=0,
        le=100,
        validation_alias=AliasChoices("cr", "crratio", "crown_ratio"),
    )
    dg: float | None = Field(
        default=None, validation_alias=AliasChoices("dg", "diameter_growth")
    )
    htg: float | None = Field(
        default=None, validation_alias=AliasChoices("htg", "height_growth")
    )
    httopk: float | None = Field(
        default=None,
        ge=0,
        validation_alias=AliasChoices("httopk", "height_to_topkill"),
    )
    damage1: int | None = Field(default=None, ge=1, le=99)
    severity1: int | None = Field(default=None, ge=0, le=100)
    damage2: int | None = Field(default=None, ge=1, le=99)
    severity2: int | None = Field(default=None, ge=0, le=100)
    damage3: int | None = Field(default=None, ge=1, le=99)
    severity3: int | None = Field(default=None, ge=0, le=100)
    tree_value: int | None = Field(default=None, gt=0)
    prescription: int | None = Field(default=None, gt=0)
    slope: int | None = Field(default=None, ge=0)
    aspect: int | None = Field(default=None, ge=0, le=360)
    habitat: int | str | None = None
    topo_code: int | None = Field(default=None, ge=1, le=5)
    site_prep: int | None = Field(default=None, ge=1, le=4)
    age: int | None = Field(default=None, gt=0)

    @field_validator("*", mode="before")
    @classmethod
    def to_none(cls, raw) -> float | int | str | None:
        """Transforms empty string to None."""
        if pd.isna(raw) or raw == "":
            return None
        return raw

    @field_validator("stand_id", mode="before")
    def cast_to_str(cls, raw: int | str) -> str:
        """Casts input value to a string."""
        return str(raw)


class FvsTreeInit(BaseModel):
    """Collection of tree records."""

    trees: list[FvsTreeInitRecord] | None = None

    @staticmethod
    def from_records(records: list[dict] | None) -> FvsTreeInit:
        """Creates FvsTreeInit from a list of tree records as dicts.

        Args:
            records (list[dict] | None): tree records to incorporate.
        """
        if records is None:
            return FvsTreeInit()
        return FvsTreeInit(
            trees=[FvsTreeInitRecord.model_validate(r) for r in records]
        )

    @staticmethod
    def from_dataframe(
        df: pd.DataFrame, stand_id: int | str, column_name: str = "stand_id"
    ) -> FvsTreeInit:
        """Creates a list of FvsTreeInitRecords from a Pandas DataFrame.

        Args:
            df (pd.DataFrame): a DataFrame with tree initialization data
            stand_id (str | int): the identifier used to find trees that
                have this value in the column specified in `column_name`
            column_name (str): the name of the column to use for finding
                the stand you want, usually "stand_id". Another common
                option for folks using FIA data might be "stand_cn"
        """
        tree_df = df.copy()
        tree_df.columns = [col.lower() for col in tree_df.columns]

        filtered = tree_df.loc[tree_df[column_name] == stand_id]
        if len(filtered) == 0:
            msg = (
                f"No records found in {column_name} that match {stand_id}. "
                f"Returning no trees."
            )
            warnings.warn(msg)
            return FvsTreeInit(trees=None)

        return FvsTreeInit.from_records(filtered.to_dict(orient="records"))

    def to_dataframe(self) -> pd.DataFrame:
        """Returns FvsTreeInit as a Pandas DataFrame."""
        return pd.DataFrame.from_records(self.model_dump()["trees"])


class FvsStandInit(BaseModel):
    """Model for a single stand's initialization data.

    Defined based on Users Guide to the FVS Database Extension:
    https://www.fs.usda.gov/fmsc/ftp/fvs/docs/gtr/DBSUserGuide.pdf

    """

    stand_id: str
    variant: FvsVariant
    inv_year: int
    basal_area_factor: float
    inv_plot_size: float
    brk_dbh: float
    stand_cn: str | None = None
    latitude: float | None = None
    longitude: float | None = None
    region: int | None = None
    forest: int | None = None
    district: int | None = None
    location: int | None = None
    ecoregion: str | None = None
    pv_code: int | None = None
    habitat: int | str | None = None
    pv_ref_code: int | None = None
    age: int | None = None
    aspect: float | None = None
    slope: float | None = None
    elevft: float | None = None
    elevation: float | None = None
    num_plots: int | None = None
    nonstk_plots: int | None = None
    sam_wt: float | None = None
    stk_pcnt: float | None = None
    dg_trans: int | None = None
    dg_measure: int | None = None
    htg_trans: int | None = None
    htg_measure: int | None = None
    mort_measure: int | None = None
    max_ba: int | None = None
    max_sdi: int | None = None
    site_species: str | int | None = None
    site_index: int | None = None
    model_type: int | None = None
    physio_region: int | None = None
    forest_type: int | None = None
    state: int | None = None
    county: int | None = None
    fuel_model: int | None = None
    fuel_0_25_h: float | None = None
    fuel_25_1_h: float | None = None
    fuel_1_3_h: float | None = None
    fuel_3_6_h: float | None = None
    fuel_6_12_h: float | None = None
    fuel_12_20_h: float | None = None
    fuel_20_35_h: float | None = None
    fuel_35_50_h: float | None = None
    fuel_gt_50_h: float | None = None
    fuel_0_25_s: float | None = None
    fuel_25_1_s: float | None = None
    fuel_1_3_s: float | None = None
    fuel_3_6_s: float | None = None
    fuel_6_12_s: float | None = None
    fuel_12_20_s: float | None = None
    fuel_20_35_s: float | None = None
    fuel_35_50_s: float | None = None
    fuel_gt_50_s: float | None = None
    fuel_litter: float | None = None
    fuel_duff: float | None = None
    photo_ref: int | None = None
    photo_code: str | None = None

    model_config = ConfigDict(protected_namespaces=(), use_enum_values=True)

    @field_validator("*", mode="before")
    @classmethod
    def to_none(cls, raw) -> float | int | str | None:
        """Transforms empty string to None."""
        if pd.isna(raw) or raw == "":
            return None
        return raw

    @field_validator("stand_id", mode="before")
    def cast_to_str(cls, raw: int | str) -> str:
        """Casts input value to a string."""
        return str(raw)

    @staticmethod
    def from_dataframe(
        df: pd.DataFrame,
        stand_id: int | str,
        column_name: str = "stand_id",
    ) -> FvsStandInit:
        """Creates FvsStandInit from a single row in a pandas DataFrame.

        Args:
            df (pd.DataFrame): A Pandas DataFrame with stand
                initialization data.
            stand_id (int | str): The identifier for the stand to single
                out a record by filtering `column_to_match`.
            column_name (str): the name of the column to use for finding
                the stand you want, usually "stand_id". Another common
                option for folks using FIA data might be "stand_cn".

        Returns:
            FvsStandInit

        Raises:
            ValidationError: if the record in the DataFrame does not
                have all necessary fields to create an FvsStandInit.
            ValueError: if more or less than a single row is found in
                `column_name` with the value specified in `stand_id`.
        """
        stand_df = df.copy()
        stand_df.columns = [col.lower() for col in stand_df.columns]

        filtered = stand_df.loc[stand_df[column_name] == stand_id]
        if len(filtered) != 1:
            msg = (
                f"Only one record in {column_name} is allowed to match "
                f"{stand_id}. Found {len(filtered)} records that match."
            )
            raise ValueError(msg)

        return FvsStandInit.model_validate(filtered.squeeze().to_dict())


class FvsOutputTreeListRecord(BaseModel):
    """A model for a record in a TreeList table generated by FVS."""

    caseid: str
    standid: str | int
    year: int
    speciesfvs: str
    speciesplants: str
    speciesfia: str
    tpa: float = Field(ge=0)
    mortpa: float = Field(ge=0)
    dbh: float = Field(gt=0)
    tcuft: float = Field(ge=0)
    mcuft: float = Field(ge=0)
    bdft: float = Field(ge=0)

    @classmethod
    def _random_tree(
        cls, smallest_tree: bool = False, largest_tree: bool = False
    ) -> FvsOutputTreeListRecord:
        """Returns a randomly generated TreeList Record.

        Args:
            smallest_tree (bool): if True, returns a tree with diameter
                0.1 inches
            largest_tree (bool): if True, returns a tree with maximum
                diameter
        """
        if smallest_tree and largest_tree:
            message = "Only one of smallest_tree and largest_tree can be True."
            raise ValueError(message)

        species_fields = RANDOM_SPECIES[
            np.random.randint(0, len(RANDOM_SPECIES))
        ]
        tpa = np.random.random() * MAX_TREES_PER_ACRE
        mortpa = np.random.random() * MAX_MORTALITY_TREES_PER_ACRE
        if smallest_tree:
            dbh = DIAMETER_MINIMUM
        elif largest_tree:
            dbh = DIAMETER_MAXIMUM
        else:
            dbh = np.random.random() * DIAMETER_MAXIMUM
        if dbh > GROWING_STOCK_THRESHOLD:
            ht = STANDARD_HEIGHT + np.exp(HEIGHT_B1 + HEIGHT_B2 / (dbh + 1.0))
            tcuft = (dbh / 12 / 2) ** 2 * np.pi * ht
            mcuft = tcuft * MERCH_VOL_PROP
        else:
            ht = 10
            tcuft = (dbh / 12 / 2) ** 2 * np.pi * ht
            mcuft = 0

        if dbh > SAWLOG_DIAMETER_THRESHOLD:
            merch_ht = ht * MERCH_HT_PROP
            bdft = (dbh - MERCH_TOP_DIAMETER) ** 2 * merch_ht / LOG_LENGTH
        else:
            bdft = 0

        tree_attributes = {
            "caseid": "abc12345",
            "standid": 45678,
            "year": np.random.choice(RANDOM_YEARS),
            **species_fields,
            "tpa": tpa,
            "mortpa": mortpa,
            "dbh": dbh,
            "tcuft": tcuft,
            "mcuft": mcuft,
            "bdft": bdft,
        }

        return cls.model_validate(tree_attributes)

    @classmethod
    def _random_treelist(cls, num_trees: int = 100) -> list[dict]:
        """Generates a list of random FvsOutputTreeListRecords.

        Args:
            num_trees (int): number of trees to generate

        Returns:
            a list of TreeListRecords as dictionaries, which can be read
            directly into a dataframe using pd.DataFrame.from_records()
        """
        if num_trees < 2:
            message = (
                f"num_trees must be greater than 2. You entered {num_trees}."
            )
            raise ValueError(message)
        smallest_tree = cls._random_tree(smallest_tree=True)
        largest_tree = cls._random_tree(largest_tree=True)
        return (
            [smallest_tree.model_dump()]
            + [cls._random_tree().model_dump() for i in range(num_trees - 2)]
            + [largest_tree.model_dump()]
        )


class FvsResult(BaseModel):
    """A class for results from running FVS via Command Line Interface.

    Public Attributes:
        name (str): name of the FVS run
        fvs_variant (str): value of a regional FVS variant
        stand_id (str): identifier for the stand being simulated
        treatment (str): name of a FVS treatment recipe
        disturbance (str): name of a FVS disturbance recipe
        keyfile (str): content of an FVS Keyfile used to run the
            simulation
        command (str): command that was used to execute the FVS
            simulation using the CLI
        return_code (int): return code from the execution of FVS using
            the CLI
        stdout(str | None): stdout returned from the execution of FVS
            using the CLI
        stderr (str | None): stderr returned from the execution of FVS
            using the CLI
        outfile (str): contents of the FVS outfile generated from the
            simulation
        fvs_errors (list[dict] | None): errors scraped from the FVS
            outfile
        fvs_warnings (list[dict] | None): warnings scraped from the FVS
            outfile
        fvs_data (dict[FvsOutputTableName, list[dict]]): data scraped
            from the FVS output database
    """

    model_config = ConfigDict(arbitrary_types_allowed=True, frozen=True)
    name: str
    fvs_variant: str
    stand_id: str
    treatment: str
    disturbance: str
    keyfile: str
    command: str
    return_code: int
    stdout: str | None
    stderr: str | None
    fvs_data: dict[FvsOutputTableName, list[dict]]
    outfile: str

    @computed_field
    def fvs_warnings(self) -> list[FvsOutfileProblem] | None:
        """Warnings and errors parsed from the FVS outfile."""
        return FvsResult._parse_fvs_warnings_and_errors(self.outfile)

    def __str__(self) -> str:
        report = (
            "FvsResult(\n"
            f"\tname: {self.name},\n"
            f"\tfvs_variant: {self.fvs_variant},\n"
            f"\tstand_id: {self.stand_id},\n"
            f"\ttreatment: {self.treatment},\n"
            f"\tdisturbance: {self.disturbance},\n"
            f"\tcommand: {self.command},\n"
            f"\treturn_code: {self.return_code},\n"
            f"\tstdout: {self.stdout},\n"
        )
        if self.stderr is None:
            report += f"\tstderr: {self.stderr},\n"
        else:
            lines = self.stderr.splitlines()
            report += "\tstderr:\n"
            for line in lines:
                report += f"\t\t{line}\n"

        if self.fvs_warnings is None:
            report += f"\tfvs_warnings: {self.fvs_warnings},\n"
        else:
            report += "\tfvs_warnings: [\n"
            for msg in self.fvs_warnings:
                report += f"\t\t{msg},\n"
            report += "\t]\n"

        if self.fvs_data is None:
            report += f"\tfvs_data: {self.fvs_data}\n"
        else:
            report += "\tfvs_data: {\n"
            for name, records in self.fvs_data.items():
                records_df = pd.DataFrame.from_records(records)
                rows, cols = records_df.shape
                report += f"\t\t{name}: ({rows:,} rows, {cols} columns),\n"
            report += "\t}\n"
        report += ")"

        return report

    @field_validator("fvs_data", mode="before")
    def serialize_dict_of_dataframes(
        cls,
        value: dict[FvsOutputTableName, pd.DataFrame],
    ) -> dict[FvsOutputTableName, list[dict]]:
        """Converts dataframes into dictionary of records."""
        return {
            key: val.to_dict(orient="records") for key, val in value.items()
        }

    @staticmethod
    def from_files(
        fvs_keyfile: FvsKeyfile,
        process: subprocess.CompletedProcess,
        path_to_outfile: str | os.PathLike,
        path_to_dbout: str | os.PathLike,
        add_stand_stock: bool = True,
        dbh_class: int = 4,
        large_dbh: int = 48,
    ):
        """Create an FvsResult from FVS CLI outputs.

        Args:
            fvs_keyfile (FvsKeyfile): an FvsKeyfile instance
            process (subprocess.CompletedProcess): result from FVS
                Command Line call
            path_to_dbout (str | os.PathLike): path to FVS output SQLite
                database
            path_to_outfile  (str | os.PathLike): path to *.out file
            add_stand_stock (bool, optional): Whether to generate the
                FVS_StdStk table
            dbh_class (int, optional): The width of each DBH class (in
                inches) used in the StdStk table. Ignored if
                `add_stand_stock` is False.
            large_dbh (int, optional): The largest DBH beyond which all
                DBH classes will be lumped together in the StdStk table.
                Ignored if `add_stand_stock` is False.
        """
        result_attributes = {
            "name": fvs_keyfile.name,
            "fvs_variant": fvs_keyfile.fvs_variant,
            "stand_id": fvs_keyfile.stand_id,
            "treatment": fvs_keyfile.treatment_name,
            "disturbance": fvs_keyfile.disturbance_name,
            "keyfile": fvs_keyfile.content,
            "fvs_data": FvsSqliteScraper.scrape(
                path_to_dbout,
                add_stand_stock=add_stand_stock,
                dbh_class=dbh_class,
                large_dbh=large_dbh,
            ),
            "outfile": Path(path_to_outfile).read_text(),
        }
        process_attributes = FvsResult._parse_fvs_process_attributes(process)
        result_attributes.update(process_attributes)

        return FvsResult.model_validate(result_attributes)

    @staticmethod
    def _parse_fvs_process_attributes(
        process: subprocess.CompletedProcess,
    ) -> dict[str, str | int | None]:
        """Extract attributes of CompletedProcess of FVS CLI run.

        Args:
            process (subprocess.CompletedProcess): the result of
                subprocess.run()

        Returns:
            dictionary of parsed attributes by name, including
            return_code, command, stderr, and stdout.
        """
        return {
            "command": " ".join(process.args),
            "return_code": process.returncode,
            "stdout": (
                process.stdout.decode()
                if process.stdout.decode() != ""
                else None
            ),
            "stderr": (
                process.stderr.decode() if process.returncode < 0 else None
            ),
        }

    @staticmethod
    def _parse_fvs_warnings_and_errors(
        outfile: str,
    ) -> list[FvsOutfileProblem] | None:
        """Parses warnings and errors from an FVS output file.

        Args:
        outfile (str): content of an FVS output file

        Returns:
            problems (list[FvsOutfileProblem] | None): list of any
            problems parsed from outfile
        """
        problems = []

        lines = outfile.split(LINE_ENDING_STRING)
        for i, line in enumerate(lines):
            if FVS_OUTFILE_WARNING_STRING in line:
                splitline = line.replace(
                    FVS_OUTFILE_WARNING_PREFIX, EMPTY_STRING
                ).split(FVS_OUTFILE_WARNING_STRING)
                _error_id = splitline[0].strip()
                msg = " ".join(splitline[-1].strip().split())
                if _error_id == FVS_OUTFILE_FIRE_FUELS_PREFIX and lines[
                    i + 1
                ].strip().startswith(FVS_OUTFILE_WARNING_PREFIX3):
                    add_msg = (
                        lines[i + 1]
                        .replace(FVS_OUTFILE_WARNING_PREFIX, EMPTY_STRING)
                        .strip()
                    )
                    msg = msg + f"; {add_msg}"

                error_id = _error_id if _error_id != EMPTY_STRING else None

                problems.append(
                    FvsOutfileProblem(
                        line_number=i,
                        type=FVS_OUTFILE_WARNING_STRING[:-1],
                        id=error_id,
                        message=msg,
                    )
                )

            elif FVS_OUTFILE_ERROR_STRING in line:
                splitline = line.replace(
                    FVS_OUTFILE_WARNING_PREFIX, EMPTY_STRING
                ).split(FVS_OUTFILE_ERROR_STRING)
                _error_id = splitline[0].strip()
                msg = " ".join(splitline[-1].strip().split())
                error_id = _error_id if _error_id != EMPTY_STRING else None
                problems.append(
                    FvsOutfileProblem(
                        line_number=i,
                        type=FVS_OUTFILE_ERROR_STRING[:-1],
                        id=error_id,
                        message=msg,
                    )
                )

        return problems if len(problems) > 0 else None


class FvsOutfileProblem(BaseModel):
    """Packages errors and warnings identified in FVS output files."""

    line_number: int
    type: str
    id: str | None
    message: str | None
