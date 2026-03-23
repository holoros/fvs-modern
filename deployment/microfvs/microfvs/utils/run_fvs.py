import os
import sqlite3
import subprocess
import tempfile

import pandas as pd

from microfvs.constants import (
    FVS_DATABASE_NAME,
    FVS_VARIANT_COLUMN_NAME,
    STAND_ID_COLUMN_NAME,
)
from microfvs.enums import FvsKeyfileTemplate
from microfvs.models import (
    FvsEvent,
    FvsKeyfile,
    FvsKeyfileTemplateParams,
    FvsResult,
    FvsStandInit,
    FvsStandStockParams,
    FvsTreeInit,
    FvsTreeInitRecord,
)


def run_fvs(
    stand_init: FvsStandInit,
    tree_init: FvsTreeInit | None = None,
    limit: int = 1,
    treatments: list[FvsEvent | str] | dict[str, FvsEvent] | str = ["GROW"],
    disturbances: list[FvsEvent | str] | dict[str, FvsEvent | str] = [
        "UNDISTURBED"
    ],
    template: str = FvsKeyfileTemplate.DEFAULT,
    template_params: dict = {},
    stand_stock_params=FvsStandStockParams(),
) -> FvsResult | list[FvsResult]:
    """Runs a batch of FVS simulations and returns the result(s).

    Args:
        stand_init (FvsStandInit): Stand initialization data for one or
            more stands. All stands represented in stand_init will be
            run in the order specified unless or until `limit` is
            reached.
        tree_init (FvsTreeInit, optional): Tree initialization data for
            one or more stands. If not provided, bare ground will be
            simulated.
        limit (int, optional): batch size to which the number of
            simulations will be capped.
        treatments (list[FvsEvent], dict[str, list[FvsEvent]], optional):  # noqa: W505
            Treatments to be simulated. If specified as a
            list of FvsEvents, the same set of events will be applied to
            all stands. Can be specified as a dict with keys referring
            to stand_ids and dict values corresponding to a list of
            treatments to apply for that stand. By default, no
            treatments will be simulated for any stand.
        disturbances (list[FvsEvent], dict[str, list[FvsEvent]], optional):  # noqa: W505
            Disturbances to be simulated. If specified as a
            list of FvsEvents, the same set of events will be applied to
            all stands. Can be specified as a dict with keys referring
            to stand_ids and dict values corresponding to a list of
            disturbances to apply for that stand. By default,
            no disturbance will be simulated for any stand.
        template (str, optional): FVS keyfile template to use. Defaults
            to FvsKeyfileTemplate.DEFAULT
        template_params (dict, optional):
            Additional parameters to inject into the template
        stand_stock_params (FvsStandStockParams): Optional set of
            parameters to govern the generation of a Stand and Stock
            Table in the FVS outputs. Default is to produce the Stand
            and Stock Table, and to do so using DBH classes of 4 inches
            and a large diameter category starting at 48 inches DBH.

    Returns:
        A single FvsResult (if `limit`=1) or a list of FvsResults if
        `limit` > 1.
    """  # noqa: W505
    results: list[FvsResult] = []

    stand_init_df = pd.DataFrame.from_records([stand_init.model_dump()])
    if not (len(stand_init_df) > 0):
        msg = "Must provide data for one or more stands. Found zero."
        raise ValueError(msg)
    MORE_STANDS_TO_PROCESS = True

    if tree_init is None:
        tree_init_df = pd.DataFrame(columns=FvsTreeInitRecord.__fields__.keys())
    else:
        tree_init_df = tree_init.to_dataframe()

    while len(results) < limit and MORE_STANDS_TO_PROCESS:
        for idx, row in stand_init_df.iterrows():
            with tempfile.TemporaryDirectory() as temp_dir:
                db_path = os.path.join(temp_dir, FVS_DATABASE_NAME)
                conn = sqlite3.connect(db_path)

                fvs_variant = row[FVS_VARIANT_COLUMN_NAME]
                stand_id = row[STAND_ID_COLUMN_NAME]
                stand_data = stand_init_df.loc[idx:idx]
                stand_data.to_sql(
                    "fvs_standinit", conn, if_exists="replace", index=False
                )
                tree_data = tree_init_df.loc[
                    tree_init_df[STAND_ID_COLUMN_NAME] == stand_id
                ]
                tree_data.to_sql(
                    "fvs_treeinit", conn, if_exists="replace", index=False
                )

                params = FvsKeyfileTemplateParams(
                    variant=fvs_variant,
                    stand_id=stand_id,
                    treatments=treatments,
                    disturbances=disturbances,
                    **template_params,
                )
                keyfile = FvsKeyfile(template=template, params=params)

                keyfile_path = os.path.join(temp_dir, keyfile.name + ".key")
                with open(keyfile_path, "w") as f:
                    f.write(keyfile.content)

                cmd = [
                    f"/usr/local/bin/FVS{keyfile.fvs_variant.lower()}",
                    f"--keywordfile={keyfile.name}.key",
                ]
                process = subprocess.run(cmd, capture_output=True, cwd=temp_dir)

                results.append(
                    FvsResult.from_files(
                        fvs_keyfile=keyfile,
                        process=process,
                        path_to_dbout=db_path,
                        path_to_outfile=keyfile_path.replace(".key", ".out"),
                        add_stand_stock=stand_stock_params.add_stand_stock,
                        dbh_class=stand_stock_params.dbh_class,
                        large_dbh=stand_stock_params.large_dbh,
                    )
                )
        MORE_STANDS_TO_PROCESS = False
    if limit == 1:
        return results[0]
    return results
