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
    config_version: str | None = None,
    config_dir: str | None = None,
    custom_config: str | None = None,
    uncertainty: bool = False,
    n_draws: int = 100,
    seed: int | None = None,
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
        config_version (str, optional): Which parameter set to use:
            None or 'default' = original FVS parameters
            'calibrated' = Bayesian posterior estimates from FIA data
            When 'calibrated', FVS keywords (SDIMAX, MORTMULT, etc.)
            are injected into the keyfile before running.
        config_dir (str, optional): Override path to the config directory.
        custom_config (str, optional): Path to a user supplied JSON
            config. Required when config_version='custom'.
        uncertainty (bool, optional): Enable Monte Carlo uncertainty
            estimation. When True, runs FVS n_draws times with different
            parameter sets sampled from the Bayesian posterior. Returns
            a list of FvsResults regardless of the limit parameter.
        n_draws (int, optional): Number of posterior draws for uncertainty
            estimation. Default is 100. Ignored when uncertainty=False.
        seed (int, optional): Random seed for reproducible uncertainty
            draws. Ignored when uncertainty=False.

    Returns:
        A single FvsResult (if `limit`=1 and uncertainty=False) or a
        list of FvsResults if `limit` > 1 or uncertainty=True.
    """  # noqa: W505
    # If uncertainty mode, delegate to the ensemble runner
    if uncertainty and config_version and config_version.lower() in ("calibrated", "custom"):
        return _run_fvs_ensemble(
            stand_init=stand_init,
            tree_init=tree_init,
            limit=limit,
            treatments=treatments,
            disturbances=disturbances,
            template=template,
            template_params=template_params,
            stand_stock_params=stand_stock_params,
            config_version=config_version,
            config_dir=config_dir,
            custom_config=custom_config,
            n_draws=n_draws,
            seed=seed,
        )

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
                keyfile_content = keyfile.content

                # Inject calibrated parameters if requested
                if config_version and config_version.lower() in ("calibrated", "custom"):
                    try:
                        from config.config_loader import FvsConfigLoader

                        loader = FvsConfigLoader(
                            fvs_variant.lower(),
                            version=config_version.lower(),
                            config_dir=config_dir,
                            custom_config=custom_config,
                        )
                        cal_keywords = loader.generate_keywords(include_comments=True)
                        # Insert before the PROCESS keyword
                        if "PROCESS" in keyfile_content:
                            keyfile_content = keyfile_content.replace(
                                "PROCESS",
                                cal_keywords + "\nPROCESS",
                            )
                        else:
                            keyfile_content += "\n" + cal_keywords
                    except Exception as e:
                        import logging

                        logging.warning(
                            f"Could not inject calibrated parameters: {e}"
                        )

                with open(keyfile_path, "w") as f:
                    f.write(keyfile_content)

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


def _run_fvs_ensemble(
    stand_init: FvsStandInit,
    tree_init: FvsTreeInit | None,
    limit: int,
    treatments,
    disturbances,
    template: str,
    template_params: dict,
    stand_stock_params,
    config_version: str,
    config_dir: str | None,
    custom_config: str | None,
    n_draws: int,
    seed: int | None,
) -> list[FvsResult]:
    """Run FVS ensemble for uncertainty quantification.

    Executes FVS n_draws times, each time injecting a different set of
    parameters drawn from the Bayesian posterior distribution. The result
    is a list of FvsResults whose variation represents parameter uncertainty
    in the projections.

    This function is called internally by run_fvs() when uncertainty=True.
    """
    import logging

    try:
        from config.uncertainty import UncertaintyEngine
        from config.config_loader import FvsConfigLoader
    except ImportError:
        raise ImportError(
            "config.uncertainty and config.config_loader must be importable "
            "for uncertainty estimation. Add fvs-modern root to PYTHONPATH."
        )

    stand_init_df = pd.DataFrame.from_records([stand_init.model_dump()])
    if not (len(stand_init_df) > 0):
        raise ValueError("Must provide data for one or more stands.")

    if tree_init is None:
        tree_init_df = pd.DataFrame(columns=FvsTreeInitRecord.__fields__.keys())
    else:
        tree_init_df = tree_init.to_dataframe()

    # Use the first stand's variant for the uncertainty engine
    fvs_variant = stand_init_df.iloc[0][FVS_VARIANT_COLUMN_NAME]

    engine = UncertaintyEngine(
        fvs_variant.lower(),
        config_dir=config_dir,
        seed=seed,
    )

    default_loader = FvsConfigLoader(
        fvs_variant.lower(),
        version="default",
        config_dir=config_dir,
    )
    default_config = default_loader.config

    actual_n = min(n_draws, engine.n_draws)
    ensemble_results: list[FvsResult] = []

    logging.info(f"Running uncertainty ensemble: {actual_n} draws for {fvs_variant}")

    for draw_i in range(actual_n):
        draw_idx = engine.sample_draw_index()
        draw = engine.get_draw(draw_idx)

        # Generate keywords for this specific draw
        draw_keywords = engine.generate_keywords_for_draw(
            draw, default_config, draw_idx=draw_idx
        )

        # Run each stand with this draw's parameters
        for idx, row in stand_init_df.iterrows():
            with tempfile.TemporaryDirectory() as temp_dir:
                db_path = os.path.join(temp_dir, FVS_DATABASE_NAME)
                conn = sqlite3.connect(db_path)

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
                keyfile_content = keyfile.content

                # Inject draw specific parameters before PROCESS
                if "PROCESS" in keyfile_content:
                    keyfile_content = keyfile_content.replace(
                        "PROCESS",
                        draw_keywords + "\nPROCESS",
                    )
                else:
                    keyfile_content += "\n" + draw_keywords

                with open(keyfile_path, "w") as f:
                    f.write(keyfile_content)

                cmd = [
                    f"/usr/local/bin/FVS{keyfile.fvs_variant.lower()}",
                    f"--keywordfile={keyfile.name}.key",
                ]
                process = subprocess.run(cmd, capture_output=True, cwd=temp_dir)

                ensemble_results.append(
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

            if len(ensemble_results) >= limit * actual_n:
                break

        if (draw_i + 1) % 10 == 0 or draw_i == actual_n - 1:
            logging.info(f"  Completed draw {draw_i + 1}/{actual_n}")

    logging.info(f"Ensemble complete: {len(ensemble_results)} total results")
    return ensemble_results
