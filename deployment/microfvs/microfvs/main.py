from __future__ import annotations

import importlib.resources
from typing import Annotated

from fastapi import Body, FastAPI, HTTPException
from fastapi.responses import PlainTextResponse

from microfvs.constants import TEST_STANDINIT_RECORDS, TEST_TREEINIT_RECORDS
from microfvs.enums import FvsKeyfileTemplate
from microfvs.models import (
    FvsEvent,
    FvsEventLibrary,
    FvsEventType,
    FvsKeyfile,
    FvsKeyfileTemplateParams,
    FvsResult,
    FvsStandInit,
    FvsStandStockParams,
    FvsTreeInit,
    FvsVariant,
)
from microfvs.utils.fvs_version import get_fvs_versions
from microfvs.utils.run_fvs import run_fvs

app = FastAPI(
    title="fvs-modern API",
    description=(
        "REST API for the Forest Vegetation Simulator, powered by "
        "fvs-modern (modernized Fortran source) and microFVS (API layer by "
        "Vibrant Planet). Supports all 24 regional variants (22 US + 2 Canadian)."
    ),
    version="0.2.0",
)


@app.get("/version")
def check_fvs_version() -> dict[str, str]:
    """Reports the version of FVS running in the web service."""
    return get_fvs_versions()


@app.get("/variants")
def list_variants() -> dict[str, list[dict[str, str]]]:
    """Lists all available FVS variants with their codes and descriptions."""
    return {
        "variants": [
            {"code": v.value, "name": v.name.replace("_", " ").title()}
            for v in FvsVariant
        ]
    }


@app.get("/template", response_class=PlainTextResponse)
def example_keyfile_template() -> str:
    """Return default template for simulating a single stand in FVS."""
    return FvsKeyfileTemplate.DEFAULT


@app.post("/keyfile", response_class=PlainTextResponse)
def generate_keyfile_from_template(
    template: Annotated[
        str,
        Body(examples=[FvsKeyfileTemplate.DEFAULT]),
    ],
    params: Annotated[
        FvsKeyfileTemplateParams,
        Body(
            examples=[
                FvsKeyfileTemplateParams(
                    variant=FvsVariant.PACIFIC_COAST.value,
                    stand_id="12345",
                    num_cycles=1,
                    cycle_length=5,
                    treatments=[
                        FvsEvent(name="GROW", content="*** NO TREATMENT ***")
                    ],
                    disturbances=[
                        FvsEvent(
                            name="UNDISTURBED", content="*** NO DISTURBANCE ***"
                        )
                    ],
                )
            ]
        ),
    ],
) -> str:
    """Generates a FVS Keyfile with user-specified parameters.

    Args:
        template (str): str containing jinja2 FVS Keyfile template
        params (FvsKeyfileTemplateParams, optional): parameters to
            inject into the keyfile template, parameter values will be
            injected to parameter names found in the template.
    """
    return FvsKeyfile(
        template=template,
        params=params,
    ).content


@app.post("/run")
def run_fvs_batch(
    stand_init: Annotated[
        FvsStandInit,
        Body(examples=[FvsStandInit.model_validate(TEST_STANDINIT_RECORDS[0])]),
    ],
    tree_init: Annotated[
        FvsTreeInit,
        Body(examples=[FvsTreeInit.from_records(TEST_TREEINIT_RECORDS)]),
    ] = FvsTreeInit(),
    limit: int = 1,
    template: Annotated[
        str, Body(examples=[FvsKeyfileTemplate.DEFAULT])
    ] = FvsKeyfileTemplate.DEFAULT,
    template_params: Annotated[
        dict, Body(examples=[{"first_cycle_length": 3}])
    ] = {},
    stand_stock_params: FvsStandStockParams = FvsStandStockParams(),
) -> FvsResult | list[FvsResult]:
    """Runs FVS and returns the results.

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
            Treatments to be simulated. If specified as a list of
            FvsEvents, the same set of events will be applied to all
            stands. Can be specified as a dict with keys referring to
            stand_ids and dict values corresponding to a list of
            treatments to apply for that stand. By default, no
            treatments will be simulated for any stand.
        disturbances (list[FvsEvent], dict[str, list[FvsEvent]], optional):
            Disturbances to be simulated. If specified as a list of FvsEvents,
            the same set of events will be applied to all stands. Can be
            specified as a dict with keys referring to stand_ids and dict values
            corresponding to a list of disturbances to apply for that stand. By
            default, no disturbance will be simulated for any stand.
        template (str, optional): FVS keyfile template to use. Defaults to
            FvsKeyfileTemplate.DEFAULT
        template_params (dict, optional):
            Additional parameters to inject into the template
        stand_stock_params (FvsStandStockParams): Optional set of parameters to
            govern the generation of a Stand and Stock Table in the FVS
            outputs. Default is to produce the Stand and Stock Table, and
            to do so using DBH classes of 4 inches and a large diameter
            category starting at 48 inches DBH.
    """  # noqa: W505
    return run_fvs(
        stand_init=stand_init,
        tree_init=tree_init,
        limit=limit,
        template=template,
        template_params=template_params,
        stand_stock_params=stand_stock_params,
    )


@app.post("/outfile", response_class=PlainTextResponse)
def get_outfile(
    stand_init: Annotated[
        FvsStandInit,
        Body(examples=[FvsStandInit.model_validate(TEST_STANDINIT_RECORDS[0])]),
    ],
    tree_init: Annotated[
        FvsTreeInit,
        Body(examples=[FvsTreeInit.from_records(TEST_TREEINIT_RECORDS)]),
    ] = FvsTreeInit(),
    template: Annotated[
        str, Body(examples=[FvsKeyfileTemplate.DEFAULT])
    ] = FvsKeyfileTemplate.DEFAULT,
    template_params: Annotated[
        dict, Body(examples=[{"first_cycle_length": 3}])
    ] = {},
    stand_stock_params: FvsStandStockParams = FvsStandStockParams(),
) -> str:
    """Runs FVS and returns the OUT file.

    If multiple stands are included in `stand_init`, only the outfile
    from thefirst stand is returned.

    Args:
        stand_init (FvsStandInit): Stand initialization data for one or
            more stands. All stands represented in stand_init will be
            run in the order specified unless or until `limit` is
            reached.
        tree_init (FvsTreeInit, optional): Tree initialization data for
            one or more stands. If not provided, bare ground will be
            simulated.
        treatments (list[FvsEvent], dict[str, list[FvsEvent]], optional):  # noqa: W505
            Treatments to be simulated. If specified as a list of
            FvsEvents, the same set of events will be applied to all
            stands. Can be specified as a dict with keys referring to
            stand_ids and dict values corresponding to a list of
            treatments to apply for that stand. By default, no
            treatments will be simulated for any stand.
        disturbances (list[FvsEvent], dict[str, list[FvsEvent]], optional):  # noqa: W505
            Disturbances to be simulated. If specified as a list of
            FvsEvents, the same set of events will be applied to all
            stands. Can be specified as a dict with keys referring to
            stand_ids and dict values corresponding to a list of
            disturbances to apply for that stand. By default, no
            disturbance will be simulated for any stand.
        template (str, optional): FVS keyfile template to use. Defaults to
            FvsKeyfileTemplate.DEFAULT
        template_params (dict, optional):
            Additional parameters to inject into the template
        stand_stock_params (FvsStandStockParams): Optional set of parameters to
            govern the generation of a Stand and Stock Table in the FVS
            outputs. Default is to produce the Stand and Stock Table, and
            to do so using DBH classes of 4 inches and a large diameter
            category starting at 48 inches DBH.
    """  # noqa: W505
    result = run_fvs(
        stand_init=stand_init,
        tree_init=tree_init,
        limit=1,
        template=template,
        template_params=template_params,
        stand_stock_params=stand_stock_params,
    )
    return (
        result.outfile if isinstance(result, FvsResult) else result[0].outfile
    )


@app.get("/treatments")
def all_usfs_fvs_treatment_codes() -> dict[str, list[str]]:
    """Returns all USFS FVS treament codes."""
    with importlib.resources.as_file(
        importlib.resources.files(
            "microfvs.keyword_components.treatments.usfs"
        ).joinpath("")
    ) as path:
        return {
            "USFS Treatments": sorted([x.stem for x in path.rglob("*.kcp")])
        }


@app.get("/treatments/{treatment_code}", response_class=PlainTextResponse)
def get_treatment_kcp(treatment_code: str) -> str:
    """Gets the content of a FVS FvsEvent KCP file.

    Args:
        treatment_code (str): code for treatment recognized by MicroFVS
    """
    library = FvsEventLibrary()
    if treatment_code not in library.treatments:
        raise HTTPException(status_code=404, detail="Treatment not found.")
    treatment = library.lookup(
        event_type=FvsEventType.TREATMENT, event_key=treatment_code
    )
    return treatment.content
