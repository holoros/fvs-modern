from __future__ import annotations

import errno
import os
import warnings
from pathlib import Path

import pandas as pd
import sqlalchemy as db
from pandas._typing import DtypeArg
from sqlalchemy.engine.base import Engine

from microfvs.enums import FvsOutputTableName
from microfvs.utils.stand_stock import (
    FvsStandStockWarning,
    make_stand_stock_table,
)


def get_sqlite_engine(path_to_db: str | os.PathLike) -> Engine:
    """Get a SQL Alchemy "engine" for interacting with a SQLite DB.

    Args:
        path_to_db (str | os.PathLike): path to the SQLite database.

    Returns:
        engine: "home base" for the actual database and its DBAPI.
        Details here: https://docs.sqlalchemy.org/en/20/core/engines.html
    """
    path = os.path.abspath(path_to_db)
    if not os.path.exists(path):
        raise FileNotFoundError(errno.ENOENT, os.strerror(errno.ENOENT), path)

    path = Path(path)
    return db.create_engine(f"sqlite:////{path}")


class SqliteScraper:
    """Scraper for SQLite databases into a dictionary of DataFrames."""

    @staticmethod
    def _scrape_engine(
        engine: Engine, dtype: DtypeArg | None = None
    ) -> dict[str, pd.DataFrame]:
        """Scrapes a SQLite database into a dict of Pandas DataFrames.

        Args:
            engine (Engine): SQLAlchemy engine that can be used to
                connect to the DB.
            dtype (DtypeArg, optional): Type name or dict of columns.
                Data type for data or columns. E.g. np.float64 or {'a':
                np.float64, 'b': 'Int64'}. Passed directly to
                `pd.read_sql`.

        Returns:
            dict with table names as keys and DataFrames as values.
        """
        tables = pd.read_sql(
            "SELECT name FROM sqlite_master WHERE type='table'", engine
        )["name"].to_numpy()

        scraped = {}
        for t in tables:
            data = pd.read_sql(f"SELECT * FROM {t}", engine, dtype=dtype)
            data.columns = [
                col.lower().replace(" ", "_") for col in data.columns
            ]
            scraped[t.lower()] = data

        return scraped

    @classmethod
    def scrape(
        cls, path_to_db: str | os.PathLike, dtype: DtypeArg | None = None
    ) -> dict[str, pd.DataFrame]:
        """Scrapes a SQLite database into a dict of DataFrames.

        Args:
            path_to_db (str | os.PathLike): path to the SQLite database.
            dtype (DtypeArg, optional): Type name or dict of columns.
                Data type fordata or columns. E.g. np.float64 or {'a':
                np.float64, 'b': 'Int64'}. Passed to `pd.read_sql`.


        Returns:
            dict with table names as keys and DataFrames as values.
        """
        return cls._scrape_engine(get_sqlite_engine(path_to_db), dtype=dtype)


class FvsSqliteScraper(SqliteScraper):
    """Extension of SqliteScraper to add StdStk and FVS-specific tables.

    The extension to SqliteScraper is primarily intended to extend the
    scrape method to add the generation of the FVS_StdStk table.
    """

    @classmethod
    def scrape(
        cls,
        path_to_db: str | os.PathLike,
        dtype: DtypeArg | None = None,
        add_stand_stock: bool = True,
        dbh_class: int = 4,
        large_dbh: int = 48,
    ) -> dict[str, pd.DataFrame]:
        """Scrapes a FVS SQLite database into a dict of DataFrames.

        Args:
            path_to_db (str | os.PathLike): path to the SQLite database.
            dtype (DtypeArg, optional): Type name or dict of columns.
                Data type for data or columns. E.g. np.float64 or {'a':
                np.float64, 'b': 'Int64'}. Passed to `pd.read_sql` when
                scraping tables from the database.
            add_stand_stock (bool, optional): Whether to generate the
                FVS_StdStk table.
            dbh_class (int, optional): The width of each DBH class (in
                inches) used in the StdStk table. Ignored if
                `add_stand_stock` is False.
            large_dbh (int, optional): The largest DBH beyond which all
                DBH classes will be lumped together in the StdStk table.
                Ignored if `add_stand_stock` is False.

        Returns:
            scraped_fvs_data (dict[FvsOutputTableName: pd.DataFrame]):
            Dict with table names as keys and DataFrames as values.
        """
        scraped_fvs_data = super().scrape(path_to_db, dtype=dtype)

        if add_stand_stock:
            with warnings.catch_warnings(record=True) as recorded_warnings:
                scraped_fvs_data[FvsOutputTableName.FVS_STAND_STOCK.value] = (
                    make_stand_stock_table(
                        scraped_fvs_data, dbh_class, large_dbh
                    )
                )
            for w in recorded_warnings:
                # want to add path_to_db to warning from
                # make_stand_stock_table, which doesn't mention the
                # original database, just the dataframes scraped from
                # it. Catch that warning and update message.
                if isinstance(w, FvsStandStockWarning):
                    w.message = (
                        "No tree list tables found in FVS output database at"
                        f" {path_to_db}, FVS_StdStk will be empty."
                    )
                # continue to issue all warnings
                warnings.warn_explicit(
                    message=w.message,
                    category=w.category,
                    filename=w.filename,
                    lineno=w.lineno,
                    source=w.source,
                )

        return scraped_fvs_data
