#!/usr/bin/env python
"""
FVS Keyword File Export Tool

Generates FVS keyword files from calibrated JSON configurations. Supports
single variant export, keyfile injection, uncertainty ensembles, and batch
processing of all variants.

Usage examples:

    # Single variant keyword export
    python export_keywords.py --variant ne --output ne_calibrated.key

    # Inject into existing keyfile
    python export_keywords.py --variant ne --keyfile input.key --output output.key

    # Uncertainty ensemble (50 draws with seed)
    python export_keywords.py --variant ne --keyfile input.key \\
        --n-draws 50 --seed 42 --output-dir ensemble/

    # All variants
    python export_keywords.py --all-variants --output-dir calibrated_keywords/

    # List available variants
    python export_keywords.py --list
"""

from __future__ import annotations

import argparse
import logging
import sys
from pathlib import Path
from typing import Optional

from config_loader import FvsConfigLoader
from uncertainty import UncertaintyEngine

logger = logging.getLogger(__name__)


class KeywordExporter:
    """Generates and manages FVS keyword file export operations."""

    def __init__(self, config_dir: Optional[str | Path] = None):
        """Initialize the exporter.

        Args:
            config_dir: Path to config directory (defaults to script directory)
        """
        self.config_dir = Path(config_dir) if config_dir else Path(__file__).parent

    def list_available_variants(self) -> list[str]:
        """List all variants with calibrated configurations.

        Returns:
            Sorted list of variant codes
        """
        calibrated_dir = self.config_dir / "calibrated"
        if not calibrated_dir.exists():
            return []

        variants = set()
        for json_file in calibrated_dir.glob("*.json"):
            name = json_file.stem
            # Filter out draws files (e.g., ne_draws.json)
            if not name.endswith("_draws"):
                variants.add(name)

        return sorted(variants)

    def export_single_variant(
        self, variant: str, output_path: str | Path, include_comments: bool = True
    ) -> None:
        """Export keywords for a single variant.

        Args:
            variant: FVS variant code (e.g., 'ne')
            output_path: Path to write keyword file
            include_comments: Include descriptive comments in output
        """
        output_path = Path(output_path)
        output_path.parent.mkdir(parents=True, exist_ok=True)

        loader = FvsConfigLoader(variant, version="calibrated")
        keywords = loader.generate_keywords(include_comments=include_comments)

        with open(output_path, "w") as f:
            f.write(keywords)

        logger.info(f"Exported {variant} keywords to {output_path}")

    def inject_into_keyfile(
        self,
        variant: str,
        input_keyfile: str | Path,
        output_keyfile: str | Path,
        include_comments: bool = True,
    ) -> None:
        """Inject calibrated keywords into an existing keyfile.

        The keywords are inserted before the PROCESS line. If no PROCESS line
        exists, they are appended at the end.

        Args:
            variant: FVS variant code
            input_keyfile: Path to input .key file
            output_keyfile: Path to write modified .key file
            include_comments: Include descriptive comments
        """
        input_keyfile = Path(input_keyfile)
        output_keyfile = Path(output_keyfile)
        output_keyfile.parent.mkdir(parents=True, exist_ok=True)

        # Read input keyfile
        with open(input_keyfile) as f:
            content = f.read()

        # Generate keywords
        loader = FvsConfigLoader(variant, version="calibrated")
        keywords = loader.generate_keywords(include_comments=include_comments)

        # Find PROCESS line
        lines = content.split("\n")
        process_idx = None
        for i, line in enumerate(lines):
            if line.strip().upper().startswith("PROCESS"):
                process_idx = i
                break

        # Build output
        if process_idx is not None:
            # Insert before PROCESS
            output_lines = lines[:process_idx] + ["", keywords] + lines[process_idx:]
        else:
            # Append at end
            output_lines = lines + ["", keywords]

        output_content = "\n".join(output_lines)

        with open(output_keyfile, "w") as f:
            f.write(output_content)

        logger.info(f"Injected {variant} keywords into {output_keyfile}")

    def export_uncertainty_ensemble(
        self,
        variant: str,
        input_keyfile: str | Path,
        output_dir: str | Path,
        n_draws: int = 50,
        seed: Optional[int] = None,
    ) -> None:
        """Generate an ensemble of keyfiles with different posterior draws.

        Each keyfile receives a different sample from the posterior distribution,
        allowing for Monte Carlo uncertainty quantification when run repeatedly.

        Args:
            variant: FVS variant code
            input_keyfile: Path to template .key file
            output_dir: Directory to write ensemble files
            n_draws: Number of draws to generate
            seed: Random seed for reproducibility
        """
        output_dir = Path(output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)

        # Initialize uncertainty engine
        engine = UncertaintyEngine(variant, config_dir=self.config_dir, seed=seed)

        if not engine.draws_available:
            raise FileNotFoundError(
                f"No posterior draws available for variant '{variant}'. "
                f"Check that {self.config_dir}/calibrated/{variant}_draws.json exists."
            )

        # Load default config for use in draw processing
        default_loader = FvsConfigLoader(variant, version="default")
        default_config = default_loader.config

        # Read template keyfile
        with open(input_keyfile) as f:
            content = f.read()

        lines = content.split("\n")
        process_idx = None
        for i, line in enumerate(lines):
            if line.strip().upper().startswith("PROCESS"):
                process_idx = i
                break

        # Generate draws
        for draw_num in range(n_draws):
            # Sample a random draw index
            draw_idx = engine.sample_draw_index()

            # Get the draw parameters
            draw = engine.get_draw(draw_idx)

            # Generate keywords for this draw
            keywords = engine.generate_keywords_for_draw(
                draw, default_config, draw_idx=draw_idx
            )

            # Build output
            if process_idx is not None:
                output_lines = (
                    lines[:process_idx]
                    + ["", keywords]
                    + lines[process_idx:]
                )
            else:
                output_lines = lines + ["", keywords]

            output_content = "\n".join(output_lines)

            # Write ensemble file
            output_path = output_dir / f"draw_{draw_num + 1:03d}.key"
            with open(output_path, "w") as f:
                f.write(output_content)

            if (draw_num + 1) % 10 == 0 or draw_num == 0:
                logger.info(
                    f"Generated {draw_num + 1}/{n_draws} ensemble files "
                    f"({draw_num + 1} of {n_draws})"
                )

        logger.info(
            f"Completed uncertainty ensemble: {n_draws} keyfiles in {output_dir}"
        )

    def export_all_variants(
        self, output_dir: str | Path, include_comments: bool = True
    ) -> None:
        """Export keyword files for all available variants.

        Args:
            output_dir: Directory to write variant keyfiles
            include_comments: Include descriptive comments
        """
        output_dir = Path(output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)

        variants = self.list_available_variants()
        if not variants:
            logger.warning("No calibrated variants found")
            return

        for i, variant in enumerate(variants, 1):
            try:
                output_path = output_dir / f"{variant}_calibrated.key"
                self.export_single_variant(variant, output_path, include_comments)
                logger.info(f"[{i}/{len(variants)}] Exported {variant}")
            except Exception as e:
                logger.error(f"[{i}/{len(variants)}] Failed to export {variant}: {e}")


def main() -> int:
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )

    # Mode selection
    mode_group = parser.add_mutually_exclusive_group(required=True)
    mode_group.add_argument(
        "--variant",
        type=str,
        help="FVS variant code (e.g., 'ne', 'ca'). Use with --output or --keyfile.",
    )
    mode_group.add_argument(
        "--all-variants",
        action="store_true",
        help="Export keyword files for all available variants.",
    )
    mode_group.add_argument(
        "--list", action="store_true", help="List all available variants."
    )

    # Output options
    parser.add_argument(
        "--output",
        type=str,
        help="Output keyfile path (for single variant export)",
    )
    parser.add_argument(
        "--keyfile",
        type=str,
        help="Input keyfile to inject or use as template",
    )
    parser.add_argument(
        "--output-dir",
        type=str,
        help="Output directory (for ensemble or all-variants mode)",
    )

    # Uncertainty ensemble options
    parser.add_argument(
        "--n-draws",
        type=int,
        default=None,
        help="Number of posterior draws for ensemble (triggers ensemble mode)",
    )
    parser.add_argument(
        "--seed",
        type=int,
        help="Random seed for reproducible draw sequences",
    )

    # Misc options
    parser.add_argument(
        "--config-dir",
        type=str,
        help="Path to config directory (default: script directory)",
    )
    parser.add_argument(
        "-v",
        "--verbose",
        action="store_true",
        help="Enable verbose logging",
    )

    args = parser.parse_args()

    # Configure logging
    log_level = logging.DEBUG if args.verbose else logging.INFO
    logging.basicConfig(
        level=log_level,
        format="%(levelname)s: %(message)s",
    )

    exporter = KeywordExporter(config_dir=args.config_dir)

    try:
        # List mode
        if args.list:
            variants = exporter.list_available_variants()
            if variants:
                print(f"Available variants ({len(variants)}):")
                for variant in variants:
                    print(f"  {variant}")
            else:
                print("No calibrated variants found")
            return 0

        # All variants mode
        if args.all_variants:
            if not args.output_dir:
                parser.error("--output-dir is required with --all-variants")
            exporter.export_all_variants(args.output_dir)
            return 0

        # Single variant mode
        if args.variant:
            if args.n_draws is not None:
                # Ensemble mode
                if not args.keyfile:
                    parser.error(
                        "--keyfile is required for uncertainty ensemble (--n-draws)"
                    )
                if not args.output_dir:
                    parser.error(
                        "--output-dir is required for uncertainty ensemble"
                    )
                exporter.export_uncertainty_ensemble(
                    args.variant,
                    args.keyfile,
                    args.output_dir,
                    n_draws=args.n_draws,
                    seed=args.seed,
                )
            elif args.keyfile:
                # Injection mode
                if not args.output:
                    parser.error("--output is required with --keyfile")
                exporter.inject_into_keyfile(
                    args.variant, args.keyfile, args.output
                )
            else:
                # Simple export mode
                if not args.output:
                    parser.error("--output is required for single variant export")
                exporter.export_single_variant(args.variant, args.output)
            return 0

    except FileNotFoundError as e:
        logger.error(str(e))
        return 1
    except Exception as e:
        logger.error(f"Unexpected error: {e}")
        if args.verbose:
            raise
        return 1

    return 0


if __name__ == "__main__":
    sys.exit(main())
