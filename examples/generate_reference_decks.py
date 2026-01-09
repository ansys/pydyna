# Copyright (C) 2023 - 2026 ANSYS, Inc. and/or its affiliates.
# SPDX-License-Identifier: MIT
#
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

"""
Reference Deck Generation Script
================================

This script runs pre examples and collects the generated .k files as reference
decks for testing the keywords-based backend implementation.

The generated reference decks are saved to a known artifact collection location
for CI workflow upload.

Usage:
    python examples/generate_reference_decks.py [--output-dir OUTPUT_DIR] [--hostname HOSTNAME]

Examples to generate reference decks for:
    - Explicit/ball_plate.py
    - (More examples to be added)
"""

import argparse
import logging
import os
from pathlib import Path
import sys

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(levelname)s - %(message)s",
    datefmt="%Y-%m-%d %H:%M:%S",
)
logger = logging.getLogger(__name__)


def get_default_output_dir() -> Path:
    """Get the default output directory for reference decks."""
    return Path(__file__).parent / "reference_decks"


def generate_ball_plate_deck(hostname: str, output_dir: Path) -> bool:
    """Generate the ball_plate reference deck.

    Parameters
    ----------
    hostname : str
        The hostname of the pre server.
    output_dir : Path
        The directory to save the generated deck.

    Returns
    -------
    bool
        True if successful, False otherwise.
    """
    logger.info("Generating ball_plate reference deck...")

    try:
        from ansys.dyna.core.pre import examples, launch_dynapre
        from ansys.dyna.core.pre.dynamaterial import MatPiecewiseLinearPlasticity, MatRigid
        from ansys.dyna.core.pre.dynamech import (
            AnalysisType,
            Contact,
            ContactSurface,
            ContactType,
            DynaMech,
            NodeSet,
            PartSet,
            ShellFormulation,
            ShellPart,
            SolidFormulation,
            SolidPart,
            Velocity,
        )

        solution = launch_dynapre(ip=hostname)

        # Open input files
        fns = []
        path = examples.ball_plate + os.sep
        fns.append(path + "ball_plate.k")
        solution.open_files(fns)

        # Set termination time
        solution.set_termination(termination_time=10)

        # Create DynaMech
        ballplate = DynaMech(AnalysisType.NONE)
        solution.add(ballplate)

        # Define materials
        matrigid = MatRigid(mass_density=7.83e-6, young_modulus=207, poisson_ratio=0.3)
        matplastic = MatPiecewiseLinearPlasticity(
            mass_density=7.83e-6, young_modulus=207, yield_stress=0.2, tangent_modulus=2
        )

        # Define parts
        plate = ShellPart(1)
        plate.set_element_formulation(ShellFormulation.BELYTSCHKO_TSAY)
        plate.set_material(matplastic)
        plate.set_thickness(1)
        plate.set_integration_points(5)
        ballplate.parts.add(plate)

        ball = SolidPart(2)
        ball.set_material(matrigid)
        ball.set_element_formulation(SolidFormulation.CONSTANT_STRESS_SOLID_ELEMENT)
        ballplate.parts.add(ball)

        # Define contacts
        selfcontact = Contact(type=ContactType.AUTOMATIC)
        surf1 = ContactSurface(PartSet([1, 2]))
        selfcontact.set_slave_surface(surf1)
        ballplate.contacts.add(selfcontact)

        # Define SPCs
        spc = [
            34,
            35,
            51,
            52,
            68,
            69,
            85,
            86,
            102,
            103,
            119,
            120,
            136,
            137,
            153,
            154,
            170,
            171,
            187,
            188,
            204,
            205,
            221,
            222,
            238,
            239,
            255,
            256,
        ]
        for i in range(1, 19):
            spc.append(i)
        for i in range(272, 290):
            spc.append(i)
        ballplate.boundaryconditions.create_spc(NodeSet(spc), rx=False, ry=False, rz=False)

        # Define initial velocities
        for i in range(1, 1652):
            ballplate.initialconditions.create_velocity_node(i, trans=Velocity(0, 0, -10))

        # Define database outputs
        solution.set_output_database(glstat=0.1, matsum=0.1, sleout=0.1)
        solution.create_database_binary(dt=1)

        # Save the file
        serverpath = solution.save_file()

        # Download the output file
        serveroutfile = "/".join((serverpath, "ball_plate.k"))
        output_dir.mkdir(parents=True, exist_ok=True)
        downloadfile = output_dir / "ball_plate.k"
        solution.download(serveroutfile, str(downloadfile))

        logger.info(f"Generated ball_plate reference deck at: {downloadfile}")
        return True

    except Exception as e:
        logger.error(f"Failed to generate ball_plate reference deck: {e}")
        return False


# Registry of example generators
EXAMPLE_GENERATORS = {
    "ball_plate": generate_ball_plate_deck,
}


def main():
    """Main entry point for the reference deck generation script."""
    parser = argparse.ArgumentParser(description="Generate reference decks from pre examples for testing.")
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=get_default_output_dir(),
        help="Directory to save generated reference decks.",
    )
    parser.add_argument(
        "--hostname",
        type=str,
        default="localhost",
        help="Hostname of the pre server.",
    )
    parser.add_argument(
        "--examples",
        nargs="*",
        choices=list(EXAMPLE_GENERATORS.keys()) + ["all"],
        default=["all"],
        help="Examples to generate reference decks for.",
    )

    args = parser.parse_args()

    # Determine which examples to run
    if "all" in args.examples:
        examples_to_run = list(EXAMPLE_GENERATORS.keys())
    else:
        examples_to_run = args.examples

    logger.info(f"Output directory: {args.output_dir}")
    logger.info(f"Pre server hostname: {args.hostname}")
    logger.info(f"Examples to generate: {examples_to_run}")

    # Create output directory
    args.output_dir.mkdir(parents=True, exist_ok=True)

    # Run example generators
    results = {}
    for example_name in examples_to_run:
        generator = EXAMPLE_GENERATORS[example_name]
        success = generator(args.hostname, args.output_dir)
        results[example_name] = success

    # Summary
    logger.info("=" * 50)
    logger.info("Generation Summary:")
    for name, success in results.items():
        status = "SUCCESS" if success else "FAILED"
        logger.info(f"  {name}: {status}")

    # Exit with error if any failed
    if not all(results.values()):
        sys.exit(1)


if __name__ == "__main__":
    main()
