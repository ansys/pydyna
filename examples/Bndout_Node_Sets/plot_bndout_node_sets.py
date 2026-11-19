# Copyright (C) 2026 Ji Peng.
# SPDX-License-Identifier: MIT

"""
Link node sets to bndout force histories
------------------------------------------------------------
This example links explicit ``*SET_NODE_LIST`` definitions to nodal force
records in an existing ASCII ``bndout`` file. It uses
``Deck.get_set_by_id()`` and sums force components by node ID and time.
No solver, DPF server, or model API is required.

The bundled data are synthetic arithmetic examples, not simulation results.
Set 1 selects velocity-boundary forces and set 2 selects discrete applied
forces. These sources are kept separate, including when a node occurs in both.

For real files, select the appropriate source explicitly. The ``setid`` printed
on a bndout record is not used as the input deck's node-set ID. Forces retain
the coordinate basis and consistent units of the source output; the example
does not transform vectors or infer units. A difference between two histories
does not, on its own, demonstrate a force-balance error in a dynamic model.
"""

import argparse
import logging
import math
from pathlib import Path
import re
import tempfile

import matplotlib.pyplot as plt
import pandas as pd

from ansys.dyna.core import Deck, keywords

logger = logging.getLogger(__name__)

SOURCES = {
    "discrete": "discrete nodal point forces",
    "velocity": "velocity boundary condition forces/rigid body moments",
}
COMPONENTS = ["Fx", "Fy", "Fz"]
_TIME = re.compile(r"^\s*n o d a l\s+f o r c e/e n e r g y\s+o u t p u t\s+t=\s*(\S+)\s*$")
_NODE = re.compile(
    r"^\s*nd#\s*(\d+)\s+xforce=\s*(\S+)\s+yforce=\s*(\S+)"
    r"\s+zforce=\s*(\S+)\s+energy=\s*(\S+)(?:\s+setid\s*=\s*\d+)?\s*$"
)


###############################################################################
# Read the two supported nodal-force sections
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read each line once. Keep the source on every record so that applied loads
# and velocity-boundary forces cannot be accidentally added together.


def _number(text: str, location: str) -> float:
    """Read a finite real number, including Fortran D exponents."""
    try:
        value = float(text.replace("D", "E").replace("d", "e"))
    except ValueError as exc:
        raise ValueError(f"{location}: invalid number {text!r}") from exc
    if not math.isfinite(value):
        raise ValueError(f"{location}: non-finite number {text!r}")
    return value


def read_nodal_forces(path: Path) -> pd.DataFrame:
    """Read supported ``nd#`` records with their time, node ID, and source.

    Raises ``ValueError`` for malformed records, empty supported sections, and
    duplicate (source, time, node ID) records. Other section types are ignored;
    rigid-body records inside a supported section are rejected. This reader
    deliberately covers the two ASCII formats demonstrated here.
    """
    rows = []
    blocks = {}
    time = None
    source = None
    headings = {heading: name for name, heading in SOURCES.items()}
    with Path(path).open(encoding="utf-8-sig") as stream:
        for line_number, line in enumerate(stream, start=1):
            location = f"{path}:{line_number}"
            heading = " ".join(line.split())
            if match := _TIME.fullmatch(line):
                if source is not None:
                    raise ValueError(f"{location}: incomplete {source} section before the next time header")
                time = _number(match[1], location)
                source = None
            elif "n o d a l" in line and "t=" in line:
                raise ValueError(f"{location}: malformed time header")
            elif heading in headings:
                if source is not None:
                    raise ValueError(f"{location}: incomplete {source} section before the next force section")
                source = headings[heading]
                if time is None:
                    raise ValueError(f"{location}: force section has no time header")
                key = (time, source)
                if key in blocks:
                    raise ValueError(f"{location}: repeated {source} section at time {time}")
                blocks[key] = 0
            elif source is not None and heading:
                if heading.startswith("xtotal="):
                    source = None
                    continue
                match = _NODE.fullmatch(line)
                if match is None:
                    raise ValueError(f"{location}: malformed or unsupported nodal-force record")
                node_id = int(match[1])
                if node_id <= 0:
                    raise ValueError(f"{location}: node ID must be positive")
                values = [_number(value, location) for value in match.groups()[1:]]
                rows.append((time, node_id, source, *values[:3]))
                blocks[(time, source)] += 1
    if source is not None:
        raise ValueError(f"{path}: incomplete {source} section at end of file")
    if not rows:
        raise ValueError(f"{path}: no supported nodal-force records")
    if any(count == 0 for count in blocks.values()):
        raise ValueError(f"{path}: empty nodal-force section")
    forces = pd.DataFrame(rows, columns=["time", "node_id", "source", *COMPONENTS])
    duplicates = forces.duplicated(["source", "time", "node_id"])
    if duplicates.any():
        duplicate = forces.loc[duplicates, ["source", "time", "node_id"]].iloc[0].to_dict()
        raise ValueError(f"{path}: duplicate nodal-force record: {duplicate}")
    return forces


###############################################################################
# Join input node sets to the selected force source
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Membership comes from the deck. All selected nodes must be present in the
# chosen source at every recorded time. Missing output is not treated as zero.
# Repeated node IDs in an input set represent one member and are counted once.


def aggregate_node_sets(deck: Deck, forces: pd.DataFrame, selections: dict[int, str]) -> pd.DataFrame:
    """Sum each selected set's force components, retaining time and source.

    ``selections`` maps input node-set IDs to ``discrete`` or ``velocity``.
    All selected sources must cover the same reported times. Generated sets,
    missing sets or nodes, and empty sets produce an explicit error.
    """
    if not selections:
        raise ValueError("Select at least one node set and force source")
    times = set(forces["time"])
    histories = []
    for set_id, source in selections.items():
        if source not in SOURCES:
            raise ValueError(f"Unknown force source: {source!r}")
        node_set = deck.get_set_by_id(set_id)
        if node_set is None:
            raise ValueError(f"Node set {set_id} was not found in the deck")
        if not isinstance(node_set, keywords.SetNodeList):
            raise ValueError(f"Set {set_id}: only explicit SET_NODE_LIST sets are supported")
        nodes = set(node_set.nodes.data)
        if not nodes or any(node is None or node <= 0 for node in nodes):
            raise ValueError(f"Set {set_id}: expected a nonempty list of positive node IDs")
        selected = forces[(forces["source"] == source) & forces["node_id"].isin(nodes)]
        groups = selected.groupby("time", sort=True)
        counts = groups["node_id"].count()
        incomplete = [time for time in sorted(times) if counts.get(time, 0) != len(nodes)]
        if incomplete:
            time = incomplete[0]
            present = set(selected.loc[selected["time"] == time, "node_id"])
            missing = sorted(nodes - present)
            raise ValueError(f"Set {set_id}, source {source}, time {time}: missing node IDs {missing}")
        history = groups[COMPONENTS].agg(math.fsum).reset_index()
        history.insert(1, "set_id", set_id)
        history.insert(2, "source", source)
        histories.append(history)
    return pd.concat(histories, ignore_index=True)


###############################################################################
# Plot the component sums
# ~~~~~~~~~~~~~~~~~~~~~~~
# Sum signed components first. Summing individual force magnitudes would lose
# cancellation between nodes and would not give the resultant force.


def plot_histories(histories: pd.DataFrame):
    """Plot the three force-component histories in the model's units."""
    figure, axes = plt.subplots(3, 1, sharex=True, figsize=(8, 7), layout="constrained")
    for (set_id, source), history in histories.groupby(["set_id", "source"], sort=False):
        for axis, component in zip(axes, COMPONENTS):
            axis.plot(history["time"], history[component], marker="o", markersize=3, label=f"Set {set_id}: {source}")
            axis.set_ylabel(f"{component} [model units]")
            axis.grid(alpha=0.25)
    axes[0].legend()
    axes[0].set_title("Node-set force components from bndout")
    axes[-1].set_xlabel("Time [model units]")
    return figure


def _selection(text: str) -> tuple[int, str]:
    """Parse an explicit SET_ID:SOURCE command-line selection."""
    try:
        set_id, source = text.split(":")
        set_id = int(set_id)
        if set_id <= 0 or source not in SOURCES:
            raise ValueError
    except ValueError as exc:
        raise argparse.ArgumentTypeError("Use a positive SET_ID:discrete or SET_ID:velocity") from exc
    return set_id, source


def main(argv=None) -> pd.DataFrame:
    """Run the synthetic example or process a supplied deck and bndout pair."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--deck", type=Path, help="Existing LS-DYNA keyword input file")
    parser.add_argument("--bndout", type=Path, help="Corresponding ASCII bndout file")
    parser.add_argument("--set", dest="selections", action="append", type=_selection, help="Repeat SET_ID:SOURCE")
    parser.add_argument("--output", type=Path, help="Output directory; defaults to a new temporary directory")
    args = parser.parse_args(argv)
    if (args.deck is None) != (args.bndout is None):
        parser.error("Supply --deck and --bndout together")
    if args.deck is not None and not args.selections:
        parser.error("Select the force source for each node set with --set SET_ID:SOURCE")
    selections = args.selections or [(1, "velocity"), (2, "discrete")]
    if len(dict(selections)) != len(selections):
        parser.error("Select each node-set ID only once")
    # Sphinx-Gallery runs in the example directory without defining __file__.
    directory = Path(__file__).resolve().parent if "__file__" in globals() else Path.cwd()
    data = directory / "data"
    deck = Deck()
    deck.import_file(args.deck or data / "input.k")
    forces = read_nodal_forces(args.bndout or data / "bndout.txt")
    histories = aggregate_node_sets(deck, forces, dict(selections))
    output = args.output or Path(tempfile.mkdtemp(prefix="pydyna-bndout-"))
    output.mkdir(parents=True, exist_ok=True)
    histories.to_csv(output / "node_set_forces.csv", index=False)
    figure = plot_histories(histories)
    figure.savefig(output / "node_set_forces.png", dpi=150)
    logger.info("Force histories and plot written to %s", output.resolve())
    return histories


if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO, format="%(message)s")
    main()
