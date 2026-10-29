# Copyright (C) 2026 Ji Peng.
# SPDX-License-Identifier: MIT

"""Exercise the bndout example with independently specified force histories."""

import importlib.util
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import pandas as pd
import pytest

from ansys.dyna.core import Deck, keywords


EXAMPLE = Path(__file__).resolve().parents[1] / "examples" / "Bndout_Node_Sets" / "plot_bndout_node_sets.py"
spec = importlib.util.spec_from_file_location("bndout_example", EXAMPLE)
example = importlib.util.module_from_spec(spec)
spec.loader.exec_module(example)


def section(time="0.1", source="discrete", records=None):
    heading = {
        "discrete": "discrete nodal point forces",
        "velocity": "velocity boundary condition forces/rigid body moments",
    }[source]
    if records is None:
        records = ["nd# 11 xforce= 3 yforce= 4 zforce= -1 energy= 0"]
    return (
        f"n o d a l   f o r c e/e n e r g y    o u t p u t  t= {time}\n"
        + heading + "\n\n" + "\n".join(records) + "\n"
        + "xtotal= 999 ytotal= 999 ztotal= 999 etotal= 999\n"
    )


def deck_with_nodes(nodes=(11,)):
    deck = Deck()
    node_set = keywords.SetNodeList(sid=1)
    node_set.nodes = list(nodes)
    deck.append(node_set)
    return deck


@pytest.fixture
def read(tmp_path):
    def read_text(text):
        path = tmp_path / "bndout.txt"
        path.write_text(text, encoding="utf-8")
        return example.read_nodal_forces(path)
    return read_text


@pytest.fixture(autouse=True)
def close_figures():
    yield
    plt.close("all")


def test_bundled_example_exports_known_component_sums(tmp_path):
    output = tmp_path / "results with spaces"
    histories = example.main(["--output", str(output)])
    expected = pd.DataFrame({
        "time": [0.0, 0.1, 0.0, 0.1],
        "set_id": [1, 1, 2, 2],
        "source": ["velocity", "velocity", "discrete", "discrete"],
        "Fx": [0.0, 0.0, 0.0, 0.0],
        "Fy": [0.0, 10.0, 0.0, -8.0],
        "Fz": [0.0, 0.0, 0.0, 0.0],
    })
    pd.testing.assert_frame_equal(histories, expected)
    pd.testing.assert_frame_equal(pd.read_csv(output / "node_set_forces.csv"), expected)
    assert (output / "node_set_forces.png").read_bytes().startswith(b"\x89PNG\r\n\x1a\n")
    axes = plt.gcf().axes
    assert len(axes) == 3
    assert list(axes[1].lines[0].get_ydata()) == [0.0, 10.0]
    assert list(axes[1].lines[1].get_ydata()) == [0.0, -8.0]


def test_cli_explicit_files(tmp_path):
    data = EXAMPLE.parent / "data"
    result = example.main([
        "--deck", str(data / "input.k"), "--bndout", str(data / "bndout.txt"),
        "--set", "2:discrete", "--output", str(tmp_path),
    ])
    assert result["set_id"].tolist() == [2, 2]
    assert result["Fy"].tolist() == [0.0, -8.0]


def test_sources_and_bndout_setid_do_not_define_membership(read):
    forces = read(section(source="discrete") + section(source="velocity", records=[
        "nd# 11 xforce= 30 yforce= 40 zforce= -10 energy= 0 setid = 987",
        "nd# 99 xforce= 100 yforce= 100 zforce= 100 energy= 0 setid = 1",
    ]))
    deck = deck_with_nodes()
    velocity = example.aggregate_node_sets(deck, forces, {1: "velocity"})
    discrete = example.aggregate_node_sets(deck, forces, {1: "discrete"})
    assert velocity.loc[0, ["Fx", "Fy", "Fz"]].tolist() == [30.0, 40.0, -10.0]
    assert discrete.loc[0, ["Fx", "Fy", "Fz"]].tolist() == [3.0, 4.0, -1.0]


def test_time_order_d_exponents_and_duplicate_set_members(read):
    forces = read("\ufeff" + section(time="2.0D-01") + section(time="1.0d-01", records=[
        "nd# 11 xforce= 3D+0 yforce= -4d+0 zforce= 1D-1 energy= 0D0",
    ]))
    result = example.aggregate_node_sets(deck_with_nodes([11, 11]), forces, {1: "discrete"})
    assert result["time"].tolist() == [0.1, 0.2]
    assert result["Fx"].tolist() == [3.0, 3.0]
    assert result["Fy"].tolist() == [-4.0, 4.0]
    assert result["Fz"].tolist() == [0.1, -1.0]


@pytest.mark.parametrize("value", ["****", "NaN", "Inf", "-Inf", "1E999"])
def test_reject_invalid_force_numbers(read, value):
    with pytest.raises(ValueError, match="number"):
        read(section(records=[f"nd# 11 xforce= {value} yforce= 0 zforce= 0 energy= 0"]))


@pytest.mark.parametrize("time", ["****", "NaN", "Inf"])
def test_reject_invalid_time(read, time):
    with pytest.raises(ValueError, match="number"):
        read(section(time=time))


@pytest.mark.parametrize("record", [
    "nd# 11 xforce= 3 yforce= 4 energy= 0",
    "nd# 11 xforce= 3 yforce= 4 zforce= 1 energy= 0 unexpected",
    "rb# 11 xmoment= 3 ymoment= 4 zmoment= 1 energy= 0",
])
def test_reject_malformed_or_rigid_body_record(read, record):
    with pytest.raises(ValueError, match="malformed or unsupported"):
        read(section(records=[record]))


def test_reject_duplicate_record(read):
    record = "nd# 11 xforce= 3 yforce= 4 zforce= 1 energy= 0"
    with pytest.raises(ValueError, match="duplicate nodal-force record"):
        read(section(records=[record, record]))


def test_reject_repeated_block(read):
    with pytest.raises(ValueError, match="repeated discrete section"):
        read(section() + section())


@pytest.mark.parametrize("tail", ["", section(time="0.2"), "velocity boundary condition forces/rigid body moments\n"])
def test_reject_incomplete_section(read, tail):
    text = section().split("xtotal=")[0] + tail
    with pytest.raises(ValueError, match="incomplete discrete section"):
        read(text)


def test_reject_zero_node_id(read):
    with pytest.raises(ValueError, match="positive"):
        read(section(records=["nd# 0 xforce= 0 yforce= 0 zforce= 0 energy= 0"]))


@pytest.mark.parametrize("text, message", [
    ("Not a bndout file\n", "no supported"),
    ("discrete nodal point forces\n", "no time header"),
    ("n o d a l output t= 0.1\n", "malformed time header"),
    (section() + section(source="velocity", records=[]), "empty nodal-force section"),
])
def test_reject_invalid_file_structure(read, text, message):
    with pytest.raises(ValueError, match=message):
        read(text)


def test_ignore_unrelated_section(read):
    text = section() + (
        "n o d a l   f o r c e/e n e r g y    o u t p u t  t= 0.1\n"
        "unrelated output section\nnd# 11 unsupported layout\n"
    )
    result = read(text)
    assert len(result) == 1


def test_missing_node_is_not_zero_filled(read):
    forces = read(section())
    with pytest.raises(ValueError, match=r"missing node IDs \[12\]"):
        example.aggregate_node_sets(deck_with_nodes([11, 12]), forces, {1: "discrete"})


def test_missing_source_at_one_time_is_not_dropped(read):
    forces = read(section(time="0.1") + section(time="0.2", source="velocity"))
    with pytest.raises(ValueError, match=r"time 0.2: missing node IDs \[11\]"):
        example.aggregate_node_sets(deck_with_nodes(), forces, {1: "discrete"})


@pytest.mark.parametrize("selections, message", [
    ({}, "at least one"),
    ({1: "unknown"}, "Unknown force source"),
    ({999: "discrete"}, "not found"),
])
def test_invalid_selection(read, selections, message):
    with pytest.raises(ValueError, match=message):
        example.aggregate_node_sets(deck_with_nodes(), read(section()), selections)


@pytest.mark.parametrize("nodes", [[], [0]])
def test_invalid_input_node_list(read, nodes):
    with pytest.raises(ValueError, match="nonempty list of positive"):
        example.aggregate_node_sets(deck_with_nodes(nodes), read(section()), {1: "discrete"})


def test_generated_node_set_is_not_misread_as_explicit_nodes(read):
    deck = Deck()
    deck.append(keywords.SetNodeListGenerate(sid=1))
    with pytest.raises(ValueError, match="only explicit"):
        example.aggregate_node_sets(deck, read(section()), {1: "discrete"})


@pytest.mark.parametrize("arguments", [
    ["--deck", "input.k"],
    ["--bndout", "bndout"],
    ["--deck", "input.k", "--bndout", "bndout"],
    ["--set", "1:discrete", "--set", "1:velocity"],
    ["--set", "1:unknown"],
    ["--set", "0:discrete"],
    ["--set", "not-an-id"],
])
def test_invalid_cli_inputs_fail_before_processing(arguments):
    with pytest.raises(SystemExit) as error:
        example.main(arguments)
    assert error.value.code == 2
