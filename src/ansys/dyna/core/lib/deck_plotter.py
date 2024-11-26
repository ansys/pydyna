# Copyright (C) 2021 - 2024 ANSYS, Inc. and/or its affiliates.
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

import typing

import numpy as np
import pandas as pd

from ansys.dyna.core import Deck


def get_nid_to_index_mapping(nodes) -> typing.Dict:
    """Given a node id, output the node index as a dict"""
    mapping = {}
    for idx, node in nodes.iterrows():
        mapping[node["nid"]] = idx
    return mapping


def merge_keywords(
    deck: Deck,
) -> typing.Tuple[pd.DataFrame, typing.Dict]:
    """
    Merge mesh keywords.

    Given a deck, merges specific keywords (NODE, ELEMENT_SHELL, ELEMENT_BEAM, ELEMENT_SOLID)
    and returns tham as data frames.
    """
    nodes_temp = [kwd.nodes for kwd in deck.get_kwds_by_type("NODE")]
    nodes = pd.concat(nodes_temp) if len(nodes_temp) else pd.DataFrame()

    df_list = {}
    for item in ["SHELL", "BEAM", "SOLID"]:
        matching_elements = [kwd.elements for kwd in deck.get_kwds_by_type("ELEMENT") if kwd.subkeyword == item]
        df_list[item] = pd.concat(matching_elements) if len(matching_elements) else pd.DataFrame()

    return (
        nodes,
        df_list,
    )  # solids


def process_nodes(nodes_df):
    nodes_xyz = nodes_df[["x", "y", "z"]]
    return nodes_xyz.to_numpy()


def shell_facet_array(facets: pd.DataFrame) -> np.array:
    """
    Get the shell facet array from the DataFrame.

    Facets are a pandas frame that is a sequence of integers
    or NAs with max length of 8.
    valid rows contain 3,4,6, or 8 items consecutive from the
    left.  we don't plot quadratic edges so 6/8 collapse to 3/4
    invalid rows are ignored, meaning they return an empty array
    return an array of length 4 or 5 using the pyvista spec
    for facets which includes a length prefix
    [1,2,3]=>[3,1,2,3]
    [1,2,3,0]=>[3,1,2,3]
    [1,2,3,NA]=>[3,1,2,3]
    """
    facet_array = np.empty(5, dtype=np.int32)

    for idx, item in enumerate(facets):
        # find the first empty column
        if pd.isna(item) or item == 0:
            if idx == 3 or idx == 6:
                facet_array[0] = 3
                return facet_array[:-1]
            elif idx == 4:
                facet_array[0] = 4
                return facet_array
            else:
                # invalid
                return np.empty(0)
        # fill the output to the right of the prefix
        if idx < 4:
            facet_array[idx + 1] = item
    facet_array[0] = 4
    return facet_array


def solid_array(solids: pd.DataFrame):
    """
    Get the solid array from the DataFrame.

    Solids are a pandas frame that is a sequence of integers
    or NAs with max length of 28.
    valid rows contain 3, 4, 6, or 8 items consecutive from the
    left.  We don't plot quadratic edges so 6/8 collapse to 3/4
    invalid rows are ignored, meaning they return an empty array
    return an array of length 4 or 5 using the pyvista spec
    for facets which includes a length prefix
    [1,2,3]=>[3,1,2,3]
    [1,2,3,0]=>[3,1,2,3]
    [1,2,3,NA]=>[3,1,2,3]
    """

    # FACES CREATED BY THE SOLIDS BASED ON MANUAL
    # A DUMMY ZERO IS PUT AS A PLACEHOLDER FOR THE LEN PREFIX
    four_node_faces = [[0, 1, 2, 3], [0, 1, 2, 4], [0, 1, 3, 4], [0, 2, 3, 4]]
    six_node_faces = [
        [0, 1, 2, 5],
        [0, 3, 4, 6],
        [0, 2, 3, 5, 6],
        [0, 1, 5, 6, 4],
        [0, 1, 2, 3, 4],
    ]
    eight_node_faces = [
        [0, 1, 2, 3, 4],
        [0, 1, 2, 5, 6],
        [0, 5, 6, 7, 8],
        [0, 3, 4, 7, 8],
        [0, 2, 3, 6, 7],
        [0, 1, 4, 5, 8],
    ]

    facet_array = []

    for idx, item in enumerate(solids):
        # find the first empty column
        if pd.isna(item) or item == 0:
            if idx == 4:
                facet_array = [len(facet) - 1 if i == 0 else solids[i - 1] for facet in four_node_faces for i in facet]
                return facet_array
            elif idx == 6:
                facet_array = [len(facet) - 1 if i == 0 else solids[i - 1] for facet in six_node_faces for i in facet]
                return facet_array
            elif idx == 8:
                facet_array = [len(facet) - 1 if i == 0 else solids[i - 1] for facet in eight_node_faces for i in facet]
                return facet_array
            else:
                # invalid
                return []
        # fill the output to the right of the prefix
    return np.array(facet_array)


def line_array(lines: pd.DataFrame) -> np.array:
    """
    Convert DataFrame to lines array.

    `lines` is a pandas frame that is a sequence of integers
    or NAs with max length of 2.
    valid rows contain 2 items consecutive from the
    left.
    invalid rows are ignored, meaning they return an empty array
    return an array of length 3 using the pyvista spec
    for facets which includes a length prefix
    [1,2,]=>[2,1,2]
    [1,2,3,0]=>[]
    [1,2,3,NA]=>[]
    """
    line_array = np.empty(3, dtype=np.int32)

    for idx, item in enumerate(lines):
        # find the first empty column
        if pd.isna(item) or item == 0:
            if idx == 0 or idx == 1:
                return np.empty(0)
        # fill the output to the right of the prefix
        if idx < 2:
            line_array[idx + 1] = item

    line_array[0] = 2
    return line_array


def map_facet_nid_to_index(flat_facets: np.array, mapping: typing.Dict) -> np.array:
    """Convert mapping to numpy array.

    Given a flat list of facets or lines, use the mapping from nid to python index
    to output the numbering system for pyvista from the numbering from dyna
    """
    # Map the indexes but skip the prefix
    flat_facets_indexed = np.empty(len(flat_facets), dtype=np.int32)

    skip_flag = 0
    for idx, item in np.ndenumerate(flat_facets):
        if skip_flag == 0:
            flat_facets_indexed[idx] = item
            skip_flag -= int(item)
        else:
            flat_facets_indexed[idx] = mapping[item]
            skip_flag += 1

    return flat_facets_indexed


def extract_shell_facets(shells: pd.DataFrame, mapping):
    """Extract shell faces from DataFrame.

    Shells table comes in with the form
    |  eid  | nid1 | nid2 | nid3 | nid4
    |  1    | 10   | 11   | 12   |
    |  20   | 21   | 22   | 23   | 24

    but the array needed for pyvista polydata is
    of the form where each element is prefixed by the length of the element node list
    [3,10,11,12,4,21,22,23,24]

    Take individual rows, extract the appropriate nid's and output a flat list of
    facets for pyvista
    """

    if len(shells) == 0:
        return []

    # extract only the node information
    # could keep in the future to separate the parts or elements
    shells = shells.drop(columns=["eid", "pid"])

    facet_with_prefix = []

    idx = 0
    for row in shells.itertuples(index=False):
        array = shell_facet_array(row)
        facet_with_prefix.append(array)
        idx += 1

    flat_facets = np.concatenate(facet_with_prefix, axis=0)

    flat_facets_indexed = map_facet_nid_to_index(flat_facets, mapping)

    return flat_facets_indexed


def extract_lines(beams: pd.DataFrame, mapping: typing.Dict[int, int]) -> np.ndarray:
    """Extract lines from DataFrame.

    Beams table comes in with the form with extra information not supported,
    |  eid  | nid1 | nid2
    |  1    | 10   | 11
    |  20   | 21   | 22

      we only care about nid 1 and 2

    but the array needed for pyvista polydata is the same as in extract facets
    of the form where each element is prefixed by the length of the element node list
    [2,10,11,2,21,22]

    Take individual rows, extract the appropriate nid's and output a flat list of
    facets for pyvista
    """
    # dont need to do this if there is no beams
    if len(beams) == 0:
        return np.empty((0), dtype=int)

    # extract only the node information
    # could keep in the future to separate the parts or elements
    beams = beams[["n1", "n2"]]

    line_with_prefix = []

    for row in beams.itertuples(index=False):
        line_with_prefix.append(line_array(row))

    flat_lines = np.concatenate(line_with_prefix, axis=0)

    flat_lines_indexed = map_facet_nid_to_index(flat_lines, mapping)

    return flat_lines_indexed


def extract_solids(solids: pd.DataFrame, mapping: typing.Dict[int, int]):
    if len(solids) == 0:
        return []

    solids = solids.drop(columns=["eid", "pid"])

    idx = 0

    solid_with_prefix = []

    for row in solids.itertuples(index=False):
        solid_with_prefix.append(solid_array(row))
        idx += 1

    flat_solids = np.concatenate(solid_with_prefix, axis=0)

    flat_solids_indexed = map_facet_nid_to_index(flat_solids, mapping)

    return flat_solids_indexed


def get_pyvista():
    try:
        import pyvista as pv
    except ImportError:
        raise Exception("plot is only supported if pyvista is installed")
    return pv


def get_polydata(deck: Deck, cwd=None):
    """Create the PolyData Object for plotting from a given deck with nodes and elements."""

    # import this lazily (otherwise this adds over a second to the import time of pyDyna)
    pv = get_pyvista()

    # check kwargs for cwd. future more arguments to plot
    flat_deck = deck.expand(cwd)
    nodes_df, element_dict = merge_keywords(flat_deck)

    shells_df = element_dict["SHELL"]
    beams_df = element_dict["BEAM"]
    solids_df = element_dict["SOLID"]

    nodes_list = process_nodes(nodes_df)

    if len(nodes_df) == 0 or len(shells_df) + len(beams_df) + len(solids_df) == 0:
        raise Exception("missing node or element keyword to plot")

    mapping = get_nid_to_index_mapping(nodes_df)

    facets = extract_shell_facets(shells_df, mapping)
    lines = extract_lines(beams_df, mapping)
    solids = extract_solids(solids_df, mapping)
    plot_data = pv.PolyData(nodes_list, [*facets, *solids])
    if len(lines) > 0:
        plot_data.lines = lines
    return plot_data


def plot_deck(deck, **args):
    """Plot the deck."""
    plot_data = get_polydata(deck, args.pop("cwd", ""))
    return plot_data.plot()
