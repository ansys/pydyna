# Copyright (C) 2023 - 2025 ANSYS, Inc. and/or its affiliates.
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
    """
    Given a node id, output the node index as a dict.

    Parameters
    ----------
    nodes : pd.DataFrame
        The nodes DataFrame.

    Returns
    -------
    dict
        Mapping from node id to index.
    """
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
    and returns them as data frames.

    Parameters
    ----------
    deck : Deck
        The deck object containing mesh keywords.

    Returns
    -------
    nodes : pd.DataFrame
        DataFrame of nodes.
    df_list : dict
        Dictionary of element type to DataFrame.
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
    """
    Extract xyz coordinates from nodes DataFrame.

    Parameters
    ----------
    nodes_df : pd.DataFrame
        DataFrame of nodes.

    Returns
    -------
    np.ndarray
        Array of xyz coordinates.
    """
    nodes_xyz = nodes_df[["x", "y", "z"]]
    return nodes_xyz.to_numpy()


def shell_facet_array(facets: pd.DataFrame) -> np.array:
    """
    Get the shell facet array from the DataFrame.

    Parameters
    ----------
    facets : pd.DataFrame
        DataFrame row of facet node ids.

    Returns
    -------
    np.ndarray
        Array of facet node ids with length prefix for PyVista.
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

    Parameters
    ----------
    solids : pd.DataFrame
        DataFrame row of solid node ids.

    Returns
    -------
    np.ndarray
        Array of solid node ids with length prefix for PyVista.
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

    Parameters
    ----------
    lines : pd.DataFrame
        DataFrame row of line node ids.

    Returns
    -------
    np.ndarray
        Array of line node ids with length prefix for PyVista.
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
    """
    Convert mapping to numpy array.

    Given a flat list of facets or lines, use the mapping from nid to python index
    to output the numbering system for PyVista from the numbering from dyna.

    Parameters
    ----------
    flat_facets : np.ndarray
        Flat array of facet or line node ids with length prefix.
    mapping : dict
        Mapping from node id to index.

    Returns
    -------
    np.ndarray
        Array of node indices for PyVista.
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
    """
    Extract shell faces from DataFrame.

    Parameters
    ----------
    shells : pd.DataFrame
        DataFrame of shell elements.
    mapping : dict
        Mapping from node id to index.

    Returns
    -------
    tuple
        facets : np.ndarray
            Flat array of shell facet node indices for PyVista.
        eid : np.ndarray
            Array of element ids.
        pid : np.ndarray
            Array of part ids.
    """

    if len(shells) == 0:
        return np.empty((0), dtype=int), np.empty((0), dtype=int), np.empty((0), dtype=int)

    # extract the node information, element_ids and part_ids
    facet_with_prefix = []
    eid = []
    pid = []

    idx = 0
    for row in shells.itertuples(index=False):
        array = shell_facet_array(row[2:])
        facet_with_prefix.append(array)
        eid.append(row[0])
        pid.append(row[1])
        idx += 1

    # Convert list to np.ndarray
    flat_facets = np.concatenate(facet_with_prefix, axis=0)
    flat_facets_indexed = map_facet_nid_to_index(flat_facets, mapping)

    return flat_facets_indexed, np.array(eid), np.array(pid)


def extract_lines(beams: pd.DataFrame, mapping: typing.Dict[int, int]) -> np.ndarray:
    """
    Extract lines from DataFrame.

    Parameters
    ----------
    beams : pd.DataFrame
        DataFrame of beam elements.
    mapping : dict
        Mapping from node id to index.

    Returns
    -------
    tuple
        lines : np.ndarray
            Flat array of line node indices for PyVista.
        eid : np.ndarray
            Array of element ids.
        pid : np.ndarray
            Array of part ids.
    """
    # dont need to do this if there is no beams
    if len(beams) == 0:
        return np.empty((0), dtype=int), np.empty((0), dtype=int), np.empty((0), dtype=int)

    # extract the node information, element_ids and part_ids
    line_with_prefix = []
    eid = []
    pid = []

    for row in beams.itertuples(index=False):
        line_with_prefix.append(line_array(row[2:]))
        eid.append(row[0])
        pid.append(row[1])

    # Convert list to np.ndarray
    flat_lines = np.concatenate(line_with_prefix, axis=0)
    flat_lines_indexed = map_facet_nid_to_index(flat_lines, mapping)

    return flat_lines_indexed, np.array(eid), np.array(pid)


def extract_solids(solids: pd.DataFrame, mapping: typing.Dict[int, int]):
    """
    Extract solid elements from DataFrame.

    Parameters
    ----------
    solids : pd.DataFrame
        DataFrame of solid elements.
    mapping : dict
        Mapping from node id to index.

    Returns
    -------
    dict
        Dictionary keyed by number of nodes (4, 5, 6, 8) with values:
            [connectivity, element_ids, part_ids]
            connectivity : np.ndarray
                Flat array of solid node indices for PyVista.
            element_ids : np.ndarray
                Array of element ids.
            part_ids : np.ndarray
                Array of part ids.
    """
    if len(solids) == 0:
        return {}

    solid_with_prefix = {
        8: [[], [], []],  # Hexa
        6: [[], [], []],  # Wedge
        5: [[], [], []],  # Pyramid
        4: [[], [], []],  # Tetra
    }

    # extract the node information, element_ids and part_ids
    for row in solids.itertuples(index=False):
        arr = np.array(row[2:10])
        temp_array, indices = np.unique(arr, return_index=True)
        key = len(temp_array)

        sorted_unique_indices = np.sort(indices)
        temp_array = arr[sorted_unique_indices]

        if key == 6:  # convert node numbering to PyVista Style for Wedge
            temp_array = temp_array[np.array((0, 1, 4, 3, 2, 5))]

        solid_with_prefix[key][0].append([key, *temp_array])  # connectivity
        solid_with_prefix[key][1].append(row[0])  # element_ids
        solid_with_prefix[key][2].append(row[1])  # part_ids

    # Convert list to np.ndarray
    for key, value in solid_with_prefix.items():
        if value[0]:
            flat_solids = np.concatenate(value[0], axis=0)

            value[0] = map_facet_nid_to_index(flat_solids, mapping)  # connectivity

            value[1] = np.array(value[1])  # element_ids
            value[2] = np.array(value[2])  # part_ids

    return solid_with_prefix


def get_pyvista():
    """
    Import pyvista if available.

    Returns
    -------
    pyvista module
        The pyvista module.
    """
    try:
        import pyvista as pv
    except ImportError:
        raise Exception("plot is only supported if pyvista is installed")
    return pv


def get_polydata(deck: Deck, cwd=None):
    """
    Create the PolyData Object for plotting from a given deck with nodes and elements.

    Parameters
    ----------
    deck : Deck
        The deck object containing mesh keywords.
    cwd : str, optional
        Current working directory for deck expansion.

    Returns
    -------
    pyvista.UnstructuredGrid
        The PyVista UnstructuredGrid object for plotting.
    """

    # import this lazily (otherwise this adds over a second to the import time of pyDyna)
    pv = get_pyvista()

    # check kwargs for cwd. future more arguments to plot
    # flatten deck
    if cwd is not None:
        flat_deck = deck.expand(cwd=cwd, recurse=True)
    else:
        flat_deck = deck.expand(recurse=True)

    # get dataframes for each element types
    nodes_df, element_dict = merge_keywords(flat_deck)
    shells_df = element_dict["SHELL"]
    beams_df = element_dict["BEAM"]
    solids_df = element_dict["SOLID"]

    nodes_list = process_nodes(nodes_df)

    if len(nodes_df) == 0 or len(shells_df) + len(beams_df) + len(solids_df) == 0:
        raise Exception("missing node or element keyword to plot")

    mapping = get_nid_to_index_mapping(nodes_df)

    # get the node information, element_ids and part_ids
    facets, shell_eids, shell_pids = extract_shell_facets(shells_df, mapping)
    lines, line_eids, line_pids = extract_lines(beams_df, mapping)
    solids_info = extract_solids(solids_df, mapping)

    # celltype_dict for beam and shell
    celltype_dict = {
        pv.CellType.LINE: lines.reshape([-1, 3])[:, 1:],
        pv.CellType.QUAD: facets.reshape([-1, 5])[:, 1:],
    }

    # dict of cell types for node counts
    solid_celltype = {
        4: pv.CellType.TETRA,
        5: pv.CellType.PYRAMID,
        6: pv.CellType.WEDGE,
        8: pv.CellType.HEXAHEDRON,
    }

    # Update celltype_dict with solid elements
    solids_pids = np.empty((0), dtype=int)
    solids_eids = np.empty((0), dtype=int)

    for n_points, elements in solids_info.items():
        if len(elements[0]) == 0:
            continue

        temp_solids, temp_solids_eids, temp_solids_pids = elements

        celltype_dict[solid_celltype[n_points]] = temp_solids.reshape([-1, n_points + 1])[:, 1:]

        # Update part_ids and element_ids info for solid elements
        if len(solids_pids) != 0:
            solids_pids = np.concatenate((solids_pids, temp_solids_pids), axis=0)
        else:
            solids_pids = temp_solids_pids

        if len(solids_pids) != 0:
            solids_eids = np.concatenate((solids_eids, temp_solids_eids), axis=0)
        else:
            solids_eids = temp_solids_eids

    # Create UnstructuredGrid
    plot_data = pv.UnstructuredGrid(celltype_dict, nodes_list)

    # Mapping part_ids and element_ids
    plot_data.cell_data["part_ids"] = np.concatenate((line_pids, shell_pids, solids_pids), axis=0)
    plot_data.cell_data["element_ids"] = np.concatenate((line_eids, shell_eids, solids_eids), axis=0)

    return plot_data


def plot_deck(deck, **args):
    """
    Plot the deck.

    Parameters
    ----------
    deck : Deck
        The deck object containing mesh keywords.
    **args
        Additional arguments for PyVista plot.

    Returns
    -------
    Any
        PyVista plot output.
    """

    # import this lazily (otherwise this adds over a second to the import time of pyDyna)
    pv = get_pyvista()

    plot_data = get_polydata(deck, args.pop("cwd", ""))

    # set default color if both color and scalars are not specified
    color = args.pop("color", None)
    scalars = args.pop("scalars", None)

    if scalars is not None:
        return plot_data.plot(scalars=scalars, **args)  # User specified scalars
    elif color is not None:
        return plot_data.plot(color=color, **args)  # User specified color
    else:
        return plot_data.plot(color=pv.global_theme.color, **args)  # Default color
