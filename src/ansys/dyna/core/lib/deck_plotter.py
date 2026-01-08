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
        return np.empty((0), dtype=int), np.empty((0), dtype=int), np.empty((0), dtype=int)

    # extract the node information, element_ids and part_ids
    facet_with_prefix = []
    eid = []
    pid = []

    idx = 0
    for row in shells.itertuples(index=False):
        array = shell_facet_array(row[2:])
        if len(array) > 0:  # Only add if valid facet
            facet_with_prefix.append(array)
            eid.append(row[0])
            pid.append(row[1])
        idx += 1

    # Convert list to np.ndarray
    flat_facets = np.concatenate(facet_with_prefix, axis=0) if facet_with_prefix else np.empty((0), dtype=int)
    flat_facets_indexed = map_facet_nid_to_index(flat_facets, mapping)

    return flat_facets_indexed, np.array(eid), np.array(pid)


def separate_triangles_and_quads(facets: np.ndarray, eids: np.ndarray, pids: np.ndarray):
    """
    Separate mixed triangle and quad facets into separate arrays.
    
    The input facets array has variable-length entries:
    - Triangles: [3, n1, n2, n3]
    - Quads: [4, n1, n2, n3, n4]
    
    Returns separate arrays for triangles and quads with their corresponding eids and pids.
    """
    if len(facets) == 0:
        return (
            np.empty((0), dtype=int),
            np.empty((0), dtype=int),
            np.empty((0), dtype=int),
            np.empty((0), dtype=int),
            np.empty((0), dtype=int),
            np.empty((0), dtype=int),
        )
    
    triangles = []
    quads = []
    tri_eids = []
    tri_pids = []
    quad_eids = []
    quad_pids = []
    
    i = 0
    element_idx = 0
    while i < len(facets):
        count = facets[i]
        if count == 3:
            # Triangle: [3, n1, n2, n3]
            triangles.extend(facets[i:i+4])
            tri_eids.append(eids[element_idx])
            tri_pids.append(pids[element_idx])
            i += 4
        elif count == 4:
            # Quad: [4, n1, n2, n3, n4]
            quads.extend(facets[i:i+5])
            quad_eids.append(eids[element_idx])
            quad_pids.append(pids[element_idx])
            i += 5
        else:
            # Should not happen, but skip invalid entries
            i += 1
            continue
        element_idx += 1
    
    return (
        np.array(triangles, dtype=int),
        np.array(tri_eids, dtype=int),
        np.array(tri_pids, dtype=int),
        np.array(quads, dtype=int),
        np.array(quad_eids, dtype=int),
        np.array(quad_pids, dtype=int),
    )


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
        return np.empty((0), dtype=int), np.empty((0), dtype=int), np.empty((0), dtype=int)

    # extract the node information, element_ids and part_ids
    line_with_prefix = []
    eid = []
    pid = []

    for row in beams.itertuples(index=False):
        array = line_array(row[2:])
        if len(array) > 0:  # Only add if valid line
            line_with_prefix.append(array)
            eid.append(row[0])
            pid.append(row[1])

    # Convert list to np.ndarray
    flat_lines = np.concatenate(line_with_prefix, axis=0) if line_with_prefix else np.empty((0), dtype=int)
    flat_lines_indexed = map_facet_nid_to_index(flat_lines, mapping)

    return flat_lines_indexed, np.array(eid), np.array(pid)


def extract_solids(solids: pd.DataFrame, mapping: typing.Dict[int, int]):
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
    try:
        import pyvista as pv
    except ImportError:
        raise Exception("plot is only supported if pyvista is installed")
    return pv


def get_polydata(deck: Deck, cwd=None, extract_surface=True):
    """Create the PolyData Object for plotting from a given deck with nodes and elements.
    
    Parameters
    ----------
    deck : Deck
        The deck to plot
    cwd : str, optional
        Current working directory for expanding includes
    extract_surface : bool, default=True
        If True, extract only the exterior surface for solid elements.
        This dramatically improves performance for large solid meshes with no visual difference,
        since only the surface is visible anyway. Set to False to include all cells.
    
    Returns
    -------
    pyvista.UnstructuredGrid
        UnstructuredGrid containing the mesh for visualization
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
    
    # Separate triangles and quads
    triangles, tri_eids, tri_pids, quads, quad_eids, quad_pids = separate_triangles_and_quads(
        facets, shell_eids, shell_pids
    )
    
    lines, line_eids, line_pids = extract_lines(beams_df, mapping)
    solids_info = extract_solids(solids_df, mapping)

    # celltype_dict for beam and shell
    celltype_dict = {
        pv.CellType.LINE: lines.reshape([-1, 3])[:, 1:] if len(lines) > 0 else np.empty((0, 2), dtype=int),
    }
    
    # Add triangles if present
    if len(triangles) > 0:
        celltype_dict[pv.CellType.TRIANGLE] = triangles.reshape([-1, 4])[:, 1:]
    
    # Add quads if present
    if len(quads) > 0:
        celltype_dict[pv.CellType.QUAD] = quads.reshape([-1, 5])[:, 1:]

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

    # Combine shell metadata (triangles and quads)
    shell_pids_combined = np.concatenate((tri_pids, quad_pids), axis=0)
    shell_eids_combined = np.concatenate((tri_eids, quad_eids), axis=0)
    
    # Extract only the exterior surface for performance if solids are present
    # This dramatically speeds up plotting with no visual difference since only surface is visible
    has_solids = len(solids_info) > 0 and any(len(v[0]) > 0 for v in solids_info.values())
    if extract_surface and has_solids:
        # Add cell data before extraction (extract_surface preserves cell_data for extracted cells)
        plot_data.cell_data["part_ids"] = np.concatenate((line_pids, shell_pids_combined, solids_pids), axis=0)
        plot_data.cell_data["element_ids"] = np.concatenate((line_eids, shell_eids_combined, solids_eids), axis=0)
        
        # Extract surface - removes interior solid cells
        plot_data = plot_data.extract_surface()
    else:
        # Add cell data without surface extraction
        plot_data.cell_data["part_ids"] = np.concatenate((line_pids, shell_pids_combined, solids_pids), axis=0)
        plot_data.cell_data["element_ids"] = np.concatenate((line_eids, shell_eids_combined, solids_eids), axis=0)

    return plot_data


def plot_deck(deck, **args):
    """Plot the deck."""

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
