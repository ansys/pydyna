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
"""Module for plotting decks."""

import typing

import numpy as np
import pandas as pd

from ansys.dyna.core import Deck


def get_nid_to_index_mapping(nodes) -> np.ndarray:
    """Create array-based node ID to index mapping.

    Returns an array where mapping[nid] = index.
    This is much faster than dict-based lookup (5000x+ speedup).
    """
    nids = nodes["nid"].values
    max_nid = nids.max()

    # Create lookup array: mapping[nid] = index
    # Use -1 for invalid entries
    mapping = np.full(max_nid + 1, -1, dtype=np.int32)
    mapping[nids] = np.arange(len(nids), dtype=np.int32)

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
    """Process nodes DataFrame to extract XYZ coordinates as a numpy array."""
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


def map_facet_nid_to_index(flat_facets: np.array, mapping: np.ndarray) -> np.array:
    """Convert node IDs to indices using array-based mapping.

    Given a flat list of facets or lines, use the mapping array to convert
    node IDs to Python indices for PyVista visualization.

    The mapping is a numpy array where mapping[nid] = index.
    """
    result = flat_facets.copy()

    # Build a mask for positions that are node IDs (not length prefixes)
    mask = np.ones(len(flat_facets), dtype=bool)

    i = 0
    while i < len(flat_facets):
        count = flat_facets[i]
        mask[i] = False  # Don't transform the count
        i += count + 1

    # Apply mapping only to node ID positions (vectorized)
    result[mask] = mapping[flat_facets[mask]]

    return result


def extract_shell_facets(shells: pd.DataFrame, mapping: np.ndarray):
    """Extract shell faces from DataFrame - returns triangles and quads separately.

    Shells table comes in with the form
    |  eid  | nid1 | nid2 | nid3 | nid4
    |  1    | 10   | 11   | 12   |
    |  20   | 21   | 22   | 23   | 24

    Returns triangles and quads in separate arrays (already prefixed with count):
    - Triangles: [3, n1, n2, n3, 3, n1, n2, n3, ...]
    - Quads: [4, n1, n2, n3, n4, 4, n1, n2, n3, n4, ...]

    Elements with fewer than 3 valid nodes are skipped.

    Returns
    -------
    tuple
        (triangles, tri_eids, tri_pids, quads, quad_eids, quad_pids)
    """
    empty_return = (
        np.empty((0), dtype=np.int32),
        np.empty((0), dtype=np.int32),
        np.empty((0), dtype=np.int32),
        np.empty((0), dtype=np.int32),
        np.empty((0), dtype=np.int32),
        np.empty((0), dtype=np.int32),
    )

    if len(shells) == 0:
        return empty_return

    # Extract columns as numpy arrays (much faster than itertuples)
    eids = shells["eid"].values.astype(np.int32)
    pids = shells["pid"].values.astype(np.int32)

    # Get node columns as 2D array - fill NA values with 0 before conversion
    node_cols = ["n1", "n2", "n3", "n4"]
    nodes = shells[node_cols].fillna(0).to_numpy(dtype=np.float64)

    # Filter out invalid elements (need at least 3 valid nodes: n1, n2, n3 > 0)
    valid_mask = (nodes[:, 0] > 0) & (nodes[:, 1] > 0) & (nodes[:, 2] > 0)
    nodes = nodes[valid_mask]
    eids = eids[valid_mask]
    pids = pids[valid_mask]

    if len(nodes) == 0:
        return empty_return

    # Determine element type (3 or 4 nodes)
    is_quad = (nodes[:, 3] > 0) & ~np.isnan(nodes[:, 3])
    tri_mask = ~is_quad

    n_tris = np.sum(tri_mask)
    n_quads = np.sum(is_quad)

    # Process triangles - fully vectorized
    if n_tris > 0:
        tri_nodes = nodes[tri_mask, :3].astype(np.int32)
        tri_mapped = mapping[tri_nodes.ravel()].reshape(n_tris, 3)

        triangles = np.empty((n_tris, 4), dtype=np.int32)
        triangles[:, 0] = 3
        triangles[:, 1:4] = tri_mapped
        triangles = triangles.ravel()

        tri_eids = eids[tri_mask]
        tri_pids = pids[tri_mask]
    else:
        triangles = np.empty((0), dtype=np.int32)
        tri_eids = np.empty((0), dtype=np.int32)
        tri_pids = np.empty((0), dtype=np.int32)

    # Process quads - fully vectorized
    if n_quads > 0:
        quad_nodes = nodes[is_quad, :4].astype(np.int32)
        quad_mapped = mapping[quad_nodes.ravel()].reshape(n_quads, 4)

        quads = np.empty((n_quads, 5), dtype=np.int32)
        quads[:, 0] = 4
        quads[:, 1:5] = quad_mapped
        quads = quads.ravel()

        quad_eids = eids[is_quad]
        quad_pids = pids[is_quad]
    else:
        quads = np.empty((0), dtype=np.int32)
        quad_eids = np.empty((0), dtype=np.int32)
        quad_pids = np.empty((0), dtype=np.int32)

    return triangles, tri_eids, tri_pids, quads, quad_eids, quad_pids


def extract_lines(beams: pd.DataFrame, mapping: np.ndarray) -> np.ndarray:
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
        return np.empty((0), dtype=np.int32), np.empty((0), dtype=np.int32), np.empty((0), dtype=np.int32)

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
    flat_lines = np.concatenate(line_with_prefix, axis=0) if line_with_prefix else np.empty((0), dtype=np.int32)
    flat_lines_indexed = map_facet_nid_to_index(flat_lines, mapping)

    return flat_lines_indexed, np.array(eid, dtype=np.int32), np.array(pid, dtype=np.int32)


def extract_solids(solids: pd.DataFrame, mapping: np.ndarray):
    """Extract solid elements from DataFrame (optimized vectorized version)."""
    if len(solids) == 0:
        return {}

    # Extract data as numpy arrays
    eids = solids["eid"].values.astype(np.int32)
    pids = solids["pid"].values.astype(np.int32)

    node_cols = ["n1", "n2", "n3", "n4", "n5", "n6", "n7", "n8"]
    nodes = solids[node_cols].values.astype(np.int64)

    # Vectorized unique count: sort each row and count changes
    sorted_nodes = np.sort(nodes, axis=1)
    unique_counts = 1 + np.sum(np.diff(sorted_nodes, axis=1) != 0, axis=1)

    solid_with_prefix = {
        8: [np.empty(0, dtype=np.int32), np.empty(0, dtype=np.int32), np.empty(0, dtype=np.int32)],
        6: [np.empty(0, dtype=np.int32), np.empty(0, dtype=np.int32), np.empty(0, dtype=np.int32)],
        5: [np.empty(0, dtype=np.int32), np.empty(0, dtype=np.int32), np.empty(0, dtype=np.int32)],
        4: [np.empty(0, dtype=np.int32), np.empty(0, dtype=np.int32), np.empty(0, dtype=np.int32)],
    }

    # Process hex elements (8 unique nodes) - most common case, fully vectorized
    hex_mask = unique_counts == 8
    n_hex = np.sum(hex_mask)
    if n_hex > 0:
        hex_nodes = nodes[hex_mask].astype(np.int32)
        hex_eids = eids[hex_mask]
        hex_pids = pids[hex_mask]

        # Map all node IDs at once
        mapped_nodes = mapping[hex_nodes]  # Shape: (n_hex, 8)

        # Build connectivity: [8, n1, n2, ..., n8] for each element
        connectivity = np.empty((n_hex, 9), dtype=np.int32)
        connectivity[:, 0] = 8
        connectivity[:, 1:] = mapped_nodes

        solid_with_prefix[8] = [connectivity.ravel(), hex_eids, hex_pids]

    # For other element types (less common), fall back to per-element processing
    for elem_type in [4, 5, 6]:
        mask = unique_counts == elem_type
        n_of_type = np.sum(mask)
        if n_of_type == 0:
            continue

        type_nodes = nodes[mask]
        type_eids = eids[mask]
        type_pids = pids[mask]

        # Get unique nodes preserving order (first occurrence)
        connectivity_list = []
        for i in range(n_of_type):
            row = type_nodes[i]
            _, indices = np.unique(row, return_index=True)
            sorted_indices = np.sort(indices)
            unique_nodes = row[sorted_indices]

            if elem_type == 6:  # Wedge: reorder for PyVista
                unique_nodes = unique_nodes[np.array([0, 1, 4, 3, 2, 5])]

            connectivity_list.append(np.concatenate([[elem_type], mapping[unique_nodes.astype(np.int32)]]))

        if connectivity_list:
            flat_connectivity = np.concatenate(connectivity_list).astype(np.int32)
            solid_with_prefix[elem_type] = [flat_connectivity, type_eids, type_pids]

    return solid_with_prefix


def get_pyvista():
    """Method to import pyvista, raising an exception if not installed."""
    try:
        import pyvista as pv
    except ImportError:
        raise Exception("plot is only supported if pyvista is installed")
    return pv


def is_jupyter_environment():
    """Check if code is running in a Jupyter notebook environment.

    Returns
    -------
    bool
        True if running in Jupyter notebook/lab, False otherwise.
    """
    try:
        from IPython import get_ipython

        ipython = get_ipython()
        if ipython is None:
            return False
        # Check for notebook or lab kernels
        if "IPKernelApp" in ipython.config:
            return True
    except (ImportError, AttributeError):
        pass
    return False


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

    # Extract shell facets - returns triangles and quads separately
    triangles, tri_eids, tri_pids, quads, quad_eids, quad_pids = extract_shell_facets(shells_df, mapping)

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
    """Plot the deck with automatic Jupyter notebook support.

    Parameters
    ----------
    deck : Deck
        The deck to plot
    cwd : str, optional
        Current working directory for expanding includes
    jupyter_backend : str, optional
        Jupyter backend to use. Options are 'static', 'server', 'trame', or None.
        If not specified, automatically detects Jupyter environment and uses 'static'.
        Set to None to disable Jupyter mode explicitly.
    color : str, optional
        Color of the mesh
    scalars : str, optional
        Scalars to color by (e.g., 'part_ids', 'element_ids')
    **args :
        Additional keyword arguments passed to pyvista.plot()

    Returns
    -------
    pyvista plot or camera position
        Depends on the plotting backend used
    """
    # import this lazily (otherwise this adds over a second to the import time of pyDyna)
    pv = get_pyvista()

    plot_data = get_polydata(deck, args.pop("cwd", ""))

    # set default color if both color and scalars are not specified
    color = args.pop("color", None)
    scalars = args.pop("scalars", None)

    # Handle Jupyter backend - auto-detect if not specified
    jupyter_backend = args.pop("jupyter_backend", "auto")
    if jupyter_backend == "auto":
        if is_jupyter_environment():
            jupyter_backend = "static"  # Default to static for simplicity
        else:
            jupyter_backend = None

    # Add jupyter_backend to args if specified
    if jupyter_backend is not None:
        args["jupyter_backend"] = jupyter_backend

    if scalars is not None:
        return plot_data.plot(scalars=scalars, **args)  # User specified scalars
    elif color is not None:
        return plot_data.plot(color=color, **args)  # User specified color
    else:
        return plot_data.plot(color=pv.global_theme.color, **args)  # Default color
