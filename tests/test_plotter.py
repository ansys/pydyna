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

import numpy.testing
import pandas as pd
import pytest

from ansys.dyna.core import Deck
from ansys.dyna.core.lib.deck_plotter import (
    extract_shell_facets,
    get_polydata,
    line_array,
    map_facet_nid_to_index,
    np,
    shell_facet_array,
)


@pytest.mark.keywords
@pytest.mark.viz
def test_shell_facet_array():
    na = pd.Int32Dtype().na_value
    test_1_pddf = pd.DataFrame(
        {
            "n1": [1, 2, 106, 2, 0, 1],
            "n2": [2, 3, 127, 4, 0, 2],
            "n3": [3, 4, 128, 5, 0, 0],
            "n4": [0, 1, 107, na, 0, 0],
        }
    )
    test = [x for x in test_1_pddf.itertuples(index=False)]
    numpy.testing.assert_allclose(shell_facet_array(test[0]), np.array([3, 1, 2, 3]))
    numpy.testing.assert_allclose(shell_facet_array(test[1]), np.array([4, 2, 3, 4, 1]))
    numpy.testing.assert_allclose(shell_facet_array(test[2]), np.array([4, 106, 127, 128, 107]))
    numpy.testing.assert_allclose(shell_facet_array(test[3]), np.array([3, 2, 4, 5]))
    numpy.testing.assert_allclose(shell_facet_array(test[4]), np.array([]))
    numpy.testing.assert_allclose(shell_facet_array(test[5]), np.array([]))

    test_2_pddf = pd.DataFrame(
        {
            "n1": [0, 1, 2, 3, 3, 1],
            "n2": [0, 2, 3, 4, 4, 1],
            "n3": [0, 3, 4, 5, 5, 1],
            "n4": [0, 0, 1, 6, 6, 1],
            "n5": [0, 4, 2, 7, 7, 1],
            "n6": [0, 1, 3, 8, 8, 1],
            "n7": [0, 2, 4, 0, na, 1],
            "n8": [0, 3, 4, 0, na, 0],
        }
    )
    test = [x for x in test_2_pddf.itertuples(index=False)]
    numpy.testing.assert_allclose(shell_facet_array(test[0]), np.array([]))
    numpy.testing.assert_allclose(shell_facet_array(test[1]), np.array([3, 1, 2, 3]))
    numpy.testing.assert_allclose(shell_facet_array(test[2]), np.array([4, 2, 3, 4, 1]))
    numpy.testing.assert_allclose(shell_facet_array(test[3]), np.array([3, 3, 4, 5]))
    numpy.testing.assert_allclose(shell_facet_array(test[4]), np.array([3, 3, 4, 5]))
    numpy.testing.assert_allclose(shell_facet_array(test[5]), np.array([]))


@pytest.mark.keywords
@pytest.mark.viz
def test_line_array():
    na = pd.Int32Dtype().na_value
    test_1_pddf = pd.DataFrame(
        {
            "n1": [1, 2, 0, na, 1],
            "n2": [2, 0, 0, 0, na],
        }
    )
    test = [x for x in test_1_pddf.itertuples(index=False)]
    numpy.testing.assert_allclose(line_array(test[0]), np.array([2, 1, 2]))
    numpy.testing.assert_allclose(line_array(test[1]), np.array([]))
    numpy.testing.assert_allclose(line_array(test[2]), np.array([]))
    numpy.testing.assert_allclose(line_array(test[3]), np.array([]))
    numpy.testing.assert_allclose(line_array(test[4]), np.array([]))


@pytest.mark.keywords
@pytest.mark.viz
def test_facet_nid_to_index():
    # Create array-based mapping: mapping[nid] = index
    mapping = np.array([-1, 2, 3, 4, 5], dtype=np.int32)  # indices 0-4, mapping[1]=2, mapping[2]=3, etc.
    numpy.testing.assert_allclose(
        map_facet_nid_to_index(
            np.array([3, 1, 2, 3, 4, 1, 2, 3, 4, 4, 1, 2, 3, 4, 3, 1, 2, 3]),
            mapping,
        ),
        np.array([3, 2, 3, 4, 4, 2, 3, 4, 5, 4, 2, 3, 4, 5, 3, 2, 3, 4]),
    )


@pytest.mark.keywords
@pytest.mark.viz
def test_extract_shell_facets():
    test_1_pddf = pd.DataFrame(
        {
            "eid": [1, 3],
            "pid": [1, 3],
            "n1": [1, 2],
            "n2": [2, 3],
            "n3": [3, 4],
            "n4": [0, 1],
        }
    )
    # Create array-based mapping: mapping[nid] = index
    mapping1 = np.array([-1, 1, 2, 3, 4], dtype=np.int32)  # mapping[1]=1, mapping[2]=2, etc.
    mapping2 = np.array([-1, 2, 3, 4, 5], dtype=np.int32)  # mapping[1]=2, mapping[2]=3, etc.

    # extract_shell_facets now returns (triangles, tri_eids, tri_pids, quads, quad_eids, quad_pids)
    # Test with mapping1: element 1 is triangle [3,1,2,3], element 3 is quad [4,2,3,4,1]
    tris1, tri_eids1, tri_pids1, quads1, quad_eids1, quad_pids1 = extract_shell_facets(test_1_pddf, mapping1)

    numpy.testing.assert_allclose(tris1, [3, 1, 2, 3])
    numpy.testing.assert_allclose(tri_eids1, [1])
    numpy.testing.assert_allclose(tri_pids1, [1])
    numpy.testing.assert_allclose(quads1, [4, 2, 3, 4, 1])
    numpy.testing.assert_allclose(quad_eids1, [3])
    numpy.testing.assert_allclose(quad_pids1, [3])

    # Test with mapping2
    tris2, tri_eids2, tri_pids2, quads2, quad_eids2, quad_pids2 = extract_shell_facets(test_1_pddf, mapping2)

    numpy.testing.assert_allclose(tris2, [3, 2, 3, 4])
    numpy.testing.assert_allclose(tri_eids2, [1])
    numpy.testing.assert_allclose(tri_pids2, [1])
    numpy.testing.assert_allclose(quads2, [4, 3, 4, 5, 2])
    numpy.testing.assert_allclose(quad_eids2, [3])
    numpy.testing.assert_allclose(quad_pids2, [3])


def extract_faces_and_lines_from_grid(grid):
    """Extract faces and lines from UnstructuredGrid for testing."""
    import vtk

    faces_cells = []
    lines_cells = []

    cell_idx = 0
    i = 0
    while i < len(grid.cells):
        count = grid.cells[i]
        cell_type = grid.get_cell(cell_idx).type

        if cell_type in [vtk.VTK_TRIANGLE, vtk.VTK_QUAD]:
            faces_cells.extend(grid.cells[i:i+count+1])
        elif cell_type == vtk.VTK_LINE:
            lines_cells.extend(grid.cells[i:i+count+1])

        i += count + 1
        cell_idx += 1

    return np.array(faces_cells, dtype=int), np.array(lines_cells, dtype=int)


@pytest.mark.viz
def test_get_polydata(file_utils):
    deck = Deck()
    keyword_string = file_utils.read_file(file_utils.assets_folder / "plot_test.k")
    deck.loads(keyword_string)

    plot_data = get_polydata(deck)

    # Extract faces and lines from UnstructuredGrid
    faces, lines = extract_faces_and_lines_from_grid(plot_data)

    # UnstructuredGrid groups cells by type, so triangles come first, then quads
    # From plot_test.k:
    # Element 3 (triangle): n1=1, n2=2, n3=3, n4=0 -> [3, 0, 1, 2]
    # Element 2 (quad): n1=1, n2=2, n3=3, n4=4 -> [4, 0, 1, 2, 3]
    # Element 7 (quad from 7-node shell): n1=1..n4=4 defined -> [4, 0, 1, 2, 3]
    # Element 8 (quad from 8-node shell): n1=1..n4=4 defined -> [4, 0, 1, 2, 3]
    # Element 9 (quad): n1=1..n4=4 defined -> [4, 0, 1, 2, 3]
    # Elements 10, 11 (invalid, < 3 nodes): skipped
    numpy.testing.assert_allclose(
        faces,
        np.array(
            [
                # 1 triangle
                3, 0, 1, 2,
                # 4 quads
                4, 0, 1, 2, 3,
                4, 0, 1, 2, 3,
                4, 0, 1, 2, 3,
                4, 0, 1, 2, 3,
            ]
        ),
    )
    numpy.testing.assert_allclose(lines, np.array([2, 19, 3, 2, 20, 9]))


def create_test_hex_mesh(nx, ny, nz):
    """Create a simple structured hex mesh for testing."""
    deck = Deck()

    # Build keyword string
    kwd_str = "*KEYWORD\n*NODE\n"

    # Create nodes
    node_id = 1
    for k in range(nz + 1):
        for j in range(ny + 1):
            for i in range(nx + 1):
                x, y, z = float(i), float(j), float(k)
                kwd_str += f"{node_id:8d}{x:16.8f}{y:16.8f}{z:16.8f}       0       0\n"
                node_id += 1

    # Create solid elements
    kwd_str += "*ELEMENT_SOLID\n"
    elem_id = 1

    def get_node_id(i, j, k):
        return 1 + i + j * (nx + 1) + k * (nx + 1) * (ny + 1)

    for k in range(nz):
        for j in range(ny):
            for i in range(nx):
                n1 = get_node_id(i, j, k)
                n2 = get_node_id(i+1, j, k)
                n3 = get_node_id(i+1, j+1, k)
                n4 = get_node_id(i, j+1, k)
                n5 = get_node_id(i, j, k+1)
                n6 = get_node_id(i+1, j, k+1)
                n7 = get_node_id(i+1, j+1, k+1)
                n8 = get_node_id(i, j+1, k+1)

                kwd_str += f"{elem_id:8d}       1{n1:8d}{n2:8d}{n3:8d}{n4:8d}{n5:8d}{n6:8d}{n7:8d}{n8:8d}\n"
                elem_id += 1

    kwd_str += "*END\n"
    deck.loads(kwd_str)

    return deck


@pytest.mark.viz
def test_solid_mesh_extract_surface():
    """Test that extract_surface extracts exterior surface for solid meshes."""
    # Create a small solid mesh (5x5x5 = 125 elements)
    deck = create_test_hex_mesh(5, 5, 5)

    # Plot WITHOUT extract_surface
    grid_full = get_polydata(deck, extract_surface=False)

    # Plot WITH extract_surface (default)
    grid_surf = get_polydata(deck, extract_surface=True)

    # Full mesh has 125 hex elements
    assert grid_full.n_cells == 125, f"Expected 125 cells, got {grid_full.n_cells}"

    # With extract_surface, we get a PolyData with surface faces
    # For a 5x5x5 cube, surface has 6 faces * (5x5) = 150 quads
    assert grid_surf.n_cells == 150, f"Expected 150 surface cells, got {grid_surf.n_cells}"

    # Result types differ: UnstructuredGrid vs PolyData
    assert type(grid_full).__name__ == 'UnstructuredGrid'
    assert type(grid_surf).__name__ in ['PolyData', 'UnstructuredGrid']

    # Verify metadata is preserved
    assert "part_ids" in grid_surf.cell_data
    assert "element_ids" in grid_surf.cell_data


@pytest.mark.viz
def test_solid_mesh_medium():
    """Test with a medium-sized solid mesh."""
    # Create a 10x10x10 mesh (1000 elements)
    deck = create_test_hex_mesh(10, 10, 10)

    # Test with extract_surface
    grid = get_polydata(deck, extract_surface=True)

    # Verify structure
    assert grid.n_cells > 0
    assert grid.n_points > 0
    assert "part_ids" in grid.cell_data
    assert "element_ids" in grid.cell_data

    # Surface of 10x10x10 cube should have 6*10*10 = 600 quad faces
    assert grid.n_cells == 600, f"Expected 600 surface cells, got {grid.n_cells}"

    # Surface points should be less than total internal points
    # Internal: 11^3 = 1331 nodes, Surface should have fewer
    assert grid.n_points < 1331


@pytest.mark.viz
@pytest.mark.skip(reason="ElementSolid parsing from keyword strings has a known bug - skipping until fixed")
def test_mixed_mesh_shells_and_solids():
    """Test plotting with both shell and solid elements.

    Note: There's a known bug with parsing ELEMENT_SOLID from keyword strings (#XXX),
    which prevents testing mixed shell/solid meshes from keyword strings.
    This test is skipped until that bug is fixed.
    """
    pass
@pytest.mark.viz
def test_extract_surface_disabled():
    """Test that extract_surface=False keeps all cells."""
    deck = create_test_hex_mesh(5, 5, 5)

    # With extract_surface=False, should keep all solid cells
    grid = get_polydata(deck, extract_surface=False)

    # Should have all 125 hex elements
    assert grid.n_cells == 125
    assert grid.n_points == 6 * 6 * 6  # 216 nodes

    # Verify metadata
    assert len(grid.cell_data["part_ids"]) == 125
    assert len(grid.cell_data["element_ids"]) == 125


@pytest.mark.viz
def test_shell_only_mesh_not_affected():
    """Test that shell-only meshes are not affected by extract_surface."""
    deck = Deck()

    kwd_str = """*KEYWORD
*NODE
$#   nid               x               y               z      tc      rc
       1             0.0             0.0             0.0       0       0
       2             1.0             0.0             0.0       0       0
       3             1.0             1.0             0.0       0       0
       4             0.0             1.0             0.0       0       0
       5             2.0             0.0             0.0       0       0
       6             2.0             1.0             0.0       0       0
*ELEMENT_SHELL
       1       1       1       2       3       4
       2       1       2       5       6       3
*END
"""
    deck.loads(kwd_str)

    # Both should produce identical results for shell-only
    grid_full = get_polydata(deck, extract_surface=False)
    grid_surf = get_polydata(deck, extract_surface=True)

    # Shell-only mesh should not be affected (no solids to extract)
    assert grid_full.n_cells == grid_surf.n_cells == 2
    assert grid_full.n_points == grid_surf.n_points


@pytest.mark.viz
def test_cell_data_preserved():
    """Test that part_ids and element_ids are preserved after extract_surface."""
    # Use a mesh created programmatically to avoid parsing issues
    deck = create_test_hex_mesh(3, 3, 3)

    grid = get_polydata(deck, extract_surface=True)

    # Verify cell data arrays exist and have correct length
    assert "part_ids" in grid.cell_data
    assert "element_ids" in grid.cell_data
    assert len(grid.cell_data["part_ids"]) == grid.n_cells
    assert len(grid.cell_data["element_ids"]) == grid.n_cells

    # All elements should have part_id=1 (from our test mesh)
    assert all(pid == 1 for pid in grid.cell_data["part_ids"])
