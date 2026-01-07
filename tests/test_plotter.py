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
def test_facet_nid_to_index():
    numpy.testing.assert_allclose(
        map_facet_nid_to_index(
            np.array([3, 1, 2, 3, 4, 1, 2, 3, 4, 4, 1, 2, 3, 4, 3, 1, 2, 3]),
            {1: 2, 2: 3, 3: 4, 4: 5},
        ),
        np.array([3, 2, 3, 4, 4, 2, 3, 4, 5, 4, 2, 3, 4, 5, 3, 2, 3, 4]),
    )


@pytest.mark.keywords
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
    numpy.testing.assert_allclose(
        extract_shell_facets(test_1_pddf, {1: 1, 2: 2, 3: 3, 4: 4})[0],
        [3, 1, 2, 3, 4, 2, 3, 4, 1],
    )
    numpy.testing.assert_allclose(
        extract_shell_facets(test_1_pddf, {1: 1, 2: 2, 3: 3, 4: 4})[1],
        np.array([1,3]),
    )
    numpy.testing.assert_allclose(
        extract_shell_facets(test_1_pddf, {1: 1, 2: 2, 3: 3, 4: 4})[2],
        np.array([1,3]),
    )

    numpy.testing.assert_allclose(
        extract_shell_facets(test_1_pddf, {1: 2, 2: 3, 3: 4, 4: 5})[0],
        [3, 2, 3, 4, 4, 3, 4, 5, 2],
    )
    numpy.testing.assert_allclose(
        extract_shell_facets(test_1_pddf, {1: 2, 2: 3, 3: 4, 4: 5})[1],
        np.array([1,3]),
    )
    numpy.testing.assert_allclose(
        extract_shell_facets(test_1_pddf, {1: 2, 2: 3, 3: 4, 4: 5})[2],
        np.array([1,3]),
    )


@pytest.mark.viz
def test_get_polydata(file_utils):
    deck = Deck()
    keyword_string = file_utils.read_file(file_utils.assets_folder / "plot_test.k")
    deck.loads(keyword_string)

    plot_data = get_polydata(deck)
    numpy.testing.assert_allclose(
        plot_data.faces,
        np.array(
            [
                4,
                0,
                1,
                2,
                3,
                3,
                0,
                1,
                2,
                4,
                0,
                1,
                2,
                3,
                3,
                0,
                1,
                2,
            ]
        ),
    )
    numpy.testing.assert_allclose(plot_data.lines, np.array([2, 19, 3, 2, 20, 9]))
