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

import cProfile
import io
import os
import pstats
import time

import numpy as np
import pandas as pd

from ansys.dyna.core import Deck, keywords as kwd
from ansys.dyna.core.pre.examples.download_utilities import DownloadManager


def _gen_nodes(count):
    nodes = kwd.Node()
    node_ids = np.arange(count) + 1
    xs = np.zeros(count) + 0.1
    ys = np.zeros(count) + 0.2
    zs = np.zeros(count) + 0.3
    df = pd.DataFrame({"nid": node_ids, "x": xs, "y": ys, "z": zs})
    nodes.nodes = df
    return nodes


def write1(s: io.StringIO, nodes: kwd.Node):
    """Write the keyword with the implementation that is built into PyDyna."""
    nodes.write(s)
    return s


def write2(s: io.StringIO, nodes: kwd.Node):
    """Write the keyword using an alternative more performant implementation
    Issues:
        - Does not account for NaN or <NA> values (maybe that can be handled by
        a string replace at the end)
        - Incorrectly converts some float values to string. Builtin python float
        formatting can overflow width for some values and widths.
    """
    line_format = ""
    for card in nodes.cards:
        for field in card._fields:
            if field.type == float:
                fmt_append = "{:>" + "{0}.5f".format(field.width) + "}"
            elif field.type == int:
                fmt_append = "{:>" + "{0}.0f".format(field.width) + "}"
            line_format = line_format + fmt_append
        line_format = line_format + "\n"

    # format list of strings from data
    s.write(f"{nodes.get_title()}\n")
    nodes_array = nodes.nodes.to_numpy()
    for node in nodes_array:
        s.write(line_format.format(*node))


def _profile(func):
    global PROFILER
    if PROFILER == 2:
        t1 = time.time()
        func()
        t2 = time.time()
        print(f"Function ran in {t2-t1}")
    elif PROFILER == 1:
        profiler = cProfile.Profile()
        profiler.enable()
        func()
        profiler.disable()
        stats = pstats.Stats(profiler).sort_stats("tottime")
        stats.print_stats()


def perf_001() -> io.StringIO:
    """Test performance of writing nodes keyword using the `write` method"""
    nodes = _gen_nodes(100000)
    s = io.StringIO()
    _profile(lambda: write1(s, nodes))
    return s


def perf_002() -> io.StringIO:
    """Test performance of writing nodes using an alternative
    algorithm that does not handle <NA> properly.
    """
    nodes = _gen_nodes(100000)
    s = io.StringIO()
    _profile(lambda: write2(s, nodes))
    return s


def perf_003() -> None:
    """Test loading the yaris deck - this takes about 7 seconds now."""
    path = ("ls-dyna", "yaris_stat_shock_absorber_loading")

    downloader = DownloadManager()
    downloader.download_file("fixed_ground.k", *path)
    downloader.download_file("implicit_control_cards_R9.k", *path)
    downloader.download_file("YarisD_V2g_shock_abs_load_01.k", *path)
    downloader.download_file("YarisD_V2g_shock_abs_load_01_nodes.k", *path)
    main_filename = "000_yaris_stat_shock_absorber_loading_01.k"
    output = downloader.download_file(main_filename, *path)
    folder = os.path.dirname(output)
    deck = Deck()
    fname = os.path.join(folder, main_filename)
    t1 = time.time()
    result = deck.import_file(fname)
    elapsed = time.time() - t1
    print(f"read {fname} in {elapsed} seconds:")
    print(result.get_summary())
    t1 = time.time()
    deck.expand(cwd=folder)
    elapsed = time.time() - t1
    print(f"expanded deck in {elapsed} seconds:")


if __name__ == "__main__":
    """system to run these outside of pytest for experimentation purposes"""
    import sys

    flag = sys.argv[1]
    PROFILER = int(sys.argv[2])  # 1-cProfile, 2-time.time
    if flag == "1":
        s = perf_001()
    elif flag == "2":
        s = perf_002()
    elif flag == "3":
        perf_003()
    elif flag == "4":
        perf_004()
