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

"""Benchmark for reading INITIAL_STRAIN_SHELL and INITIAL_STRESS_SHELL keywords.

This benchmark addresses GitHub issue #592: Performance optimization for these
keywords which contain thousands of small tables (one per shell element).

The main bottleneck is calling pandas read_fwf() many times with fixed overhead
per call. This benchmark measures the effect of optimizations like:
- Disabling comment checking (comment=None) when comments are pre-filtered
- Batch reading all data with a single pandas call (future optimization)

Usage:
    python benchmarks/bench_initial_stress_strain.py
"""

import io
import time
from typing import Callable, List, Tuple

from ansys.dyna.core import Deck, keywords as kwd


def generate_initial_strain_shell_data(num_elements: int, nplane: int = 1, nthick: int = 2) -> str:
    """Generate INITIAL_STRAIN_SHELL keyword data with multiple elements.

    Args:
        num_elements: Number of shell elements to generate
        nplane: Number of in-plane integration points (default 1)
        nthick: Number of through-thickness integration points (default 2)

    Returns:
        String containing the keyword data
    """
    lines = ["*INITIAL_STRAIN_SHELL"]

    for eid in range(1, num_elements + 1):
        # Header card: eid, nplane, nthick, large, ilocal
        lines.append(f"{eid:>10}{nplane:>10}{nthick:>10}{0:>10}{0:>10}")
        # Strain cards: epsxx, epsyy, epszz, epsxy, epsyz, epszx, t
        num_points = nplane * nthick
        for i in range(num_points):
            t = -1.0 + 2.0 * i / (num_points - 1) if num_points > 1 else 0.0
            lines.append(f"{0.001:>10.4f}{0.002:>10.4f}{0.003:>10.4f}{0.0:>10.4f}{0.0:>10.4f}{0.0:>10.4f}{t:>10.4f}")

    return "\n".join(lines)


def generate_initial_stress_shell_data(
    num_elements: int, nplane: int = 1, nthick: int = 2, nhisv: int = 0
) -> str:
    """Generate INITIAL_STRESS_SHELL keyword data with multiple elements.

    Args:
        num_elements: Number of shell elements to generate
        nplane: Number of in-plane integration points (default 1)
        nthick: Number of through-thickness integration points (default 2)
        nhisv: Number of history variables (default 0)

    Returns:
        String containing the keyword data
    """
    lines = ["*INITIAL_STRESS_SHELL"]

    for eid in range(1, num_elements + 1):
        # Header card: eid, nplane, nthick, nhisv, large, ilocal
        lines.append(f"{eid:>10}{nplane:>10}{nthick:>10}{nhisv:>10}{0:>10}{0:>10}")
        # Stress cards: t, sigxx, sigyy, sigzz, sigxy, sigyz, sigzx, eps
        num_points = nplane * nthick
        for i in range(num_points):
            t = -1.0 + 2.0 * i / (num_points - 1) if num_points > 1 else 0.0
            lines.append(
                f"{t:>10.4f}{100.0:>10.2f}{200.0:>10.2f}{0.0:>10.2f}{50.0:>10.2f}{0.0:>10.2f}{0.0:>10.2f}{0.01:>10.4f}"
            )

    return "\n".join(lines)


def time_function(func: Callable, iterations: int = 3) -> Tuple[float, float]:
    """Time a function over multiple iterations.

    Returns:
        Tuple of (average_time, min_time)
    """
    times = []
    for _ in range(iterations):
        t1 = time.perf_counter()
        func()
        t2 = time.perf_counter()
        times.append(t2 - t1)
    return sum(times) / len(times), min(times)


def benchmark_strain_shell_read(num_elements: int) -> dict:
    """Benchmark reading INITIAL_STRAIN_SHELL keyword."""
    data = generate_initial_strain_shell_data(num_elements)

    def read_keyword():
        d = Deck()
        d.loads(data)
        return d

    avg_time, min_time = time_function(read_keyword)
    return {
        "keyword": "INITIAL_STRAIN_SHELL",
        "num_elements": num_elements,
        "avg_time_ms": avg_time * 1000,
        "min_time_ms": min_time * 1000,
        "elements_per_second": num_elements / avg_time,
    }


def benchmark_stress_shell_read(num_elements: int) -> dict:
    """Benchmark reading INITIAL_STRESS_SHELL keyword."""
    data = generate_initial_stress_shell_data(num_elements)

    def read_keyword():
        d = Deck()
        d.loads(data)
        return d

    avg_time, min_time = time_function(read_keyword)
    return {
        "keyword": "INITIAL_STRESS_SHELL",
        "num_elements": num_elements,
        "avg_time_ms": avg_time * 1000,
        "min_time_ms": min_time * 1000,
        "elements_per_second": num_elements / avg_time,
    }


def run_benchmarks():
    """Run all benchmarks and print results."""
    print("=" * 70)
    print("INITIAL_STRAIN_SHELL / INITIAL_STRESS_SHELL Read Performance Benchmark")
    print("=" * 70)
    print()

    element_counts = [10, 100, 500, 1000]

    print("INITIAL_STRAIN_SHELL:")
    print("-" * 50)
    for count in element_counts:
        result = benchmark_strain_shell_read(count)
        print(
            f"  {count:>5} elements: {result['avg_time_ms']:>8.1f} ms avg, "
            f"{result['elements_per_second']:>8.0f} elem/s"
        )

    print()
    print("INITIAL_STRESS_SHELL:")
    print("-" * 50)
    for count in element_counts:
        result = benchmark_stress_shell_read(count)
        print(
            f"  {count:>5} elements: {result['avg_time_ms']:>8.1f} ms avg, "
            f"{result['elements_per_second']:>8.0f} elem/s"
        )

    print()
    print("=" * 70)


if __name__ == "__main__":
    run_benchmarks()
