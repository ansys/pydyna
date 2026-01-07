#!/usr/bin/env python

# Copyright (C) 2023 - 2026 ANSYS, Inc. and/or its affiliates.
# SPDX-License-Identifier: MIT
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

"""
Dead Code Detection Script for Codegen

This script runs coverage analysis on the codegen system and identifies files
and code segments with low coverage that may be dead code candidates.

Usage:
    python find_dead_code.py [--threshold PERCENT] [--html]

Options:
    --threshold PERCENT    Coverage threshold below which files are flagged (default: 80)
    --html                Generate HTML coverage report in codegen/coverage_html/
    --show-missing        Show line numbers of missing coverage
"""

import argparse
from pathlib import Path
import subprocess
import sys


def run_coverage_analysis(show_missing: bool = False) -> tuple[int, str]:
    """
    Run coverage analysis on codegen.

    This runs coverage on both:
    1. The code generation script (generate.py) - tests production code paths
    2. The test suite (pytest -m codegen) - tests error handling and edge cases

    Args:
        show_missing: If True, include missing line numbers in report

    Returns:
        Tuple of (return_code, output_text)
    """
    script_dir = Path(__file__).parent
    generate_script = script_dir / "generate.py"
    project_root = script_dir.parent
    coverage_file = script_dir / ".coverage"

    print("Running coverage analysis on codegen...")
    print("=" * 70)

    # Remove old coverage data
    if coverage_file.exists():
        coverage_file.unlink()

    # Step 1: Run coverage on generate.py
    print("Step 1/2: Running coverage on generate.py...")
    result = subprocess.run(
        [
            sys.executable,
            "-m",
            "coverage",
            "run",
            f"--data-file={coverage_file}",
            "--source=keyword_generation",
            "--omit=*/templates/*,*/__pycache__/*",
            str(generate_script),
        ],
        capture_output=True,
        text=True,
        cwd=script_dir,
    )

    if result.returncode != 0:
        print("Error running coverage on generate.py:")
        print(result.stderr)
        return result.returncode, ""

    # Step 2: Append coverage from test suite
    print("Step 2/2: Running coverage on test suite...")
    result = subprocess.run(
        [
            sys.executable,
            "-m",
            "coverage",
            "run",
            "--append",
            f"--data-file={coverage_file}",
            "--source=codegen/keyword_generation",
            "--omit=*/templates/*,*/__pycache__/*",
            "-m",
            "pytest",
            "tests/test_codegen/test_handlers.py",
            "-m",
            "codegen",
            "-q",
        ],
        capture_output=True,
        text=True,
        cwd=project_root,
    )

    if result.returncode != 0:
        print("Warning: Test suite had issues (coverage data still collected)")
        # Don't fail here - we still want the coverage report

    # Generate report
    report_args = [sys.executable, "-m", "coverage", "report", f"--data-file={coverage_file}"]
    if show_missing:
        report_args.append("--show-missing")

    result = subprocess.run(report_args, capture_output=True, text=True, cwd=script_dir)

    return result.returncode, result.stdout


def generate_html_report() -> bool:
    """
    Generate HTML coverage report.

    Returns:
        True if successful, False otherwise
    """
    script_dir = Path(__file__).parent
    html_dir = script_dir / "coverage_html"
    coverage_file = script_dir / ".coverage"

    print(f"\nGenerating HTML coverage report in {html_dir}...")

    result = subprocess.run(
        [sys.executable, "-m", "coverage", "html", f"--data-file={coverage_file}", "-d", str(html_dir)],
        capture_output=True,
        text=True,
        cwd=script_dir,
    )

    if result.returncode != 0:
        print("Error generating HTML report:")
        print(result.stderr)
        return False

    print(f"HTML report generated: {html_dir / 'index.html'}")
    return True


def parse_coverage_report(report: str, threshold: int) -> list[dict]:
    """
    Parse coverage report and extract low-coverage files.

    Args:
        report: Coverage report text output
        threshold: Coverage percentage threshold

    Returns:
        List of dicts with file info for files below threshold
    """
    low_coverage_files = []
    lines = report.split("\n")

    # Skip header lines
    for line in lines[2:]:
        if not line.strip() or line.startswith("---") or line.startswith("TOTAL"):
            continue

        parts = line.split()
        if len(parts) >= 4:
            try:
                # Extract: Name, Stmts, Miss, Cover
                name = parts[0]
                stmts = int(parts[1])
                miss = int(parts[2])
                cover = int(parts[3].rstrip("%"))

                if cover < threshold and stmts > 0:  # Ignore empty files
                    low_coverage_files.append({"name": name, "statements": stmts, "missed": miss, "coverage": cover})
            except (ValueError, IndexError):
                continue

    return low_coverage_files


def print_low_coverage_summary(low_coverage_files: list[dict], threshold: int) -> None:
    """
    Print summary of low-coverage files.

    Args:
        low_coverage_files: List of low-coverage file info dicts
        threshold: Coverage threshold used
    """
    if not low_coverage_files:
        print(f"\n✓ All files meet the coverage threshold of {threshold}%")
        return

    print(f"\n⚠ Files below {threshold}% coverage threshold:")
    print("=" * 70)
    print(f"{'File':<50} {'Coverage':<10} {'Missed Lines':<12}")
    print("-" * 70)

    # Sort by coverage (lowest first)
    for file_info in sorted(low_coverage_files, key=lambda x: x["coverage"]):
        name = file_info["name"]
        coverage = file_info["coverage"]
        missed = file_info["missed"]

        # Truncate long names
        if len(name) > 48:
            name = "..." + name[-45:]

        print(f"{name:<50} {coverage:>3}%        {missed:>4} lines")

    print("=" * 70)
    print(f"\nTotal files flagged: {len(low_coverage_files)}")
    print("\nThese files may contain dead code or need additional test coverage.")
    print("Review them manually to determine if code can be safely removed.")


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(description="Detect potential dead code in codegen via coverage analysis")
    parser.add_argument(
        "--threshold",
        type=int,
        default=80,
        help="Coverage threshold below which files are flagged (default: 80)",
    )
    parser.add_argument("--html", action="store_true", help="Generate HTML coverage report")
    parser.add_argument(
        "--show-missing",
        action="store_true",
        help="Show line numbers of missing coverage in terminal output",
    )

    args = parser.parse_args()

    # Run coverage analysis
    return_code, report_output = run_coverage_analysis(args.show_missing)

    if return_code != 0:
        print("Coverage analysis failed")
        return 1

    # Print the full report
    print("\n" + report_output)

    # Generate HTML if requested
    if args.html:
        generate_html_report()

    # Parse and analyze low-coverage files
    low_coverage_files = parse_coverage_report(report_output, args.threshold)
    print_low_coverage_summary(low_coverage_files, args.threshold)

    # Return non-zero if low coverage files found (for CI/CD integration)
    return 1 if low_coverage_files else 0


if __name__ == "__main__":
    sys.exit(main())
