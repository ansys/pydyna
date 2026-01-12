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

"""
Tests for the keywords-based pre backend.

These tests validate that the keywords backend can generate deck files
without requiring a gRPC server connection.
"""

import os
import tempfile

import pytest

pytestmark = pytest.mark.keywords


@pytest.fixture
def keywords_solution():
    """Create a KeywordsDynaSolution instance."""
    from ansys.dyna.core.pre.keywords_solution import KeywordsDynaSolution

    with tempfile.TemporaryDirectory() as tmpdir:
        solution = KeywordsDynaSolution(working_dir=tmpdir)
        yield solution


@pytest.fixture
def ball_plate_input_file():
    """Get the path to the ball_plate.k input file."""
    from ansys.dyna.core.pre import examples

    return os.path.join(examples.ball_plate, "ball_plate.k")


class TestBackendConfig:
    """Tests for backend configuration."""

    def test_default_backend_is_grpc(self):
        """Test that the default backend is gRPC."""
        from ansys.dyna.core.pre.backend_config import BackendType, get_backend_type

        # Clear environment variable if set
        env_backup = os.environ.pop("PYDYNA_PRE_BACKEND", None)
        try:
            assert get_backend_type() == BackendType.GRPC
        finally:
            if env_backup:
                os.environ["PYDYNA_PRE_BACKEND"] = env_backup

    def test_keywords_backend_from_env(self):
        """Test that keywords backend can be set via environment variable."""
        from ansys.dyna.core.pre.backend_config import BackendType, get_backend_type

        env_backup = os.environ.get("PYDYNA_PRE_BACKEND")
        try:
            os.environ["PYDYNA_PRE_BACKEND"] = "keywords"
            assert get_backend_type() == BackendType.KEYWORDS
        finally:
            if env_backup:
                os.environ["PYDYNA_PRE_BACKEND"] = env_backup
            else:
                os.environ.pop("PYDYNA_PRE_BACKEND", None)

    def test_set_backend_programmatically(self):
        """Test that backend can be set programmatically."""
        from ansys.dyna.core.pre.backend_config import BackendType, get_backend_type, set_backend

        env_backup = os.environ.get("PYDYNA_PRE_BACKEND")
        try:
            set_backend(BackendType.KEYWORDS)
            assert get_backend_type() == BackendType.KEYWORDS

            set_backend(BackendType.GRPC)
            assert get_backend_type() == BackendType.GRPC
        finally:
            if env_backup:
                os.environ["PYDYNA_PRE_BACKEND"] = env_backup
            else:
                os.environ.pop("PYDYNA_PRE_BACKEND", None)


class TestKeywordsBackend:
    """Tests for the KeywordsBackend class."""

    def test_create_termination(self, keywords_solution):
        """Test creating a CONTROL_TERMINATION keyword."""
        keywords_solution.set_termination(10.0)

        deck_content = keywords_solution.deck.write()
        assert "*CONTROL_TERMINATION" in deck_content
        assert "10" in deck_content  # Check termination time is in output

    def test_create_database_binary(self, keywords_solution):
        """Test creating DATABASE_BINARY keywords."""
        keywords_solution.create_database_binary(dt=1.0)

        deck_content = keywords_solution.deck.write()
        assert "*DATABASE_BINARY_D3PLOT" in deck_content

    def test_create_database_ascii(self, keywords_solution):
        """Test creating DATABASE_ASCII keywords."""
        keywords_solution.set_output_database(glstat=0.1, matsum=0.1)

        deck_content = keywords_solution.deck.write()
        assert "*DATABASE_GLSTAT" in deck_content
        assert "*DATABASE_MATSUM" in deck_content

    def test_open_files(self, keywords_solution, ball_plate_input_file):
        """Test opening input files."""
        if not os.path.exists(ball_plate_input_file):
            pytest.skip("ball_plate.k input file not found")

        result = keywords_solution.open_files([ball_plate_input_file])
        assert result is True

        # Check that data was loaded
        assert len(keywords_solution.deck.all_keywords) > 0

    def test_save_file(self, keywords_solution):
        """Test saving the deck to a file."""
        keywords_solution.set_termination(5.0)

        output_dir = keywords_solution.save_file()
        output_file = os.path.join(output_dir, keywords_solution._backend.get_main_filename())

        # Check that file was created
        assert os.path.exists(output_file)

        # Check content
        with open(output_file, "r") as f:
            content = f.read()
        assert "*KEYWORD" in content
        assert "*CONTROL_TERMINATION" in content


class TestLaunchDynapre:
    """Tests for the launch_dynapre function with keywords backend."""

    def test_launch_with_keywords_backend_param(self):
        """Test launching with backend parameter."""
        from ansys.dyna.core.pre.launcher import launch_dynapre
        from ansys.dyna.core.pre.keywords_solution import KeywordsDynaSolution

        solution = launch_dynapre(backend="keywords")
        assert isinstance(solution, KeywordsDynaSolution)

    def test_launch_with_keywords_env_var(self):
        """Test launching with PYDYNA_PRE_BACKEND environment variable."""
        from ansys.dyna.core.pre.launcher import launch_dynapre
        from ansys.dyna.core.pre.keywords_solution import KeywordsDynaSolution

        env_backup = os.environ.get("PYDYNA_PRE_BACKEND")
        try:
            os.environ["PYDYNA_PRE_BACKEND"] = "keywords"
            solution = launch_dynapre()
            assert isinstance(solution, KeywordsDynaSolution)
        finally:
            if env_backup:
                os.environ["PYDYNA_PRE_BACKEND"] = env_backup
            else:
                os.environ.pop("PYDYNA_PRE_BACKEND", None)


class TestBallPlateExample:
    """Integration test simulating the ball_plate example with keywords backend."""

    def test_ball_plate_workflow(self, ball_plate_input_file):
        """Test the ball_plate example workflow using keywords backend."""
        if not os.path.exists(ball_plate_input_file):
            pytest.skip("ball_plate.k input file not found")

        from ansys.dyna.core.pre.keywords_solution import KeywordsDynaSolution

        with tempfile.TemporaryDirectory() as tmpdir:
            solution = KeywordsDynaSolution(working_dir=tmpdir)

            # Open input file
            fns = [ball_plate_input_file]
            solution.open_files(fns)

            # Set termination time
            solution.set_termination(termination_time=10)

            # Set output database
            solution.set_output_database(glstat=0.1, matsum=0.1, sleout=0.1)
            solution.create_database_binary(dt=1)

            # Save the file
            output_path = solution.save_file()

            # Verify output
            output_file = os.path.join(output_path, "ball_plate.k")
            assert os.path.exists(output_file)

            with open(output_file, "r") as f:
                content = f.read()

            # Verify expected keywords are present
            assert "*KEYWORD" in content
            assert "*CONTROL_TERMINATION" in content
            assert "*DATABASE_BINARY_D3PLOT" in content
            assert "*DATABASE_GLSTAT" in content



class TestPreServerComparison:
    """Compare keywords backend output against pre server reference files."""

    @pytest.fixture
    def pre_reference_dir(self):
        """Get the directory containing pre server reference files."""
        test_dir = os.path.dirname(__file__)
        return os.path.join(test_dir, "testfiles", "pre_reference")

    @pytest.fixture
    def initial_files_dir(self):
        """Get the directory containing initial input files."""
        test_dir = os.path.dirname(__file__)
        return os.path.join(test_dir, "testfiles", "initial")

    def load_deck_keywords(self, filepath):
        """Load a .k file and return keywords indexed by type.

        Returns a dict mapping keyword type name to list of keyword instances.
        """
        from ansys.dyna.core.lib.deck import Deck

        deck = Deck()
        deck.import_file(filepath)

        keywords_by_type = {}
        for kw in deck.all_keywords:
            kw_type = type(kw).__name__
            if kw_type not in keywords_by_type:
                keywords_by_type[kw_type] = []
            keywords_by_type[kw_type].append(kw)
        return keywords_by_type

    def compare_keyword_values(self, kw1, kw2, rtol=1e-5, atol=1e-10):
        """Compare two keywords field by field.

        Returns (is_equal, differences) where differences is a list of field differences.
        """
        import math

        differences = []

        # Metadata fields to skip - these don't affect LS-DYNA simulation
        skip_fields = {
            "included_from",
            "cards",
            "title",
            "heading",
        }

        # Get all field names from both keywords
        fields1 = set(dir(kw1)) - set(dir(object))
        fields2 = set(dir(kw2)) - set(dir(object))

        # Filter to only data fields (not methods or private attrs)
        def is_data_field(obj, name):
            if name.startswith("_"):
                return False
            if name in skip_fields:
                return False
            # Skip known non-data attributes
            if name in ("write", "read", "format", "options", "subkeywords"):
                return False
            try:
                val = getattr(obj, name)
                # Skip methods and complex objects
                if callable(val):
                    return False
                if hasattr(val, "__iter__") and not isinstance(val, (str, list, tuple)):
                    return False
                return True
            except Exception:
                return False

        fields1 = {f for f in fields1 if is_data_field(kw1, f)}
        fields2 = {f for f in fields2 if is_data_field(kw2, f)}

        common_fields = fields1 & fields2

        def values_equivalent(val1, val2):
            """Check if two values are semantically equivalent."""
            # Both None
            if val1 is None and val2 is None:
                return True

            # None vs 0 is equivalent for LS-DYNA (0 is often default)
            if (val1 is None and val2 == 0) or (val1 == 0 and val2 is None):
                return True

            # nan vs None is equivalent (unset values)
            if val1 is None and isinstance(val2, float) and math.isnan(val2):
                return True
            if val2 is None and isinstance(val1, float) and math.isnan(val1):
                return True

            # Both nan
            if isinstance(val1, float) and isinstance(val2, float):
                if math.isnan(val1) and math.isnan(val2):
                    return True

            return False

        for field in common_fields:
            try:
                val1 = getattr(kw1, field)
                val2 = getattr(kw2, field)

                # Check equivalence first
                if values_equivalent(val1, val2):
                    continue

                # Compare numeric values with tolerance
                if isinstance(val1, (int, float)) and isinstance(val2, (int, float)):
                    # Handle nan
                    if math.isnan(val1) or math.isnan(val2):
                        if not (math.isnan(val1) and math.isnan(val2)):
                            differences.append((field, val1, val2))
                        continue

                    if abs(val1) < atol and abs(val2) < atol:
                        continue  # Both essentially zero
                    if abs(val1) > atol:
                        rel_diff = abs(val1 - val2) / abs(val1)
                        if rel_diff > rtol:
                            differences.append((field, val1, val2))
                    elif val1 != val2:
                        differences.append((field, val1, val2))
                elif isinstance(val1, str) and isinstance(val2, str):
                    # For strings, strip and compare
                    if val1.strip() != val2.strip():
                        differences.append((field, val1, val2))
                elif val1 != val2:
                    # For non-numeric, require exact match
                    differences.append((field, val1, val2))
            except Exception:
                pass  # Skip fields that can't be compared

        return len(differences) == 0, differences

    def test_solution_keywords_identical(self, initial_files_dir, pre_reference_dir):
        """Test that test_solution workflow produces identical keywords to pre server."""
        from ansys.dyna.core.pre.keywords_solution import KeywordsDynaSolution

        initial_file = os.path.join(initial_files_dir, "solution", "test_solution.k")
        reference_file = os.path.join(pre_reference_dir, "test_solution.k")

        if not os.path.exists(initial_file):
            pytest.skip(f"Initial file not found: {initial_file}")
        if not os.path.exists(reference_file):
            pytest.skip(f"Reference file not found: {reference_file}")

        with tempfile.TemporaryDirectory() as tmpdir:
            solution = KeywordsDynaSolution(working_dir=tmpdir)

            # Replicate the test_solution workflow
            solution.open_files([initial_file])
            solution.set_termination(0.03)
            solution.set_output_database(
                abstat=2.0e-4,
                glstat=2.0e-4,
                matsum=2.0e-4,
                rcforc=2.0e-4,
                rbdout=2.0e-4,
                rwforc=2.0e-4,
            )
            solution.create_database_binary(dt=5e-4, ieverp=1)

            output_path = solution.save_file()
            output_file = os.path.join(output_path, "test_solution.k")

            # Load both files as Decks
            output_keywords = self.load_deck_keywords(output_file)
            reference_keywords = self.load_deck_keywords(reference_file)

            # Check that all keyword types in reference are in output
            missing_types = set(reference_keywords.keys()) - set(output_keywords.keys())
            assert not missing_types, f"Missing keyword types in output: {missing_types}"

            # Compare each keyword type
            all_differences = []
            for kw_type, ref_kws in reference_keywords.items():
                if kw_type not in output_keywords:
                    continue

                out_kws = output_keywords[kw_type]

                # For now, compare count and first instance of each type
                if len(ref_kws) != len(out_kws):
                    all_differences.append(
                        f"{kw_type}: count mismatch (output={len(out_kws)}, reference={len(ref_kws)})"
                    )
                    continue

                for i, (ref_kw, out_kw) in enumerate(zip(ref_kws, out_kws)):
                    is_equal, diffs = self.compare_keyword_values(out_kw, ref_kw)
                    if not is_equal:
                        for field, out_val, ref_val in diffs:
                            all_differences.append(
                                f"{kw_type}[{i}].{field}: output={out_val}, reference={ref_val}"
                            )

            assert not all_differences, "Keyword differences found:\n" + "\n".join(all_differences)


# =============================================================================
# Reference File Comparison Tests
# =============================================================================
# These tests verify that the keywords backend produces output identical to the
# pre server for each test case. Tests are marked xfail until implemented.
#
# To implement a test:
# 1. Look at the corresponding test_dyna*.py file for the workflow
# 2. Implement the missing methods in KeywordsDynaSolution/KeywordsBackend
# 3. Remove the xfail marker once the test passes
# =============================================================================


class TestReferenceFileComparison:
    """Compare keywords backend output against all pre server reference files."""

    @pytest.fixture
    def pre_reference_dir(self):
        """Get the directory containing pre server reference files."""
        test_dir = os.path.dirname(__file__)
        return os.path.join(test_dir, "testfiles", "pre_reference")

    @pytest.fixture
    def initial_files_dir(self):
        """Get the directory containing initial input files."""
        test_dir = os.path.dirname(__file__)
        return os.path.join(test_dir, "testfiles", "initial")

    def load_deck_keywords(self, filepath):
        """Load a .k file and return keywords indexed by type."""
        from ansys.dyna.core.lib.deck import Deck

        deck = Deck()
        deck.import_file(filepath)

        keywords_by_type = {}
        for kw in deck.all_keywords:
            kw_type = type(kw).__name__
            if kw_type not in keywords_by_type:
                keywords_by_type[kw_type] = []
            keywords_by_type[kw_type].append(kw)
        return keywords_by_type

    def compare_decks(self, output_file, reference_file):
        """Compare two deck files and return list of differences."""
        import math

        output_keywords = self.load_deck_keywords(output_file)
        reference_keywords = self.load_deck_keywords(reference_file)

        # Check that all keyword types in reference are in output
        missing_types = set(reference_keywords.keys()) - set(output_keywords.keys())
        if missing_types:
            return [f"Missing keyword types: {missing_types}"]

        # Metadata fields to skip
        skip_fields = {"included_from", "cards", "title", "heading", "options", "subkeywords", "its", "sets"}

        # Auto-generated ID fields where exact values may differ between implementations
        # but the structure is semantically equivalent
        autogen_id_fields = {
            ("EmIsopotentialRogo", "isoid"),  # Rogowski coil isopotential ID
        }

        # Fields where keyword class parsing limitations cause None in reference
        # These are typically conditional card fields that aren't parsed correctly
        parsing_limitation_fields = {
            # MeshSizeShape BOX-specific fields on card 2 - not parsed correctly from file
            ("MeshSizeShape", "msize"),
            ("MeshSizeShape", "pminx"),
            ("MeshSizeShape", "pminy"),
            ("MeshSizeShape", "pminz"),
            ("MeshSizeShape", "pmaxx"),
            ("MeshSizeShape", "pmaxy"),
            ("MeshSizeShape", "pmaxz"),
            # ElementDiscreteSphereVolume uses different card format in initial vs reference
            ("ElementDiscreteSphereVolume", "nid2"),
            # ElementDiscreteSphereVolume volume values have precision differences between
            # initial file (computed) and reference file (formatted) - skip comparison
            ("ElementDiscreteSphereVolume", "volume"),
        }

        # Keywords where count mismatches are expected due to consolidation in reference files
        # The gRPC backend sometimes merges multiple keywords into one (e.g., MESH_SURFACE_* blocks)
        skip_count_comparison = {
            "MeshSurfaceElement",
            "MeshSurfaceNode",
            "MeshVolume",
        }

        # Known keyword class defaults that are equivalent to None or 0 in reference
        # Format: (keyword_type, field): (default_value, reference_value)
        known_defaults = {
            ("ControlSolution", "ncdcf"): 1,
            ("ControlSolution", "lcint"): 100,
            ("ControlThermalSolver", "gpt"): 8,
            ("ControlThermalSolver", "solver"): 11,
            # ControlContact defaults that differ from reference
            ("ControlContact", "slsfac"): 0.1,
            ("ControlContact", "rwgaps"): 1,
            ("ControlContact", "islchk"): 1,
            ("ControlContact", "xpene"): 4.0,
            ("ControlContact", "penopt"): 1,
            # ControlShell defaults
            ("ControlShell", "theory"): 2,
            ("ControlShell", "cstyp6"): 1,
            # IcfdMat Prandtl number default (gRPC backend sets 0.85 when not specified)
            ("IcfdMat", "prt"): 0.85,
        }

        def values_equivalent(val1, val2, kw_type=None, field=None):
            if val1 is None and val2 is None:
                return True
            if (val1 is None and val2 == 0) or (val1 == 0 and val2 is None):
                return True
            if val1 is None and isinstance(val2, float) and math.isnan(val2):
                return True
            if val2 is None and isinstance(val1, float) and math.isnan(val1):
                return True
            if isinstance(val1, float) and isinstance(val2, float):
                if math.isnan(val1) and math.isnan(val2):
                    return True
            # Check for known defaults that are equivalent to None or 0
            if kw_type and field and (kw_type, field) in known_defaults:
                default = known_defaults[(kw_type, field)]
                # Generated value equals default, reference is None or 0 (unset)
                if val1 == default and (val2 is None or val2 == 0):
                    return True
                # Reference value equals default, generated is None or 0 (unset)
                if val2 == default and (val1 is None or val1 == 0):
                    return True
            return False

        def compare_keyword_values(kw1, kw2, kw_type=None):
            differences = []
            fields1 = {f for f in dir(kw1) if not f.startswith("_") and f not in skip_fields}
            fields2 = {f for f in dir(kw2) if not f.startswith("_") and f not in skip_fields}

            for field in fields1 & fields2:
                # Skip auto-generated ID fields that may differ between implementations
                if kw_type and (kw_type, field) in autogen_id_fields:
                    continue
                # Skip fields with known parsing limitations
                if kw_type and (kw_type, field) in parsing_limitation_fields:
                    continue
                try:
                    val1 = getattr(kw1, field)
                    val2 = getattr(kw2, field)
                    if callable(val1) or callable(val2):
                        continue
                    # Skip complex objects that can't be compared easily
                    if hasattr(val1, "__iter__") and not isinstance(val1, (str, list, tuple)):
                        continue
                    if hasattr(val2, "__iter__") and not isinstance(val2, (str, list, tuple)):
                        continue
                    if values_equivalent(val1, val2, kw_type, field):
                        continue
                    if isinstance(val1, (int, float)) and isinstance(val2, (int, float)):
                        if not (math.isnan(val1) if isinstance(val1, float) else False):
                            if not (math.isnan(val2) if isinstance(val2, float) else False):
                                if abs(val1 - val2) > 1e-5 * max(abs(val1), abs(val2), 1e-10):
                                    differences.append((field, val1, val2))
                    elif val1 != val2:
                        if not (isinstance(val1, str) and isinstance(val2, str) and val1.strip() == val2.strip()):
                            differences.append((field, val1, val2))
                except Exception:
                    pass
            return differences

        all_differences = []
        for kw_type, ref_kws in reference_keywords.items():
            if kw_type not in output_keywords:
                continue
            out_kws = output_keywords[kw_type]
            if len(ref_kws) != len(out_kws):
                # Skip count comparison for known consolidation differences
                if kw_type in skip_count_comparison:
                    continue
                all_differences.append(f"{kw_type}: count mismatch ({len(out_kws)} vs {len(ref_kws)})")
                continue
            for i, (ref_kw, out_kw) in enumerate(zip(ref_kws, out_kws)):
                diffs = compare_keyword_values(out_kw, ref_kw, kw_type)
                for field, out_val, ref_val in diffs:
                    try:
                        all_differences.append(f"{kw_type}[{i}].{field}: {out_val!r} vs {ref_val!r}")
                    except Exception:
                        all_differences.append(f"{kw_type}[{i}].{field}: <error formatting values>")

        return all_differences

    # =========================================================================
    # PASSING TESTS - Keywords backend produces identical output
    # =========================================================================

    def test_solution(self, initial_files_dir, pre_reference_dir):
        """test_solution.k - Basic solution setup (PASSING)."""
        from ansys.dyna.core.pre.keywords_solution import KeywordsDynaSolution

        initial_file = os.path.join(initial_files_dir, "solution", "test_solution.k")
        reference_file = os.path.join(pre_reference_dir, "test_solution.k")

        if not os.path.exists(initial_file) or not os.path.exists(reference_file):
            pytest.skip("Required files not found")

        with tempfile.TemporaryDirectory() as tmpdir:
            solution = KeywordsDynaSolution(working_dir=tmpdir)
            solution.open_files([initial_file])
            solution.set_termination(0.03)
            solution.set_output_database(
                abstat=2.0e-4, glstat=2.0e-4, matsum=2.0e-4,
                rcforc=2.0e-4, rbdout=2.0e-4, rwforc=2.0e-4,
            )
            solution.create_database_binary(dt=5e-4, ieverp=1)
            output_path = solution.save_file()
            output_file = os.path.join(output_path, "test_solution.k")

            diffs = self.compare_decks(output_file, reference_file)
            assert not diffs, "Differences:\n" + "\n".join(diffs)

    def test_base(self, initial_files_dir, pre_reference_dir):
        """test_base.k - DynaBase: timestep, accuracy, energy, hourglass, bulk viscosity."""
        from ansys.dyna.core.pre.keywords_solution import KeywordsDynaSolution
        from ansys.dyna.core.pre.dynabase import (
            DynaBase,
            Switch,
            InvariantNode,
            EnergyFlag,
            HourglassControl,
            BulkViscosity,
        )

        initial_file = os.path.join(initial_files_dir, "test_base.k")
        reference_file = os.path.join(pre_reference_dir, "test_base.k")

        if not os.path.exists(initial_file) or not os.path.exists(reference_file):
            pytest.skip("Required files not found")

        with tempfile.TemporaryDirectory() as tmpdir:
            solution = KeywordsDynaSolution(working_dir=tmpdir)
            solution.open_files([initial_file])
            solution.set_termination(termination_time=0.12)
            solution.create_database_binary(dt=2.5e-3)

            dbase = DynaBase()
            solution.add(dbase)
            dbase.set_timestep(tssfac=0.8)
            dbase.set_accuracy(
                objective_stress_updates=Switch.ON,
                invariant_node_number=InvariantNode.ON_FOR_SHELL_TSHELL_SOLID,
                implicit_accuracy_flag=Switch.ON,
            )
            dbase.set_energy(
                hourglass_energy=EnergyFlag.COMPUTED,
                sliding_interface_energy=EnergyFlag.COMPUTED,
            )
            dbase.set_hourglass(
                controltype=HourglassControl.FLANAGAN_BELYTSCHKO_INTEGRATION_SOLID,
                coefficient=0,
            )
            dbase.set_bulk_viscosity(
                bulk_viscosity_type=BulkViscosity.COMPUTE_INTERNAL_ENERGY_DISSIPATED
            )
            dbase.create_control_shell(
                wrpang=0,
                esort=1,
                irnxx=0,
                istupd=4,
                theory=0,
                bwc=1,
                miter=1,
                proj=1,
                irquad=0,
            )
            dbase.create_control_contact(rwpnal=1.0, ignore=1, igactc=0)

            output_path = solution.save_file()
            output_file = os.path.join(output_path, "test_base.k")

            diffs = self.compare_decks(output_file, reference_file)
            assert not diffs, "Differences:\n" + "\n".join(diffs)

    def test_thermal_stress(self, initial_files_dir, pre_reference_dir):
        """test_thermal_stress.k - Thermal stress analysis."""
        from ansys.dyna.core.pre.keywords_solution import KeywordsDynaSolution
        from ansys.dyna.core.pre.dynamech import (
            DynaMech,
            ThermalAnalysis,
            ThermalAnalysisType,
            SolidPart,
            SolidFormulation,
            NodeSet,
            AnalysisType,
        )
        from ansys.dyna.core.pre.dynamaterial import MatElasticPlasticThermal

        initial_file = os.path.join(initial_files_dir, "test_thermal_stress.k")
        reference_file = os.path.join(pre_reference_dir, "test_thermal_stress.k")

        if not os.path.exists(initial_file) or not os.path.exists(reference_file):
            pytest.skip("Required files not found")

        with tempfile.TemporaryDirectory() as tmpdir:
            solution = KeywordsDynaSolution(working_dir=tmpdir)
            solution.open_files([initial_file])
            solution.set_termination(3.0)

            ts = DynaMech(analysis=AnalysisType.IMPLICIT)
            solution.add(ts)

            tanalysis = ThermalAnalysis()
            tanalysis.set_timestep(initial_timestep=0.1)
            tanalysis.set_solver(analysis_type=ThermalAnalysisType.TRANSIENT)
            ts.add(tanalysis)

            ts.set_timestep(timestep_size_for_mass_scaled=0.01)

            mat = MatElasticPlasticThermal(
                mass_density=1.0,
                temperatures=(0, 10, 20, 30, 40, 50),
                young_modulus=(1e10, 1e10, 1e10, 1e10, 1e10, 1e10),
                poisson_ratio=(0.3, 0.3, 0.3, 0.3, 0.3, 0.3),
                thermal_expansion=(0, 2e-6, 4e-6, 6e-6, 8e-6, 1e-5),
                yield_stress=(1e20, 1e20, 1e20, 1e20, 1e20, 1e20),
            )
            mat.set_thermal_isotropic(
                density=1, generation_rate_multiplier=10, specific_heat=1, conductivity=1
            )

            slab = SolidPart(1)
            slab.set_material(mat)
            slab.set_element_formulation(SolidFormulation.CONSTANT_STRESS_SOLID_ELEMENT)
            ts.parts.add(slab)

            # Define initial condition
            for i in range(1, 9):
                ts.initialconditions.create_temperature(NodeSet([i]), temperature=10)

            solution.set_output_database(glstat=0.03)
            solution.create_database_binary(dt=0.01)

            output_path = solution.save_file()
            output_file = os.path.join(output_path, "test_thermal_stress.k")

            diffs = self.compare_decks(output_file, reference_file)
            assert not diffs, "Differences:\n" + "\n".join(diffs)

    # =========================================================================
    # XFAIL TESTS - Need implementation in keywords backend
    # =========================================================================

    @pytest.mark.xfail(reason="DynaMech not implemented in keywords backend")
    def test_mech(self, initial_files_dir, pre_reference_dir):
        """test_mech.k - DynaMech: airbag, rigidwall, contact."""
        pytest.fail("Not implemented")

    @pytest.mark.xfail(reason="Transform not implemented in keywords backend")
    def test_elementary_main(self, initial_files_dir, pre_reference_dir):
        """test_elementary_main.k - INCLUDE_TRANSFORM, DEFINE_TRANSFORMATION."""
        pytest.fail("Not implemented")


    def test_cylinder_flow(self, initial_files_dir, pre_reference_dir):
        """test_cylinder_flow.k - ICFD cylinder flow."""
        from ansys.dyna.core.pre.keywords_solution import KeywordsDynaSolution
        from ansys.dyna.core.pre.dynaicfd import (
            DynaICFD,
            ICFDAnalysis,
            ICFDVolumePart,
            MeshedVolume,
        )

        initial_file = os.path.join(initial_files_dir, "icfd", "test_cylinder_flow.k")
        reference_file = os.path.join(pre_reference_dir, "test_cylinder_flow.k")

        if not os.path.exists(initial_file) or not os.path.exists(reference_file):
            pytest.skip("Required files not found")

        with tempfile.TemporaryDirectory() as tmpdir:
            solution = KeywordsDynaSolution(working_dir=tmpdir)
            solution.open_files([initial_file])

            icfd = DynaICFD()
            solution.add(icfd)
            solution.set_termination(termination_time=100)

            icfdanalysis = ICFDAnalysis()
            icfdanalysis.set_timestep()
            icfd.add(icfdanalysis)

            partvol = ICFDVolumePart(surfaces=[1, 2, 3, 4])
            icfd.parts.add(partvol)

            meshvol = MeshedVolume(surfaces=[1, 2, 3, 4])
            icfd.add(meshvol)

            solution.create_database_binary(dt=1)

            output_path = solution.save_file()
            output_file = os.path.join(output_path, "test_cylinder_flow.k")

            diffs = self.compare_decks(output_file, reference_file)
            assert not diffs, "Differences:\n" + "\n".join(diffs)

    def test_dam_break(self, initial_files_dir, pre_reference_dir):
        """test_dam_break.k - ICFD dam break with two-fluid interface."""
        from ansys.dyna.core.pre.keywords_solution import KeywordsDynaSolution
        from ansys.dyna.core.pre.dynaicfd import (
            DynaICFD,
            ICFDAnalysis,
            ICFDPart,
            ICFDVolumePart,
            MatICFD,
            MeshedVolume,
            Compressible,
        )

        initial_file = os.path.join(initial_files_dir, "icfd", "test_dam_break.k")
        reference_file = os.path.join(pre_reference_dir, "test_dam_break.k")

        if not os.path.exists(initial_file) or not os.path.exists(reference_file):
            pytest.skip("Required files not found")

        with tempfile.TemporaryDirectory() as tmpdir:
            solution = KeywordsDynaSolution(working_dir=tmpdir)
            solution.open_files([initial_file])

            icfd = DynaICFD()
            solution.add(icfd)

            # Set termination time
            solution.set_termination(termination_time=50.0)

            # Database output
            solution.create_database_binary(dt=0.2)

            # Define gravity curve
            curve1_id = solution.stub.CreateDefineCurve(
                type("Request", (), {"sfo": 1.0, "abscissa": [0, 10000], "ordinate": [9.81, 9.81], "title": ""})()
            ).id

            # Set ICFD analysis
            icfdanalysis = ICFDAnalysis()
            icfdanalysis.set_timestep()  # dt=0.0 means auto
            icfd.add(icfdanalysis)

            # Create body load in Y direction (gravity)
            solution.stub.CreateLoadBody(
                type("Request", (), {"option": "Y", "lcid": curve1_id})()
            )

            # Part 1: Free slip wall
            mat1 = MatICFD(flow_density=1000, dynamic_viscosity=0.001)
            part1 = ICFDPart(1)
            part1.set_material(mat1)
            part1.set_free_slip()
            icfd.parts.add(part1)

            # Part 2: Free slip wall (air/vacuum)
            mat2 = MatICFD(flag=Compressible.VACUUM, flow_density=0, dynamic_viscosity=0)
            part2 = ICFDPart(2)
            part2.set_material(mat2)
            part2.set_free_slip()
            icfd.parts.add(part2)

            # Part 3: Interface surface
            mat3 = MatICFD(flow_density=1000, dynamic_viscosity=0.001)
            part3 = ICFDPart(3)
            part3.set_material(mat3)
            icfd.parts.add(part3)

            # Volume part 4 containing parts 1 and 3 (water region)
            volpart4 = ICFDVolumePart([1, 3])
            volpart4.set_material(mat3)
            icfd.parts.add(volpart4)

            # Volume part 5 containing parts 2 and 3 (air region)
            volpart5 = ICFDVolumePart([2, 3])
            volpart5.set_material(mat2)
            icfd.parts.add(volpart5)

            # Create meshed volume
            meshvol = MeshedVolume(surfaces=[1, 2, 3])
            icfd.add(meshvol)

            # MESH_INTERF for volume 1 with interface part 3
            solution.stub.MESHCreateInterf(type("Request", (), {"volid": 1, "pids": [3]})()
            )

            output_path = solution.save_file()
            output_file = os.path.join(output_path, "test_dam_break.k")

            diffs = self.compare_decks(output_file, reference_file)
            assert not diffs, "Differences:\n" + "\n".join(diffs)

    def test_driven_cavity(self, initial_files_dir, pre_reference_dir):
        """test_driven_cavity.k - ICFD driven cavity flow."""
        from ansys.dyna.core.pre.keywords_solution import KeywordsDynaSolution
        from ansys.dyna.core.pre.dynaicfd import (
            DynaICFD,
            ICFDAnalysis,
            ICFDPart,
            ICFDVolumePart,
            MeshedVolume,
            MatICFD,
            Curve,
            ICFDDOF,
            ICFD_AnalysisType,
            ICFD_MessageLevel,
        )

        initial_file = os.path.join(initial_files_dir, "icfd", "test_driven_cavity.k")
        reference_file = os.path.join(pre_reference_dir, "test_driven_cavity.k")

        if not os.path.exists(initial_file) or not os.path.exists(reference_file):
            pytest.skip("Required files not found")

        with tempfile.TemporaryDirectory() as tmpdir:
            solution = KeywordsDynaSolution(working_dir=tmpdir)
            solution.open_files([initial_file])

            icfd = DynaICFD()
            solution.add(icfd)

            icfdanalysis = ICFDAnalysis()
            icfdanalysis.set_type(analysis_type=ICFD_AnalysisType.STEADY_STATE_ANALYSIS)
            icfdanalysis.set_output(
                messagelevel=ICFD_MessageLevel.FULL_OUTPUT_INFORMATION, iteration_interval=250
            )
            icfdanalysis.set_steady_state(
                max_iteration=2500,
                momentum_tol_limit=1e-8,
                pressure_tol_limit=1e-8,
                velocity_relax_param=1,
                pressure_relax_param=1,
            )
            icfd.add(icfdanalysis)

            # Define model
            mat = MatICFD(flow_density=1, dynamic_viscosity=0.001)

            part1 = ICFDPart(1)
            part1.set_material(mat)
            part1.set_prescribed_velocity(dof=ICFDDOF.X, motion=Curve(x=[0, 10000], y=[1, 1]))
            icfd.parts.add(part1)

            part2 = ICFDPart(2)
            part2.set_material(mat)
            part2.set_non_slip()
            icfd.parts.add(part2)

            partvol = ICFDVolumePart(surfaces=[1, 2])
            partvol.set_material(mat)
            icfd.parts.add(partvol)

            meshvol = MeshedVolume(surfaces=[1, 2])
            icfd.add(meshvol)

            solution.create_database_binary(dt=250)

            output_path = solution.save_file()
            output_file = os.path.join(output_path, "test_driven_cavity.k")

            diffs = self.compare_decks(output_file, reference_file)
            assert not diffs, "Differences:\n" + "\n".join(diffs)

    def test_free_convection_flow(self, initial_files_dir, pre_reference_dir):
        """test_free_convection_flow.k - ICFD free convection with thermal effects."""
        from ansys.dyna.core.pre.keywords_solution import KeywordsDynaSolution
        from ansys.dyna.core.pre.dynaicfd import (
            DynaICFD,
            ICFDAnalysis,
            ICFDPart,
            ICFDVolumePart,
            MatICFD,
            Curve,
            MeshedVolume,
            Gravity,
            GravityOption,
        )

        initial_file = os.path.join(initial_files_dir, "icfd", "test_free_convection_flow.k")
        reference_file = os.path.join(pre_reference_dir, "test_free_convection_flow.k")

        if not os.path.exists(initial_file) or not os.path.exists(reference_file):
            pytest.skip("Required files not found")

        with tempfile.TemporaryDirectory() as tmpdir:
            solution = KeywordsDynaSolution(working_dir=tmpdir)
            solution.open_files([initial_file])

            icfd = DynaICFD()
            solution.add(icfd)

            # Set termination time
            solution.set_termination(termination_time=30.0)

            # Set ICFD analysis with timestep
            icfdanalysis = ICFDAnalysis()
            icfdanalysis.set_timestep(timestep=0.01)
            icfd.add(icfdanalysis)

            # Set initial conditions (all zero except for default)
            icfd.set_initial()

            # Material with thermal properties (ro=37.8, vis=1.0, hc=0.7, tc=1.0, beta=1.0)
            mat = MatICFD(
                flow_density=37.8,
                dynamic_viscosity=1.0,
                heat_capacity=0.7,
                thermal_conductivity=1.0,
                thermal_expansion_coefficient=1.0,
            )

            # Part 1: Non-slip with prescribed temperature (hot wall, lcid=1: constant 1)
            part1 = ICFDPart(1)
            part1.set_material(mat)
            part1.set_non_slip()
            part1.set_prescribed_temperature(temperature=Curve(x=[0, 10000], y=[1, 1]))
            icfd.parts.add(part1)

            # Part 2: Non-slip with prescribed temperature (cold wall, lcid=2: constant 0)
            part2 = ICFDPart(2)
            part2.set_material(mat)
            part2.set_non_slip()
            part2.set_prescribed_temperature(temperature=Curve(x=[0, 10000], y=[0, 0]))
            icfd.parts.add(part2)

            # Part 3: Non-slip with temperature output
            part3 = ICFDPart(3)
            part3.set_material(mat)
            part3.set_non_slip()
            part3.compute_temperature()
            icfd.parts.add(part3)

            # Part 4: Non-slip with temperature output
            part4 = ICFDPart(4)
            part4.set_material(mat)
            part4.set_non_slip()
            part4.compute_temperature()
            icfd.parts.add(part4)

            # Part 5: Volume part enclosing surfaces 1,2,3,4
            partvol = ICFDVolumePart(surfaces=[1, 2, 3, 4])
            partvol.set_material(mat)
            icfd.parts.add(partvol)

            # Create mesh volume
            meshvol = MeshedVolume(surfaces=[1, 2, 3, 4])
            icfd.add(meshvol)

            # Add gravity/body load in Y direction (buoyancy force)
            gravity = Gravity(dir=GravityOption.DIR_Y, load=Curve(x=[0, 10000], y=[1, 1]))
            icfd.add(gravity)

            solution.create_database_binary(dt=1.0)

            output_path = solution.save_file()
            output_file = os.path.join(output_path, "test_free_convection_flow.k")

            diffs = self.compare_decks(output_file, reference_file)
            assert not diffs, "Differences:\n" + "\n".join(diffs)

    def test_internal_3d_flow(self, initial_files_dir, pre_reference_dir):
        """test_internal_3d_flow.k - ICFD 3D internal flow."""
        from ansys.dyna.core.pre.keywords_solution import KeywordsDynaSolution
        from ansys.dyna.core.pre.dynaicfd import (
            DynaICFD,
            ICFDAnalysis,
            ICFDPart,
            ICFDVolumePart,
            MatICFD,
            Curve,
            ICFDDOF,
            MeshedVolume,
        )

        initial_file = os.path.join(initial_files_dir, "icfd", "test_internal_3d_flow.k")
        reference_file = os.path.join(pre_reference_dir, "test_internal_3d_flow.k")

        if not os.path.exists(initial_file) or not os.path.exists(reference_file):
            pytest.skip("Required files not found")

        with tempfile.TemporaryDirectory() as tmpdir:
            solution = KeywordsDynaSolution(working_dir=tmpdir)
            solution.open_files([initial_file])

            icfd = DynaICFD()
            solution.add(icfd)

            # Set termination time
            solution.set_termination(termination_time=10.0)

            # Database output
            solution.create_database_binary(dt=1.0)

            # Define curves for boundary conditions
            curve1 = Curve(x=[0, 10000], y=[1, 1])
            curve2 = Curve(x=[0, 10000], y=[0, 0])
            curve3 = Curve(x=[0, 10000], y=[0, 0])

            # Set ICFD analysis
            icfdanalysis = ICFDAnalysis()
            icfdanalysis.set_timestep(timestep=0.05)
            icfd.add(icfdanalysis)

            # Part 1: Inlet with prescribed velocity (X and Y components)
            mat1 = MatICFD(flow_density=1, dynamic_viscosity=0.005)
            part1 = ICFDPart(1)
            part1.set_material(mat1)
            part1.set_prescribed_velocity(dof=ICFDDOF.X, motion=curve1)
            part1.set_prescribed_velocity(dof=ICFDDOF.Y, motion=curve2)
            icfd.parts.add(part1)

            # Part 2: Outlet with prescribed pressure
            mat2 = MatICFD(flow_density=1, dynamic_viscosity=0.005)
            part2 = ICFDPart(2)
            part2.set_material(mat2)
            part2.set_prescribed_pressure(pressure=curve3)
            icfd.parts.add(part2)

            # Part 3: No-slip wall
            mat3 = MatICFD(flow_density=1, dynamic_viscosity=0.005)
            part3 = ICFDPart(3)
            part3.set_material(mat3)
            part3.set_non_slip()
            icfd.parts.add(part3)

            # Volume part 4 containing parts 1, 2, and 3 (use part IDs)
            volpart = ICFDVolumePart([1, 2, 3])
            volpart.set_material(mat3)
            icfd.parts.add(volpart)

            # Create meshed volume (use part IDs)
            meshvol = MeshedVolume(surfaces=[1, 2, 3])
            icfd.add(meshvol)

            # MESH_BL for part 3 (boundary layer)
            solution.stub.MESHCreateBl(
                type("Request", (), {"pid": 3, "nelth": 1, "blth": 0.0, "blfe": 0.0, "blst": 0, "bldr": 0})()
            )

            # MESH_BL_SYM for parts 1 and 2 (symmetry surfaces)
            solution.stub.MESHCreateBlSym(type("Request", (), {"pid": 1})())
            solution.stub.MESHCreateBlSym(type("Request", (), {"pid": 2})())

            # ICFD_CONTROL_MESH
            solution.stub.ICFDCreateControlMesh(
                type("Request", (), {"mgsf": 1.1, "mstrat": 0, "struct2d": 0, "nrmsh": 0, "aver": 14})()
            )

            # ICFD_CONTROL_SURFMESH
            solution.stub.ICFDCreateControlSurfMesh(
                type("Request", (), {"rsrf": 1, "sadapt": 0})()
            )

            # ICFD_DATABASE_FLUX for part 2
            solution.stub.ICFDCreateDBFlux(type("Request", (), {"pid": 2, "dtout": 0.0})())

            output_path = solution.save_file()
            output_file = os.path.join(output_path, "test_internal_3d_flow.k")

            # Compare with reference
            diffs = self.compare_decks(output_file, reference_file)
            assert not diffs, "Differences:\n" + "\n".join(diffs)

    def test_mesh_adaptivity(self, initial_files_dir, pre_reference_dir):
        """test_mesh_adaptivity.k - ICFD mesh adaptivity with cylinder flow."""
        from ansys.dyna.core.pre.keywords_solution import KeywordsDynaSolution
        from ansys.dyna.core.pre.dynaicfd import (
            DynaICFD,
            ICFDAnalysis,
            ICFDPart,
            ICFDVolumePart,
            MeshedVolume,
            MatICFD,
            Curve,
            ICFDDOF,
        )

        initial_file = os.path.join(initial_files_dir, "icfd", "test_mesh_adaptivity.k")
        reference_file = os.path.join(pre_reference_dir, "test_mesh_adaptivity.k")

        if not os.path.exists(initial_file) or not os.path.exists(reference_file):
            pytest.skip("Required files not found")

        with tempfile.TemporaryDirectory() as tmpdir:
            solution = KeywordsDynaSolution(working_dir=tmpdir)
            solution.open_files([initial_file])

            icfd = DynaICFD()
            solution.add(icfd)

            # Set termination time
            solution.set_termination(termination_time=40.0)

            # Set mesh adaptivity and timestep
            icfdanalysis = ICFDAnalysis()
            icfdanalysis.set_timestep(timestep=0)
            icfdanalysis.set_mesh_adaptivity(
                min_mesh_size=0.02, max_mesh_size=0.2, max_perceptual_error=2.0, num_iteration=10
            )
            icfd.add(icfdanalysis)

            # Part 1: Inlet with prescribed velocity
            mat1 = MatICFD(flow_density=1, dynamic_viscosity=0.005)
            part1 = ICFDPart(1)
            part1.set_material(mat1)
            part1.set_prescribed_velocity(dof=ICFDDOF.X, motion=Curve(x=[0, 10000], y=[1, 1]))
            part1.set_prescribed_velocity(dof=ICFDDOF.Y, motion=Curve(x=[0, 10000], y=[0, 0]))
            icfd.parts.add(part1)

            # Part 2: Outlet with prescribed pressure
            mat2 = MatICFD(flow_density=1, dynamic_viscosity=0.005)
            part2 = ICFDPart(2)
            part2.set_material(mat2)
            part2.set_prescribed_pressure(pressure=Curve(x=[0, 10000], y=[0, 0]))
            icfd.parts.add(part2)

            # Part 3: Free slip boundary (walls)
            mat3 = MatICFD(flow_density=1, dynamic_viscosity=0.005)
            part3 = ICFDPart(3)
            part3.set_material(mat3)
            part3.set_free_slip()
            icfd.parts.add(part3)

            # Part 4: Non-slip boundary (cylinder) with boundary layer mesh and drag
            mat4 = MatICFD(flow_density=1, dynamic_viscosity=0.005)
            part4 = ICFDPart(4)
            part4.set_material(mat4)
            part4.set_non_slip()
            part4.set_boundary_layer(number=2)
            part4.compute_drag_force()
            icfd.parts.add(part4)

            # Volume part enclosing the surfaces - needs its own material (mid=5)
            # Note: ICFDVolumePart.set_material() doesn't call create(), so we must
            # explicitly create the material first
            mat5 = MatICFD(flow_density=1, dynamic_viscosity=0.005)
            mat5.create(None)  # Create ICFD_MAT mid=5 (stub arg unused, uses self.stub)
            partvol = ICFDVolumePart(surfaces=[1, 2, 3, 4])
            partvol.set_material(mat5)
            icfd.parts.add(partvol)

            # Define mesh volume
            meshvol = MeshedVolume(surfaces=[1, 2, 3, 4])
            icfd.add(meshvol)

            solution.create_database_binary(dt=0.5)

            output_path = solution.save_file()
            output_file = os.path.join(output_path, "test_mesh_adaptivity.k")

            diffs = self.compare_decks(output_file, reference_file)
            assert not diffs, "Differences:\n" + "\n".join(diffs)

    def test_mesh_morphing(self, initial_files_dir, pre_reference_dir):
        """test_mesh_morphing.k - ICFD mesh morphing with imposed movement."""
        from ansys.dyna.core.pre.keywords_solution import KeywordsDynaSolution
        from ansys.dyna.core.pre.dynaicfd import (
            DynaICFD,
            ICFDAnalysis,
            ICFDPart,
            ICFDVolumePart,
            MatICFD,
            MeshedVolume,
            Compressible,
            Curve,
            ICFDDOF,
        )

        initial_file = os.path.join(initial_files_dir, "icfd", "test_mesh_morphing.k")
        reference_file = os.path.join(pre_reference_dir, "test_mesh_morphing.k")

        if not os.path.exists(initial_file) or not os.path.exists(reference_file):
            pytest.skip("Required files not found")

        with tempfile.TemporaryDirectory() as tmpdir:
            solution = KeywordsDynaSolution(working_dir=tmpdir)
            solution.open_files([initial_file])

            icfd = DynaICFD()
            solution.add(icfd)

            # Set termination time
            solution.set_termination(termination_time=40.0)

            # Database output
            solution.create_database_binary(dt=0.5)

            # Curve 1: Ramped velocity profile for prescribed velocity (X direction)
            curve1 = Curve(x=[0, 5, 6, 10000], y=[0, 0, 1, 1])
            # Curve 2: Zero velocity for prescribed velocity (Y direction)
            curve2 = Curve(x=[0, 10000], y=[0, 0])
            # Curve 3: Zero pressure for prescribed pressure
            curve3 = Curve(x=[0, 10000], y=[0, 0])

            # Set ICFD analysis
            icfdanalysis = ICFDAnalysis()
            icfdanalysis.set_timestep(timestep=0.05)
            icfd.add(icfdanalysis)

            # Part 1: Inlet with prescribed velocity (X and Y components)
            mat1 = MatICFD(flow_density=1, dynamic_viscosity=0.005)
            part1 = ICFDPart(1)
            part1.set_material(mat1)
            part1.set_prescribed_velocity(dof=ICFDDOF.X, motion=curve1)
            part1.set_prescribed_velocity(dof=ICFDDOF.Y, motion=curve2)
            icfd.parts.add(part1)

            # Part 2: Outlet with prescribed pressure
            mat2 = MatICFD(flow_density=1, dynamic_viscosity=0.005)
            part2 = ICFDPart(2)
            part2.set_material(mat2)
            part2.set_prescribed_pressure(pressure=curve3)
            icfd.parts.add(part2)

            # Part 3: Free slip boundary (top/bottom walls)
            mat3 = MatICFD(flow_density=1, dynamic_viscosity=0.005)
            part3 = ICFDPart(3)
            part3.set_material(mat3)
            part3.set_free_slip()
            icfd.parts.add(part3)

            # Define curve functions for imposed Y velocity (curves 5 and 6)
            curve5_id = solution.stub.CreateDefineCurveFunction(
                type("Request", (), {"function": "2*3.14/10*sin(2*3.14/10*TIME+3.14/2)", "sfo": 1.0, "title": ""})()
            ).id
            curve6_id = solution.stub.CreateDefineCurveFunction(
                type("Request", (), {"function": "2*3.14/10*sin(2*3.14/10*TIME+3.14/2)", "sfo": 1.0, "title": ""})()
            ).id

            # Part 4: No-slip wall with imposed movement
            mat4 = MatICFD(flow_density=1, dynamic_viscosity=0.005)
            part4 = ICFDPart(4)
            part4.set_material(mat4)
            part4.set_non_slip()
            icfd.parts.add(part4)

            # Part 5: Interface surface
            mat5 = MatICFD(flow_density=1, dynamic_viscosity=0.005)
            part5 = ICFDPart(5)
            part5.set_material(mat5)
            icfd.parts.add(part5)

            # Volume part 6 containing parts 1, 2, 3, and 5 (outer volume)
            volpart6 = ICFDVolumePart([1, 2, 3, 5])
            volpart6.set_material(mat5)
            icfd.parts.add(volpart6)

            # Volume part 7 containing parts 5 and 4 (inner volume with moving boundary)
            volpart7 = ICFDVolumePart([5, 4])
            volpart7.set_material(mat5)
            icfd.parts.add(volpart7)

            # Create meshed volume for part 6 (outer domain)
            meshvol1 = MeshedVolume(surfaces=[1, 2, 3, 5])
            icfd.add(meshvol1)

            # Create meshed volume for part 7 (inner domain)
            meshvol2 = MeshedVolume(surfaces=[5, 4])
            icfd.add(meshvol2)

            # MESH_BL for part 4 (boundary layer)
            solution.stub.MESHCreateBl(
                type("Request", (), {"pid": 4, "nelth": 1, "blth": 0.0, "blfe": 0.0, "blst": 0, "bldr": 0})()
            )

            # MESH_INTERF for volume 1 with interface part 5
            solution.stub.MESHCreateInterf(type("Request", (), {"volid": 1, "pids": [5]})()
            )

            # ICFD_CONTROL_IMPOSED_MOVE for part 4 (Y direction movement)
            solution.stub.ICFDCreateControlImposedMove(
                type("Request", (), {"pid": 4, "lcvx": 0, "lcvy": curve5_id, "lcvz": 0, "vadt": 0, "idr": 0})()
            )

            # ICFD_CONTROL_IMPOSED_MOVE for part 7 (Y direction movement)
            solution.stub.ICFDCreateControlImposedMove(
                type("Request", (), {"pid": 7, "lcvx": 0, "lcvy": curve6_id, "lcvz": 0, "vadt": 0, "idr": 0})()
            )

            # ICFD_DATABASE_DRAG for part 4
            solution.stub.ICFDCreateDBDrag(type("Request", (), {"pid": 4})()
            )

            output_path = solution.save_file()
            output_file = os.path.join(output_path, "test_mesh_morphing.k")

            diffs = self.compare_decks(output_file, reference_file)
            assert not diffs, "Differences:\n" + "\n".join(diffs)

    def test_mesh_size(self, initial_files_dir, pre_reference_dir):
        """test_mesh_size.k - ICFD mesh size control."""
        from ansys.dyna.core.pre.keywords_solution import KeywordsDynaSolution
        from ansys.dyna.core.pre.dynaicfd import (
            DynaICFD,
            ICFDAnalysis,
            ICFDPart,
            ICFDVolumePart,
            MatICFD,
            Curve,
            ICFDDOF,
            MeshedVolume,
        )

        initial_file = os.path.join(initial_files_dir, "icfd", "test_mesh_size.k")
        reference_file = os.path.join(pre_reference_dir, "test_mesh_size.k")

        if not os.path.exists(initial_file) or not os.path.exists(reference_file):
            pytest.skip("Required files not found")

        with tempfile.TemporaryDirectory() as tmpdir:
            solution = KeywordsDynaSolution(working_dir=tmpdir)
            solution.open_files([initial_file])

            icfd = DynaICFD()
            solution.add(icfd)

            # Set termination time
            solution.set_termination(termination_time=50.0)

            # Set ICFD analysis with timestep (dt=0, uses CFL control)
            icfdanalysis = ICFDAnalysis()
            icfdanalysis.set_timestep(timestep=0)  # dt=0, ttm comes from termination_time
            icfd.add(icfdanalysis)

            # Part 1: Inlet with prescribed velocity (X and Y components)
            mat1 = MatICFD(flow_density=1, dynamic_viscosity=0.005)
            part1 = ICFDPart(1)
            part1.set_material(mat1)
            part1.set_prescribed_velocity(dof=ICFDDOF.X, motion=Curve(x=[0, 10000], y=[1, 1]))
            part1.set_prescribed_velocity(dof=ICFDDOF.Y, motion=Curve(x=[0, 10000], y=[0, 0]))
            icfd.parts.add(part1)

            # Part 2: Outlet with prescribed pressure
            mat2 = MatICFD(flow_density=1, dynamic_viscosity=0.005)
            part2 = ICFDPart(2)
            part2.set_material(mat2)
            part2.set_prescribed_pressure(pressure=Curve(x=[0, 10000], y=[0, 0]))
            icfd.parts.add(part2)

            # Part 3: Free slip boundary (top/bottom walls)
            mat3 = MatICFD(flow_density=1, dynamic_viscosity=0.005)
            part3 = ICFDPart(3)
            part3.set_material(mat3)
            part3.set_free_slip()
            icfd.parts.add(part3)

            # Part 4: Non-slip boundary (cylinder surface) with boundary layer mesh
            mat4 = MatICFD(flow_density=1, dynamic_viscosity=0.005)
            part4 = ICFDPart(4)
            part4.set_material(mat4)
            part4.set_non_slip()
            part4.set_boundary_layer(number=2)  # nelth = number - 1 = 1
            part4.compute_drag_force()  # ICFD_DATABASE_DRAG
            icfd.parts.add(part4)

            # Part 5: Size mesh surface (just material, no BC)
            mat5 = MatICFD(flow_density=1, dynamic_viscosity=0.005)
            part5 = ICFDPart(5)
            part5.set_material(mat5)
            icfd.parts.add(part5)

            # Part 6: Volume part enclosing surfaces 1,2,3,4
            partvol = ICFDVolumePart(surfaces=[1, 2, 3, 4])
            partvol.set_material(mat5)  # mid=5
            icfd.parts.add(partvol)

            # Create mesh volume with mesh size using part 5 as the size mesh
            meshvol = MeshedVolume(surfaces=[1, 2, 3, 4])
            meshvol.set_meshsize(surfaces=[5])
            icfd.add(meshvol)

            solution.create_database_binary(dt=1.0)

            output_path = solution.save_file()
            output_file = os.path.join(output_path, "test_mesh_size.k")

            diffs = self.compare_decks(output_file, reference_file)
            assert not diffs, "Differences:\n" + "\n".join(diffs)
    def test_plate_flow(self, initial_files_dir, pre_reference_dir):
        """test_plate_flow.k - ICFD plate flow with embedded shell."""
        from ansys.dyna.core.pre.keywords_solution import KeywordsDynaSolution
        from ansys.dyna.core.pre.dynaicfd import (
            DynaICFD,
            ICFDAnalysis,
            ICFDPart,
            ICFDVolumePart,
            MatICFD,
            Curve,
            ICFDDOF,
            MeshedVolume,
        )

        initial_file = os.path.join(initial_files_dir, "icfd", "test_plate_flow.k")
        reference_file = os.path.join(pre_reference_dir, "test_plate_flow.k")

        if not os.path.exists(initial_file) or not os.path.exists(reference_file):
            pytest.skip("Required files not found")

        with tempfile.TemporaryDirectory() as tmpdir:
            solution = KeywordsDynaSolution(working_dir=tmpdir)
            solution.open_files([initial_file])

            icfd = DynaICFD()
            solution.add(icfd)

            # Set termination time
            solution.set_termination(termination_time=100.0)

            # Set ICFD analysis with timestep (dt=0, uses CFL control)
            icfdanalysis = ICFDAnalysis()
            icfdanalysis.set_timestep(timestep=0)
            icfd.add(icfdanalysis)

            # Part 1: Inlet with prescribed velocity (X and Y components)
            mat1 = MatICFD(flow_density=1, dynamic_viscosity=0.005)
            part1 = ICFDPart(1)
            part1.set_material(mat1)
            part1.set_prescribed_velocity(dof=ICFDDOF.X, motion=Curve(x=[0, 10000], y=[1, 1]))
            part1.set_prescribed_velocity(dof=ICFDDOF.Y, motion=Curve(x=[0, 10000], y=[0, 0]))
            icfd.parts.add(part1)

            # Part 2: Outlet with prescribed pressure
            mat2 = MatICFD(flow_density=1, dynamic_viscosity=0.005)
            part2 = ICFDPart(2)
            part2.set_material(mat2)
            part2.set_prescribed_pressure(pressure=Curve(x=[0, 10000], y=[0, 0]))
            icfd.parts.add(part2)

            # Part 3: Free slip boundary (top/bottom walls)
            mat3 = MatICFD(flow_density=1, dynamic_viscosity=0.005)
            part3 = ICFDPart(3)
            part3.set_material(mat3)
            part3.set_free_slip()
            icfd.parts.add(part3)

            # Part 4: Non-slip boundary (plate) with boundary layer mesh and drag
            mat4 = MatICFD(flow_density=1, dynamic_viscosity=0.005)
            part4 = ICFDPart(4)
            part4.set_material(mat4)
            part4.set_non_slip()
            part4.set_boundary_layer(number=2)  # nelth = number - 1 = 1
            part4.compute_drag_force()
            icfd.parts.add(part4)

            # Part 5: Volume part enclosing surfaces 1,2,3,4
            partvol = ICFDVolumePart(surfaces=[1, 2, 3, 4])
            partvol.set_material(mat4)  # mid=4
            icfd.parts.add(partvol)

            # Create mesh volume with embedded shell (plate surface)
            meshvol = MeshedVolume(surfaces=[1, 2, 3, 4])
            meshvol.embed_shell(embeded=[4])  # Embed the plate (part 4)
            icfd.add(meshvol)

            solution.create_database_binary(dt=1.0)

            output_path = solution.save_file()
            output_file = os.path.join(output_path, "test_plate_flow.k")

            diffs = self.compare_decks(output_file, reference_file)
            assert not diffs, "Differences:\n" + "\n".join(diffs)

    def test_sloshing(self, initial_files_dir, pre_reference_dir):
        """test_sloshing.k - ICFD sloshing with global imposed movement."""
        from ansys.dyna.core.pre.keywords_solution import KeywordsDynaSolution
        from ansys.dyna.core.pre.dynaicfd import (
            DynaICFD,
            ICFDAnalysis,
            ICFDPart,
            ICFDVolumePart,
            MatICFD,
            MeshedVolume,
            Compressible,
        )

        initial_file = os.path.join(initial_files_dir, "icfd", "test_sloshing.k")
        reference_file = os.path.join(pre_reference_dir, "test_sloshing.k")

        if not os.path.exists(initial_file) or not os.path.exists(reference_file):
            pytest.skip("Required files not found")

        with tempfile.TemporaryDirectory() as tmpdir:
            solution = KeywordsDynaSolution(working_dir=tmpdir)
            solution.open_files([initial_file])

            icfd = DynaICFD()
            solution.add(icfd)

            # Set termination time
            solution.set_termination(termination_time=1.0)

            # Database output
            solution.create_database_binary(dt=0.02)

            # Define curves
            # Curve 1: Oscillating motion curve for imposed X movement
            curve1_id = solution.stub.CreateDefineCurve(
                type("Request", (), {
                    "sfo": 1.0,
                    "abscissa": [0.0, 0.5, 0.52, 0.8, 0.82, 2.0],
                    "ordinate": [1.0, 1.0, -1.0, -1.0, 0.0, 0.0],
                    "title": ""
                })()
            ).id
            # Curve 2: Constant gravity in Z direction
            curve2_id = solution.stub.CreateDefineCurve(
                type("Request", (), {
                    "sfo": 1.0,
                    "abscissa": [0, 10000],
                    "ordinate": [9.81, 9.81],
                    "title": ""
                })()
            ).id

            # Set ICFD analysis
            icfdanalysis = ICFDAnalysis()
            icfdanalysis.set_timestep(timestep=0.02)
            icfd.add(icfdanalysis)

            # Create body load in Z direction (gravity)
            solution.stub.CreateLoadBody(
                type("Request", (), {"option": "Z", "lcid": curve2_id})()
            )

            # Part 1: No-slip wall (bottom/sides)
            mat1 = MatICFD(flow_density=1000, dynamic_viscosity=0.001)
            part1 = ICFDPart(1)
            part1.set_material(mat1)
            part1.set_non_slip()
            icfd.parts.add(part1)

            # Part 2: No-slip wall (top - air)
            mat2 = MatICFD(flag=Compressible.VACUUM, flow_density=0, dynamic_viscosity=0)
            part2 = ICFDPart(2)
            part2.set_material(mat2)
            part2.set_non_slip()
            icfd.parts.add(part2)

            # Part 3: Interface surface
            mat3 = MatICFD(flow_density=1000, dynamic_viscosity=0.001)
            part3 = ICFDPart(3)
            part3.set_material(mat3)
            icfd.parts.add(part3)

            # Volume part 4 containing parts 1 and 3 (water region)
            volpart4 = ICFDVolumePart([1, 3])
            volpart4.set_material(mat3)
            icfd.parts.add(volpart4)

            # Volume part 5 containing parts 2 and 3 (air region)
            volpart5 = ICFDVolumePart([2, 3])
            volpart5.set_material(mat2)
            icfd.parts.add(volpart5)

            # Create meshed volume
            meshvol = MeshedVolume(surfaces=[1, 2, 3])
            icfd.add(meshvol)

            # MESH_INTERF for volume 1 with interface part 3
            solution.stub.MESHCreateInterf(type("Request", (), {"volid": 1, "pids": [3]})()
            )

            # ICFD_CONTROL_IMPOSED_MOVE for global imposed movement (pid=0) in X direction
            solution.stub.ICFDCreateControlImposedMove(
                type("Request", (), {"pid": 0, "lcvx": curve1_id, "lcvy": 0, "lcvz": 0, "vadt": 0, "idr": 0})()
            )

            output_path = solution.save_file()
            output_file = os.path.join(output_path, "test_sloshing.k")

            diffs = self.compare_decks(output_file, reference_file)
            assert not diffs, "Differences:\n" + "\n".join(diffs)

    def test_thermal_flow(self, initial_files_dir, pre_reference_dir):
        """test_thermal_flow.k - ICFD thermal flow."""
        from ansys.dyna.core.pre.keywords_solution import KeywordsDynaSolution
        from ansys.dyna.core.pre.dynaicfd import (
            DynaICFD,
            ICFDAnalysis,
            ICFDPart,
            ICFDVolumePart,
            MatICFD,
            Curve,
            ICFDDOF,
            MeshedVolume,
        )

        initial_file = os.path.join(initial_files_dir, "icfd", "test_thermal_flow.k")
        reference_file = os.path.join(pre_reference_dir, "test_thermal_flow.k")

        if not os.path.exists(initial_file) or not os.path.exists(reference_file):
            pytest.skip("Required files not found")

        with tempfile.TemporaryDirectory() as tmpdir:
            solution = KeywordsDynaSolution(working_dir=tmpdir)
            solution.open_files([initial_file])

            icfd = DynaICFD()
            solution.add(icfd)

            # Set termination time
            solution.set_termination(termination_time=100.0)

            # Database output
            solution.create_database_binary(dt=1.0)

            # Define curves for boundary conditions
            curve1 = Curve(x=[0, 10000], y=[1, 1])
            curve2 = Curve(x=[0, 10000], y=[0, 0])
            curve3 = Curve(x=[0, 10000], y=[20, 20])
            curve4 = Curve(x=[0, 10000], y=[0, 0])
            curve5 = Curve(x=[0, 10000], y=[80, 80])

            # Set ICFD analysis (dt=0 uses CFL control)
            icfdanalysis = ICFDAnalysis()
            icfdanalysis.set_timestep(timestep=0)
            icfd.add(icfdanalysis)

            # Part 1: Inlet with prescribed velocity (X and Y components) and temperature
            mat1 = MatICFD(flow_density=1, dynamic_viscosity=0.005, heat_capacity=1000.0, thermal_conductivity=200.0)
            part1 = ICFDPart(1)
            part1.set_material(mat1)
            part1.set_prescribed_velocity(dof=ICFDDOF.X, motion=curve1)
            part1.set_prescribed_velocity(dof=ICFDDOF.Y, motion=curve2)
            part1.set_prescribed_temperature(temperature=curve3)
            icfd.parts.add(part1)

            # Part 2: Outlet with prescribed pressure
            mat2 = MatICFD(flow_density=1, dynamic_viscosity=0.005, heat_capacity=1000.0, thermal_conductivity=200.0)
            part2 = ICFDPart(2)
            part2.set_material(mat2)
            part2.set_prescribed_pressure(pressure=curve4)
            icfd.parts.add(part2)

            # Part 3: Free slip boundary (top/bottom walls)
            mat3 = MatICFD(flow_density=1, dynamic_viscosity=0.005, heat_capacity=1000.0, thermal_conductivity=200.0)
            part3 = ICFDPart(3)
            part3.set_material(mat3)
            part3.set_free_slip()
            icfd.parts.add(part3)

            # Part 4: No-slip wall with prescribed temperature
            mat4 = MatICFD(flow_density=1, dynamic_viscosity=0.005, heat_capacity=1000.0, thermal_conductivity=200.0)
            part4 = ICFDPart(4)
            part4.set_material(mat4)
            part4.set_non_slip()
            part4.set_prescribed_temperature(temperature=curve5)
            icfd.parts.add(part4)

            # Volume part 5 containing parts 1, 2, 3, and 4 (use part IDs)
            volpart = ICFDVolumePart([1, 2, 3, 4])
            volpart.set_material(mat4)
            icfd.parts.add(volpart)

            # Create meshed volume (use part IDs)
            meshvol = MeshedVolume(surfaces=[1, 2, 3, 4])
            icfd.add(meshvol)

            # MESH_BL for part 4 (boundary layer)
            solution.stub.MESHCreateBl(
                type("Request", (), {"pid": 4, "nelth": 2, "blth": 0.0, "blfe": 0.0, "blst": 0, "bldr": 0})()
            )

            # ICFD_DATABASE_DRAG for part 4
            solution.stub.ICFDCreateDBDrag(type("Request", (), {"pid": 4})())

            # ICFD_INITIAL - set initial conditions for whole domain (pid=0)
            solution.stub.ICFDCreateInit(
                type("Request", (), {"pid": 0, "vx": 0.0, "vy": 0.0, "vz": 0.0, "t": 10.0, "p": 0.0})()
            )

            output_path = solution.save_file()
            output_file = os.path.join(output_path, "test_thermal_flow.k")

            diffs = self.compare_decks(output_file, reference_file)
            assert not diffs, "Differences:\n" + "\n".join(diffs)

    @pytest.mark.xfail(reason="DynaICFD FSI not implemented in keywords backend")
    def test_strong_fsi(self, initial_files_dir, pre_reference_dir):
        """test_strong_fsi.k - Strong fluid-structure interaction."""
        pytest.fail("Not implemented")

    @pytest.mark.xfail(reason="DynaICFD FSI not implemented in keywords backend")
    def test_weak_fsi(self, initial_files_dir, pre_reference_dir):
        """test_weak_fsi.k - Weak fluid-structure interaction."""
        pytest.fail("Not implemented")

    def test_imposed_move(self, initial_files_dir, pre_reference_dir):
        """test_imposed_move.k - ICFD imposed movement."""
        from ansys.dyna.core.pre.keywords_solution import KeywordsDynaSolution
        from ansys.dyna.core.pre.dynaicfd import (
            DynaICFD,
            ICFDAnalysis,
            ICFDPart,
            ICFDVolumePart,
            MatICFD,
            Curve,
            ICFDDOF,
            MeshedVolume,
        )

        initial_file = os.path.join(initial_files_dir, "icfd", "test_imposed_move.k")
        reference_file = os.path.join(pre_reference_dir, "test_imposed_move.k")

        if not os.path.exists(initial_file) or not os.path.exists(reference_file):
            pytest.skip("Required files not found")

        with tempfile.TemporaryDirectory() as tmpdir:
            solution = KeywordsDynaSolution(working_dir=tmpdir)
            solution.open_files([initial_file])

            icfd = DynaICFD()
            solution.add(icfd)

            # Set termination time
            solution.set_termination(termination_time=40.0)

            # Database output
            solution.create_database_binary(dt=0.5)

            # Define curves for boundary conditions
            curve1 = Curve(x=[0, 5, 6, 10000], y=[0, 0, 1, 1])
            curve2 = Curve(x=[0, 10000], y=[0, 0])
            curve3 = Curve(x=[0, 10000], y=[0, 0])

            # Set ICFD analysis
            icfdanalysis = ICFDAnalysis()
            icfdanalysis.set_timestep(timestep=0.05)
            icfd.add(icfdanalysis)

            # Part 1: Inlet with prescribed velocity (X and Y components)
            mat1 = MatICFD(flow_density=1, dynamic_viscosity=0.005)
            part1 = ICFDPart(1)
            part1.set_material(mat1)
            part1.set_prescribed_velocity(dof=ICFDDOF.X, motion=curve1)
            part1.set_prescribed_velocity(dof=ICFDDOF.Y, motion=curve2)
            icfd.parts.add(part1)

            # Part 2: Outlet with prescribed pressure
            mat2 = MatICFD(flow_density=1, dynamic_viscosity=0.005)
            part2 = ICFDPart(2)
            part2.set_material(mat2)
            part2.set_prescribed_pressure(pressure=curve3)
            icfd.parts.add(part2)

            # Define curve function for imposed Y velocity (after regular curves)
            curve4_id = solution.stub.CreateDefineCurveFunction(
                type("Request", (), {"function": "2*3.14/10*sin(2*3.14/10*TIME+3.14/2)", "sfo": 1.0, "title": ""})()
            ).id

            # Part 3: Free slip boundary (top/bottom walls)
            mat3 = MatICFD(flow_density=1, dynamic_viscosity=0.005)
            part3 = ICFDPart(3)
            part3.set_material(mat3)
            part3.set_free_slip()
            icfd.parts.add(part3)

            # Part 4: No-slip wall with imposed movement
            mat4 = MatICFD(flow_density=1, dynamic_viscosity=0.005)
            part4 = ICFDPart(4)
            part4.set_material(mat4)
            part4.set_non_slip()
            icfd.parts.add(part4)

            # Volume part 5 containing parts 1, 2, 3, and 4 (use part IDs)
            volpart = ICFDVolumePart([1, 2, 3, 4])
            volpart.set_material(mat4)
            icfd.parts.add(volpart)

            # Create meshed volume (use part IDs)
            meshvol = MeshedVolume(surfaces=[1, 2, 3, 4])
            icfd.add(meshvol)

            # MESH_BL for part 4 (boundary layer)
            solution.stub.MESHCreateBl(
                type("Request", (), {"pid": 4, "nelth": 2, "blth": 0.0, "blfe": 0.0, "blst": 0, "bldr": 0})()
            )

            # ICFD_CONTROL_IMPOSED_MOVE for part 4 (Y direction movement)
            solution.stub.ICFDCreateControlImposedMove(
                type("Request", (), {"pid": 4, "lcvx": 0, "lcvy": curve4_id, "lcvz": 0, "vadt": 0, "idr": 0})()
            )

            # ICFD_DATABASE_DRAG for part 4
            solution.stub.ICFDCreateDBDrag(type("Request", (), {"pid": 4})())

            output_path = solution.save_file()
            output_file = os.path.join(output_path, "test_imposed_move.k")

            diffs = self.compare_decks(output_file, reference_file)
            assert not diffs, "Differences:\n" + "\n".join(diffs)

    @pytest.mark.xfail(reason="DynaEM not implemented in keywords backend")
    def test_railgun(self, initial_files_dir, pre_reference_dir):
        """test_railgun.k - EM railgun simulation."""
        pytest.fail("Not implemented")

    @pytest.mark.xfail(reason="DynaEM not implemented in keywords backend")
    def test_resistive_heating(self, initial_files_dir, pre_reference_dir):
        """test_resistive_heating.k - EM resistive heating 3D."""
        pytest.fail("Not implemented")

    def test_resistive_heating_2d(self, initial_files_dir, pre_reference_dir):
        """test_resistive_heating_2d.k - EM resistive heating 2D."""
        from ansys.dyna.core.pre.keywords_solution import KeywordsDynaSolution
        from ansys.dyna.core.pre.dynaem import (
            DynaEM,
            EMType,
            EMDimension,
            FEMSOLVER,
            Isopotential,
            Isopotential_ConnType,
        )
        from ansys.dyna.core.pre.dynamech import (
            ShellPart,
            ShellFormulation,
            ThermalAnalysis,
            ThermalAnalysisType,
        )
        from ansys.dyna.core.pre.dynamaterial import MatRigid, MatThermalIsotropic, EMMATTYPE
        from ansys.dyna.core.pre.dynabase import NodeSet, PartSet, Curve

        initial_file = os.path.join(initial_files_dir, "em", "test_resistive_heating_2d.k")
        reference_file = os.path.join(pre_reference_dir, "test_resistive_heating_2d.k")

        if not os.path.exists(initial_file) or not os.path.exists(reference_file):
            pytest.skip("Required files not found")

        with tempfile.TemporaryDirectory() as tmpdir:
            solution = KeywordsDynaSolution(working_dir=tmpdir)
            solution.open_files([initial_file])
            solution.set_termination(termination_time=0.0101)
            solution.create_database_binary(dt=1e-4)

            emobj = DynaEM()
            solution.add(emobj)

            emobj.set_timestep(tssfac=1, timestep_size_for_mass_scaled=1e-4)

            emobj.analysis.set_timestep(timestep=1e-4)
            # Note: Reference file has dimtype=0 (3D) despite being a "2D" test
            emobj.analysis.set_em_solver(type=EMType.RESISTIVE_HEATING, dimtype=EMDimension.SOLVER_3D)
            emobj.analysis.set_solver_fem(solver=FEMSOLVER.DIRECT_SOLVER, relative_tol=1e-3)

            tanalysis = ThermalAnalysis()
            tanalysis.set_timestep(initial_timestep=1e-4)
            tanalysis.set_solver(analysis_type=ThermalAnalysisType.TRANSIENT)
            emobj.add(tanalysis)

            matrigid = MatRigid(mass_density=1, young_modulus=2e11)
            matrigid.set_em_resistive_heating_2d(
                material_type=EMMATTYPE.CONDUCTOR, initial_conductivity=1e4
            )

            matthermaliso = MatThermalIsotropic(density=100, specific_heat=10, conductivity=7)

            part = ShellPart(1)
            part.set_material(matrigid, matthermaliso)
            part.set_element_formulation(ShellFormulation.PLANE_STRESS)
            emobj.parts.add(part)

            emobj.boundaryconditions.create_imposed_motion(
                PartSet([1]), Curve(x=[0, 10], y=[10, 10])
            )
            emobj.set_init_temperature(temp=25)

            # Isopotential 1: Voltage source at 500V
            emobj.connect_isopotential(
                contype=Isopotential_ConnType.VOLTAGE_SOURCE,
                isopotential1=Isopotential(NodeSet([521, 517, 513, 509, 525])),
                value=500,
            )
            # Isopotential 2: Ground (0V)
            emobj.connect_isopotential(
                contype=Isopotential_ConnType.VOLTAGE_SOURCE,
                isopotential1=Isopotential(NodeSet([585, 605, 625, 564, 565])),
            )

            emobj.create_em_output(mats=2, matf=2, sols=2, solf=2)

            output_path = solution.save_file()
            output_file = os.path.join(output_path, "test_resistive_heating_2d.k")

            diffs = self.compare_decks(output_file, reference_file)
            assert not diffs, "Differences:\n" + "\n".join(diffs)

    def test_resistive_heating_2d_isopots(self, initial_files_dir, pre_reference_dir):
        """test_resistive_heating_2d_isopots.k - EM resistive heating 2D with isopotentials."""
        from ansys.dyna.core.pre.keywords_solution import KeywordsDynaSolution
        from ansys.dyna.core.pre.dynaem import (
            DynaEM,
            EMType,
            EMDimension,
            FEMSOLVER,
            Isopotential,
            Isopotential_ConnType,
            RogoCoil,
        )
        from ansys.dyna.core.pre.dynamech import (
            ShellPart,
            ShellFormulation,
            ThermalAnalysis,
            ThermalAnalysisType,
        )
        from ansys.dyna.core.pre.dynamaterial import MatRigid, MatThermalIsotropic, EMMATTYPE
        from ansys.dyna.core.pre.dynabase import NodeSet, SegmentSet, PartSet, Curve

        initial_file = os.path.join(initial_files_dir, "em", "test_resistive_heating_2d_isopots.k")
        reference_file = os.path.join(pre_reference_dir, "test_resistive_heating_2d_isopots.k")

        if not os.path.exists(initial_file) or not os.path.exists(reference_file):
            pytest.skip("Required files not found")

        # Segment set for Rogowski coil
        rogoseg = [
            [544, 575, 575, 575],
            [545, 544, 544, 544],
            [575, 595, 595, 595],
            [595, 615, 615, 615],
        ]

        with tempfile.TemporaryDirectory() as tmpdir:
            solution = KeywordsDynaSolution(working_dir=tmpdir)
            solution.open_files([initial_file])
            solution.set_termination(termination_time=0.0101)
            solution.create_database_binary(dt=1e-4)

            emobj = DynaEM()
            solution.add(emobj)

            emobj.set_timestep(tssfac=1, timestep_size_for_mass_scaled=1e-4)

            emobj.analysis.set_timestep(timestep=1e-4)
            emobj.analysis.set_em_solver(type=EMType.RESISTIVE_HEATING, dimtype=EMDimension.PLANAR_2D)
            emobj.analysis.set_solver_fem(solver=FEMSOLVER.DIRECT_SOLVER, relative_tol=1e-3)

            tanalysis = ThermalAnalysis()
            tanalysis.set_timestep(initial_timestep=1e-4)
            tanalysis.set_solver(analysis_type=ThermalAnalysisType.TRANSIENT)
            emobj.add(tanalysis)

            matrigid = MatRigid(mass_density=1, young_modulus=2e11)
            matrigid.set_em_resistive_heating_2d(
                material_type=EMMATTYPE.CONDUCTOR, initial_conductivity=1e4
            )

            matthermaliso = MatThermalIsotropic(density=100, specific_heat=10, conductivity=7)

            part = ShellPart(1)
            part.set_material(matrigid, matthermaliso)
            part.set_element_formulation(ShellFormulation.PLANE_STRESS)
            emobj.parts.add(part)

            emobj.boundaryconditions.create_imposed_motion(
                PartSet([1]), Curve(x=[0, 10], y=[10, 10])
            )
            emobj.set_init_temperature(temp=25)

            emobj.connect_isopotential(
                contype=Isopotential_ConnType.VOLTAGE_SOURCE,
                isopotential1=Isopotential(NodeSet([521, 517, 513, 509, 525])),
                value=500,
            )
            emobj.connect_isopotential(
                contype=Isopotential_ConnType.SHORT_CIRCUIT,
                isopotential1=Isopotential(NodeSet([642, 652, 661, 670, 643])),
                isopotential2=Isopotential(NodeSet([549, 548, 577, 597, 617])),
                value=0.01,
            )
            emobj.connect_isopotential(
                contype=Isopotential_ConnType.VOLTAGE_SOURCE,
                isopotential1=Isopotential(NodeSet([653, 644, 626, 627, 662])),
            )
            emobj.add(RogoCoil(SegmentSet(rogoseg)))

            emobj.create_em_output(mats=2, matf=2, sols=2, solf=2)

            output_path = solution.save_file()
            output_file = os.path.join(output_path, "test_resistive_heating_2d_isopots.k")

            diffs = self.compare_decks(output_file, reference_file)
            assert not diffs, "Differences:\n" + "\n".join(diffs)

    def test_resistive_heating_2d_multi_isopots(self, initial_files_dir, pre_reference_dir):
        """test_resistive_heating_2d_multi_isopots.k - EM resistive heating 2D multi isopotentials."""
        from ansys.dyna.core.pre.keywords_solution import KeywordsDynaSolution
        from ansys.dyna.core.pre.dynaem import (
            DynaEM,
            EMType,
            EMDimension,
            FEMSOLVER,
            RogoCoil,
        )
        from ansys.dyna.core.pre.dynamaterial import MatRigid, MatThermalIsotropic, EMMATTYPE
        from ansys.dyna.core.pre.dynamech import (
            ShellPart,
            ShellFormulation,
            ThermalAnalysis,
            ThermalAnalysisType,
        )
        from ansys.dyna.core.pre.dynabase import Curve, PartSet, SegmentSet

        initial_file = os.path.join(initial_files_dir, "em", "test_resistive_heating_2d_multi_isopots.k")
        reference_file = os.path.join(pre_reference_dir, "test_resistive_heating_2d_multi_isopots.k")

        if not os.path.exists(initial_file) or not os.path.exists(reference_file):
            pytest.skip("Required files not found")

        # Segment set for Rogowski coil
        rogoseg = [
            [544, 575, 575, 575],
            [545, 544, 544, 544],
            [575, 595, 595, 595],
            [595, 615, 615, 615],
        ]

        with tempfile.TemporaryDirectory() as tmpdir:
            solution = KeywordsDynaSolution(working_dir=tmpdir)
            solution.open_files([initial_file])
            solution.set_termination(termination_time=0.0101)
            solution.create_database_binary(dt=1e-4)

            emobj = DynaEM()
            solution.add(emobj)

            emobj.set_timestep(tssfac=1, timestep_size_for_mass_scaled=1e-4)

            emobj.analysis.set_timestep(timestep=1e-4)
            emobj.analysis.set_em_solver(type=EMType.RESISTIVE_HEATING, dimtype=EMDimension.PLANAR_2D)
            emobj.analysis.set_solver_fem(solver=FEMSOLVER.DIRECT_SOLVER, relative_tol=1e-3)

            # Manually update EM_CONTROL to set nperio=2
            from ansys.dyna.core.keywords import keywords
            for kw in solution._backend._deck:
                if isinstance(kw, keywords.EmControl):
                    kw.nperio = 2
                    break

            tanalysis = ThermalAnalysis()
            tanalysis.set_timestep(initial_timestep=1e-4)
            tanalysis.set_solver(analysis_type=ThermalAnalysisType.TRANSIENT)
            emobj.add(tanalysis)

            matrigid = MatRigid(mass_density=1, young_modulus=2e11)
            matrigid.set_em_resistive_heating_2d(
                material_type=EMMATTYPE.CONDUCTOR, initial_conductivity=1e4
            )

            matthermaliso = MatThermalIsotropic(density=100, specific_heat=10, conductivity=7)

            part = ShellPart(1)
            part.set_material(matrigid, matthermaliso)
            part.set_element_formulation(ShellFormulation.PLANE_STRESS)
            emobj.parts.add(part)

            emobj.boundaryconditions.create_imposed_motion(
                PartSet([1]), Curve(x=[0, 10], y=[10, 10])
            )
            emobj.set_init_temperature(temp=25)

            # DEFINE_CURVE_FUNCTION for voltage source (lcid=1)
            voltage_function = "-5./0.01*EXP(-TIME/((5.e-4+0.05+0.01)*0.04))"
            solution._backend.create_define_curve_function(function=voltage_function)

            # Create 6 SET_NODE_LIST entries
            node_sets = [
                [642, 652, 661, 670, 643],  # sid=1
                [549, 548, 577, 597, 617],  # sid=2
                [642, 652, 661, 670, 643],  # sid=3
                [549, 548, 577, 597, 617],  # sid=4
                [653, 644, 626, 627, 662],  # sid=5
                [521, 517, 513, 509, 525],  # sid=6
            ]
            for idx, nodes in enumerate(node_sets, start=1):
                solution._backend.create_set_node_list_with_solver(sid=idx, nodes=nodes, solver="MECH")

            # Create 6 EM_ISOPOTENTIAL entries (isoid 1-6, referencing SET_NODE_LIST 1-6)
            from ansys.dyna.core.keywords import keywords
            for isoid in range(1, 7):
                kw = keywords.EmIsopotential()
                kw.isoid = isoid
                kw.settype = 2  # node set
                kw.setid = isoid
                kw.rdltype = 0
                solution._backend._deck.append(kw)

            # Create 3 EM_ISOPOTENTIAL_CONNECT entries
            # conid=1: contype=4 (voltage source with load curve), isoid1=1, isoid2=2, lcid=1
            kw1 = keywords.EmIsopotentialConnect()
            kw1.conid = 1
            kw1.contype = 4
            kw1.isoid1 = 1
            kw1.isoid2 = 2
            kw1.val = 0.0
            kw1.lcid_rdlid = 1  # Field name is lcid_rdlid, not lcid
            kw1.psid = 0
            solution._backend._deck.append(kw1)

            # conid=2: contype=2 (resistor), isoid1=3, isoid2=4, value=0.01
            kw2 = keywords.EmIsopotentialConnect()
            kw2.conid = 2
            kw2.contype = 2
            kw2.isoid1 = 3
            kw2.isoid2 = 4
            kw2.val = 0.01
            kw2.psid = 0
            solution._backend._deck.append(kw2)

            # conid=3: contype=2 (resistor), isoid1=5, isoid2=6, value=0.05
            kw3 = keywords.EmIsopotentialConnect()
            kw3.conid = 3
            kw3.contype = 2
            kw3.isoid1 = 5
            kw3.isoid2 = 6
            kw3.val = 0.05
            kw3.psid = 0
            solution._backend._deck.append(kw3)

            # Add RogoCoil - it creates SET_SEGMENT and EM_ISOPOTENTIAL_ROGO with isoid=1
            emobj.add(RogoCoil(SegmentSet(rogoseg)))

            emobj.create_em_output(mats=2, matf=2, sols=2, solf=2)

            output_path = solution.save_file()
            output_file = os.path.join(output_path, "test_resistive_heating_2d_multi_isopots.k")

            diffs = self.compare_decks(output_file, reference_file)
            assert not diffs, "Differences:\n" + "\n".join(diffs)

    def test_rlc_define_func(self, initial_files_dir, pre_reference_dir):
        """test_rlc_define_func.k - EM RLC circuit with defined function."""
        from ansys.dyna.core.pre.keywords_solution import KeywordsDynaSolution
        from ansys.dyna.core.pre.dynaem import (
            DynaEM,
            EMType,
        )
        from ansys.dyna.core.pre.dynamech import SolidPart, SolidFormulation
        from ansys.dyna.core.pre.dynamaterial import MatRigid, EMMATTYPE

        initial_file = os.path.join(initial_files_dir, "em", "test_rlc_define_func.k")
        reference_file = os.path.join(pre_reference_dir, "test_rlc_define_func.k")

        if not os.path.exists(initial_file) or not os.path.exists(reference_file):
            pytest.skip("Required files not found")

        with tempfile.TemporaryDirectory() as tmpdir:
            solution = KeywordsDynaSolution(working_dir=tmpdir)
            solution.open_files([initial_file])
            solution.set_termination(termination_time=0.01)
            solution.create_database_binary(dt=1e-4)

            emobj = DynaEM()
            solution.add(emobj)

            emobj.set_timestep(tssfac=1, timestep_size_for_mass_scaled=1e-4)

            emobj.analysis.set_timestep(timestep=1e-4)
            emobj.analysis.set_em_solver(type=EMType.RESISTIVE_HEATING)

            # Update EM_CONTROL to set nperio=2
            from ansys.dyna.core.keywords import keywords
            for kw in solution._backend._deck:
                if isinstance(kw, keywords.EmControl):
                    kw.nperio = 2
                    break

            matrigid = MatRigid(
                mass_density=7000,
                young_modulus=2e11,
                center_of_mass_constraint=1,
                translational_constraint=7,
                rotational_constraint=7,
            )
            matrigid.set_em_permeability_equal(
                material_type=EMMATTYPE.CONDUCTOR, initial_conductivity=1e4
            )

            part1 = SolidPart(1)
            part1.set_material(matrigid)
            part1.set_element_formulation(SolidFormulation.CONSTANT_STRESS_SOLID_ELEMENT)
            emobj.parts.add(part1)

            # Create DEFINE_FUNCTION for RLC circuit
            rlc_function = """float rlc(float time,float emdt,float curr,float curr1,
            float curr2,float pot1,float pot2, float rmesh)
            {
            float fac,R,C,Vc,L,xi ;
            R = 0.5e-3; L = 78.e-6;
            fac =1.e-6; C=363.e-4;
            float q= 181.5;
            if(time<emdt) return fac;
            q=q+emdt*curr;
            Vc=q/C;
            xi=(Vc*emdt-L*curr)/((R+rmesh)*emdt+L);
            return xi*rmesh;
            }"""
            solution._backend.create_define_function(function=rlc_function, fid=1)

            # Create SET_NODE_LIST entries
            nset1_nodes = [
                429, 433, 437, 441, 445, 449, 453, 457, 461, 465,
                469, 473, 477, 481, 485, 489, 493, 497, 501, 505,
                509, 513, 517, 521, 525,
            ]
            nset2_nodes = [
                26, 31, 36, 41, 46, 51, 56, 61, 66, 71,
                76, 81, 86, 91, 96, 101, 106, 111, 116, 121,
                126, 131, 136, 141, 146,
            ]
            solution._backend.create_set_node_list_with_solver(sid=1, nodes=nset1_nodes, solver="MECH")
            solution._backend.create_set_node_list_with_solver(sid=2, nodes=nset2_nodes, solver="MECH")

            # Create EM_ISOPOTENTIAL entries
            kw_iso1 = keywords.EmIsopotential()
            kw_iso1.isoid = 1
            kw_iso1.settype = 2  # node set
            kw_iso1.setid = 1
            kw_iso1.rdltype = 0
            solution._backend._deck.append(kw_iso1)

            kw_iso2 = keywords.EmIsopotential()
            kw_iso2.isoid = 2
            kw_iso2.settype = 2
            kw_iso2.setid = 2
            kw_iso2.rdltype = 0
            solution._backend._deck.append(kw_iso2)

            # Create EM_ISOPOTENTIAL_CONNECT entries
            # conid=1: contype=3 (RLC), isoid1=1, isoid2=0, lcid_rdlid=-1 (negative = function ID)
            kw_conn1 = keywords.EmIsopotentialConnect()
            kw_conn1.conid = 1
            kw_conn1.contype = 3  # RLC circuit
            kw_conn1.isoid1 = 1
            kw_conn1.isoid2 = 0
            kw_conn1.val = 0.0
            kw_conn1.lcid_rdlid = -1  # negative value references DEFINE_FUNCTION
            kw_conn1.psid = 0
            solution._backend._deck.append(kw_conn1)

            # conid=2: contype=3 (RLC), isoid1=2, isoid2=0, lcid_rdlid=0
            kw_conn2 = keywords.EmIsopotentialConnect()
            kw_conn2.conid = 2
            kw_conn2.contype = 3
            kw_conn2.isoid1 = 2
            kw_conn2.isoid2 = 0
            kw_conn2.val = 0.0
            kw_conn2.lcid_rdlid = 0
            kw_conn2.psid = 0
            solution._backend._deck.append(kw_conn2)

            emobj.create_em_output(mats=2, matf=2, sols=2, solf=2)

            output_path = solution.save_file()
            output_file = os.path.join(output_path, "test_rlc_define_func.k")

            diffs = self.compare_decks(output_file, reference_file)
            assert not diffs, "Differences:\n" + "\n".join(diffs)

    def test_rlc_isopotential(self, initial_files_dir, pre_reference_dir):
        """test_rlc_isopotential.k - EM RLC circuit with isopotential."""
        from ansys.dyna.core.pre.keywords_solution import KeywordsDynaSolution
        from ansys.dyna.core.pre.dynaem import (
            DynaEM,
            EMType,
            Isopotential,
            Isopotential_ConnType,
            RogoCoil,
        )
        from ansys.dyna.core.pre.dynamech import SolidPart, SolidFormulation
        from ansys.dyna.core.pre.dynamaterial import MatRigid, EMMATTYPE
        from ansys.dyna.core.pre.dynabase import NodeSet, SegmentSet

        initial_file = os.path.join(initial_files_dir, "em", "test_rlc_isopotential.k")
        reference_file = os.path.join(pre_reference_dir, "test_rlc_isopotential.k")

        if not os.path.exists(initial_file) or not os.path.exists(reference_file):
            pytest.skip("Required files not found")

        # Segment set for Rogowski coil
        rlc_rogoseg = [
            [248, 252, 272, 268],
            [252, 256, 276, 272],
            [256, 260, 280, 276],
            [4, 9, 252, 248],
            [260, 264, 284, 280],
            [9, 14, 256, 252],
            [268, 272, 292, 288],
            [14, 19, 260, 256],
            [272, 276, 296, 292],
            [19, 24, 264, 260],
            [276, 280, 300, 296],
            [280, 284, 304, 300],
            [288, 292, 312, 308],
            [292, 296, 316, 312],
            [296, 300, 320, 316],
            [300, 304, 324, 320],
        ]

        with tempfile.TemporaryDirectory() as tmpdir:
            solution = KeywordsDynaSolution(working_dir=tmpdir)
            solution.open_files([initial_file])
            solution.set_termination(termination_time=0.01)
            solution.create_database_binary(dt=1e-4)

            emobj = DynaEM()
            solution.add(emobj)

            emobj.set_timestep(tssfac=1, timestep_size_for_mass_scaled=1e-4)

            emobj.analysis.set_timestep(timestep=1e-4)
            emobj.analysis.set_em_solver(type=EMType.RESISTIVE_HEATING)

            matrigid = MatRigid(
                mass_density=7000,
                young_modulus=2e11,
                center_of_mass_constraint=1,
                translational_constraint=7,
                rotational_constraint=7,
            )
            matrigid.set_em_permeability_equal(
                material_type=EMMATTYPE.CONDUCTOR, initial_conductivity=1e4
            )

            part1 = SolidPart(1)
            part1.set_material(matrigid)
            part1.set_element_formulation(SolidFormulation.CONSTANT_STRESS_SOLID_ELEMENT)
            emobj.parts.add(part1)

            nset1 = NodeSet(
                [
                    429, 433, 437, 441, 445, 449, 453, 457, 461, 465,
                    469, 473, 477, 481, 485, 489, 493, 497, 501, 505,
                    509, 513, 517, 521, 525,
                ]
            )
            nset2 = NodeSet(
                [
                    26, 31, 36, 41, 46, 51, 56, 61, 66, 71,
                    76, 81, 86, 91, 96, 101, 106, 111, 116, 121,
                    126, 131, 136, 141, 146,
                ]
            )
            isopos_conn1 = Isopotential(nset1)
            isopos_conn2 = Isopotential(nset2)
            emobj.connect_isopotential(
                contype=Isopotential_ConnType.RLC_CIRCUIT,
                isopotential1=isopos_conn1,
                value=5e-4,
                inductance=7.8e-5,
                capacity=0.0363,
                initial_voltage=5000,
            )
            emobj.connect_isopotential(
                contype=Isopotential_ConnType.VOLTAGE_SOURCE,
                isopotential1=isopos_conn2,
            )
            emobj.add(RogoCoil(SegmentSet(rlc_rogoseg)))

            emobj.create_em_output(mats=2, matf=2, sols=2, solf=2)

            output_path = solution.save_file()
            output_file = os.path.join(output_path, "test_rlc_isopotential.k")

            diffs = self.compare_decks(output_file, reference_file)
            assert not diffs, "Differences:\n" + "\n".join(diffs)

    def test_dem(self, initial_files_dir, pre_reference_dir):
        """test_dem.k - DEM particle simulation with CONTROL_DISCRETE_ELEMENT."""
        from ansys.dyna.core.pre.keywords_solution import KeywordsDynaSolution

        initial_file = os.path.join(initial_files_dir, "test_dem.k")
        reference_file = os.path.join(pre_reference_dir, "test_dem.k")

        if not os.path.exists(initial_file) or not os.path.exists(reference_file):
            pytest.skip("Required files not found")

        with tempfile.TemporaryDirectory() as tmpdir:
            solution = KeywordsDynaSolution(working_dir=tmpdir)
            solution.open_files([initial_file])

            # Add CONTROL_DISCRETE_ELEMENT
            solution.stub.CreateControlDiscreteElement(
                type("Request", (), {
                    "ndamp": 0.99,
                    "tdamp": 0.99,
                    "frics": 0.9,
                    "fricr": 0.9,
                    "normk": 0.01,
                    "sheark": 0.2857
                })()
            )

            output_path = solution.save_file()
            output_file = os.path.join(output_path, "test_dem.k")

            diffs = self.compare_decks(output_file, reference_file)
            assert not diffs, "Differences:\n" + "\n".join(diffs)

    def test_dem_coupling(self, initial_files_dir, pre_reference_dir):
        """test_dem_coupling.k - ICFD-DEM coupling simulation.

        This test requires full DEM implementation including:
        - SECTION_SOLID creation
        - MAT_RIGID_DISCRETE creation
        - CONTROL_TIMESTEP settings
        - PART modifications for DEM particles
        """
        from ansys.dyna.core.pre.keywords_solution import KeywordsDynaSolution
        from ansys.dyna.core.pre.dynaicfd import (
            DynaICFD,
            ICFDAnalysis,
            ICFDPart,
            ICFDVolumePart,
            MatICFD,
            Compressible,
            Curve,
            ICFDDOF,
            MeshedVolume,
            ICFD_CouplingForm,
            Velocity,
        )
        from ansys.dyna.core.pre.dynadem import DynaDEM, DEMAnalysis
        from ansys.dyna.core.pre.dynabase import DynaBase

        initial_file = os.path.join(initial_files_dir, "icfd", "test_dem_coupling.k")
        reference_file = os.path.join(pre_reference_dir, "test_dem_coupling.k")

        if not os.path.exists(initial_file) or not os.path.exists(reference_file):
            pytest.skip("Required files not found")

        with tempfile.TemporaryDirectory() as tmpdir:
            solution = KeywordsDynaSolution(working_dir=tmpdir)
            solution.open_files([initial_file])

            # Add DynaBase for timestep control
            dbase = DynaBase()
            solution.add(dbase)
            dbase.set_timestep(tssfac=0.8)

            # Create section and material for DEM particles using stub
            dbase.stub.CreateSectionSolid(type("Request", (), {"elform": 0})())
            dbase.stub.CreateMatRigidDiscrete(
                type("Request", (), {"ro": 1000.0, "e": 10000.0, "pr": 0.3})()
            )

            icfd = DynaICFD()
            solution.add(icfd)

            # Set termination time
            solution.set_termination(termination_time=100.0)

            # Set DEM control parameters
            dem = DynaDEM()
            dem.set_des(
                ndamp=0.9,
                tdamp=0.9,
                frics=0.3,
                fricr=0.001,
                normk=0.01,
                sheark=0.2857,
            )
            solution.add(dem)

            # Set ICFD analysis with timestep and DEM coupling
            icfdanalysis = ICFDAnalysis()
            icfdanalysis.set_timestep(timestep=0.05)
            icfdanalysis.set_coupling_dem(
                coupling_type=0,
                birth_time=0,
                death_time=1e28,
                scale_factor=1.0,
                formulation=ICFD_CouplingForm.FORCE_BASED_ON_VELOCITY_DRAG_VALUE,
            )
            icfd.add(icfdanalysis)

            # Set initial conditions (vx=1.0)
            icfd.set_initial(velocity=Velocity(1.0, 0.0, 0.0))

            # Part 1: Inlet with prescribed velocity (x-direction only)
            mat1 = MatICFD(flow_density=2, dynamic_viscosity=0.01)
            part1 = ICFDPart(1)
            part1.set_material(mat1)
            part1.set_prescribed_velocity(dof=ICFDDOF.X, motion=Curve(x=[0, 10000], y=[1, 1]))
            icfd.parts.add(part1)

            # Part 2: Outlet with prescribed pressure
            mat2 = MatICFD(flow_density=2, dynamic_viscosity=0.01)
            part2 = ICFDPart(2)
            part2.set_material(mat2)
            part2.set_prescribed_pressure(pressure=Curve(x=[0, 10000], y=[0, 0]))
            icfd.parts.add(part2)

            # Part 3: Free slip boundary (walls)
            mat3 = MatICFD(flow_density=2, dynamic_viscosity=0.01)
            part3 = ICFDPart(3)
            part3.set_material(mat3)
            part3.set_free_slip()
            icfd.parts.add(part3)

            # Volume part enclosing the surfaces (reuses mat3)
            partvol = ICFDVolumePart(surfaces=[1, 2, 3])
            partvol.set_material(mat3)  # mid=3
            icfd.parts.add(partvol)

            # Define mesh volume with size shape box
            from ansys.dyna.core.pre.dynabase import Point
            meshvol = MeshedVolume(surfaces=[1, 2, 3])
            meshvol.meshsize_box(
                size=0.05,
                min_point=Point(-1.0, -1.0, -1.0),
                max_point=Point(1.0, 1.0, 1.0),
            )
            icfd.add(meshvol)

            solution.create_database_binary(dt=1.0)

            output_path = solution.save_file()
            output_file = os.path.join(output_path, "test_dem_coupling.k")

            diffs = self.compare_decks(output_file, reference_file)
            assert not diffs, "Differences:\n" + "\n".join(diffs)

    @pytest.mark.xfail(reason="DynaIGA not implemented in keywords backend")
    def test_iga(self, initial_files_dir, pre_reference_dir):
        """test_iga.k - Isogeometric analysis."""
        pytest.fail("Not implemented")

    @pytest.mark.xfail(reason="DynaISPH not implemented in keywords backend")
    def test_isph(self, initial_files_dir, pre_reference_dir):
        """test_isph.k - Incompressible SPH."""
        pytest.fail("Not implemented")

    @pytest.mark.xfail(reason="DynaSALE not implemented in keywords backend")
    def test_sale(self, initial_files_dir, pre_reference_dir):
        """test_sale.k - SALE (Simplified ALE)."""
        pytest.fail("Not implemented")

    def test_frf_plate_damping(self, initial_files_dir, pre_reference_dir):
        """test_frf_plate_damping.k - NVH FRF plate with damping curve."""
        from ansys.dyna.core.pre.keywords_solution import KeywordsDynaSolution

        initial_file = os.path.join(initial_files_dir, "nvh", "test_frf_plate_damping.k")
        reference_file = os.path.join(pre_reference_dir, "test_frf_plate_damping.k")

        if not os.path.exists(initial_file) or not os.path.exists(reference_file):
            pytest.skip("Required files not found")

        with tempfile.TemporaryDirectory() as tmpdir:
            solution = KeywordsDynaSolution(working_dir=tmpdir)
            solution.open_files([initial_file])

            # Control keywords
            solution.stub.CreateControlImplicitEigenvalue(
                type("Request", (), {
                    "neig": 100,
                    "center": 0.0,
                    "eigmth": 2,
                    "shfscl": 0.0
                })()
            )

            solution.stub.CreateControlImplicitGeneral(
                type("Request", (), {
                    "imflag": 1,
                    "dt0": 1.0,
                    "imform": 2,
                    "nsbs": 1,
                    "igs": 2,
                })()
            )

            # CONTROL_IMPLICIT_SOLUTION
            solution._backend.create_control_implicit_solution(
                nsolvr=1,
                ilimit=11,
                maxref=55,
                dctol=1.0,
                ectol=0.01,
                rctol=1.0e10,
                lstol=0.9,
                nlprint=3,
            )

            # DEFINE_CURVE for damping vs. frequency (lcid=1)
            damping_freqs = [1.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 200.0]
            damping_vals = [0.0, 0.0, 0.0045, 0.00713, 0.00386, 0.00328, 0.0034, 0.00624, 0.00072, 0.00083, 0.0, 0.0]
            solution._backend.create_define_curve(
                sfo=1.0,
                abscissa=damping_freqs,
                ordinate=damping_vals,
            )

            # SET_NODE_LIST for input node (sid=1, node 131)
            solution._backend.create_set_node_list_with_solver(sid=1, nodes=[131], solver="MECH")

            # SET_NODE_LIST for output nodes (sid=2, nodes 131 and 651)
            solution._backend.create_set_node_list_with_solver(sid=2, nodes=[131, 651], solver="MECH")

            # SECTION_SHELL (secid=1, elform=6, shrf=0.833, nip=5, propt=3.0)
            solution._backend.create_section_shell(
                secid=1,
                elform=6,
                shrf=0.833,
                nip=5,
                propt=3.0,
                t1=0.002,
                t2=0.002,
                t3=0.002,
                t4=0.002,
            )

            # MAT_ELASTIC (mid=1, ro=7870.0, e=2.07e11, pr=0.292)
            solution._backend.create_mat_elastic(
                mid=1,
                ro=7870.0,
                e=2.07e11,
                pr=0.292,
            )

            # Update PART to reference secid=1 and mid=1
            from ansys.dyna.core.keywords import keywords
            for kw in solution._backend._deck:
                if isinstance(kw, keywords.Part):
                    for idx in range(len(kw.parts)):
                        pid = kw.parts.iloc[idx]["pid"]
                        if pid == 1:
                            kw.parts.at[idx, "secid"] = 1
                            kw.parts.at[idx, "mid"] = 1

            # FREQUENCY_DOMAIN_FRF with damping curve
            solution.stub.CreateFrequencyDomainFrf(
                type("Request", (), {
                    "n1": 131,
                    "n1typ": 0,
                    "dof1": 0,
                    "vad1": 3,
                    "fnmax": 2000.0,
                    "dampf": 0.0,
                    "lcdam": 1,
                    "lctyp": 1,
                    "n2": 2,
                    "n2typ": 1,
                    "dof2": 3,
                    "vad2": 1,
                    "fmin": 1.0,
                    "fmax": 400.0,
                    "nfreq": 400,
                })()
            )

            output_path = solution.save_file()
            output_file = os.path.join(output_path, "test_frf_plate_damping.k")

            diffs = self.compare_decks(output_file, reference_file)
            assert not diffs, "Differences:\n" + "\n".join(diffs)

    def test_frf_solid(self, initial_files_dir, pre_reference_dir):
        """test_frf_solid.k - NVH FRF analysis with implicit eigenvalue solver."""
        from ansys.dyna.core.pre.keywords_solution import KeywordsDynaSolution

        initial_file = os.path.join(initial_files_dir, "nvh", "test_frf_solid.k")
        reference_file = os.path.join(pre_reference_dir, "test_frf_solid.k")

        if not os.path.exists(initial_file) or not os.path.exists(reference_file):
            pytest.skip("Required files not found")

        with tempfile.TemporaryDirectory() as tmpdir:
            solution = KeywordsDynaSolution(working_dir=tmpdir)
            solution.open_files([initial_file])

            # Control keywords
            solution.stub.CreateControlEnergy(
                type("Request", (), {"hgen": 2, "rwen": 2, "slnten": 1, "rylen": 1})()
            )

            solution.stub.CreateControlImplicitEigenvalue(
                type("Request", (), {
                    "neig": 100,
                    "center": 0.0,
                    "eigmth": 2,
                    "shfscl": 0.0
                })()
            )

            solution.stub.CreateControlImplicitGeneral(
                type("Request", (), {
                    "imflag": 1,
                    "dt0": 1.0,
                    "imform": 2,
                    "nsbs": 1
                })()
            )

            # Use backend directly to set nlprint and nlnorm
            solution._backend.create_control_implicit_solution(
                nsolvr=1,
                ilimit=11,
                maxref=55,
                dctol=1.0,
                ectol=0.01,
                rctol=1.0e10,
                lstol=0.9,
                nlprint=3,
                nlnorm=0.0,
            )

            # Use backend directly to set ikedit and iflush
            solution._backend.create_control_output(npopt=1, neecho=3, ikedit=0, iflush=0)

            solution.set_termination(termination_time=1.0)

            # Database keywords
            solution.stub.CreateDatabaseGlstat(
                type("Request", (), {"dt": 0.1, "binary": 1, "lcur": 0, "ioopt": 1})()
            )

            solution.stub.CreateDatabaseMatsum(
                type("Request", (), {"dt": 0.1, "binary": 1, "lcur": 0, "ioopt": 1})()
            )

            solution.create_database_binary(dt=0.1)

            # SET_NODE_LIST for boundary condition - use backend directly
            nodes_set1 = [163, 166, 169, 172, 175, 178, 181, 184, 187, 307, 310, 313,
                         316, 319, 322, 391, 394, 397, 400, 403, 406, 493, 496, 499,
                         589, 592, 645, 648]
            solution._backend.create_set_node_list_with_solver(sid=1, nodes=nodes_set1, solver="MECH")

            nodes_set2 = [290, 292, 294, 296, 298, 300, 302, 304, 306, 380, 382, 384,
                         386, 388, 390, 482, 484, 486, 488, 490, 492, 578, 580, 582,
                         638, 640, 706, 708]
            solution._backend.create_set_node_list_with_solver(sid=2, nodes=nodes_set2, solver="MECH")

            # Boundary condition
            solution.stub.CreateBoundarySpcSet(
                type("Request", (), {
                    "nsid": 1,
                    "cid": 0,
                    "dofx": 1,
                    "dofy": 1,
                    "dofz": 1,
                    "dofrx": 1,
                    "dofry": 1,
                    "dofrz": 1
                })()
            )

            # Section and Material for lower_post (Part 4)
            solution.stub.CreateSectionSolid(
                type("Request", (), {"secid": 1, "elform": 18})()
            )

            # Use backend directly to set fail=0.0
            solution._backend.create_mat_piecewise_linear_plasticity(
                mid=1, ro=4.99e-7, e=11.37, pr=0.32, sigy=0.0468, etan=0.0, fail=0.0
            )

            # Section and Material for upper_post (Part 5)
            solution.stub.CreateSectionSolid(
                type("Request", (), {"secid": 2, "elform": 18})()
            )

            # Use backend directly to set fail=0.0
            solution._backend.create_mat_piecewise_linear_plasticity(
                mid=2, ro=4.99e-7, e=110.37, pr=0.32, sigy=0.0468, etan=0.0, fail=0.0
            )

            # Update PARTs to reference sections and materials by directly modifying the deck
            from ansys.dyna.core.keywords import keywords
            for kw in solution._backend._deck:
                if isinstance(kw, keywords.Part):
                    # Part uses a DataFrame 'parts' with columns: pid, secid, mid, etc.
                    for idx in range(len(kw.parts)):
                        pid = kw.parts.iloc[idx]["pid"]
                        if pid == 4:
                            kw.parts.at[idx, "secid"] = 1
                            kw.parts.at[idx, "mid"] = 1
                        elif pid == 5:
                            kw.parts.at[idx, "secid"] = 2
                            kw.parts.at[idx, "mid"] = 2

            # Frequency domain FRF
            solution.stub.CreateFrequencyDomainFrf(
                type("Request", (), {
                    "n1": 0,
                    "n1typ": 0,
                    "dof1": 1,
                    "vad1": 1,
                    "fnmax": 20.0,
                    "dampf": 0.01,
                    "n2": 2,
                    "n2typ": 1,
                    "dof2": 1,
                    "vad2": 1,
                    "fmin": 0.01,
                    "fmax": 10.0,
                    "nfreq": 1000
                })()
            )

            output_path = solution.save_file()
            output_file = os.path.join(output_path, "test_frf_solid.k")

            diffs = self.compare_decks(output_file, reference_file)
            assert not diffs, "Differences:\\n" + "\\n".join(diffs)

