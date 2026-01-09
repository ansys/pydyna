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
        skip_fields = {"included_from", "cards", "title", "heading", "options", "subkeywords", "its"}

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


    @pytest.mark.xfail(reason="DynaICFD not implemented in keywords backend")
    def test_cylinder_flow(self, initial_files_dir, pre_reference_dir):
        """test_cylinder_flow.k - ICFD cylinder flow."""
        pytest.fail("Not implemented")

    @pytest.mark.xfail(reason="DynaICFD not implemented in keywords backend")
    def test_dam_break(self, initial_files_dir, pre_reference_dir):
        """test_dam_break.k - ICFD dam break simulation."""
        pytest.fail("Not implemented")

    @pytest.mark.xfail(reason="DynaICFD not implemented in keywords backend")
    def test_driven_cavity(self, initial_files_dir, pre_reference_dir):
        """test_driven_cavity.k - ICFD driven cavity flow."""
        pytest.fail("Not implemented")

    @pytest.mark.xfail(reason="DynaICFD not implemented in keywords backend")
    def test_free_convection_flow(self, initial_files_dir, pre_reference_dir):
        """test_free_convection_flow.k - ICFD free convection."""
        pytest.fail("Not implemented")

    @pytest.mark.xfail(reason="DynaICFD not implemented in keywords backend")
    def test_internal_3d_flow(self, initial_files_dir, pre_reference_dir):
        """test_internal_3d_flow.k - ICFD 3D internal flow."""
        pytest.fail("Not implemented")

    @pytest.mark.xfail(reason="DynaICFD not implemented in keywords backend")
    def test_mesh_adaptivity(self, initial_files_dir, pre_reference_dir):
        """test_mesh_adaptivity.k - ICFD mesh adaptivity."""
        pytest.fail("Not implemented")

    @pytest.mark.xfail(reason="DynaICFD not implemented in keywords backend")
    def test_mesh_morphing(self, initial_files_dir, pre_reference_dir):
        """test_mesh_morphing.k - ICFD mesh morphing."""
        pytest.fail("Not implemented")

    @pytest.mark.xfail(reason="DynaICFD not implemented in keywords backend")
    def test_mesh_size(self, initial_files_dir, pre_reference_dir):
        """test_mesh_size.k - ICFD mesh size control."""
        pytest.fail("Not implemented")

    @pytest.mark.xfail(reason="DynaICFD not implemented in keywords backend")
    def test_plate_flow(self, initial_files_dir, pre_reference_dir):
        """test_plate_flow.k - ICFD plate flow."""
        pytest.fail("Not implemented")

    @pytest.mark.xfail(reason="DynaICFD not implemented in keywords backend")
    def test_sloshing(self, initial_files_dir, pre_reference_dir):
        """test_sloshing.k - ICFD sloshing simulation."""
        pytest.fail("Not implemented")

    @pytest.mark.xfail(reason="DynaICFD not implemented in keywords backend")
    def test_thermal_flow(self, initial_files_dir, pre_reference_dir):
        """test_thermal_flow.k - ICFD thermal flow."""
        pytest.fail("Not implemented")

    @pytest.mark.xfail(reason="DynaICFD FSI not implemented in keywords backend")
    def test_strong_fsi(self, initial_files_dir, pre_reference_dir):
        """test_strong_fsi.k - Strong fluid-structure interaction."""
        pytest.fail("Not implemented")

    @pytest.mark.xfail(reason="DynaICFD FSI not implemented in keywords backend")
    def test_weak_fsi(self, initial_files_dir, pre_reference_dir):
        """test_weak_fsi.k - Weak fluid-structure interaction."""
        pytest.fail("Not implemented")

    @pytest.mark.xfail(reason="DynaICFD imposed move not implemented in keywords backend")
    def test_imposed_move(self, initial_files_dir, pre_reference_dir):
        """test_imposed_move.k - ICFD imposed movement."""
        pytest.fail("Not implemented")

    @pytest.mark.xfail(reason="DynaEM not implemented in keywords backend")
    def test_railgun(self, initial_files_dir, pre_reference_dir):
        """test_railgun.k - EM railgun simulation."""
        pytest.fail("Not implemented")

    @pytest.mark.xfail(reason="DynaEM not implemented in keywords backend")
    def test_resistive_heating(self, initial_files_dir, pre_reference_dir):
        """test_resistive_heating.k - EM resistive heating 3D."""
        pytest.fail("Not implemented")

    @pytest.mark.xfail(reason="DynaEM not implemented in keywords backend")
    def test_resistive_heating_2d(self, initial_files_dir, pre_reference_dir):
        """test_resistive_heating_2d.k - EM resistive heating 2D."""
        pytest.fail("Not implemented")

    @pytest.mark.xfail(reason="DynaEM not implemented in keywords backend")
    def test_resistive_heating_2d_isopots(self, initial_files_dir, pre_reference_dir):
        """test_resistive_heating_2d_isopots.k - EM resistive heating 2D with isopotentials."""
        pytest.fail("Not implemented")

    @pytest.mark.xfail(reason="DynaEM not implemented in keywords backend")
    def test_resistive_heating_2d_multi_isopots(self, initial_files_dir, pre_reference_dir):
        """test_resistive_heating_2d_multi_isopots.k - EM resistive heating 2D multi isopotentials."""
        pytest.fail("Not implemented")

    @pytest.mark.xfail(reason="DynaEM not implemented in keywords backend")
    def test_rlc_define_func(self, initial_files_dir, pre_reference_dir):
        """test_rlc_define_func.k - EM RLC circuit with defined function."""
        pytest.fail("Not implemented")

    @pytest.mark.xfail(reason="DynaEM not implemented in keywords backend")
    def test_rlc_isopotential(self, initial_files_dir, pre_reference_dir):
        """test_rlc_isopotential.k - EM RLC circuit with isopotential."""
        pytest.fail("Not implemented")

    @pytest.mark.xfail(reason="DynaDEM not implemented in keywords backend")
    def test_dem(self, initial_files_dir, pre_reference_dir):
        """test_dem.k - DEM particle simulation."""
        pytest.fail("Not implemented")

    @pytest.mark.xfail(reason="DynaDEM not implemented in keywords backend")
    def test_dem_coupling(self, initial_files_dir, pre_reference_dir):
        """test_dem_coupling.k - DEM-FEM coupling."""
        pytest.fail("Not implemented")

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

    @pytest.mark.xfail(reason="DynaNVH not implemented in keywords backend")
    def test_frf_plate_damping(self, initial_files_dir, pre_reference_dir):
        """test_frf_plate_damping.k - NVH FRF plate with damping."""
        pytest.fail("Not implemented")

    @pytest.mark.xfail(reason="DynaNVH not implemented in keywords backend")
    def test_frf_solid(self, initial_files_dir, pre_reference_dir):
        """test_frf_solid.k - NVH FRF solid."""
        pytest.fail("Not implemented")

