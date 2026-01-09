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
