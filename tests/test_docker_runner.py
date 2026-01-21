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

"""Tests for Docker runner functionality."""

import logging
import os
import pytest

from ansys.dyna.core.run.docker_runner import DockerRunner
from ansys.dyna.core.run.options import MpiOption, Precision
from ansys.dyna.core.run.local_solver import run_dyna

logger = logging.getLogger(__name__)

# These tests should only run when Docker is available and the container image is present
docker_available = True
container_image = os.environ.get("PYDYNA_RUN_CONTAINER", None)

try:
    import docker
    client = docker.from_env()
    if container_image:
        client.images.get(container_image)
        logger.info(f"Docker image {container_image} found for testing")
    else:
        docker_available = False
        logger.warning("PYDYNA_RUN_CONTAINER not set - Docker tests will be skipped")
except Exception as e:
    docker_available = False
    logger.warning(f"Docker not available for testing: {e}")

pytestmark = pytest.mark.skipif(not docker_available or not container_image, 
                               reason="Docker or container image not available")

@pytest.mark.run
class TestDockerRunner:
    """Test suite for DockerRunner functionality."""

    def test_docker_runner_initialization(self):
        """Test basic DockerRunner initialization."""
        runner = DockerRunner(container=container_image)
        assert runner._name == container_image
        assert runner.mpi_option == MpiOption.SMP
        assert runner.precision == Precision.DOUBLE

    def test_docker_runner_with_options(self):
        """Test DockerRunner initialization with custom options."""
        runner = DockerRunner(
            container=container_image,
            mpi_option=MpiOption.SMP,
            precision=Precision.SINGLE,
            ncpu=4,
            memory=100
        )
        assert runner.mpi_option == MpiOption.SMP
        assert runner.precision == Precision.SINGLE
        assert runner.ncpu == 4
        assert runner.memory == 100

    def test_get_executable_name(self):
        """Test executable name generation for different configurations."""
        # Test SMP Double precision (default)
        runner = DockerRunner(container=container_image)
        executable = runner._get_executable_name()
        assert "ls-dyna_smp_s_R16_1_1_x64_centos79_ifort190_sse2" in executable

        # Test SMP Single precision
        runner = DockerRunner(
            container=container_image,
            mpi_option=MpiOption.SMP,
            precision=Precision.SINGLE
        )
        executable = runner._get_executable_name()
        assert executable.endswith("_s")

        # Test custom executable name
        runner = DockerRunner(
            container=container_image,
            executable_name="custom_executable"
        )
        executable = runner._get_executable_name()
        assert executable == "custom_executable"

    def test_set_input(self):
        """Test setting input file and working directory."""
        runner = DockerRunner(container=container_image)
        
        # Create a temporary directory for testing
        import tempfile
        with tempfile.TemporaryDirectory() as tmpdir:
            runner.set_input("test.k", tmpdir)
            assert runner._input_file == "test.k"
            assert runner._working_directory == os.path.abspath(tmpdir)

    def test_set_input_invalid_directory(self):
        """Test error handling for invalid working directory."""
        runner = DockerRunner(container=container_image)
        
        with pytest.raises(Exception, match="`working directory` is not a directory"):
            runner.set_input("test.k", "/non/existent/directory")


@pytest.mark.run
class TestDockerRunnerExecution:
    """Test suite for Docker runner execution (requires test files)."""

    def test_docker_runner_simple_execution(self, file_utils):
        """Test basic Docker runner execution with a simple input file."""
        input_file = file_utils.testfiles_folder / "run" / "i.k"
        if not input_file.exists():
            pytest.skip("Test input file not available")

        example_folder = str(input_file.parent.resolve())
        input_filename = input_file.name

        runner = DockerRunner(container=container_image)
        runner.set_input(input_filename, example_folder)
        
        try:
            result_dir = runner.run()
            assert result_dir == example_folder
            
            # Check that output files were created
            assert os.path.isfile(os.path.join(example_folder, "d3plot"))
            
        except Exception as e:
            logger.error(f"Docker runner execution failed: {e}")
            raise
        finally:
            # Clean up generated files
            generated_files = [f for f in os.listdir(example_folder) 
                             if not f.endswith(".k")]
            for file in generated_files:
                try:
                    os.remove(os.path.join(example_folder, file))
                except:
                    pass  # Ignore cleanup errors

    def test_docker_runner_case_option(self, file_utils):
        """Test Docker runner with CASE option."""
        input_file = file_utils.testfiles_folder / "run" / "case-keywords" / "projectile.k"
        if not input_file.exists():
            pytest.skip("Test input file not available")

        example_folder = str(input_file.parent.resolve())
        input_filename = input_file.name

        runner = DockerRunner(
            container=container_image,
            activate_case=True
        )
        runner.set_input(input_filename, example_folder)
        
        try:
            result_dir = runner.run()
            assert result_dir == example_folder
            
            # Check that output files from both cases were created
            d3plot_files = [f for f in os.listdir(example_folder) 
                          if f.endswith(".d3plot")]
            assert len(d3plot_files) > 0
            assert any("ZERO_VELOCITY" in f for f in d3plot_files)
            assert any("LOW_VELOCITY" in f for f in d3plot_files)
            
        except Exception as e:
            logger.error(f"Docker runner CASE execution failed: {e}")
            raise
        finally:
            # Clean up generated files
            generated_files = [f for f in os.listdir(example_folder) 
                             if not f.endswith(".k")]
            for file in generated_files:
                try:
                    os.remove(os.path.join(example_folder, file))
                except:
                    pass  # Ignore cleanup errors

    def test_docker_runner_specific_case_ids(self, file_utils):
        """Test Docker runner with specific case IDs."""
        input_file = file_utils.testfiles_folder / "run" / "case-keywords" / "projectile.k"
        if not input_file.exists():
            pytest.skip("Test input file not available")

        example_folder = str(input_file.parent.resolve())
        input_filename = input_file.name

        runner = DockerRunner(
            container=container_image,
            activate_case=True,
            case_ids=[1, 2]  # Assuming these are valid case IDs
        )
        runner.set_input(input_filename, example_folder)
        
        try:
            result_dir = runner.run()
            assert result_dir == example_folder
            
            # Check that output files were created
            output_files = [f for f in os.listdir(example_folder) 
                          if f.startswith("d3plot") or f.endswith(".d3plot")]
            assert len(output_files) > 0
            
        except Exception as e:
            logger.error(f"Docker runner case IDs execution failed: {e}")
            raise
        finally:
            # Clean up generated files  
            generated_files = [f for f in os.listdir(example_folder) 
                             if not f.endswith(".k")]
            for file in generated_files:
                try:
                    os.remove(os.path.join(example_folder, file))
                except:
                    pass  # Ignore cleanup errors

    def test_docker_runner_through_run_dyna(self, file_utils):
        """Test Docker runner through the run_dyna interface."""
        input_file = file_utils.testfiles_folder / "run" / "i.k"
        if not input_file.exists():
            pytest.skip("Test input file not available")

        example_folder = str(input_file.parent.resolve())
        input_filename = input_file.name

        try:
            wdir = run_dyna(
                input_filename,
                working_directory=example_folder,
                container=container_image
            )
            assert wdir == example_folder
            assert os.path.isfile(os.path.join(example_folder, "d3plot"))
            
        except Exception as e:
            logger.error(f"Docker runner via run_dyna failed: {e}")
            raise
        finally:
            # Clean up generated files
            generated_files = [f for f in os.listdir(example_folder) 
                             if not f.endswith(".k")]
            for file in generated_files:
                try:
                    os.remove(os.path.join(example_folder, file))
                except:
                    pass  # Ignore cleanup errors