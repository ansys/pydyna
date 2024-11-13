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

import os
import sys

import docker

from ansys.dyna.core.run.base_runner import BaseRunner
from ansys.dyna.core.run.options import MpiOption, Precision


class DockerRunner(BaseRunner):
    """Docker implementation to Run LS-DYNA. Tested with a custom exutable
    and when LS-DYNA is installed as part of the unified Ansys installation
    """

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._client: docker.client.DockerClient = docker.from_env()
        self.__ensure_image(kwargs["container"])
        self._container_env = kwargs.get("container_env", dict())
        self._stream = kwargs.get("stream", True)

    def __ensure_image(self, name):
        self._name = name
        try:
            _ = self._client.images.get(name)
        except:
            raise Exception("Exception in DockerRunner, container image {name} not found!")

    def set_input(self, input_file: str, working_directory: str) -> None:
        self._input_file = input_file
        self._working_directory = os.path.abspath(working_directory)
        if not os.path.isdir(self._working_directory):
            raise Exception("`working directory` is not a directory")

    def _get_solver_option(self) -> str:
        solver_option = {
            (MpiOption.SMP, Precision.SINGLE): "SP",
            (MpiOption.SMP, Precision.DOUBLE): "DP",
            (MpiOption.MPP_INTEL_MPI, Precision.SINGLE): "SP_MPP",
            (MpiOption.MPP_INTEL_MPI, Precision.DOUBLE): "DP_MPP",
        }[(self.mpi_option, self.precision)]
        return solver_option

    def run(self) -> None:
        env = {
            "DYNA_OPTION": self._get_solver_option(),
            "DYNA_NCPU": f"{self.ncpu}",
            "DYNA_ARGS": f"i={self._input_file} memory={self.get_memory_string()}",
        }
        env.update(self._container_env)
        volumes = [f"{self._working_directory}:/run"]
        if self._stream:
            cont: docker.models.containers.Container = self._client.containers.run(
                self._name, environment=env, volumes=volumes, detach=True
            )
            logs: docker.types.daemon.CancellableStream = cont.logs(stream=True)
            while cont.status != "exited":
                try:
                    log = next(logs)
                except StopIteration:
                    break
                sys.stdout.write(log.decode("utf-8"))
        else:
            result = self._client.containers.run(self._name, environment=env, volumes=volumes)
            return result
