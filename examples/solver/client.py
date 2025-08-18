# Copyright (C) 2023 - 2025 ANSYS, Inc. and/or its affiliates.
# SPDX-License-Identifier: MIT
#
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

# !/usr/bin/python3

# %% [markdown]
# # LS-DYNA Solver Client Example
#
# This notebook demonstrates how to connect to and control an LS-DYNA solver instance using the PyDyna solver API. The workflow includes handling different deployment environments (local, Kubernetes, Minikube), connecting to the solver, running commands from a file or interactively, and managing the solver session. Each step is explained with theoretical background for educational use.
#
# **Background:**
# The PyDyna solver API provides a Pythonic interface to LS-DYNA's gRPC server, allowing users to automate simulation workflows, manage remote or containerized solver instances, and integrate with cloud or cluster environments. This flexibility is essential for modern CAE workflows and high-throughput simulation.

# %% [markdown]
# ## 1. Import Required Modules
# Import standard Python modules and the PyDyna solver API.
#
# The solver API enables programmatic control of LS-DYNA, while standard modules are used for system interaction and command-line argument parsing.

# %%
import os
import sys

import ansys.dyna.core.solver as solver

# %% [markdown]
# ## 2. Define Hostname and Port
# Set the default hostname and port for the LS-DYNA solver service.
#
# These can be overridden by command-line arguments or dynamically determined for Kubernetes/Minikube deployments.

# %%
hostname = "localhost"
port = "5000"

# %% [markdown]
# ## 3. Helper Functions for Kubernetes and Minikube
# Define functions to retrieve the IP address and port of the LS-DYNA server when running in Kubernetes or Minikube environments.
#
# These functions use `kubectl` and `minikube` commands to query the service endpoints, enabling seamless integration with cloud-native deployments.


# %%
def get_from_k8s(service):
    """Get the port of the DYNA server service when running Kubernetes locally."""
    ip = "localhost"  # for local k8s cluster
    f = os.popen("kubectl get service %s" % service, "r")
    f.readline()
    p = f.readline()
    n1 = p.find(":")
    n2 = p.find("/")
    p = p[n1 + 1 : n2]
    return (ip, p)


def get_from_minikube(service):
    """Get the IP address and port of the DYNA server service when running under minikube locally."""
    f = os.popen("minikube ip", "r")
    ip = f.readline().strip()
    f.close
    f = os.popen("kubectl get svc %s" % service, "r")
    f.readline()
    p = f.readline()
    n1 = p.find(":")
    n2 = p.find("/")
    p = p[n1 + 1 : n2]
    return (ip, p)


# %% [markdown]
# ## 4. Parse Command-Line Arguments
# Parse command-line arguments to determine the hostname, port, and optional runfile for batch command execution.
#
# This allows the client to be used flexibly in different environments and workflows, including batch and interactive modes.

# %%
args = sys.argv[1:]
if "runfile" in args:
    i = args.index("runfile")
    runfile = args[i + 1]
    args = args[:i] + args[i + 2 :]
else:
    runfile = None
try:
    hostname = args[0]
except IndexError:
    hostname = "localhost"
try:
    port = args[1]
    service = port
except IndexError:
    port = "5000"
    service = "server"

# %% [markdown]
# ## 5. Handle Kubernetes and Minikube Environments
# Dynamically determine the correct hostname and port if running under Kubernetes or Minikube.
#
# This enables the client to connect to LS-DYNA services deployed in cloud-native environments without manual configuration.

# %%
if hostname == "minikube":
    (hostname, port) = get_from_minikube(service)
    print(f"Using {hostname}:{port}")
elif hostname == "k8s":
    (hostname, port) = get_from_k8s(service)
    print(f"Using {hostname}:{port}")

# %% [markdown]
# ## 6. Connect to the LS-DYNA Solver
# Open a gRPC connection to the LS-DYNA solver using the specified hostname and port.
#
# This connection allows you to send commands, upload files, and control the simulation remotely.

# %%
dyna = solver.DynaSolver(hostname, port)

# %% [markdown]
# ## 7. Run Commands from File or Interactively
# If a runfile is specified, execute commands from the file. Otherwise, enter an interactive loop to send commands to the solver.
#
# This dual-mode operation supports both automated batch workflows and manual interactive sessions.

# %%
if runfile:
    dyna.runfile(runfile)

while 1:
    # cmdin = input("> ").rstrip()
    # cannot use input() builtin function in Sphinx-Gallery examples
    cmdin = "quit"
    try:
        dyna.send(cmdin)
        if cmdin == "quit":
            sys.exit(0)
    except solver.RunningError as err:
        print(err)

# %% [markdown]
# ## 8. Conclusion
#
# This notebook has demonstrated how to connect to and control an LS-DYNA solver instance using the PyDyna solver API. The workflow included handling different deployment environments, connecting to the solver, running commands from a file or interactively, and managing the solver session. This approach can be adapted for cloud, cluster, or local deployments, providing a flexible and scriptable interface for advanced simulation workflows.
