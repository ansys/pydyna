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
# # LS-DYNA Run Job Example (Minikube)
#
# This notebook demonstrates how to launch, monitor, and interact with an LS-DYNA simulation job using the PyDyna solver API in a Minikube (Kubernetes) environment. The workflow includes connecting to the solver, uploading input files, starting and resuming jobs, monitoring progress, and retrieving results. Each step is explained with theoretical background for educational use.
#
# **Background:**
# In high-performance computing and cloud environments, it is common to automate simulation job submission, monitoring, and data retrieval. The PyDyna solver API enables this by providing a Python interface to LS-DYNA's gRPC server, supporting both batch and interactive workflows.

# %% [markdown]
# ## 1. Import Required Modules
# Import standard Python modules and the PyDyna solver API.
#
# The solver API enables programmatic control of LS-DYNA, while standard modules are used for system interaction and timing.

# %%
import os
import time

import ansys.dyna.core.solver as solver

# %% [markdown]
# ## 2. Helper Function for Minikube
# Define a function to retrieve the IP address and port of the LS-DYNA server when running in a Minikube environment.
#
# This function uses `minikube` and `kubectl` commands to query the service endpoint, enabling seamless integration with cloud-native deployments.


# %%
def get_from_minikube():
    """Get the IP address and port of the DYNA server service when running under minikube locally."""
    f = os.popen("minikube ip", "r")
    ip = f.readline().strip()
    f.close
    f = os.popen("kubectl get svc server", "r")
    f.readline()
    p = f.readline()
    n1 = p.find(":")
    n2 = p.find("/")
    p = p[n1 + 1 : n2]
    return (ip, p)


# %% [markdown]
# ## 3. Start and Run a Simulation Job
# Define a function to chain together basic commands for launching a simulation job.
#
# This function connects to the solver, uploads the input file, starts the job with the specified number of processors, and runs the simulation with the given command line.


# %%
def start_job(nproc, fname, cmdline):
    (hostname, port) = get_from_minikube()
    dyna = solver.DynaSolver(hostname, port)
    dyna.push(fname)
    dyna.start(nproc)
    time.sleep(1.0)  # let dyna get going
    dyna.run(cmdline)
    return dyna


# %% [markdown]
# ## 4. Launch and Monitor the Simulation
# Start a simulation job, monitor its progress, and interactively retrieve results.
#
# The example demonstrates how to resume the job to a specific cycle, monitor cycles and time, query node data, and retrieve switch output. This is typical in automated or remote simulation workflows.

# %%
dyna = start_job(3, "hemi.k", "i=hemi.k jobid=x")
print("File uploaded and job started")

dyna.resume(cycle=2600)

while 1:
    time.sleep(1.0)
    try:
        (c, t) = dyna.time()
    except solver.RunningError as err:
        print(err)
    else:
        print(f"cycle={c}, time={t:.10e}")
        if c >= 2600:
            break

(c, t) = dyna.time()
(x, v) = dyna.node(569)
print(f"At cycle={c} and time={t}")
print(f"Node 569 has Y position {x[1]} and Y velocity {v[1]}")

dyna.resume()
time.sleep(1.0)
s = dyna.switch("sw2.")
print("Output from switch sw2.")
print(s)

getnode = 1
while 1:
    time.sleep(1.0)
    try:
        (c, t) = dyna.time()
    except solver.RunningError as err:
        print(err)
        break
    else:
        print(f"cycle={c}, time={t:.10e}")
        if getnode and (c > 10000):
            (x, v) = dyna.node(569)
            print(f"Node 569 has Y position {x[1]} and Y velocity {v[1]}")
            getnode = 0
print("Job Complete")

# %% [markdown]
# ## 5. Conclusion
#
# This notebook has demonstrated how to launch, monitor, and interact with an LS-DYNA simulation job using the PyDyna solver API in a Minikube environment. The workflow included connecting to the solver, uploading input files, starting and resuming jobs, monitoring progress, and retrieving results. This approach can be adapted for other cloud, cluster, or local deployments, providing a flexible and scriptable interface for advanced simulation workflows.
