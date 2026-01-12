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
Sample interactive client for DynaSolver.
=========================================

Commands are read from an optional file, or interactively, and sent to DYNA via gRPC
"""

#!/usr/bin/python3
import os
import sys

import ansys.dyna.core.solver as solver

#
hostname = "localhost"
port = "5000"


def get_from_k8s(service):
    """Get the port of the DYNA server service when running Kubernetes.
    locally
    """
    ip = "localhost"  # for local k8s cluster
    f = os.popen("kubectl get service %s" % service, "r")
    f.readline()
    p = f.readline()
    n1 = p.find(":")
    n2 = p.find("/")
    p = p[n1 + 1 : n2]
    return (ip, p)


def get_from_minikube(service):
    """Get the IP address and port of the DYNA server service when running
    under minikube locally.
    """
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


args = sys.argv[1:]
#
# Check for special command-line arg ``runfile <filename>`` to pull commands.
# from
#
if "runfile" in args:
    i = args.index("runfile")
    runfile = args[i + 1]
    args = args[:i] + args[i + 2 :]
else:
    runfile = None
#
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
#
# Special code here for testing on my system with minikube:
# if run with just "minikube" as the argument, figure out
# the correct IP address and port to use.  If there is a second
# argument, it is the name of the "server" service, which defaults to "server"
#
if hostname == "minikube":
    (hostname, port) = get_from_minikube(service)
    print("Using %s:%s" % (hostname, port))
#
# Similarly, if running under kubernetes locally, get the hostname
# and port to use
#
elif hostname == "k8s":
    (hostname, port) = get_from_k8s(service)
    print("Using %s:%s" % (hostname, port))

#
# Open gRPC connection to the server
#
dyna = solver.DynaSolver(hostname, port)
#
# Run commands from the runfile first, if there are any
#
if runfile:
    dyna.runfile(runfile)
#
# If the runfile didn't end with "quit" then process commands from the terminal
# until we get a "quit" command
#
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
