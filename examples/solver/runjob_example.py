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
DynaSolver example
===================

Sample use of DynaSolver to run a small job in minikube
"""

#!/usr/bin/python3
# import logging
import os
import time

import ansys.dyna.core.solver as solver


def get_from_minikube():
    """Get the IP address and port of the DYNA server service when running
    under minikube locally
    """
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


#
# Example of chaining some basic commands together to make "simple" commands
#
def start_job(nproc, fname, cmdline):
    (hostname, port) = get_from_minikube()
    dyna = solver.DynaSolver(hostname, port)
    dyna.push(fname)
    dyna.start(nproc)
    time.sleep(1.0)  # let dyna get going?  Shouldn't the pipe just hang?
    dyna.run(cmdline)
    return dyna


#
##############################################################################
#
# start a 3 processor job with an input I have
#
dyna = start_job(3, "hemi.k", "i=hemi.k jobid=x")
print("File uploaded and job started")
#
# Let it run until cycle 2600
#
dyna.resume(cycle=2600)
#
# And watch it until it gets there
#
while 1:
    time.sleep(1.0)
    try:
        (c, t) = dyna.time()
    except solver.RunningError as err:
        print(err)
    else:
        print("cycle=%d, time=%.10e" % (c, t))
        if c >= 2600:
            break
#
# Get some information about a node
#
(c, t) = dyna.time()
(x, v) = dyna.node(569)
print("At cycle=%d and time=%f" % (c, t))
print("Node 569 has Y position %f and Y velocity %f" % (x[1], v[1]))
#
# wait a moment, then get some switch 2 output
#
dyna.resume()
time.sleep(1.0)
s = dyna.switch("sw2.")
print("Output from switch sw2.")
print(s)
#
# Now just watch the job until it is done
#
getnode = 1
while 1:
    time.sleep(1.0)
    try:
        (c, t) = dyna.time()
    except solver.RunningError as err:
        print(err)
        break
    else:
        print("cycle=%d, time=%.10e" % (c, t))
        if getnode and (c > 10000):
            (x, v) = dyna.node(569)
            print("Node 569 has Y position %f and Y velocity %f" % (x[1], v[1]))
            getnode = 0
#
print("Job Complete")
