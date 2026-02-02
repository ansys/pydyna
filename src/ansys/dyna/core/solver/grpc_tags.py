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

#
# Tags for requests to DYNA
#
# The format here kind of sucks, but you can blame PEP8/flake8 for
# that because it INSISTS on having EXACTLY 1 space before and after
# the = signs, so I can't line them up unless I make all the tag names
# the same number of characters long.  This would be much more readable:
#
# SWITCH = 1
# RUN    = 2
# CWD    = 3
# PAUSE  = 4
#
# But no, that is not PEP8 compliant.
#
# At least it lets me line up my comments, provided they have at least TWO
# spaces before the #.
#
"""Module defining gRPC tags for communication with DYNA solver."""

SWITCH = 1  # sense switch
RUN = 2  # send command line arguments & begin execution
CWD = 3  # server sends to DYNA to find its working directory
PAUSE = 4  # pause until told to resume
TIME = 5  # return simulation time
RESUME = 6  # resume (until optional cycle/time)
REQUIRES_SYNC = 1000  # values above here require MPP synchorinzation
NODE = 1001  # return (pos, vel) for a node
SETLC = 1002  # set the value of a load curve to a supplied constant
#
# Possible return codes:
#
ACK = 0  # Success
NOTRUNNING = 1  # Dyna is not running (used by server, not DYNA)
RUNNING = 2  # Dyna is already running (used by server, not DYNA)
NOTFOUND = 3  # requested item not found
