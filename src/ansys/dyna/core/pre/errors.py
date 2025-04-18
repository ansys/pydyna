# Copyright (C) 2021 - 2025 ANSYS, Inc. and/or its affiliates.
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

"""Module containing PyDYNA-specific errors."""

SIGINT_TRACKER = []


LOCKFILE_MSG = """
Either another Ansys job with the same job name is already running in this
directory or the lock file has not been deleted from an abnormally
terminated Ansys run.

Disable this check by passing ``override=True``

"""


class VersionError(ValueError):
    """Provides the error raised when the kwserver is invalid."""

    def __init__(self, msg="Invalid kwserver version"):
        ValueError.__init__(self, msg)


class PydynaRuntimeError(RuntimeError):
    """Provides the error raised when PyDYNA passes an error."""

    pass


class PydynaInvalidRoutineError(RuntimeError):
    """Provides the error raised when MAPDL is in the wrong routine."""

    def __init__(self, msg=""):
        RuntimeError.__init__(self, msg)


class LockFileException(RuntimeError):
    """Provides the error raised when the lockfile has not been removed."""

    def __init__(self, msg=LOCKFILE_MSG):
        RuntimeError.__init__(self, msg)


class KwserverDidNotStart(RuntimeError):
    """Provides the error raised when the kwserver process does not start."""

    def __init__(self, msg=""):
        RuntimeError.__init__(self, msg)
