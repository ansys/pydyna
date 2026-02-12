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


class LockFileExceptionError(RuntimeError):
    """Provides the error raised when the lockfile has not been removed."""

    def __init__(self, msg=LOCKFILE_MSG):
        RuntimeError.__init__(self, msg)


class KwserverDidNotStartError(RuntimeError):
    """Provides the error raised when the kwserver process does not start."""

    def __init__(self, msg=""):
        RuntimeError.__init__(self, msg)


class ValidationError(Exception):
    """Base class for validation errors in PyDYNA."""

    def __init__(self, msg="", keyword=None, severity="error"):
        """Initialize a validation error.

        Parameters
        ----------
        msg : str
            Error message describing the validation failure.
        keyword : KeywordBase, optional
            The keyword that failed validation.
        severity : str
            Severity level: "error", "warning", or "info".
        """
        self.keyword = keyword
        self.severity = severity
        super().__init__(msg)


class RequiredFieldError(ValidationError):
    """Error raised when a required field is missing or None."""

    def __init__(self, keyword, field_name):
        """Initialize a required field error.

        Parameters
        ----------
        keyword : KeywordBase
            The keyword missing the required field.
        field_name : str
            Name of the required field.
        """
        kwd_type = type(keyword).__name__
        msg = f"{kwd_type} requires field '{field_name}' but it is None or missing"
        super().__init__(msg, keyword=keyword, severity="error")
        self.field_name = field_name


class DuplicateIDError(ValidationError):
    """Error raised when duplicate IDs are found."""

    def __init__(self, keyword_type, field_name, duplicate_values):
        """Initialize a duplicate ID error.

        Parameters
        ----------
        keyword_type : str
            Type of keyword with duplicates.
        field_name : str
            Name of the field with duplicate values.
        duplicate_values : list
            List of duplicate values found.
        """
        msg = f"Keywords of type '{keyword_type}' have duplicate {field_name} values: {duplicate_values}"
        super().__init__(msg, keyword=None, severity="error")
        self.keyword_type = keyword_type
        self.field_name = field_name
        self.duplicate_values = duplicate_values


class DuplicateKeywordError(ValidationError):
    """Error raised when a globally unique keyword appears more than once."""

    def __init__(self, keyword_type, subkeyword, count):
        """Initialize a duplicate keyword error.

        Parameters
        ----------
        keyword_type : str
            Main keyword type (e.g., "CONTROL").
        subkeyword : str
            Sub-keyword type (e.g., "TIMESTEP").
        count : int
            Number of times the keyword appears.
        """
        full_name = f"{keyword_type}_{subkeyword}" if subkeyword else keyword_type
        msg = f"Keyword '{full_name}' appears {count} times but should appear at most once"
        super().__init__(msg, keyword=None, severity="error")
        self.keyword_type = keyword_type
        self.subkeyword = subkeyword
        self.count = count
