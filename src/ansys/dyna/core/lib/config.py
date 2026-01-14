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

from contextlib import contextmanager

USE_LSPP_DEFAULTS = True
ENABLE_CSV_AUTODETECT = True
USE_LEGACY_FLOAT_FORMAT = False


def use_lspp_defaults():
    return USE_LSPP_DEFAULTS


def use_legacy_float_format():
    """Return whether legacy float formatting is enabled.

    When enabled, floats are formatted with explicit exponent format (e.g., 1.000000E8)
    instead of the default concise format (e.g., 1e+08).
    """
    return USE_LEGACY_FLOAT_FORMAT


def csv_autodetect_enabled():
    """Return whether CSV format auto-detection is enabled."""
    return ENABLE_CSV_AUTODETECT


@contextmanager
def disable_lspp_defaults():
    global USE_LSPP_DEFAULTS
    USE_LSPP_DEFAULTS = False
    try:
        yield
    finally:
        USE_LSPP_DEFAULTS = True


@contextmanager
def legacy_float_format():
    """Context manager to enable legacy float formatting.

    When enabled, floats are formatted with explicit exponent format (e.g., 1.000000E8)
    instead of the default concise format (e.g., 1e+08). This is useful for compatibility
    with reference files or legacy LS-DYNA input decks.

    Examples
    --------
    >>> from ansys.dyna.core.lib.config import legacy_float_format
    >>> with legacy_float_format():
    ...     output = str(deck)  # Floats formatted as 1.000000E8
    """
    global USE_LEGACY_FLOAT_FORMAT
    USE_LEGACY_FLOAT_FORMAT = True
    try:
        yield
    finally:
        USE_LEGACY_FLOAT_FORMAT = False


@contextmanager
def disable_csv_autodetect():
    """Context manager to temporarily disable CSV format auto-detection.

    By default, pydyna auto-detects comma-delimited (CSV) format in keyword card data.
    Use this context manager if CSV detection causes issues with specific files.

    Examples
    --------
    >>> from ansys.dyna.core.lib.config import disable_csv_autodetect
    >>> with disable_csv_autodetect():
    ...     deck.loads(content)  # CSV detection disabled
    """
    global ENABLE_CSV_AUTODETECT
    ENABLE_CSV_AUTODETECT = False
    try:
        yield
    finally:
        ENABLE_CSV_AUTODETECT = True
