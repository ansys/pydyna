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

"""Tests for *SENSOR_CONTROL."""

import warnings

import pytest

from ansys.dyna.core import Deck
from ansys.dyna.core.keywords import keywords


@pytest.mark.keywords
def test_sensor_control_eleset_estyp():
    """ESTYP (string) in column 6 of card 1 must be read without warnings."""
    deck = Deck()
    with warnings.catch_warnings():
        warnings.simplefilter("error")
        deck.loads(
            "*KEYWORD\n*SENSOR_CONTROL\n"
            "         1ELESET         10800         0         0SOLID\n"
            "On                 1\n*END\n"
        )
    kw = deck.keywords[0]
    assert isinstance(kw, keywords.SensorControl)
    assert kw.cntlid == 1
    assert kw.type == "ELESET"
    assert kw.typeid == 10800
    assert kw.defcv == "SOLID"
    assert kw.initstt == "On"
    assert kw.swit1 == 1


@pytest.mark.keywords
def test_sensor_control_curve_defcv():
    kw = keywords.SensorControl(cntlid=2, type="CURVE", typeid=5, timeoff=1, defcv="LASTSTEP", initstt="OFF")
    kw2 = keywords.SensorControl()
    kw2.loads(kw.write())
    assert kw2.defcv == "LASTSTEP"
    assert kw2.timeoff == 1
