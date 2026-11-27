# Copyright (C) 2023 - 2026 ANSYS, Inc. and/or its affiliates.
# SPDX-License-Identifier: MIT

"""Tests for *SENSOR_CONTROL card variants (issue #1367)."""

import warnings

from ansys.dyna.core.keywords.keyword_classes.auto.sensor.sensor_control import SensorControl
from ansys.dyna.core.lib.deck import Deck


def _field(value) -> str:
    return f"{'' if value is None else value:>10}"


def _eleset_keyword(nrep: int = 0) -> str:
    """The reporter's ELESET control: card 1c, then the switch card."""
    control = (
        _field(1)
        + _field("ELESET")
        + _field(10800)
        + _field(0)
        + _field(nrep)
        + _field("SOLID")
    )
    switch = _field("On") + _field(1)
    return f"*SENSOR_CONTROL\n{control}\n{switch}\n"


def _curve_keyword() -> str:
    control = (
        _field(4)
        + _field("CURVE")
        + _field(20)
        + _field(1)
        + _field(2)
        + _field("LASTSTEP")
    )
    switch = _field("Off") + _field(9)
    return f"*SENSOR_CONTROL\n{control}\n{switch}\n"


def _airbag_keyword() -> str:
    # SIDR sits in column 7. Columns 4 and 6 are blank on card 1a.
    control = (
        _field(3)
        + _field("AIRBAG")
        + _field(9)
        + _field("")
        + _field(0)
        + _field("")
        + _field(2)
    )
    switch = _field("On") + _field(6)
    return f"*SENSOR_CONTROL\n{control}\n{switch}\n"


def _load(text: str) -> SensorControl:
    with warnings.catch_warnings():
        warnings.simplefilter("error")
        deck = Deck()
        deck.loads(text)
    found = list(deck.get_kwds_by_type("SENSOR"))
    assert len(found) == 1
    return found[0]


class TestSensorControl:
    def test_read_eleset_without_warning(self):
        control = _load(_eleset_keyword(nrep=5))
        assert len(control.sets) == 1
        item = control.sets[0]
        assert item.type == "ELESET"
        assert item.cntlid == 1
        assert item.typeid == 10800
        assert item.idiscl == 0
        assert item.nrep == 5
        assert item.estyp == "SOLID"
        assert item.swit1 == 1
        assert control.estyp == "SOLID"

    def test_read_curve_variant(self):
        item = _load(_curve_keyword()).sets[0]
        assert item.type == "CURVE"
        assert item.cntlid == 4
        assert item.typeid == 20
        assert item.timeoff == 1
        assert item.nrep == 2
        assert item.defcv == "LASTSTEP"
        assert item.swit1 == 9

    def test_read_default_variant_keeps_sidr(self):
        item = _load(_airbag_keyword()).sets[0]
        assert item.type == "AIRBAG"
        assert item.typeid == 9
        assert item.sidr == 2
        assert item.timeoff == 0
        assert item.estyp is None

    def test_two_controls_in_one_keyword(self):
        eleset_body = _eleset_keyword().split("\n", 1)[1]
        airbag_body = _airbag_keyword().split("\n", 1)[1]
        control = _load("*SENSOR_CONTROL\n" + eleset_body + airbag_body)
        assert len(control.sets) == 2
        assert control.sets[0].type == "ELESET"
        assert control.sets[0].estyp == "SOLID"
        assert control.sets[1].type == "AIRBAG"
        assert control.sets[1].sidr == 2
        again = SensorControl().loads(control.write())
        assert len(again.sets) == 2
        assert again.sets[0].estyp == "SOLID"
        assert again.sets[1].sidr == 2

    def test_round_trip_and_type_change_keeps_shared_fields(self):
        control = _load(_eleset_keyword(nrep=5))
        control.type = "CURVE"
        control.timeoff = 1
        control.defcv = "LASTSTEP"
        assert control.cntlid == 1
        assert control.typeid == 10800
        assert control.nrep == 5

        written = control.write()
        again = SensorControl().loads(written)
        assert again.type == "CURVE"
        assert again.cntlid == 1
        assert again.typeid == 10800
        assert again.nrep == 5
        assert again.timeoff == 1
        assert again.defcv == "LASTSTEP"
        assert "ELESET" not in written
        assert "SOLID" not in written

    def test_construct_eleset(self):
        control = SensorControl()
        control.type = "ELESET"
        control.cntlid = 1
        control.typeid = 10800
        control.idiscl = 1
        control.estyp = "SOLID"
        control.swit1 = 1
        again = SensorControl().loads(control.write())
        assert again.type == "ELESET"
        assert again.estyp == "SOLID"
        assert again.idiscl == 1
        assert again.sidr is None
