# Copyright (C) 2021 - 2024 ANSYS, Inc. and/or its affiliates.
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

"""Test the card set feature."""

import io

from ansys.dyna.core.lib.card_set import CardSet
from ansys.dyna.core.lib.format_type import format_type
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.keywords.keyword_classes.auto.boundary.initial_strain_shell import InitialStrainShellCardSet
from ansys.dyna.core.keywords.keyword_classes.auto.boundary.initial_stress_shell import (
    InitialStressShellCardSet,
    InitialStressShellThicknessLargeCardSet,
)

import pytest

class Parent(KeywordBase):
    """Mock keyword to use as the parent for the card sets under test."""

    keyword = "FOO"
    subkeyword = "BAR"
    nhisv = 4
    nplane = 1
    nthick = 1
    large = 0

PARENT_REF_STRING = """*FOO_BAR
$#       t     sigxx     sigyy     sigzz     sigxy     sigyz     sigzx       eps
       2.0       0.0       0.0       0.0       0.0       0.0       0.0       2.0
$#    hisv      hisv      hisv      hisv
       1.0       2.0       3.0          """

def _to_string(cards):
    s = io.StringIO()
    cards.write(s, format_type.standard)
    return s.getvalue()

@pytest.mark.keywords
def test_initial_strain_shell_card_set():
    cs = InitialStrainShellCardSet(eid=1, nplane=1, nthick=5, parent=None, keyword=None)
    assert cs.eid == 1
    assert cs.nplane == 1
    assert cs.nthick == 5
    assert cs.large == 0
    assert 5 == cs._cards[1]._length_func()
    assert len(cs.strains) == 5


@pytest.mark.keywords
def test_initial_stress_shell_card_set_bounded():
    parent = Parent()
    kwargs = {"parent": parent, "keyword": parent}
    cs = InitialStressShellThicknessLargeCardSet(t=2, eps=2.0, hisv=[1, 2, 3], **kwargs)
    card_set = CardSet(
        InitialStressShellThicknessLargeCardSet,
        lambda: 1,
        **kwargs,
    )

    parent._cards = [card_set]
    card_set._base_items = [cs]
    assert len(card_set) == 1
    assert len(card_set._items) == 1
    assert card_set._length_func() == 1
    assert parent.write() == PARENT_REF_STRING


@pytest.mark.keywords
def test_initial_stress_shell_card_set_unbounded():
    parent = Parent()
    kwargs = {"parent": parent, "keyword": parent}
    card_set = CardSet(
        InitialStressShellThicknessLargeCardSet,
        **kwargs,
    )
    parent._cards = [card_set]
    card_set.add_item(t=2, eps=2.0, hisv=[1, 2, 3])
    assert len(card_set) == 1
    assert parent.write() == PARENT_REF_STRING


@pytest.mark.keywords
def test_initial_stress_shell_card_set_unbounded_implicit_initialize():
    parent = Parent()
    kwargs = {"parent": parent, "keyword": parent, "t": 2, "eps": 2.0, "hisv":[1, 2, 3]}
    card_set = CardSet(
        InitialStressShellThicknessLargeCardSet,
        **kwargs,
    )
    parent._cards = [card_set]
    assert len(card_set) == 1
    assert parent.write() == PARENT_REF_STRING


@pytest.mark.keywords
def test_initial_stress_shell_card_set_to_string():
    ref_string1 = """$#       t     sigxx     sigyy     sigzz     sigxy     sigyz     sigzx       eps
       2.0       0.0       0.0       0.0       0.0       0.0       0.0       2.0
$#    hisv      hisv      hisv      hisv
       1.0       2.0       3.0          """

    parent = Parent()
    cs = InitialStressShellThicknessLargeCardSet(t=2, eps=2.0, hisv=[1, 2, 3], parent=parent, keyword=parent)
    cs_str = _to_string(cs)
    assert cs_str == ref_string1


@pytest.mark.keywords
def test_initial_stress_shell_card_set(string_utils):
    parent = Parent()
    cs = InitialStressShellThicknessLargeCardSet(t=2, eps=2.0, hisv=[1, 2, 3], parent=parent, keyword=parent)

    card_set = CardSet(
        InitialStressShellThicknessLargeCardSet,
        lambda: parent.nplane * parent.nthick if (parent.nplane and parent.nthick) else 0,
        lambda: parent.large == None or parent.large == 0,
        parent=parent,
        keyword=parent,
    )

    parent._cards = [card_set]
    card_set._base_items = [cs]
    assert parent.write() == PARENT_REF_STRING

    card_set2 = CardSet(
        InitialStressShellThicknessLargeCardSet,
        lambda: parent.nplane * parent.nthick if (parent.nplane and parent.nthick) else 0,
        lambda: parent.large == None or parent.large == 0,
        parent=parent,
        keyword=parent,
    )

    data_lines = [line for line in PARENT_REF_STRING.splitlines()[1:]]
    content = "\n".join(data_lines)
    card_set2.read(string_utils.as_buffer(content))
    assert len(card_set2.items()) == 1
    first_item: InitialStressShellThicknessLargeCardSet = card_set2.items()[0]
    assert first_item.t == 2.0

    z = InitialStressShellCardSet(nhisv=4, nplane=1, nthick=1, large=0, parent=parent, keyword=parent)
    z.sets[0].t = 2
    z.sets[0].eps = 2.0
    z.sets[0].hisv[0] = 1
    z.sets[0].hisv[1] = 2
    z.sets[0].hisv[2] = 3
    ref_string3 = """$#     eid    nplane    nthick     nhisv    ntensr     large    nthint    nthhsv
                   1         1         4         0         0         0         0
$#       t     sigxx     sigyy     sigzz     sigxy     sigyz     sigzx       eps
       2.0       0.0       0.0       0.0       0.0       0.0       0.0       2.0
$#    hisv      hisv      hisv      hisv
       1.0       2.0       3.0          """
    assert _to_string(z) == ref_string3


@pytest.mark.keywords
class TestCardSetWithDiscriminator:
    """Tests for CardSet items with discriminator fields.

    A discriminator is a field (like ftype) whose value determines which of multiple
    mutually-exclusive cards should be active. This pattern is used in MAT_295 fiber
    families where FTYPE=1 means use Holzapfel-Gasser-Ogden model parameters, and
    FTYPE=2 means use Freed-Doehring model parameters.
    """

    def test_discriminator_read_with_custom_read_data(self, string_utils):
        """Test that CardSet correctly delegates to _read_data when present.

        The _read_data method is used when a CardSet item needs peek-ahead logic
        to determine card activation before reading (discriminator pattern).
        """
        from ansys.dyna.core.lib.card import Card, Field
        from ansys.dyna.core.lib.card_set import read_cards_with_discriminator
        from ansys.dyna.core.lib.cards import Cards

        class MockDiscriminatorItem(Cards):
            """Mock CardSet item with _read_data method for discriminator pattern."""

            def __init__(self, **kwargs):
                super().__init__(kwargs.get("keyword"))
                self._parent = kwargs.get("parent")
                self._cards = [
                    Card(
                        [
                            Field("theta", float, 0, 10),
                            Field("a", float, 10, 10),
                        ],
                    ),
                    Card(
                        [
                            Field("dtype", int, 0, 10, 1),
                            Field("val1", float, 10, 10),
                        ],
                        lambda: self.dtype == 1,
                    ),
                    Card(
                        [
                            Field("dtype", int, 0, 10, 1),
                            Field("val2", float, 10, 10),
                            Field("val3", float, 20, 10),
                        ],
                        lambda: self.dtype == 2,
                    ),
                ]

            @property
            def dtype(self):
                return self._cards[1].get_value("dtype")

            def _read_data(self, buf, parameters) -> bool:
                return read_cards_with_discriminator(
                    self._cards,
                    buf,
                    parameters,
                    discriminator=self._cards[1]._fields[0],
                    cards_with_field=[1, 2],
                )

        parent = Parent()
        kwargs = {"parent": parent, "keyword": parent}
        card_set = CardSet(MockDiscriminatorItem, lambda: 1, **kwargs)
        parent._cards = [card_set]

        # Test reading dtype=1 data
        data1 = "       1.0       2.0\n         1       3.0"
        card_set.read(string_utils.as_buffer(data1))

        items = card_set.items()
        assert len(items) == 1
        assert items[0]._cards[0].get_value("theta") == 1.0
        assert items[0]._cards[0].get_value("a") == 2.0
        assert items[0].dtype == 1
        assert items[0]._cards[1].get_value("val1") == 3.0

    def test_discriminator_read_alternate_type(self, string_utils):
        """Test reading with alternate discriminator value."""
        from ansys.dyna.core.lib.card import Card, Field
        from ansys.dyna.core.lib.card_set import read_cards_with_discriminator
        from ansys.dyna.core.lib.cards import Cards

        class MockDiscriminatorItem(Cards):
            def __init__(self, **kwargs):
                super().__init__(kwargs.get("keyword"))
                self._parent = kwargs.get("parent")
                self._cards = [
                    Card([Field("theta", float, 0, 10)]),
                    Card([Field("dtype", int, 0, 10, 1), Field("val1", float, 10, 10)], lambda: self.dtype == 1),
                    Card(
                        [Field("dtype", int, 0, 10, 1), Field("val2", float, 10, 10), Field("val3", float, 20, 10)],
                        lambda: self.dtype == 2,
                    ),
                ]

            @property
            def dtype(self):
                return self._cards[1].get_value("dtype")

            def _read_data(self, buf, parameters) -> bool:
                return read_cards_with_discriminator(
                    self._cards,
                    buf,
                    parameters,
                    discriminator=self._cards[1]._fields[0],
                    cards_with_field=[1, 2],
                )

        parent = Parent()
        kwargs = {"parent": parent, "keyword": parent}
        card_set = CardSet(MockDiscriminatorItem, lambda: 1, **kwargs)
        parent._cards = [card_set]

        # Test reading dtype=2 data
        data2 = "       1.0\n         2       3.0       4.0"
        card_set.read(string_utils.as_buffer(data2))

        items = card_set.items()
        assert len(items) == 1
        assert items[0].dtype == 2
        assert items[0]._cards[2].get_value("val2") == 3.0
        assert items[0]._cards[2].get_value("val3") == 4.0
