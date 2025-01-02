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

import pandas as pd

from ansys.dyna.core import Deck
from ansys.dyna.core.lib.format_type import format_type
from ansys.dyna.core import keywords as kwd

import pytest

BLOCK1 = "$a\n$b\n$c\n"
BLOCK2 = "*KEYWORD\n"
BLOCK3 = "*TITLE\nTest Title\n"
BLOCK4 = "*END"

HEADER = BLOCK1 + BLOCK2 + BLOCK3


def __reference_keyword_file(code):
    return code + "*END"


@pytest.mark.keywords
def test_deck_001():
    """test title and user comment"""
    deck = Deck()
    assert deck.write() == __reference_keyword_file(BLOCK2)
    deck.title = "Test Title"
    assert deck.write() == __reference_keyword_file(BLOCK2 + BLOCK3)
    deck.comment_header = "a\nb\nc"
    assert deck.write() == __reference_keyword_file(BLOCK1 + BLOCK2 + BLOCK3)

    deck2 = Deck(title="Test Title")
    assert deck2.write() == __reference_keyword_file(BLOCK2 + BLOCK3)
    deck2.comment_header = "a\nb\nc"
    assert deck2.write() == __reference_keyword_file(BLOCK1 + BLOCK2 + BLOCK3)

    # test user comment after keyword but before keywords?
    deck3 = Deck()
    deck3.loads(BLOCK2 + BLOCK1 + BLOCK4)
    assert "b" in deck.comment_header


@pytest.mark.keywords
def test_deck_002():
    """test loading deck header from string"""
    deck = Deck()
    deck.loads(HEADER)
    assert deck.comment_header == "a\nb\nc"
    assert deck.title == "Test Title"


@pytest.mark.keywords
def test_deck_003(file_utils):
    deck = Deck()
    keyword_string = file_utils.read_file(file_utils.assets_folder / "test.k")
    deck.loads(keyword_string)
    assert deck.title == "Basic 001"
    assert len(deck._keywords) == 12


@pytest.mark.keywords
def test_deck_004(ref_string):
    """test stop reading after *END"""
    keyword_string = ref_string.test_deck_004_string
    deck = Deck()
    deck.loads(keyword_string)
    assert len(deck._keywords) == 2


@pytest.mark.keywords
def test_deck_005():
    """test handling unknown keywords without throwing or discarding them"""
    keyword_string = """*INCLUDE
$#                                                                      filename
                                                                 /path/to/test.k

*FOO_BAR_QUUX
$#    ssid
         1
"""
    deck = Deck()
    deck.loads(keyword_string)
    assert len(deck._keywords) == 2
    str_keywords = [kwd for kwd in deck._keywords if isinstance(kwd, str)]
    assert len(str_keywords) == 1

    assert (
        str_keywords[0]
        == """*FOO_BAR_QUUX
$#    ssid
         1"""
    )


@pytest.mark.keywords
def test_deck_006(ref_string):
    """test adding two decks together"""
    deck1 = Deck()
    deck2 = Deck()
    decksum = deck1 + deck2
    assert isinstance(decksum, Deck)
    assert len(decksum._keywords) == 0

    keyword_string_1 = ref_string.test_deck_006_string_1

    keyword_string_2 = ref_string.test_deck_006_string_2
    keyword_string_sum = ref_string.test_deck_006_string_sum

    deck3 = Deck()
    deck4 = Deck()
    deck5 = Deck()

    deck3.loads(keyword_string_1)
    deck4.loads(keyword_string_2)
    deck5.loads(keyword_string_sum)
    assert isinstance(deck5, Deck)
    assert len(deck5._keywords) == len(deck3._keywords) + len(deck4._keywords)


@pytest.mark.keywords
def test_deck_read_parameters():
    """Test reading a deck with parameters."""
    test_string = """*CONTACT_TIED_SHELL_EDGE_TO_SURFACE_BEAM_OFFSET_ID
99999999  Mycontact
  99999999  99999998         4         0                             0         0
                                             &vdct
        1.        1.       -2.       -2.        1.        1.        1.        1."""
    deck = Deck()
    deck.parameters.add("vdct", 1.12)
    deck.loads(test_string)
    assert len(deck.string_keywords) == 0
    kwd = deck.keywords[0]
    assert kwd.vdc == 1.12

@pytest.mark.keywords
def test_deck_007():
    "unit testing for .extend"
    deck = Deck()
    deck.extend([])
    assert isinstance(deck, Deck)
    assert len(deck._keywords) == 0


@pytest.mark.keywords
def test_kwdeck_basic_001(file_utils):
    "deck with only kw"
    deck = Deck()
    deck.title = "Basic 001"
    curve1 = kwd.DefineCurve()
    curve1.user_comment = "My first curve"
    curve2 = kwd.DefineCurve(offa=2.0)
    curve2.curves = pd.DataFrame({"a1": [1, 3, 5], "o1": [2, 4, 6]})
    include1 = kwd.Include(filename="/path/to/test.k")
    volume1 = kwd.DefineContactVolume(type=0)
    volume2 = kwd.DefineContactVolume(type=1)
    volume3 = kwd.DefineContactVolume(type=2)
    tshell1 = kwd.SectionTShell(secid=4, icomp=1, nip=9)
    seatbelt1 = kwd.SectionSeatbelt()
    smoothing1 = kwd.AleSmoothing()
    smoothing1.nid2 = 3
    boundary1 = kwd.BoundaryPrecrack()
    boundary2 = kwd.BoundaryAcousticCoupling()
    boundary3 = kwd.BoundaryTemperatureSet()
    deck.extend(
        [
            curve1,
            curve2,
            include1,
            volume1,
            volume2,
            volume3,
            tshell1,
            seatbelt1,
            smoothing1,
            boundary1,
            boundary2,
            boundary3,
        ]
    )
    deck_string = deck.write()
    file_utils.compare_string_with_file(deck_string, "test.k")


@pytest.mark.keywords
def test_kwdeck_basic_002(file_utils, ref_string):
    "kw with deck and string"
    deck = Deck()
    deck.title = "Basic 001"
    curve1 = kwd.DefineCurve()
    curve1.user_comment = "My first curve"
    curve2 = kwd.DefineCurve(offa=2.0)
    curve2.curves = pd.DataFrame({"a1": [1, 3, 5], "o1": [2, 4, 6]})
    include1 = kwd.Include(filename="/path/to/test.k")
    volume1 = kwd.DefineContactVolume(type=0)
    volume2 = kwd.DefineContactVolume(type=1)
    volume3 = kwd.DefineContactVolume(type=2)
    tshell1 = kwd.SectionTShell(secid=4, icomp=1, nip=9)
    seatbelt1 = kwd.SectionSeatbelt()
    string = ref_string.test_kwdeck_basic_001_string
    deck.extend([curve1, curve2, include1, volume1, volume2, volume3, tshell1, seatbelt1] + string)
    deck_string = deck.write()
    file_utils.compare_string_with_file(deck_string, "test.k")


@pytest.mark.keywords
def test_deck_kwlist(ref_string):
    "kw with deck and string"
    deck = Deck()
    deck.title = "Basic 001"
    curve = kwd.DefineCurve()
    seatbelt = kwd.SectionSeatbelt()
    string = ref_string.test_kwlist_string
    deck.extend([curve, seatbelt] + string)
    all_keywords = deck.all_keywords
    keywords = deck.keywords
    str_keywords = deck.string_keywords
    assert all_keywords == [
        curve,
        seatbelt,
        ref_string.test_kwlist_string[0],
        ref_string.test_kwlist_string[1],
    ]
    assert keywords == [curve, seatbelt]
    assert str_keywords == ref_string.test_kwlist_string


@pytest.mark.keywords
def test_deck_load_title(ref_string):
    deck = Deck()
    deck.loads(ref_string.test_title_string)
    print(type(deck))
    print(type(kwd.DefineCurve()))
    print(type(deck.keywords[0]))
    assert isinstance(deck.keywords[0], kwd.DefineCurve)
    assert deck.keywords[0].title == "title"
    assert deck.keywords[0].options["TITLE"].active


@pytest.mark.keywords
def test_deck_default_write_long(ref_string):
    """Test that write(format=format_type.long) on a default deck writes keywords using the long format."""
    deck = Deck()
    deck.append(kwd.SectionSeatbelt())
    deck_string = deck.write(format=format_type.long)
    assert deck_string == ref_string.test_long_deck_string


@pytest.mark.keywords
def test_deck_long_deck_write_noargs(ref_string):
    "Test that write() on a long deck written writes keywords using with the long format"
    deck = Deck(format=format_type.long)
    deck.append(kwd.SectionSeatbelt())
    deck_string = deck.write()
    assert deck_string == ref_string.test_long_deck_string


@pytest.mark.keywords
def test_deck_long_deck_write_standard(ref_string):
    "Test that write(format=format_type.standard) on a long deck writes keywords using with the standard format"
    deck = Deck(format=format_type.long)
    deck.append(kwd.SectionSeatbelt())
    deck_string = deck.write(format=format_type.standard)
    assert deck_string == ref_string.test_standard_deck_string


@pytest.mark.keywords
def test_deck_long_deck_write_standard_keyword(ref_string):
    "Test that write(format=format_type.standard) on a long deck writes keywords using with the standard format"
    deck = Deck(format=format_type.long)
    deck.append(kwd.SectionSeatbelt(format=format_type.standard))
    deck_string = deck.write()
    assert deck_string == ref_string.test_long_deck_standard_keyword_string


@pytest.mark.keywords
def test_deck_read_long_deck_long_keyword():
    """Test that read a long deck with a long keyword works."""
    deck = Deck()
    deck.append(kwd.SectionSeatbelt(area=1.0))
    deck_string = deck.write(format=format_type.long)
    deck.clear()
    deck.loads(deck_string)
    assert deck.format == format_type.long
    assert len(deck.keywords) == 1
    assert deck.keywords[0].area == 1.0


@pytest.mark.keywords
def test_deck_read_long_deck_standard_keyword():
    """Test that read a long deck with a long keyword works."""
    deck = Deck()
    deck.append(kwd.SectionSeatbelt(area=1.0, format=format_type.standard))
    deck_string = deck.write(format=format_type.long)
    deck.clear()
    deck.loads(deck_string)
    assert deck.format == format_type.long
    assert len(deck.keywords) == 1
    assert deck.keywords[0].area == 1.0


@pytest.mark.keywords
def test_deck_expand(file_utils):
    """Test that a long deck can read a standard deck."""
    # I think there are more corner cases related to this to iron out..
    file_utils.get_asset_file_path("test.k")
    deck = Deck(format=format_type.long)
    deck.append(kwd.Include(filename=file_utils.get_asset_file_path("test.k"), format=format_type.standard))
    expanded_deck = deck.expand()
    assert len(expanded_deck.keywords) == 12
    assert expanded_deck.keywords[1].format == format_type.standard


@pytest.mark.keywords
def test_deck_unprocessed(ref_string):
    deck = Deck()
    deck.loads(ref_string.test_deck_with_unknown_keywords)
    string_keywords = deck.string_keywords
    assert (len(string_keywords)) == 1


@pytest.mark.keywords
def test_deck_remove_superfluous_newlines(ref_string):
    deck = Deck()
    deck.loads(ref_string.test_section_solid_title_deck_string)
    assert deck.write() == ref_string.test_section_solid_title_deck_string


@pytest.mark.keywords
def test_variable_card_read_write_set(ref_string):
    """test to read and write variable cards, especially checking case where last card contains all fields"""
    set_string = ref_string.test_variable_card_sets_string
    input_deck = Deck()
    input_deck.loads(set_string)
    assert input_deck.write() == set_string


@pytest.mark.keywords
def test_deck_with_contacts_and_ids(ref_string):
    """if the keyword is read correctly it should end up in the deck as a keyword object and not a string
    this test contact has no optional cards"""
    deck = Deck()

    deck.loads(ref_string.test_deck_contact_tied_shell_edge_to_surface_id2)
    assert len(deck.keywords) == 1
    assert deck.write() == ref_string.test_deck_contact_tied_shell_edge_to_surface_id2

    """if the keyword is read correctly it should end up in the deck as a keyword object and not a string
    this test contact has optional cards"""
    deck.clear()
    deck.loads(ref_string.test_deck_contact_tied_shell_edge_to_surface_id3)
    assert len(deck.keywords) == 1
    assert deck.write() == ref_string.test_deck_contact_tied_shell_edge_to_surface_id3
