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

import os
import re

import pandas as pd

from ansys.dyna.core import Deck
from ansys.dyna.core.lib.format_type import format_type
from ansys.dyna.core.lib.import_handler import ImportHandler
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
    """Import the deck with a custom handler.

    The custom handler skips all BOUNDARY_PRECRACK
    and sets the nid1 property on ALE_SMOOTHING."""
    deck = Deck()
    filepath = file_utils.assets_folder / "test.k"
    class TestImportHandler(ImportHandler):
        def __init__(self):
            self._num_keywords = 0
        def before_import(self, context, keyword, buffer):
            self._num_keywords += 1
            if keyword == "*BOUNDARY_PRECRACK":
                return False
            return True
        def after_import(self, context, keyword):
            if isinstance(keyword, kwd.AleSmoothing):
                keyword.nid1 = 1
    import_handler = TestImportHandler()
    deck.register_import_handler(import_handler)
    deck.import_file(filepath)
    assert deck.title == "Basic 001"
    assert import_handler._num_keywords == 12
    assert len(deck.keywords) == 11
    assert len(deck.all_keywords) == 11
    assert deck.keywords[0].included_from == filepath
    assert deck.get(type="ALE")[0].nid1 == 1



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
def test_control_debug():
    from ansys.dyna.core.lib.deck import Deck
    deck = Deck()
    deck.loads('*CONTROL_DEBUG')
    deck.write()


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
def test_deck_copy():
    import copy
    x = Deck()
    x.append(kwd.Mat001(mid=99))
    y = copy.deepcopy(x)
    y.keywords[0].mid = 100
    assert y.keywords[0].included_from is None
    assert x.keywords[0].mid == 99
    assert y.keywords[0].mid == 100


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
def test_deck_read_parameter_keyword(ref_string):
    """Test reading a deck with parameters."""
    deck = Deck()
    deck.loads(ref_string.test_parametrized_deck_string)
    assert len(deck.string_keywords) == 0
    assert len(deck.keywords) == 2
    kwd = deck.keywords[1]
    assert kwd.vdc == 5.0e-4
    assert kwd.vc == 5.0e-6
    assert kwd.sfsa == -2.65

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
    assert curve1.deck is None
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
    assert curve1.deck == deck
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
    assert deck.keyword_names[0] == "*DEFINE_CURVE"
    assert deck.keyword_names[2] == "*INCLUDE"


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
    assert isinstance(deck.keywords[0], kwd.DefineCurve)
    assert deck.keywords[0].title == "title"
    assert deck.keywords[0].options["TITLE"].active

@pytest.mark.keywords
def test_deck_active_options(ref_string):
    """Test that active options set with setters."""
    deck = Deck()
    deck_test = Deck()
    deck_test.append(kwd.DefineCurve())
    deck_test.keywords[0].title = "title"
    deck.loads(ref_string.test_title_string)
    assert deck.keywords[0].title == deck_test.keywords[0].title
    assert deck_test.keywords[0].options["TITLE"].active


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
def test_deck_links():
    deck = Deck()
    deck.append(kwd.DefineTransformation(tranid=10, option="POINT", a1=1, a2=2.0, a3=0.0, a4=1.0))
    i_c = kwd.IncludeTransform(tranid=10)
    deck.append(i_c)
    xforms = i_c.tranid_link.transforms
    assert len(xforms) == 1
    assert xforms["option"][0] == "POINT"
    assert xforms["a1"][0] == 1
    assert xforms["a2"][0] == 2.0
    assert xforms["a3"][0] == 0.0
    assert xforms["a4"][0] == 1.0


@pytest.mark.keywords
def test_deck_expand(file_utils):
    """Test that a long deck can read a standard deck."""
    # I think there are more corner cases related to this to iron out..
    deck = Deck(format=format_type.long)
    include_path = file_utils.get_asset_file_path("test.k")
    deck.append(kwd.Include(filename=file_utils.get_asset_file_path("test.k"), format=format_type.standard))
    expanded_deck = deck.expand()
    assert len(expanded_deck.keywords) == 12
    assert expanded_deck.keywords[1].format == format_type.standard
    assert expanded_deck.keywords[0].included_from == include_path


@pytest.mark.keywords
def test_deck_expand_encoding(file_utils):
    """Test that a long deck can read a standard deck."""
    deck = Deck(format=format_type.long)
    deck.append(kwd.Include(filename=file_utils.get_asset_file_path("encoding_sample.k"), format=format_type.standard))
    expanded_deck = deck.expand()

    # check all_keywords instead of keywords because the deck has comma-delimited cards which is not yet supported
    assert len(expanded_deck.all_keywords) == 1

@pytest.mark.keywords
def test_deck_expand_recursive_include_path(file_utils):
    deck = Deck()
    include_path1 = file_utils.get_asset_file_path("expand_test")
    include_path2 = os.path.join(include_path1, "fol")
    deck.append(kwd.IncludePath(filename=include_path1))
    deck.append(kwd.IncludePath(filename=include_path2))
    deck.append(kwd.Include(filename='bird_B.k'))
    deck = deck.expand(recurse=True)
    assert len(deck.all_keywords) == 40
    assert len(deck.keywords) == 40

@pytest.mark.keywords
def test_deck_expand_transform(file_utils):
    deck = Deck()
    include_path = file_utils.get_asset_file_path("transform")
    xform = kwd.IncludeTransform(filename = os.path.join(include_path, "test.k"))
    xform.idnoff = 10
    xform.ideoff = 40
    xform.idpoff = 100
    deck.append(xform)
    deck = deck.expand(recurse=True)
    assert len(deck.keywords) == 3
    assert deck.keywords[0].elements["eid"][2] == 47
    assert deck.keywords[0].elements["pid"][5] == 101
    assert deck.keywords[0].elements["n1"][1] == 11
    assert deck.keywords[0].elements["n8"][2] == 0
    assert deck.keywords[0].elements["n5"][3] == 15
    assert deck.keywords[1].elements["eid"][3] == 45043
    assert deck.keywords[1].elements["pid"][0] == 145
    assert deck.keywords[1].elements["n1"][1] == 31
    assert pd.isna(deck.keywords[1].elements["n2"][2])
    assert deck.keywords[1].elements["n3"][0] == 22
    assert deck.keywords[2].nodes["nid"][0] == 11
    assert deck.keywords[2].nodes["nid"][20] == 31

@pytest.mark.keywords
def test_deck_expand_transform_custom_handler(file_utils):
    """Test using a custom transform handler as an override."""
    deck = Deck()
    include_path = file_utils.get_asset_file_path("transform")
    xform = kwd.IncludeTransform(filename = os.path.join(include_path, "test.k"))
    xform.idnoff = 10
    xform.ideoff = 40
    xform.idpoff = 100
    deck.append(xform)

    from ansys.dyna.core.lib.transform import Transform

    class TransformElementBeam(Transform):
        def transform(self, keyword):
            self._transform_part_ids(keyword.elements)
        def _transform_part_ids(self, elements: pd.DataFrame):
            offset = -1
            elements['pid'] = elements['pid'] + offset

    deck.transform_handler.register_transform_handler(("ELEMENT", "BEAM"), TransformElementBeam)
    deck = deck.expand(recurse=True)
    assert len(deck.keywords) == 3
    assert deck.keywords[1].elements["pid"][0] == 44
    assert deck.keywords[1].elements["pid"][3] == 44


@pytest.mark.keywords
def test_deck_expand_with_define_transform(file_utils):
    """Test using a custom transform handler as an override."""

    # single translation
    include_path = file_utils.get_asset_file_path("transform")
    define_transform_kwd = kwd.DefineTransformation(tranid=1, option="TRANSL", a1=-100.0)
    xform = kwd.IncludeTransform(filename = os.path.join(include_path, "test.k"))
    xform.tranid_link = define_transform_kwd

    deck = Deck()
    deck.extend([define_transform_kwd, xform])
    expanded = deck.expand()
    assert len(expanded.keywords) == 4
    assert expanded.keywords[3].nodes["x"][0] == -600

    # error case
    deck = Deck()
    define_transform_kwd = kwd.DefineTransformation(tranid=1, option="ROTATE", a1=0.0, a2=0.0, a3=0.0, a7=45.0)
    xform = kwd.IncludeTransform(filename = os.path.join(include_path, "test.k"))
    xform.tranid_link = define_transform_kwd
    deck.extend([define_transform_kwd, xform])
    expected_warning = "Error applying transformation to *NODE: Direction vector A1, A2, A3 cannot be all zero!"
    expected_warning_expression = re.escape(expected_warning)
    with pytest.warns(UserWarning, match=expected_warning_expression):
        deck.expand()

    # unhandled case
    deck = Deck()
    define_transform_kwd = kwd.DefineTransformation(tranid=1, option="ROTATE", a4=1.0)
    xform = kwd.IncludeTransform(filename = os.path.join(include_path, "test.k"))
    xform.tranid_link = define_transform_kwd
    deck.extend([define_transform_kwd, xform])
    with pytest.warns(UserWarning, match="DEFINE_TRANFORMATION ROTATE option with parameters"):
        deck.expand()

    # multiple transforms
    deck = Deck()
    define_transform_kwd = kwd.DefineTransformation()
    define_transform_kwd.transforms = pd.DataFrame(
        {
            "option": ["ROTATE", "TRANSL"],
            "a1": [0.0, -100.0],
            "a2": [0.0, 0.0],
            "a3": [1.0, 0.0],
            "a4": [0.0, 0.0],
            "a5": [0.0, 0.0],
            "a6": [0.0, 0.0],
            "a7": [25.0, 0.0],
        }
    )
    xform = kwd.IncludeTransform(filename = os.path.join(include_path, "test.k"))
    xform.tranid_link = define_transform_kwd
    deck.extend([define_transform_kwd, xform])
    expanded = deck.expand()
    assert len(expanded.keywords) == 4
    assert expanded.keywords[3].nodes["x"][0] == pytest.approx(-543.784672)
    assert expanded.keywords[3].nodes["y"][0] == pytest.approx(-253.570957)


@pytest.mark.keywords
def test_deck_clear():
    node = kwd.Node()
    deck = Deck()
    deck.append(node)
    deck.clear()
    # ok after clear
    deck = Deck()
    deck.append(node)

    # not ok without clear
    deck2 = Deck()
    with pytest.raises(Exception):
        deck2.append(node)


@pytest.mark.keywords
def test_deck_unprocessed(ref_string):
    deck = Deck()
    deck.loads(ref_string.test_deck_with_unknown_keywords)
    string_keywords = deck.string_keywords
    assert (len(string_keywords)) == 1


def _verify_encrypted_deck(deck):
    assert len(deck.encrypted_keywords) == 1
    s = deck.write()
    assert "-----BEGIN PGP MESSAGE-----" in s
    s = deck.encrypted_keywords[0].data
    assert s.startswith("\nhQEOA7Xd7ZA651ZYEAP+OgA+NS90p")

@pytest.mark.keywords
def test_deck_encrypted_import(file_utils):
    """Import an encrypted file as a deck."""
    deck = Deck()
    filename = file_utils.assets_folder / "test_input_deck_1_1024bit.asc"
    deck.import_file(filename)
    _verify_encrypted_deck(deck)

@pytest.mark.keywords
def test_deck_encrypted_import_expand(file_utils):
    """Import an encrypted file as a deck."""
    deck = Deck()
    filename = file_utils.assets_folder / "test_input_deck_1_1024bit.asc"
    deck.append(kwd.Node())
    deck.append(kwd.Include(filename=filename))
    deck = deck.expand()
    _verify_encrypted_deck(deck)
    assert len(deck.get(type="NODE")) == 1


@pytest.mark.keywords
def test_deck_encrypted_import_expand_transform(file_utils):
    """Import an encrypted file as a deck."""
    deck = Deck()
    filename = file_utils.assets_folder / "test_input_deck_1_1024bit.asc"
    deck.append(kwd.DefineTransformation(tranid=10, option="POINT", a1=1, a2=2.0, a3=0.0, a4=1.0))
    i_c = kwd.IncludeTransform(filename = filename, tranid=10)
    deck.append(i_c)
    deck = deck.expand()
    _verify_encrypted_deck(deck)
    assert len(deck.get(type="DEFINE")) == 1


@pytest.mark.keywords
def test_deck_remove_superfluous_newlines(ref_string):
    deck = Deck()
    deck.loads(ref_string.test_section_solid_title_deck_string)
    assert deck.write() == ref_string.test_section_solid_title_deck_string


@pytest.mark.keywords
def test_series_card_read_write_set(ref_string):
    """test to read and write set cards, especially checking case where last card contains all fields"""
    set_string = ref_string.test_series_card_sets_string
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


@pytest.mark.keywords
def test_deck_remove():
    """a deck is created afterward individual und multiple string_keywords/keywords are removed
    the deck should be empty again in the end"""
    deck = Deck()
    for i in range(0, 5):
        deck.append(kwd.SectionSolid(secid=i))
        deck.loads(f"*NOT_A_KEYWORD_{i}")
    assert len(deck.all_keywords) == 10

    """remove keyword in the middle of the deck"""
    rm_individual = int(len(deck.all_keywords)/2)
    deck.remove(rm_individual)
    assert len(deck.all_keywords) == 9

    """beginning/remove keyword at end of the deck"""
    deck.remove(0)
    deck.remove(-1)
    assert len(deck.all_keywords) == 7

    """remove rest of keywords and string_keywords"""
    rm_list = list(range(len(deck.all_keywords)))
    deck.remove(rm_list)

    """deck should be empty"""
    assert len(deck.all_keywords) == 0


@pytest.mark.keywords
def test_deck_expand_nonlocal_parameters(file_utils):
    """Test reading a deck with parameters without resolving parameter."""
    deck = Deck()
    cwd = file_utils.assets_folder / "expand_parameters" / "nonlocal"
    filename = cwd / "top.k"
    deck.import_file(filename) # pass new argument resolve_parameter = True / False)
    assert len(deck.keywords) == 3
    deck = deck.expand(recurse=True, cwd=cwd)
    assert len(deck.keywords) == 4
    sections: list[kwd.SectionSolid] = list(deck.get_kwds_by_type("SECTION"))
    pm_main_value = deck.parameters.get("pm_main")
    assert pm_main_value == 100.0
    assert len(sections) == 3
    expected_secid_to_elform = {10:100, 20:1, 30:100}
    for section in sections:
        assert section.elform == expected_secid_to_elform[section.secid]


@pytest.mark.keywords
def test_deck_expand_local_parameters_isolation(file_utils):
    """Test that PARAMETER_LOCAL parameters are isolated to their definition file.

    This test demonstrates the BUG where PARAMETER_LOCAL leaks to parent decks.
    After the fix, local parameters should NOT be accessible outside their definition file.
    """
    deck = Deck()
    cwd = file_utils.assets_folder / "expand_parameters" / "local"
    filename = cwd / "top.k"
    deck.import_file(filename)

    # Before expansion, we should have loaded PARAMETER but not PARAMETER_LOCAL from include
    # Only gbl should be in the top-level deck's parameters
    pm_global_value = deck.parameters.get("gbl")
    assert pm_global_value == 200.0

    # loc should NOT be accessible from top-level deck before expansion
    with pytest.raises(KeyError):
        deck.parameters.get("loc")

    # Expand the deck
    deck = deck.expand(recurse=True, cwd=cwd)

    # After expansion, get all sections
    sections: list[kwd.SectionSolid] = list(deck.get_kwds_by_type("SECTION"))
    # FIXED BEHAVIOR: 3 sections (10, 20, 30) - section 40 fails because it can't resolve &loc
    assert len(sections) == 3

    # Section 10 uses gbl (200.0) from top-level
    section_10 = next(s for s in sections if s.secid == 10)
    assert section_10.elform == 200

    # Section 20 uses loc (999.0) - substituted during include processing
    section_20 = next(s for s in sections if s.secid == 20)
    assert section_20.elform == 999

    # Section 30 uses gbl (200.0) - global param visible in include
    section_30 = next(s for s in sections if s.secid == 30)
    assert section_30.elform == 200

    # Section 40 tries to use loc from top level - should fail
    # The substitution happens during loads() before PARAMETER_LOCAL from the include is loaded
    assert len(deck.string_keywords) == 1  # Section 40

    # FIXED: After expansion, loc should NOT be in the top-level parameters
    with pytest.raises(KeyError):
        deck.parameters.get("loc")


@pytest.mark.keywords
def test_deck_expand_local_parameters_sibling_isolation(file_utils):
    """Test that PARAMETER_LOCAL parameters don't leak between sibling includes.

    This test demonstrates the BUG where PARAMETER_LOCAL leaks between sibling includes.
    After the fix, sibling includes should not see each other's local parameters.
    """
    deck = Deck()
    cwd = file_utils.assets_folder / "expand_parameters" / "local"
    filename = cwd / "sibling_test_top.k"
    deck.import_file(filename)

    # Expand the deck
    deck = deck.expand(recurse=True, cwd=cwd)

    # Get all sections
    sections: list[kwd.SectionSolid] = list(deck.get_kwds_by_type("SECTION"))
    # FIXED BEHAVIOR: 3 sections (50, 51, 61) - section 60 fails because it can't resolve &aloc
    assert len(sections) == 3

    # Section 50 uses aloc (111.0) from sibling_a.k
    section_50 = next(s for s in sections if s.secid == 50)
    assert section_50.elform == 111

    # Section 51 uses shr (100.0) from top
    section_51 = next(s for s in sections if s.secid == 51)
    assert section_51.elform == 100

    # Section 60 - FIXED: Should not be created because aloc is not accessible
    assert len(deck.string_keywords) == 1  # Section 60 failed to parse

    # Section 61 uses shr (100.0) from top
    section_61 = next(s for s in sections if s.secid == 61)
    assert section_61.elform == 100

    # FIXED: aloc should NOT be in the top-level parameters after expansion
    with pytest.raises(KeyError):
        deck.parameters.get("aloc")
