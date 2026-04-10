from ansys.dyna.core.keywords.keyword_classes.auto.contact.contact_automatic_single_surface import ContactAutomaticSingleSurface
from ansys.dyna.core.keywords.keyword_classes.auto.contact.contact_automatic_single_surface_mortar import ContactAutomaticSingleSurfaceMortar
from ansys.dyna.core.keywords.keyword_classes.auto.contact.contact_automatic_single_surface_tiebreak import ContactAutomaticSingleSurfaceTiebreak
import pytest
import warnings
from ansys.dyna.core import Deck
from ansys.dyna.core.lib.option_card import Options

def test_contact_automatic_single_surface_option_dependencies():
    """Test basic option dependencies for ContactAutomaticSingleSurface"""
    # Create instance of keyword
    keyword = ContactAutomaticSingleSurface()

    # Wrap with Options interface
    options = Options(keyword)

    # Start by activating "D"
    options["D"].active = True

    # Assert cascading dependencies
    assert options["D"].active is True
    assert options["C"].active is True
    assert options["B"].active is True
    assert options["A"].active is True

    # Other options should not be active
    assert options["E"].active is False
    assert options["F"].active is False
    assert options["G"].active is False

    # Activate G, all others should be activated
    options["G"].active = True

    for opt in ["A", "B", "C", "D", "E", "F", "G"]:
        assert options[opt].active is True



def test_contact_option_cascading_activation():
    """Test cascading activation for all contact option levels."""
    test_cases = [
        ("A", ["A"]),
        ("B", ["A", "B"]),
        ("C", ["A", "B", "C"]),
        ("D", ["A", "B", "C", "D"]),
        ("E", ["A", "B", "C", "D", "E"]),
        ("F", ["A", "B", "C", "D", "E", "F"]),
        ("G", ["A", "B", "C", "D", "E", "F", "G"]),
    ]

    for option_to_activate, expected_active in test_cases:
        keyword = ContactAutomaticSingleSurface()
        options = Options(keyword)

        options[option_to_activate].active = True


        for opt in ["A", "B", "C", "D", "E", "F", "G"]:
            if opt in expected_active:
                assert options[opt].active is True, f"Option {opt} should be active when {option_to_activate} is activated"
            else:
                assert options[opt].active is False, f"Option {opt} should not be active when {option_to_activate} is activated"


def test_contact_option_deactivation():
    """Test that options can be deactivated properly"""
    keyword = ContactAutomaticSingleSurface()
    options = Options(keyword)

    # Activate G (which activates all)
    options["G"].active = True

    # Verify all are active
    for opt in ["A", "B", "C", "D", "E", "F", "G"]:
        assert options[opt].active is True

    # Deactivate D
    options["D"].active = False

    # Only D should be deactivated
    assert options["D"].active is False
    for opt in ["A", "B", "C", "E", "F", "G"]:
        assert options[opt].active is True


def test_multiple_contact_card_types():
    """Test that the cascading logic works for different contact card types"""
    contact_classes = [
        ContactAutomaticSingleSurface,
        ContactAutomaticSingleSurfaceMortar,
        ContactAutomaticSingleSurfaceTiebreak
    ]

    for contact_class in contact_classes:
        keyword = contact_class()
        options = Options(keyword)

        # Test activating option C
        options["C"].active = True

        # Should activate A, B, C but not D, E, F, G
        for opt in ["A", "B", "C"]:
            assert options[opt].active is True, f"Option {opt} should be active for {contact_class.__name__}"

        for opt in ["D", "E", "F", "G"]:
            assert options[opt].active is False, f"Option {opt} should not be active for {contact_class.__name__}"


def test_contact_card_writing_with_options():
    """Test that contact cards write properly with options activated"""
    keyword = ContactAutomaticSingleSurface()

    # Test writing with no options
    output_no_options = keyword.write()
    assert "*CONTACT_AUTOMATIC_SINGLE_SURFACE" in output_no_options
    assert "sofscl" not in output_no_options  # No options should be present

    # Activate some options
    keyword.options["A"].active = True
    assert "sofscl" in keyword.write()  # Should include A option
    assert "dtpchk" not in keyword.write()  # DTPCHK should not be present without option D
    keyword.options["D"].active = True
    assert "dtpchk" in keyword.write()  # DTPCHK should be present
    assert "sofscl" in keyword.write()  # A should still be present
    assert "ignore" in keyword.write()  # Ignore should be present option C
    assert "snlog" in keyword.write()  # SNLOG should be present option B



def test_contact_card_option_persistence():
    """Test that option states persist correctly"""
    keyword = ContactAutomaticSingleSurface()
    options = Options(keyword)

    # Set some options
    options["C"].active = True

    # Create a new Options interface to the same keyword
    options2 = Options(keyword)

    # Check that the options are still active
    assert options2["A"].active is True
    assert options2["B"].active is True
    assert options2["C"].active is True
    assert options2["D"].active is False



def test_contact_card_with_parameters_and_options():
    """Test contact card with both parameters and options"""
    keyword = ContactAutomaticSingleSurface()

    options = Options(keyword)
    options["B"].active = True


    assert options["A"].active is True
    assert options["B"].active is True
    assert options["C"].active is False

    output = keyword.write()
    assert "*CONTACT_AUTOMATIC_SINGLE_SURFACE" in output


def test_contact_automatic_surface_to_surface_title_loads_without_warning(file_utils):
    """Regression test: *CONTACT_AUTOMATIC_SURFACE_TO_SURFACE_TITLE should load
    without warnings.  Previously, the '_TITLE' suffix was not mapped to the 'ID'
    option, causing the CID+heading line to be parsed as the SSID/MSID card, which
    raised 'could not convert string to float' for the NAME field value.
    """
    deck = Deck()
    input_deck = file_utils.get_asset_file_path("contact_surface_inputdeck.txt")
    with open(input_deck) as f:
        content = f.read()

    with warnings.catch_warnings():
        warnings.simplefilter("error", UserWarning)
        deck.loads(content)

    assert len(list(deck.keywords)) == 1
    kw = list(deck.keywords)[0]
    # ID option is activated via the TITLE suffix → CID + heading card is read
    assert kw.is_option_active("ID")
    assert kw.surfa == 9700101
    assert kw.surfb == 101
    assert kw.surfatyp == 2


def test_title_suffix_does_not_alias_to_id_when_explicit_title_option_exists():
    """When a keyword has its own explicit 'TITLE' option spec, '_TITLE' in the
    keyword name must activate that 'TITLE' option directly and must NOT also
    inject a spurious 'ID' activation.

    SectionSolidLegacy is the canonical example: it declares
        OptionSpec("TITLE", -1, 1)
    so 'TITLE' → 'ID' aliasing must be skipped entirely.
    """
    import warnings
    from ansys.dyna.core.keywords.keyword_classes.manual.section_solid_version_0_11_0 import SectionSolidLegacy

    SECTION_SOLID_TITLE_DECK = """\
*SECTION_SOLID_TITLE
My solid section title
         1         1
"""
    with warnings.catch_warnings():
        # Suppress the DeprecationWarning raised by SectionSolidLegacy.__init__
        warnings.simplefilter("ignore", DeprecationWarning)
        # No other UserWarning should be raised
        warnings.simplefilter("error", UserWarning)

        kw = SectionSolidLegacy()
        kw.loads(SECTION_SOLID_TITLE_DECK)

    # The explicit TITLE option must be active
    assert kw.is_option_active("TITLE")
    # ID must NOT have been spuriously activated (no ID option spec exists)
    assert not kw.is_option_active("ID")
    # Data card parsed correctly
    assert kw.secid == 1
    assert kw.elform == 1
    # Title text was read
    assert kw.title == "My solid section title"