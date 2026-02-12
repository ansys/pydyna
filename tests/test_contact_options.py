from ansys.dyna.core.keywords.keyword_classes.auto.contact.contact_automatic_single_surface import ContactAutomaticSingleSurface
from ansys.dyna.core.keywords.keyword_classes.auto.contact.contact_automatic_single_surface_mortar import ContactAutomaticSingleSurfaceMortar
from ansys.dyna.core.keywords.keyword_classes.auto.contact.contact_automatic_single_surface_tiebreak import ContactAutomaticSingleSurfaceTiebreak
import pytest
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