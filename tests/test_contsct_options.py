from ansys.dyna.core.keywords.keyword_classes.auto.contact_automatic_single_surface import ContactAutomaticSingleSurface
import pytest
from ansys.dyna.core.lib.option_card import Options

@pytest.mark.keywords
def test_contact_automatic_single_surface_option_dependencies():
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