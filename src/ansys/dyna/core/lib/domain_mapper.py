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

"""Maps keywords to logical domains based on their prefix.

This module is duplicated from codegen/keyword_generation/utils/domain_mapper.py
to enable runtime categorization of keywords without requiring codegen dependencies.
"""

import typing

if typing.TYPE_CHECKING:
    from ansys.dyna.core.lib.keyword_base import KeywordBase

# Define keyword prefix to domain mappings
KEYWORD_DOMAIN_MAPPING = {
    "MAT_": "mat",
    "SECTION_": "section",
    "DEFINE_": "define",
    "ELEMENT_": "element",
    "PART_": "part",
    "DISCRETE_": "part",
    "BOUNDARY_": "boundary",
    "INITIAL_": "boundary",
    "CONTACT_": "contact",
    "CONSTRAINT_": "constraint",
    "CONSTRAINED_": "constrained",
    "CONTROL_": "control",
    "SOLVER_": "control",
    "TIMESTEP_": "control",
    "DATABASE_": "database",
    "PRINT_": "print",
    "OUTPUT_": "output",
    "LOAD_": "load",
    "PRESSURE_": "load",
    "POINT_": "load",
    "BODY_": "load",
    "SENSOR_": "sensor",
    "SET_": "set",
    "HOURGLASS_": "hourglass",
    "DAMPING_": "damping",
    "PARAMETER_": "parameter",
    "INCLUDE_": "include",
    "INCLUDE_TRANSFORM": "include",
    "KEYWORD_OPTIMIZATION": "keyword",
    "OPTIMIZATION_": "optimization",
    "EM_": "em",
    "ELECTROMAGNETIC_": "em",
    "ICFD_": "icfd",
    "AIRBAG_": "airbag",
    "THERMAL_": "thermal",
    "HEAT_": "thermal",
    "NVH_": "nvh",
    "ISPH_": "isph",
    "FREQUENCY_": "frequency",
    "TABLE_": "table",
    "INTERFACE_": "interface",
    "RIGIDWALL_": "rigidwall",
    "NODE_": "node",
    "MESH_": "mesh",
    "MODULE_": "module",
    "PERTURBATION_": "perturbation",
    "RAIL_": "rail",
    "RIGID_": "rigid",
    "SECTION_ALE": "section",
    # First-token domains (replacing misc)
    "ALE_": "ale",
    "BATTERY_": "battery",
    "CASE": "case",
    "CASE_": "case",
    "CESE_": "cese",
    "CHANGE_": "change",
    "CHEMISTRY_": "chemistry",
    "COMMENT": "comment",
    "COMPONENT_": "component",
    "CONTROLLER_": "controller",
    "COSIM_": "cosim",
    "DEFORMABLE_": "deformable",
    "DELETE_": "delete",
    "DUALCESE_": "dualcese",
    "EF_": "ef",
    "EOS": "eos",
    "EOS_": "eos",
    "FATIGUE_": "fatigue",
    "IGA_": "iga",
    "INTEGRATION_": "integration",
    "LSO_": "lso",
    "PARTICLE_": "particle",
    "RVE_": "rve",
    "TITLE": "title",
}


def _extract_keyword_name(keyword: typing.Union["KeywordBase", str]) -> str:
    """Extract the keyword name from a KeywordBase or string keyword.

    Parameters
    ----------
    keyword : Union[KeywordBase, str]
        A keyword object or raw string.

    Returns
    -------
    str
        The extracted keyword name (e.g., "SECTION_SHELL", "MAT_ELASTIC").
    """
    from ansys.dyna.core.lib.keyword_base import KeywordBase

    if isinstance(keyword, KeywordBase):
        # For KeywordBase objects, construct the full name
        if keyword.subkeyword:
            return f"{keyword.keyword}_{keyword.subkeyword}"
        return keyword.keyword
    elif isinstance(keyword, str):
        # For string keywords, extract from the first line
        # Handle both actual newlines and literal \n in test strings
        keyword_text = keyword.replace("\\n", "\n")
        lines = keyword_text.strip().split("\n")
        for line in lines:
            line = line.strip()
            # Skip comments
            if line.startswith("$"):
                continue
            # Look for keyword line starting with *
            if line.startswith("*"):
                # Remove leading * and any trailing comments/options
                kwd_name = line[1:].split()[0]
                return kwd_name
        # If no keyword found, return empty string
        return ""
    else:
        return ""


def by_domain(keyword: typing.Union["KeywordBase", str]) -> str:
    """Categorize a keyword by domain based on its prefix.

    This function can be used as a key function for splitting decks by domain.

    Parameters
    ----------
    keyword : Union[KeywordBase, str]
        The keyword to categorize (either a KeywordBase object or raw string).

    Returns
    -------
    str
        The domain name (e.g., "mat", "section", "control", "other").

    Examples
    --------
    >>> from ansys.dyna.core.lib.deck import Deck
    >>> from ansys.dyna.core.lib.domain_mapper import by_domain
    >>> deck = Deck()
    >>> # ... populate deck ...
    >>> decks_by_domain = deck.split(by_domain)
    >>> mat_deck = decks_by_domain["mat"]
    """
    keyword_name = _extract_keyword_name(keyword)

    for prefix, domain in KEYWORD_DOMAIN_MAPPING.items():
        if keyword_name.startswith(prefix):
            return domain

    # Default to 'other' if no prefix matches
    return "other"


def get_all_domains() -> typing.List[str]:
    """Get a sorted list of all unique domains.

    Returns
    -------
    List[str]
        A sorted list of all domain names.
    """
    return sorted(set(KEYWORD_DOMAIN_MAPPING.values()))
