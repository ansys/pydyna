# Copyright (C) 2023 - 2025 ANSYS, Inc. and/or its affiliates.
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

"""Maps keywords to logical domains based on their prefix."""

# Define keyword prefix to domain mappings
KEYWORD_DOMAIN_MAPPING = {
    "MAT_": "materials",
    "SECTION_": "sections",
    "DEFINE_": "definitions",
    "ELEMENT_": "elements",
    "PART_": "parts",
    "DISCRETE_": "parts",
    "BOUNDARY_": "boundaries",
    "INITIAL_": "boundaries",
    "CONTACT_": "contacts",
    "CONSTRAINT_": "constraints",
    "CONSTRAINED_": "constraints",
    "CONTROL_": "controls",
    "SOLVER_": "controls",
    "TIMESTEP_": "controls",
    "DATABASE_": "output",
    "PRINT_": "output",
    "OUTPUT_": "output",
    "LOAD_": "loading",
    "PRESSURE_": "loading",
    "POINT_": "loading",
    "BODY_": "loading",
    "SENSOR_": "sensors",
    "SET_": "sets",
    "HOURGLASS_": "hourglass",
    "DAMPING_": "damping",
    "PARAMETER_": "parameters",
    "INCLUDE_": "includes",
    "INCLUDE_TRANSFORM": "includes",
    "TITLE": "misc",
    "KEYWORD_OPTIMIZATION": "optimization",
    "OPTIMIZATION_": "optimization",
    "EM_": "electromagnetics",
    "ELECTROMAGNETIC_": "electromagnetics",
    "ICFD_": "icfd",
    "AIRBAG_": "airbag",
    "THERMAL_": "thermal",
    "HEAT_": "thermal",
    "NVH_": "nvh",
    "ISPH_": "isph",
    "FREQUENCY_": "frequency",
    "TABLE_": "tables",
    "INTERFACE_": "interfaces",
    "RIGIDWALL_": "rigidwalls",
}


def get_keyword_domain(keyword_name: str) -> str:
    """
    Categorize a keyword by domain based on its prefix.

    Parameters
    ----------
    keyword_name : str
        The keyword name (e.g., "MAT_ELASTIC", "BOUNDARY_SPC_NODE")

    Returns
    -------
    str
        The domain directory name (e.g., "materials", "boundaries")
    """
    for prefix, domain in KEYWORD_DOMAIN_MAPPING.items():
        if keyword_name.startswith(prefix):
            return domain

    # Default to miscellaneous if no prefix matches
    return "misc"


def get_all_domains() -> list:
    """Get a sorted list of all unique domains."""
    return sorted(set(KEYWORD_DOMAIN_MAPPING.values()))
