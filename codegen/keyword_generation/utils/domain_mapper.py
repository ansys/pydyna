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

"""Maps keywords to logical domains based on their prefix."""

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

    # Default to 'other' if no prefix matches
    return "other"


def get_all_domains() -> list:
    """Get a sorted list of all unique domains."""
    return sorted(set(KEYWORD_DOMAIN_MAPPING.values()))
