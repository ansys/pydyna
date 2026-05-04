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

# Auto-generated imports for element domain

from .element_beam import ElementBeam
from .element_beam_elbow import ElementBeamElbow
from .element_beam_offset import ElementBeamOffset
from .element_beam_orientation import ElementBeamOrientation
from .element_beam_orientation_offset import ElementBeamOrientationOffset
from .element_beam_pid import ElementBeamPid
from .element_beam_pid_offset import ElementBeamPidOffset
from .element_beam_pid_orientation import ElementBeamPidOrientation
from .element_beam_pulley import ElementBeamPulley
from .element_beam_scalar import ElementBeamScalar
from .element_beam_scalar_offset import ElementBeamScalarOffset
from .element_beam_scalar_orientation import ElementBeamScalarOrientation
from .element_beam_scalar_pid import ElementBeamScalarPid
from .element_beam_section import ElementBeamSection
from .element_beam_section_offset import ElementBeamSectionOffset
from .element_beam_section_orientation import ElementBeamSectionOrientation
from .element_beam_section_pid import ElementBeamSectionPid
from .element_beam_section_scalar import ElementBeamSectionScalar
from .element_beam_source import ElementBeamSource
from .element_beam_thickness import ElementBeamThickness
from .element_beam_thickness_offset import ElementBeamThicknessOffset
from .element_beam_thickness_orientation import ElementBeamThicknessOrientation
from .element_beam_thickness_pid import ElementBeamThicknessPid
from .element_beam_thickness_scalar import ElementBeamThicknessScalar
from .element_beam_warpage import ElementBeamWarpage
from .element_bearing import ElementBearing
from .element_blanking import ElementBlanking
from .element_direct_matrix_input import ElementDirectMatrixInput
from .element_direct_matrix_input_binary import ElementDirectMatrixInputBinary
from .element_discrete import ElementDiscrete
from .element_discrete_lco import ElementDiscreteLco
from .element_discrete_sphere import ElementDiscreteSphere
from .element_discrete_sphere_volume import ElementDiscreteSphereVolume
from .element_generalized_shell import ElementGeneralizedShell
from .element_generalized_solid import ElementGeneralizedSolid
from .element_inertia import ElementInertia
from .element_inertia_offset import ElementInertiaOffset
from .element_interpolation_shell import ElementInterpolationShell
from .element_interpolation_solid import ElementInterpolationSolid
from .element_lancing import ElementLancing
from .element_mass import ElementMass
from .element_mass_matrix import ElementMassMatrix
from .element_mass_matrix_node_set import ElementMassMatrixNodeSet
from .element_mass_node_set import ElementMassNodeSet
from .element_mass_part import ElementMassPart
from .element_mass_part_set import ElementMassPartSet
from .element_nurbs_patch_2d_trimmed import ElementNurbsPatch2DTrimmed
from .element_plotel import ElementPlotel
from .element_seatbelt import ElementSeatbelt
from .element_seatbelt_accelerometer import ElementSeatbeltAccelerometer
from .element_seatbelt_pretensioner import ElementSeatbeltPretensioner
from .element_seatbelt_retractor import ElementSeatbeltRetractor
from .element_seatbelt_sensor import ElementSeatbeltSensor
from .element_seatbelt_slipring import ElementSeatbeltSlipring
from .element_shell import ElementShell
from .element_shell_beta import ElementShellBeta
from .element_shell_beta_composite import ElementShellBetaComposite
from .element_shell_beta_composite_long import ElementShellBetaCompositeLong
from .element_shell_beta_offset import ElementShellBetaOffset
from .element_shell_beta_offset_composite import ElementShellBetaOffsetComposite
from .element_shell_beta_offset_composite_long import ElementShellBetaOffsetCompositeLong
from .element_shell_bext_patch import ElementShellBextPatch
from .element_shell_composite import ElementShellComposite
from .element_shell_composite_long import ElementShellCompositeLong
from .element_shell_dof import ElementShellDof
from .element_shell_mcid import ElementShellMcid
from .element_shell_mcid_offset import ElementShellMcidOffset
from .element_shell_nurbs_patch import ElementShellNurbsPatch
from .element_shell_nurbs_patch_trimmed import ElementShellNurbsPatchTrimmed
from .element_shell_nurbs_patch_v3 import ElementShellNurbsPatchV3
from .element_shell_offset import ElementShellOffset
from .element_shell_offset_composite import ElementShellOffsetComposite
from .element_shell_offset_composite_long import ElementShellOffsetCompositeLong
from .element_shell_shl4_to_shl8 import ElementShellShl4ToShl8
from .element_shell_source_sink import ElementShellSourceSink
from .element_shell_thickness import ElementShellThickness
from .element_shell_thickness_beta import ElementShellThicknessBeta
from .element_shell_thickness_beta_offset import ElementShellThicknessBetaOffset
from .element_shell_thickness_mcid import ElementShellThicknessMcid
from .element_shell_thickness_mcid_offset import ElementShellThicknessMcidOffset
from .element_shell_thickness_offset import ElementShellThicknessOffset
from .element_solid_dof import ElementSolidDof
from .element_solid_dof__ten_nodes_format_ import ElementSolidDofTenNodesFormat
from .element_solid_h20 import ElementSolidH20
from .element_solid_h20_dof import ElementSolidH20Dof
from .element_solid_h27 import ElementSolidH27
from .element_solid_h27_dof import ElementSolidH27Dof
from .element_solid_h64 import ElementSolidH64
from .element_solid_h8toh20 import ElementSolidH8Toh20
from .element_solid_h8toh20_dof import ElementSolidH8Toh20Dof
from .element_solid_h8toh20_format2 import ElementSolidH8Toh20Format2
from .element_solid_h8toh20_ortho import ElementSolidH8Toh20Ortho
from .element_solid_h8toh20_ortho_dof import ElementSolidH8Toh20OrthoDof
from .element_solid_h8toh20_ortho_format2 import ElementSolidH8Toh20OrthoFormat2
from .element_solid_h8toh27 import ElementSolidH8Toh27
from .element_solid_h8toh27_dof import ElementSolidH8Toh27Dof
from .element_solid_h8toh27_format2 import ElementSolidH8Toh27Format2
from .element_solid_h8toh27_ortho import ElementSolidH8Toh27Ortho
from .element_solid_h8toh27_ortho_dof import ElementSolidH8Toh27OrthoDof
from .element_solid_h8toh27_ortho_format2 import ElementSolidH8Toh27OrthoFormat2
from .element_solid_h8toh64 import ElementSolidH8Toh64
from .element_solid_h8toh64_format2 import ElementSolidH8Toh64Format2
from .element_solid_nurbs_patch import ElementSolidNurbsPatch
from .element_solid_ortho_dof import ElementSolidOrthoDof
from .element_solid_ortho_dof__ten_nodes_format_ import ElementSolidOrthoDofTenNodesFormat
from .element_solid_p21 import ElementSolidP21
from .element_solid_p40 import ElementSolidP40
from .element_solid_p6top21 import ElementSolidP6Top21
from .element_solid_p6top21_format2 import ElementSolidP6Top21Format2
from .element_solid_peri import ElementSolidPeri
from .element_solid_t15 import ElementSolidT15
from .element_solid_t20 import ElementSolidT20
from .element_solid_t4tot10 import ElementSolidT4Tot10
from .element_solid_t4tot10_format2 import ElementSolidT4Tot10Format2
from .element_solid_t4tot15 import ElementSolidT4Tot15
from .element_solid_t4tot15_format2 import ElementSolidT4Tot15Format2
from .element_solid_tet4totet10 import ElementSolidTet4Totet10
from .element_solid_tet4totet10_dof_format2 import ElementSolidTet4Totet10DofFormat2
from .element_solid_tet4totet10_format2 import ElementSolidTet4Totet10Format2
from .element_solid_tet4totet10_ortho import ElementSolidTet4Totet10Ortho
from .element_solid_tet4totet10_ortho_dof import ElementSolidTet4Totet10OrthoDof
from .element_sph import ElementSph
from .element_trim import ElementTrim
from .element_tshell import ElementTshell
from .element_tshell_beta import ElementTshellBeta
from .element_tshell_beta_composite import ElementTshellBetaComposite
from .element_tshell_composite import ElementTshellComposite
from .element_tshell_composite_beta import ElementTshellCompositeBeta
