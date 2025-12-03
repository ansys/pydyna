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

# Auto-generated imports for load domain

from .load_acoustic_source import LoadAcousticSource
from .load_ale_convection import LoadAleConvection
from .load_beam_element import LoadBeamElement
from .load_beam_set import LoadBeamSet
from .load_blast import LoadBlast
from .load_blast_clearing import LoadBlastClearing
from .load_blast_enhanced import LoadBlastEnhanced
from .load_blast_segment import LoadBlastSegment
from .load_blast_segment_set import LoadBlastSegmentSet
from .load_body_generalized import LoadBodyGeneralized
from .load_body_generalized_set_node import LoadBodyGeneralizedSetNode
from .load_body_generalized_set_part import LoadBodyGeneralizedSetPart
from .load_body_parts import LoadBodyParts
from .load_body_porous import LoadBodyPorous
from .load_body_rx import LoadBodyRx
from .load_body_ry import LoadBodyRy
from .load_body_rz import LoadBodyRz
from .load_body_vector import LoadBodyVector
from .load_body_x import LoadBodyX
from .load_body_y import LoadBodyY
from .load_body_z import LoadBodyZ
from .load_brode import LoadBrode
from .load_density_depth import LoadDensityDepth
from .load_eroding_part_set import LoadErodingPartSet
from .load_expansion_pressure import LoadExpansionPressure
from .load_face_uvw import LoadFaceUvw
from .load_face_uvw_set import LoadFaceUvwSet
from .load_face_xyz import LoadFaceXyz
from .load_face_xyz_set import LoadFaceXyzSet
from .load_gravity_part import LoadGravityPart
from .load_gravity_part_set import LoadGravityPartSet
from .load_heat_controller import LoadHeatController
from .load_heat_exothermic_reaction import LoadHeatExothermicReaction
from .load_heat_generation_set import LoadHeatGenerationSet
from .load_heat_generation_set_shell import LoadHeatGenerationSetShell
from .load_heat_generation_set_solid import LoadHeatGenerationSetSolid
from .load_heat_generation_shell import LoadHeatGenerationShell
from .load_heat_generation_solid import LoadHeatGenerationSolid
from .load_mask import LoadMask
from .load_motion_node import LoadMotionNode
from .load_moving_pressure import LoadMovingPressure
from .load_node import LoadNode
from .load_node_point import LoadNodePoint
from .load_node_set import LoadNodeSet
from .load_node_set_once import LoadNodeSetOnce
from .load_nurbs_shell import LoadNurbsShell
from .load_point_uvw import LoadPointUvw
from .load_point_uvw_set import LoadPointUvwSet
from .load_pyro_actuator import LoadPyroActuator
from .load_pze import LoadPze
from .load_remove_part import LoadRemovePart
from .load_remove_part_set import LoadRemovePartSet
from .load_rigid_body import LoadRigidBody
from .load_segment import LoadSegment
from .load_segment_id import LoadSegmentId
from .load_segment_contact_mask import LoadSegmentContactMask
from .load_segment_file import LoadSegmentFile
from .load_segment_fsilnk import LoadSegmentFsilnk
from .load_segment_nonuniform import LoadSegmentNonuniform
from .load_segment_set import LoadSegmentSet
from .load_segment_set_id import LoadSegmentSetId
from .load_segment_set_angle import LoadSegmentSetAngle
from .load_segment_set_nonuniform import LoadSegmentSetNonuniform
from .load_seismic_ssi_aux import LoadSeismicSsiAux
from .load_seismic_ssi_aux_id import LoadSeismicSsiAuxId
from .load_seismic_ssi_deconv import LoadSeismicSsiDeconv
from .load_seismic_ssi_deconv_id import LoadSeismicSsiDeconvId
from .load_seismic_ssi_node import LoadSeismicSsiNode
from .load_seismic_ssi_node_id import LoadSeismicSsiNodeId
from .load_seismic_ssi_point import LoadSeismicSsiPoint
from .load_seismic_ssi_point_id import LoadSeismicSsiPointId
from .load_seismic_ssi_set import LoadSeismicSsiSet
from .load_seismic_ssi_set_id import LoadSeismicSsiSetId
from .load_shell_element import LoadShellElement
from .load_shell_set import LoadShellSet
from .load_spcforc import LoadSpcforc
from .load_ssa import LoadSsa
from .load_steady_state_rolling import LoadSteadyStateRolling
from .load_stiffen_part import LoadStiffenPart
from .load_stiffen_part_set import LoadStiffenPartSet
from .load_superplastic_forming import LoadSuperplasticForming
from .load_surface_stress import LoadSurfaceStress
from .load_surface_stress_set import LoadSurfaceStressSet
from .load_thermal_binout import LoadThermalBinout
from .load_thermal_constant import LoadThermalConstant
from .load_thermal_constant_element_beam import LoadThermalConstantElementBeam
from .load_thermal_constant_element_shell import LoadThermalConstantElementShell
from .load_thermal_constant_element_solid import LoadThermalConstantElementSolid
from .load_thermal_constant_element_tshell import LoadThermalConstantElementTshell
from .load_thermal_constant_node import LoadThermalConstantNode
from .load_thermal_d3plot import LoadThermalD3Plot
from .load_thermal_load_curve import LoadThermalLoadCurve
from .load_thermal_rsw import LoadThermalRsw
from .load_thermal_topaz import LoadThermalTopaz
from .load_thermal_variable import LoadThermalVariable
from .load_thermal_variable_beam import LoadThermalVariableBeam
from .load_thermal_variable_beam_set import LoadThermalVariableBeamSet
from .load_thermal_variable_element_beam import LoadThermalVariableElementBeam
from .load_thermal_variable_element_shell import LoadThermalVariableElementShell
from .load_thermal_variable_element_solid import LoadThermalVariableElementSolid
from .load_thermal_variable_element_tshell import LoadThermalVariableElementTshell
from .load_thermal_variable_node import LoadThermalVariableNode
from .load_thermal_variable_shell import LoadThermalVariableShell
from .load_thermal_variable_shell_set import LoadThermalVariableShellSet
from .load_vibro_acoustic import LoadVibroAcoustic
from .load_volume_loss import LoadVolumeLoss
