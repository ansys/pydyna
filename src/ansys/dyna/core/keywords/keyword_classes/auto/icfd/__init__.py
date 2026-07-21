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

# Auto-generated imports for icfd domain

from .icfd_boundary_conj_heat import IcfdBoundaryConjHeat
from .icfd_boundary_convection_temp import IcfdBoundaryConvectionTemp
from .icfd_boundary_flux_species import IcfdBoundaryFluxSpecies
from .icfd_boundary_flux_temp import IcfdBoundaryFluxTemp
from .icfd_boundary_freeslip import IcfdBoundaryFreeslip
from .icfd_boundary_fsi import IcfdBoundaryFsi
from .icfd_boundary_fsi_exclude import IcfdBoundaryFsiExclude
from .icfd_boundary_fsi_fixed import IcfdBoundaryFsiFixed
from .icfd_boundary_fsi_oneway import IcfdBoundaryFsiOneway
from .icfd_boundary_fswave import IcfdBoundaryFswave
from .icfd_boundary_ground import IcfdBoundaryGround
from .icfd_boundary_navierslip import IcfdBoundaryNavierslip
from .icfd_boundary_nonslip import IcfdBoundaryNonslip
from .icfd_boundary_periodic import IcfdBoundaryPeriodic
from .icfd_boundary_prescribed_levelset import IcfdBoundaryPrescribedLevelset
from .icfd_boundary_prescribed_movemesh import IcfdBoundaryPrescribedMovemesh
from .icfd_boundary_prescribed_movemesh_dr import IcfdBoundaryPrescribedMovemeshDr
from .icfd_boundary_prescribed_pre import IcfdBoundaryPrescribedPre
from .icfd_boundary_prescribed_sptransp_conc import IcfdBoundaryPrescribedSptranspConc
from .icfd_boundary_prescribed_temp import IcfdBoundaryPrescribedTemp
from .icfd_boundary_prescribed_turbulence import IcfdBoundaryPrescribedTurbulence
from .icfd_boundary_prescribed_vel import IcfdBoundaryPrescribedVel
from .icfd_boundary_prescribed_viscoelastic import IcfdBoundaryPrescribedViscoelastic
from .icfd_boundary_prescribed_watervapor import IcfdBoundaryPrescribedWatervapor
from .icfd_boundary_weakvel import IcfdBoundaryWeakvel
from .icfd_boundary_windkessel import IcfdBoundaryWindkessel
from .icfd_control_adapt import IcfdControlAdapt
from .icfd_control_adapt_size import IcfdControlAdaptSize
from .icfd_control_advection import IcfdControlAdvection
from .icfd_control_backflow import IcfdControlBackflow
from .icfd_control_conj import IcfdControlConj
from .icfd_control_dem_coupling import IcfdControlDemCoupling
from .icfd_control_embedshell import IcfdControlEmbedshell
from .icfd_control_fsi import IcfdControlFsi
from .icfd_control_gap import IcfdControlGap
from .icfd_control_general import IcfdControlGeneral
from .icfd_control_immersed import IcfdControlImmersed
from .icfd_control_immersed_fsi import IcfdControlImmersedFsi
from .icfd_control_imposed_move import IcfdControlImposedMove
from .icfd_control_levelset import IcfdControlLevelset
from .icfd_control_load import IcfdControlLoad
from .icfd_control_mesh import IcfdControlMesh
from .icfd_control_mesh_mov import IcfdControlMeshMov
from .icfd_control_monolithic import IcfdControlMonolithic
from .icfd_control_output import IcfdControlOutput
from .icfd_control_output_subdom import IcfdControlOutputSubdom
from .icfd_control_output_var import IcfdControlOutputVar
from .icfd_control_partition import IcfdControlPartition
from .icfd_control_porous import IcfdControlPorous
from .icfd_control_rans import IcfdControlRans
from .icfd_control_steady import IcfdControlSteady
from .icfd_control_surfmesh import IcfdControlSurfmesh
from .icfd_control_taverage import IcfdControlTaverage
from .icfd_control_time import IcfdControlTime
from .icfd_control_transient import IcfdControlTransient
from .icfd_control_turbulence import IcfdControlTurbulence
from .icfd_control_turb_synthesis import IcfdControlTurbSynthesis
from .icfd_database_average import IcfdDatabaseAverage
from .icfd_database_bin import IcfdDatabaseBin
from .icfd_database_drag import IcfdDatabaseDrag
from .icfd_database_drag_csys import IcfdDatabaseDragCsys
from .icfd_database_drag_vol import IcfdDatabaseDragVol
from .icfd_database_flux import IcfdDatabaseFlux
from .icfd_database_flux_surf import IcfdDatabaseFluxSurf
from .icfd_database_force_dem import IcfdDatabaseForceDem
from .icfd_database_goa import IcfdDatabaseGoa
from .icfd_database_htc import IcfdDatabaseHtc
from .icfd_database_nodeavg import IcfdDatabaseNodeavg
from .icfd_database_nodout import IcfdDatabaseNodout
from .icfd_database_ntempout import IcfdDatabaseNtempout
from .icfd_database_pointavg import IcfdDatabasePointavg
from .icfd_database_pointout import IcfdDatabasePointout
from .icfd_database_residuals import IcfdDatabaseResiduals
from .icfd_database_ssout import IcfdDatabaseSsout
from .icfd_database_ssout_exclude import IcfdDatabaseSsoutExclude
from .icfd_database_temp import IcfdDatabaseTemp
from .icfd_database_timestep import IcfdDatabaseTimestep
from .icfd_database_tpd import IcfdDatabaseTpd
from .icfd_database_transform import IcfdDatabaseTransform
from .icfd_database_twinbuilder import IcfdDatabaseTwinbuilder
from .icfd_database_uindex import IcfdDatabaseUindex
from .icfd_database_vol import IcfdDatabaseVol
from .icfd_database_volume import IcfdDatabaseVolume
from .icfd_database_wetness import IcfdDatabaseWetness
from .icfd_define_heatsource import IcfdDefineHeatsource
from .icfd_define_hemolysis_index import IcfdDefineHemolysisIndex
from .icfd_define_noninertial import IcfdDefineNoninertial
from .icfd_define_point import IcfdDefinePoint
from .icfd_define_porous_region import IcfdDefinePorousRegion
from .icfd_define_residencetimesource import IcfdDefineResidencetimesource
from .icfd_define_source import IcfdDefineSource
from .icfd_define_sptranspsource import IcfdDefineSptranspsource
from .icfd_define_transform import IcfdDefineTransform
from .icfd_define_turbsource import IcfdDefineTurbsource
from .icfd_define_wave_damping import IcfdDefineWaveDamping
from .icfd_if_boundary_immersed_static import IcfdIfBoundaryImmersedStatic
from .icfd_initial import IcfdInitial
from .icfd_initial_bin import IcfdInitialBin
from .icfd_initial_levelset import IcfdInitialLevelset
from .icfd_initial_sptransp import IcfdInitialSptransp
from .icfd_initial_tempnode import IcfdInitialTempnode
from .icfd_initial_turbulence import IcfdInitialTurbulence
from .icfd_initial_viscoelastic import IcfdInitialViscoelastic
from .icfd_initial_watervapor import IcfdInitialWatervapor
from .icfd_mat import IcfdMat
from .icfd_model_nonnewt import IcfdModelNonnewt
from .icfd_model_porous import IcfdModelPorous
from .icfd_model_species_transport import IcfdModelSpeciesTransport
from .icfd_model_viscoelastic import IcfdModelViscoelastic
from .icfd_part import IcfdPart
from .icfd_part_vol import IcfdPartVol
from .icfd_section import IcfdSection
from .icfd_set_node_list import IcfdSetNodeList
from .icfd_solver_split import IcfdSolverSplit
from .icfd_solver_tol_fsi import IcfdSolverTolFsi
from .icfd_solver_tol_lset import IcfdSolverTolLset
from .icfd_solver_tol_mmov import IcfdSolverTolMmov
from .icfd_solver_tol_mom import IcfdSolverTolMom
from .icfd_solver_tol_monolithic import IcfdSolverTolMonolithic
from .icfd_solver_tol_pre import IcfdSolverTolPre
from .icfd_solver_tol_temp import IcfdSolverTolTemp
