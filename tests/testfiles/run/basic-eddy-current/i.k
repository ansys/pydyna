$-----------------------------------------------------------------------------
$
$ Example provided by Inaki (LSTC)
$
$ E-Mail: info@dynamore.de
$ Web: http://www.dynamore.de
$
$ Copyright, 2015 DYNAmore GmbH
$ Copying for non-commercial usage allowed if
$ copy bears this notice completely.
$
$X------------------------------------------------------------------------------
$X
$X 1. Run file as is.
$X    Requires LS-DYNA MPP R8.0.0 (or higher) with double precision 
$X
$X------------------------------------------------------------------------------
$# UNITS: (Dimensionless) 
$X------------------------------------------------------------------------------
$X
*KEYWORD
*TITLE
EM Basic Eddy current problem set up
*PARAMETER
R    T_end      5e-4
R  dt_plot      5e-6
$
$--- em
R  em_cond      1.e4
R    em_dt     1.e-5
$
$
$--- struc
$
R dt_struc     5.e-6
Rrho_struc     7000.
R  E_struc     2.e11 
R nu_struc       0.3
*INCLUDE
struc.k
*INCLUDE
mesh.k
*INCLUDE
voltage.k
*DATABASE_BINARY_D3PLOT
&dt_plot
$---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
$                                                                              $
$                             EM CONTROL                                       $
$                                                                              $
$---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
*EM_CONTROL
$    emsol        
         1         
*EM_CONTROL_TIMESTEP
$#  tstype    dtcons      lcid                            
         1    &em_dt
$---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
$                                                                              $
$                             EM CIRCUIT                                       $
$                                                                              $
$---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
*EM_CIRCUIT
$   circid  circtype      lcid         R         L         C        V0
         1         2        10 
$ ssidCurr ssidVltin ssidVltOt    partID
         2         1         2
$---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
$                                                                              $
$                             EM MAT and EOS                                   $
$                                                                              $
$---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
*EM_MAT_001
$   em_mid     mtype     sigma       eos              
         1         2  &em_cond 
*END  
