$# LS-DYNA Keyword file created by LS-KwdReader(R) V1.0 (Beta) - 07May2020
$# Created on Jan-9-2026 (15:29:25)
*KEYWORD
*CONTROL_CONTACT
$#  slsfac    rwpnal    islchk    shlthk    penopt    thkchg     orien    enmass
       0.0       1.0         0         0         0         0         1         0
$#  usrstr    usrfrc     nsbcs    interm     xpene     ssthk      ecdt   tiedprj
         0         0         0         0       0.0         0         0         0
$#   sfric     dfric       edc       vfc        th     th_sf    pen_sf     ptscl
                                                                                
$#  ignore    frceng   skiprwg    outseg   spotstp   spotdel   spothin   dir_tie
         1                                                                      
$#    isym    nserod    rwgaps    rwgdth     rwksf      icov    swradf    ithoff
                                                                                
$#  shledg    pstiff    ithcnt    tdcnof     ftall    unused    shltrw    igactc
         0         0         0         0         0                 0.0         0
*PART
$#                                                                         title

$#     pid     secid       mid     eosid      hgid      grav    adpopt      tmid
         1         0         0         0         0         0         0         0
*ELEMENT_SHELL
$#   eid     pid      n1      n2      n3      n4      n5      n6      n7      n8
       1       1       1       3       4       2       0       0       0       0
*NODE
$#   nid               x               y               z      tc      rc
       1             0.0             0.0             0.0       0       0
       2            16.0             0.0             0.0       0       0
       3             0.0            16.0             0.0       0       0
       4            16.0            16.0             0.0       0       0
*EM_OUTPUT
$#    mats      matf      sols      solf      mesh       mem    timing
         2         2         2         2         0         0         0
*EM_DATABASE_GLOBALENERGY
$#   outlv     dtout
         1       0.0
*EM_CONTROL
$#   emsol     numls   macrodt   dimtype    nperio         -   ncylfem   ncylbem
         1       100       0.0         0         2                5000      5000
*EM_CONTROL_TIMESTEP
$#  tstype    dtcons      lcid    factor     tsmin     tsmas     rlcsf    mecats
         15.00000E-6         0       1.0       0.0       0.0        25         0
*EM_SOLVER_BEM
$#  reltol    maxite     stype    precon   uselast  ncyclbem
1.00000E-6      1000         2         1         1         3
*EM_SOLVER_FEM
$#  reltol    maxite     stype    precon   uselast  ncyclfem
     0.001      1000         1         1         1         3
*EM_SOLVER_BEMMAT
$#   matid    unused    unused    unused    unused    unused    unused    reltol
         1                                                            1.00000E-6
*EM_SOLVER_BEMMAT
$#   matid    unused    unused    unused    unused    unused    unused    reltol
         2                                                            1.00000E-6
*EM_SOLVER_BEMMAT
$#   matid    unused    unused    unused    unused    unused    unused    reltol
         3                                                            1.00000E-6
*EM_CONTROL_CONTACT
$#    emct    cconly     ctype    cotype      eps1      eps2      eps3        d0
         1         0         0         0       0.3       0.3       0.3       0.0
*END
