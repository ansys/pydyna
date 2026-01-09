$# LS-DYNA Keyword file created by LS-KwdReader(R) V1.0 (Beta) - 07May2020
$# Created on Jan-9-2026 (15:29:25)
*KEYWORD
*CONTROL_ACCURACY
$#     osu       inn    pidosu      iacc     exacc
         1         4         0         1       0.0
*CONTROL_BULK_VISCOSITY
$#      q1        q2      type     btype    tstype
       1.5      0.06        -2         0         0
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
*CONTROL_ENERGY
$#    hgen      rwen    slnten     rylen     irgen     maten     drlen     disen
         2         2         2         1         2         1         1         1
*CONTROL_HOURGLASS
$#     ihq        qh
         2       0.0
*CONTROL_SHELL
$#  wrpang     esort     irnxx    istupd    theory       bwc     miter      proj
       0.0         1         0         4         0         1         1         1
$# rotascl    intgrd    lamsht    cstyp6    thshel
                                                  
$# psstupd   sidt4tu     cntco    itsflg    irquad    w-mode   stretch      icrq
         0         0         0         0         3       0.0       0.0         0
*CONTROL_TERMINATION
$#  endtim    endcyc     dtmin    endeng    endmas     nosol
      0.12         0       0.0       0.01.000000E8         0
*CONTROL_TIMESTEP
$#  dtinit    tssfac      isdo    tslimt     dt2ms      lctm     erode     ms1st
       0.0       0.8         0       0.0       0.0         0         0         0
*DATABASE_BINARY_D3PLOT
$#      dt      lcdt      beam     npltc    psetid
    0.0025         0         0         0         0
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
*END
