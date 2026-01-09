$# LS-DYNA Keyword file created by LS-KwdReader(R) V1.0 (Beta) - 07May2020
$# Created on Jan-9-2026 (15:29:26)
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
         0         0         0         0         0                 0.0         1
*CONTROL_TIMESTEP
$#  dtinit    tssfac      isdo    tslimt     dt2ms      lctm     erode     ms1st
       0.0       0.9         0       0.0-4.0000E-4         0         0         0
*PART
$#                                                                         title

$#     pid     secid       mid     eosid      hgid      grav    adpopt      tmid
         1         0         0         0         0         0         0         0
*RIGIDWALL_GEOMETRIC_CYLINDER_DISPLAY
$#    nsid    nsidex     boxid     birth     death
         0         0         0       0.0       0.0
$#      xt        yt        zt        xh        yh        zh      fric
   2472.37    -600.0   1270.98   2472.37    -600.0   2668.53       0.0
$#  radcyl    lencyl     nsegs
     100.0    1000.0         0
$#     pid        ro         e        pr
         01.00000E-91.00000E-4       0.3
         0         0         0       0.0       0.0
   3580.25    -600.0   1261.37   3580.25    -600.0   3130.49       0.0
     100.0    1000.0         0
         01.00000E-91.00000E-4       0.3
*RIGIDWALL_GEOMETRIC_CYLINDER_MOTION_DISPLAY
         0         0         0       0.0       0.0
   3090.59   -955.35   1299.42   3090.59   -955.35   2958.43       0.0
     100.0    1000.0         0
         1         0       0.0       1.0       0.0
         01.00000E-91.00000E-4       0.3
*DEFINE_CURVE
$#    lcid      sidr       sfa       sfo      offa      offo    dattyp     lcint
         1         0       1.0       1.0       0.0       0.0         0         0
$#                a1                  o1
                 0.0                20.0
               100.0                20.0
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
