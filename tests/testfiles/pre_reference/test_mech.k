$# LS-DYNA Keyword file created by LS-KwdReader(R) V1.0 (Beta) - 07May2020
$# Created on Jan-9-2026 (15:29:26)
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
*CONTROL_TERMINATION
$#  endtim    endcyc     dtmin    endeng    endmas     nosol
      0.03         0       0.0       0.01.000000E8         0
*DATABASE_ABSTAT
$#      dt    binary      lcur     ioopt
2.00000E-4         1         0         1
*DATABASE_GLSTAT
$#      dt    binary      lcur     ioopt
2.00000E-4         1         0         1
*DATABASE_MATSUM
$#      dt    binary      lcur     ioopt
2.00000E-4         1         0         1
*DATABASE_RBDOUT
$#      dt    binary      lcur     ioopt
2.00000E-4         1         0         1
*DATABASE_RCFORC
$#      dt    binary      lcur     ioopt
2.00000E-4         1         0         1
*DATABASE_RWFORC
$#      dt    binary      lcur     ioopt
2.00000E-4         1         0         1
*DATABASE_BINARY_D3PLOT
$#      dt      lcdt      beam     npltc    psetid
5.00000E-4         0         0         0         0
*DATABASE_EXTENT_BINARY
$#   neiph     neips    maxint    strflg    sigflg    epsflg    rltflg    engflg
         0         0         3         0         1         1         1         1
$#  cmpflg    ieverp    beamip     dcomp      shge     stssz    n3thdt   ialemat
         0         1         0         1         1         1         2         0
$# nintsld   pkp_sen      sclp     hydro     msscl     therm    intout    nodout
         1                                                                                          
*CONTACT_NODES_TO_SURFACE
$#     cid                                                                 title
$#   surfa     surfb  surfatyp  surfbtyp   saboxid   sbboxid      sapr      sbpr
         3         2         3         3         0         0         0         0
$#      fs        fd        dc        vc       vdc    penchk        bt        dt
       0.5       0.5       0.0       0.0       0.0         0       0.0       0.0
$#    sfsa      sfsb      sast      sbst     sfsat     sfsbt       fsf       vsf
       1.0   0.06667       0.0       0.0       1.0       1.0       1.0       1.0
*PART
$#                                                                         title

$#     pid     secid       mid     eosid      hgid      grav    adpopt      tmid
         1         0         0         0         0         0         0         0
*RIGIDWALL_PLANAR_ID
$#      id                                                                 title
         1                                                                      
$#    nsid    nsidex     boxid    offset     birth     death     rwksf
         0         0         0       0.0       0.0       0.0       0.0
$#      xt        yt        zt        xh        yh        zh      fric      wvel
       0.0       0.0       0.0       0.0       1.0       0.0       0.5       0.0
*DEFINE_CURVE
$#    lcid      sidr       sfa       sfo      offa      offo    dattyp     lcint
         1         0       1.0       1.0       0.0       0.0         0         0
$#                a1                  o1
                 0.0                 0.0
               0.032                26.0
               0.045                 0.6
                0.08                 0.1
*AIRBAG_SIMPLE_AIRBAG_MODEL
$#     sid    sidtyp      rbid      vsca      psca      vini       mwd      spsf
         1         1         0       0.0       0.0       0.0       0.0       0.0
$#      cv        cp         t      lcid        mu      area        pe        ro
    1736.0    2430.0    1200.0         1       0.7       0.0      14.73.82100E-6
$#     lou      text         a         b        mw      gasc
         0                                                  
*SET_PART_LIST
$#     sid       da1       da2       da3       da4    solver
         1                                        MECH
$#    pid1      pid2      pid3      pid4      pid5      pid6      pid7      pid8
         3         0         0         0         0         0         0         0
*SET_PART_LIST
$#     sid       da1       da2       da3       da4    solver
         2                                        MECH
$#    pid1      pid2      pid3      pid4      pid5      pid6      pid7      pid8
         3         0         0         0         0         0         0         0
*SET_PART_LIST
$#     sid       da1       da2       da3       da4    solver
         3                                        MECH
$#    pid1      pid2      pid3      pid4      pid5      pid6      pid7      pid8
         2         0         0         0         0         0         0         0
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
