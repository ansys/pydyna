$# LS-DYNA Keyword file created by LS-KwdReader(R) V1.0 (Beta) - 07May2020
$# Created on Jan-9-2026 (15:29:26)
*KEYWORD
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
