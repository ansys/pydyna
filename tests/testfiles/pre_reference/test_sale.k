$# LS-DYNA Keyword file created by LS-KwdReader(R) V1.0 (Beta) - 07May2020
$# Created on Jan-9-2026 (15:29:26)
*KEYWORD
*CONTROL_ALE
$#     dct      nadv      meth      afac      bfac      cfac      dfac      efac
         0         1         2       0.0       0.0       0.0       0.0       0.0
$#   start       end     aafac     vfact      prit       ebc      pref   nsidebc
       0.01.00000E20       1.01.00000E-6         0         0       0.0         0
$#    ncpl      nbkt    imascl    checkr    beamin   mmgpref    pdifmx   dtmufac
         1        50         0       0.0       0.0         0       0.0       0.0
$# optimpp    ialedr    bndflx    minmas
         0         0         01.00000E-5
*CONTROL_ENERGY
$#    hgen      rwen    slnten     rylen     irgen     maten     drlen     disen
         2         2         2         1         2         1         1         1
*DATABASE_GLSTAT
$#      dt    binary      lcur     ioopt
       0.2         1         0         1
*DATABASE_MATSUM
$#      dt    binary      lcur     ioopt
       0.2         1         0         1
*DATABASE_SALE
$#  on/off
         1
*DATABASE_BINARY_D3PLOT
$#      dt      lcdt      beam     npltc    psetid
       5.0         0         0         0         0
*DATABASE_EXTENT_BINARY
$#   neiph     neips    maxint    strflg    sigflg    epsflg    rltflg    engflg
         0         0         0         0         1         1         1         1
$#  cmpflg    ieverp    beamip     dcomp      shge     stssz    n3thdt   ialemat
         0         0         0         1         1         1         2         0
$# nintsld   pkp_sen      sclp     hydro     msscl     therm    intout    nodout
         0                                                                                          
*PART
$#                                                                         title

$#     pid     secid       mid     eosid      hgid      grav    adpopt      tmid
         1         0         0         0         0         0         0         0
*MAT_VACUUM
$#     mid       rho
         11.00000E-9
*MAT_NULL
$#     mid        ro        pc        mu     terod     cerod        ym        pr
         2   0.00128-1.0000E-9       0.0       0.0       0.0       0.0       0.0
*MAT_HIGH_EXPLOSIVE_BURN
$#     mid        ro         d       pcj      beta         k         g      sigy
         3     1.835      0.88      0.37       0.0       0.0       0.0       0.0
*MAT_JOHNSON_COOK
$#     mid        ro         g         e        pr       dtf        vp    rateop
         4      8.96      0.46       0.0      0.34       0.0       0.0       0.0
$#       a         b         n         c         m        tm        tr      epso
9.00000E-4   0.00292      0.31     0.025      1.09    1356.0     293.01.00000E-6
$#      cp        pc     spall        it        d1        d2        d3        d4
3.83000E-6    -0.012       2.0       0.0      0.54      4.89      3.03     0.014
$#      d5c2/p/xnp/d      erod     efmin    numint         k      eps1
      1.124.768372E7                                                  
*INITIAL_DETONATION
$#     pid         x         y         z        lt         -    mmgset
         2       0.0       0.0     19.33       0.0                   0
*ALE_STRUCTURED_MESH
$#   mshid      dpid      nbid      ebid    unused    unused    unused    tdeath
         1         2   2000001   2000001                                     0.0
$#   cpidx     cpidy     cpidz      nid0     lcsid
         1         2         3         0         0
*ALE_STRUCTURED_MESH_CONTROL_POINTS
$#    cpid    unused     icase       sfo    unused      offo
         1                   2       1.0                 0.0
$#                 n                   x            ratio/xl
                   1                                     1.0
                  11                -2.5                 0.5
                  21                                     0.5
                  31                                     1.0
*ALE_STRUCTURED_MESH_CONTROL_POINTS
$#    cpid    unused     icase       sfo    unused      offo
         2                   2       1.0                 0.0
$#                 n                   x            ratio/xl
                   1                                     1.0
                  11                -2.5                 0.5
                  21                                     0.5
                  31                                     1.0
*ALE_STRUCTURED_MESH_CONTROL_POINTS
$#    cpid    unused     icase       sfo    unused      offo
         3                   2       1.0                 0.0
$#                 n                   x            ratio/xl
                   1                                     0.5
                 269                11.0                0.25
                 309                21.0                0.25
                 339                                     5.0
*ALE_STRUCTURED_MULTI-MATERIAL_GROUP
$#  ammgnm       mid     eosid    unused    unused    unused    unused      pref
    vacuum         1         0                                               0.0
$#  ammgnm       mid     eosid    unused    unused    unused    unused      pref
       air         2         1                                        1.01325E-6
$#  ammgnm       mid     eosid    unused    unused    unused    unused      pref
        HE         3         2                                               0.0
$#  ammgnm       mid     eosid    unused    unused    unused    unused      pref
     liner         4         3                                               0.0
*ALE_STRUCTURED_MESH_VOLUME_FILLING
$#   mshid    unused    ammgto    unused   nsample    unused   unused-       vid
         1          air                          4                             0
$#    geom    in/out        e1        e2        e3        e4        e5
ALL                0       0.0       0.0       0.0       0.0       0.0
*ALE_STRUCTURED_MESH_VOLUME_FILLING
$#   mshid    unused    ammgto    unused   nsample    unused   unused-       vid
         1          HE                           4                             0
$#    geom    in/out       pid        e2        e3        e4        e5
PART               1        23       0.0         0         0         0
*ALE_STRUCTURED_MESH_VOLUME_FILLING
$#   mshid    unused    ammgto    unused   nsample    unused   unused-       vid
         1          liner                        4                             0
$#    geom    in/out       pid        e2        e3        e4        e5
PART               1        22       0.0         0         0         0
*ALE_STRUCTURED_MESH_REFINE
$#   mshid      ifx,      ify,      ifz,
         1         1         1         1
*EOS_LINEAR_POLYNOMIAL
$#   eosid        c0        c1        c2        c3        c4        c5        c6
         1       0.0       0.0       0.0       0.0       0.4       0.4       0.0
$#      e0        v0
2.53310E-6       1.0
*EOS_JWL
$#   eosid         a         b        r1        r2      omeg        e0        vo
         2     8.261    0.1724      4.55      1.32      0.38     0.102       1.0
*EOS_GRUNEISEN
$#   eosid         c        s1        s2        s3    gamma0         a        e0
         3     0.394     1.489       0.0       0.0      2.02      0.47       0.0
$#      v0    unused      lcid
       0.0                   0
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
