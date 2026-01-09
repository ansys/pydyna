$# LS-DYNA Keyword file created by LS-KwdReader(R) V1.0 (Beta) - 07May2020
$# Created on Jan-9-2026 (15:29:26)
$ Thermal Expansion of a Block
$
*KEYWORD
$
$=============================CONTROL DEFINITIONS ==============================
$
*TITLE
$#                                                                         title
thermal expansion of a block
*CONTROL_ACCURACY
$#     osu       inn    pidosu      iacc     exacc
         1         4         0         1       0.0
*CONTROL_BULK_VISCOSITY
$#      q1        q2      type     btype    tstype
       1.5      0.06        -2         0         0
*CONTROL_ENERGY
$#    hgen      rwen    slnten     rylen     irgen     maten     drlen     disen
         2         2         2         1         2         1         1         1
*CONTROL_HOURGLASS
$#     ihq        qh
         2       0.0
*CONTROL_SOLUTION
$#    soln       nlq     isnan     lcint     lcacc     ncdcf     nocop      crvp
         2                                                                      
*CONTROL_TERMINATION
$#  endtim    endcyc     dtmin    endeng    endmas     nosol
       3.0         0       0.0       0.01.000000E8         0
*CONTROL_THERMAL_SOLVER
$#   atype     ptype    solver         -       gpt    eqheat     fwork       sbc
         1                                                                      
*CONTROL_THERMAL_TIMESTEP
$#      ts       tip       its      tmin      tmax     dtemp      tscp      lcts
         0       1.0       0.1       0.0       0.0       0.0       0.0         0
*CONTROL_TIMESTEP
$#  dtinit    tssfac      isdo    tslimt     dt2ms      lctm     erode     ms1st
       0.0       0.9         0       0.0      0.01         0         0         0
*DATABASE_GLSTAT
$#      dt    binary      lcur     ioopt
      0.03         1         0         1
*DATABASE_BINARY_D3PLOT
$#      dt      lcdt      beam     npltc    psetid
      0.01         0         0         0         0
$
$============================== PART DEFINITIONS ===============================
$
*PART
$      PID     SECID       MID                                              TMID
$#                                                                         title
slab
$#     pid     secid       mid     eosid      hgid      grav    adpopt      tmid
         1         1         1         0         0         0         0         1
*SECTION_SOLID
$#   secid    elform       aet    unused    unused    unused    cohoff   gaskeit
         1         1         0                                     0.0       0.0
*MAT_ELASTIC_PLASTIC_THERMAL
$#     mid        ro
         1       1.0
$#      t1        t2        t3        t4        t5        t6        t7        t8
       0.0      10.0      20.0      30.0      40.0      50.0       0.0       0.0
$#      e1        e2        e3        e4        e5        e6        e7        e8
1.00000E101.00000E101.00000E101.00000E101.00000E101.00000E10       0.0       0.0
$#     pr1       pr2       pr3       pr4       pr5       pr6       pr7       pr8
       0.3       0.3       0.3       0.3       0.3       0.3       0.0       0.0
$#  alpha1    alpha2    alpha3    alpha4    alpha5    alpha6    alpha7    alpha8
       0.02.00000E-64.00000E-66.00000E-68.00000E-61.00000E-5       0.0       0.0
$#   sigy1     sigy2     sigy3     sigy4     sigy5     sigy6     sigy7     sigy8
1.00000E201.00000E201.00000E201.00000E201.00000E201.00000E20       0.0       0.0
$#   etan1     etan2     etan3     etan4     etan5     etan6     etan7     etan8
       0.0       0.0       0.0       0.0       0.0       0.0       0.0       0.0
*MAT_THERMAL_ISOTROPIC
$#    tmid        ro     tgrlc    tgmult      tlat      hlat
         1       1.0       0.0      10.0       0.0       0.0
$#      hc        tc
       1.0       1.0
*INITIAL_TEMPERATURE_NODE
$#     nid      temp       loc
         1      10.0         0
         2      10.0         0
         3      10.0         0
         4      10.0         0
         5      10.0         0
         6      10.0         0
         7      10.0         0
         8      10.0         0
*SET_NODE_LIST
$#     sid       da1       da2       da3       da4    solver       its         -
         1                                        MECH                          
$#    nid1      nid2      nid3      nid4      nid5      nid6      nid7      nid8
         1                                                                      
*SET_NODE_LIST
$#     sid       da1       da2       da3       da4    solver       its         -
         2                                        MECH                          
$#    nid1      nid2      nid3      nid4      nid5      nid6      nid7      nid8
         2                                                                      
*SET_NODE_LIST
$#     sid       da1       da2       da3       da4    solver       its         -
         3                                        MECH                          
$#    nid1      nid2      nid3      nid4      nid5      nid6      nid7      nid8
         3                                                                      
*SET_NODE_LIST
$#     sid       da1       da2       da3       da4    solver       its         -
         4                                        MECH                          
$#    nid1      nid2      nid3      nid4      nid5      nid6      nid7      nid8
         4                                                                      
*SET_NODE_LIST
$#     sid       da1       da2       da3       da4    solver       its         -
         5                                        MECH                          
$#    nid1      nid2      nid3      nid4      nid5      nid6      nid7      nid8
         5                                                                      
*SET_NODE_LIST
$#     sid       da1       da2       da3       da4    solver       its         -
         6                                        MECH                          
$#    nid1      nid2      nid3      nid4      nid5      nid6      nid7      nid8
         6                                                                      
*SET_NODE_LIST
$#     sid       da1       da2       da3       da4    solver       its         -
         7                                        MECH                          
$#    nid1      nid2      nid3      nid4      nid5      nid6      nid7      nid8
         7                                                                      
*SET_NODE_LIST
$#     sid       da1       da2       da3       da4    solver       its         -
         8                                        MECH                          
$#    nid1      nid2      nid3      nid4      nid5      nid6      nid7      nid8
         8                                                                      
$
$============================ ELEMENT DEFINITIONS ==============================
$
*ELEMENT_SOLID
$#   eid     pid      n1      n2      n3      n4      n5      n6      n7      n8
       1       1       1       2       3       4       5       6       7       8
*NODE
$#   nid               x               y               z      tc      rc
       1             0.0             0.0             0.0       7        
       2             1.0             0.0             0.0       5        
       3             1.0             1.0             0.0       3        
       4             0.0             1.0             0.0       6        
       5             0.0             0.0             1.0       4        
       6             1.0             0.0             1.0       2        
       7             1.0             1.0             1.0       0        
       8             0.0             1.0             1.0       1        
*END
