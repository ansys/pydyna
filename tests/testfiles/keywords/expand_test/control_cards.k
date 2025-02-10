*DATABASE_GLSTAT
$#      dt    binary      lcur     ioopt
      0.01
*DATABASE_MATSUM
$#      dt    binary      lcur     ioopt
      0.01
*DATABASE_NODFOR
$#      dt    binary      lcur     ioopt
      0.01
*DATABASE_SECFORC
$#      dt    binary      lcur     ioopt
      0.01
*DATABASE_SWFORC
$#      dt    binary      lcur     ioopt
      0.01
*DATABASE_NODAL_FORCE_GROUP
        96
*DATABASE_HISTORY_NODE_SET
$#     id1       id2       id3       id4       id5       id6       id7       id8
        96
*SET_NODE_LIST
$#     sid       da1       da2       da3       da4    solver       its         -
        96                                        MECH      1
$#    nid1      nid2      nid3      nid4      nid5      nid6      nid7      nid8
      3224      3225      3226      3324      3325      3326      3327      3227
*DATABASE_BINARY_D3PLOT
$#      dt      lcdt      beam     npltc    psetid
      0.02                   1
*DATABASE_CROSS_SECTION_PLANE_ID
$#    csid                                                                 title
         1
$#    psid       xct       yct       zct       xch       ych       zch    radius
                 1.5 -0.166667      0.09       1.5  0.833333      0.09
$#    xhev      yhev      zhev      lenl      lenm        id     itype

*CONTROL_MPP_DECOMPOSITION_DISTRIBUTE_ALE_ELEMENTS
*INCLUDE
mesh.k