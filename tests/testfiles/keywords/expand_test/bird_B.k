*KEYWORD
*TITLE
Bird
*INCLUDE
control_cards.k
*ALE_REFERENCE_SYSTEM_GROUP
         1         0         4         0         3         7

*SET_PART_LIST
         1
         4         5
*CONSTRAINED_LAGRANGE_IN_SOLID
         3         1         1         0         2         4         2         1


*CONTROL_ALE
         0         1         1      -1.0

*CONTROL_TERMINATION
 0.6000000
*CONTROL_TIMESTEP
 0.0000000 0.7000000
*CONTROL_ENERGY
         2         2         1         1
*MAT_ELASTIC
         1 1.0000000 1.00000+4 0.0000000
*MAT_NULL
         2     0.781     -10.0      0.10

*EOS_LINEAR_POLYNOMIAL
         2       0.0      10.0

*SECTION_SHELL
         1
 0.1000000 0.1000000 0.1000000 0.1000000
*SECTION_SOLID
         2        12
*INITIAL_VOID_PART
         4
*PART

         3         1         1         0         0         0         0         0
*PART

         4         2         2         2         0         0         0         0
*PART

         5         2         2         2         0         0         0         0
*PART
$#                                                                         title

$#     pid     secid       mid     eosid      hgid      grav    adpopt      tmid
         6         6         7
*SECTION_BEAM_TITLE
created from elgen two_node_sets
$#   secid    elform      shrf   qr/irid       cst     scoor       nsm     naupd
         6         9                             1
$#     ts1       ts2       tt1       tt2     print         -     itoff
     0.001     0.001
*MAT_SPOTWELD
$#     mid        ro         e        pr      sigy        eh        dt     tfail
         7       1.0   10000.0       0.3    1000.0
$#   efail       nrr       nrs       nrt       mrr       mss       mtt        nf

*INITIAL_VELOCITY
95
5.,-1.,0.,0.,0.,100.
*SET_NODE_LIST_GENERATE
95
1569,3136
*BOUNDARY_SPC_SET
$#    nsid       cid      dofx      dofy      dofz     dofrx     dofry     dofrz
        99                   1         1         1         1         1         1
*SET_NODE_LIST_TITLE
NODESET(SPC) 99
$#     sid       da1       da2       da3       da4    solver       its         -
        99                                        MECH      1
$#    nid1      nid2      nid3      nid4      nid5      nid6      nid7      nid8
      3348      3349      3350      3351
*ELEMENT_BEAM
$#   eid     pid      n1      n2      n3     rt1     rr1     rt2     rr2   local
     730       6    3181    3348    3352
     731       6    3170    3349    3353
     732       6    3160    3350    3354
     733       6    3150    3351    3355
*END
