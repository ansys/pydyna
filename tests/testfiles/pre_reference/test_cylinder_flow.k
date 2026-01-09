$# LS-DYNA Keyword file created by LS-KwdReader(R) V1.0 (Beta) - 07May2020
$# Created on Jan-9-2026 (15:29:25)
*KEYWORD
*TITLE
$#                                                                         title
ICFD Cylinder flow
*CONTROL_TERMINATION
$#  endtim    endcyc     dtmin    endeng    endmas     nosol
     100.0         0       0.0       0.01.000000E8         0
*DATABASE_BINARY_D3PLOT
$#      dt      lcdt      beam     npltc    psetid
       1.0         0         0         0         0
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
*ICFD_CONTROL_TIME
$#     ttm        dt       cfl    lcidsf     dtmin     dtmax    dtinit    tdeath
     100.0       0.0       1.0         0       0.0       0.0       0.0       0.0
*ICFD_PART_VOL
$#     pid     secid       mid
         1         1         0
$#   spid1     spid2     spid3     spid4     spid5     spid6     spid7     spid8
         1         2         3         4         0         0         0         0
*ICFD_SECTION
$#     sid
         1
*MESH_VOLUME
$#   volid
         1
$#    pid1      pid2      pid3      pid4      pid5      pid6      pid7      pid8
         1         2         3         4         0         0         0         0
*END
