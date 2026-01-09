$# LS-DYNA Keyword file created by LS-KwdReader(R) V1.0 (Beta) - 07May2020
$# Created on Jan-9-2026 (15:29:25)
*KEYWORD
*CONTROL_DISCRETE_ELEMENT
$#   ndamp     tdamp     frics     fricr     normk    sheark       cap       vtk
      0.99      0.99       0.9       0.9      0.01    0.2857         0         0
$#   gamma       vol       ang       gap    unused    ignore      nbuf  parallel
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
