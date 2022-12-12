$# LS-DYNA Keyword file created by LS-PrePost(R) V4.10DP (Beta)-17May2022
$# Created on Dec-2-2022 (16:56:53)
$ Thermal Expansion of a Block
$
*KEYWORD
$
$=============================CONTROL DEFINITIONS ==============================
$
*TITLE
$#                                                                         title
thermal expansion of a block
$
$============================== PART DEFINITIONS ===============================
$
*PART
$      PID     SECID       MID                                              TMID
$#                                                                         title
slab
$#     pid     secid       mid     eosid      hgid      grav    adpopt      tmid
         1         1         1                                                 1
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
