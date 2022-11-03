$# LS-DYNA Keyword file created by LS-PrePost(R) V4.10 (Beta)-27Sep2022
$# Created on Oct-13-2022 (16:33:48)
*KEYWORD   
*PART
$#                                                                         title
SPH Walls
$#     pid     secid       mid     eosid      hgid      grav    adpopt      tmid
         4         2       912         0         0         0         0         0
*PART
$#                                                                         title
SPH Cube
$#     pid     secid       mid     eosid      hgid      grav    adpopt      tmid
         5         2       912         0         0         0         0         0
*PART
$#                                                                         title
SPH Water
$#     pid     secid       mid     eosid      hgid      grav    adpopt      tmid
         6         1       911         0         0         0         0         0
*PART_INERTIA
$#                                                                         title
Moving cube mesh
$#     pid     secid       mid     eosid      hgid      grav    adpopt      tmid
         7        10        10         0         0         0         0         0
$#      xc        yc        zc        tm      ircs    nodeid      
    -672.0       0.0     274.08.64000E-4         0         0
$#     ixx       ixy       ixz       iyy       iyz       izz   
    2.0736       0.0       0.0    2.0736       0.0    2.0736
$#     vtx       vty       vtz       vrx       vry       vrz   
    2000.0       0.0       0.0       0.0       0.0       0.0
*PART
$#                                                                         title
Walls Mesh
$#     pid     secid       mid     eosid      hgid      grav    adpopt      tmid
         8        10        11         0         0         0         0         0
*END
