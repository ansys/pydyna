$# LS-DYNA Keyword file created by LS-KwdReader(R) V1.0 (Beta) - 07May2020
$# Created on Jan-9-2026 (15:29:26)
*KEYWORD
*CONTROL_SPH
$#    ncbs     boxid        dt      idim   nmneigh      form     start      maxv
         1         11.00000E20         3       150        13       0.01.00000E15
$#    cont     deriv       ini     ishow     ierod     icont     iavis     isymp
         0         0         0         1         0         0         0       100
*CONTROL_TERMINATION
$#  endtim    endcyc     dtmin    endeng    endmas     nosol
       0.5         0       0.0       0.01.000000E8         0
*CONTROL_TIMESTEP
$#  dtinit    tssfac      isdo    tslimt     dt2ms      lctm     erode     ms1st
       0.0       1.0         0       0.0       0.0         1         0         0
*DATABASE_GLSTAT
$#      dt    binary      lcur     ioopt
     0.001         1         0         1
*DATABASE_SPHMASSFLOW
$#      dt    binary      lcur     ioopt
     0.001         1         0         1
*DATABASE_BINARY_D3PLOT
$#      dt      lcdt      beam     npltc    psetid
      0.01         0         0         0         0
*BOUNDARY_PRESCRIBED_MOTION_RIGID
$#     pid       dof       vad      lcid        sf       vid     death     birth
         7         1         0         2       1.0         0       0.0       0.0
$#     pid       dof       vad      lcid        sf       vid     death     birth
         7         7         0         3      0.01         0       0.0       0.0
*LOAD_BODY_Z
$#    lcid        sf    lciddr        xc        yc        zc       cid
         4                                                            
*PART
$#                                                                         title
SPH Walls
$#     pid     secid       mid     eosid      hgid      grav    adpopt      tmid
         4         4         4         0         0         0         0         0
*SECTION_SPH
$#   secid      cslh      hmin      hmax    sphini     death     start   sphkern
         4       1.0       1.0       1.0      12.0       0.0       0.0         0
*MAT_SPH_INCOMPRESSIBLE_STRUCTURE
$#     mid        ro      beta     rough       adh
         41.00000E-9       0.0       0.0       0.0
*PART
$#                                                                         title
SPH Cube
$#     pid     secid       mid     eosid      hgid      grav    adpopt      tmid
         5         5         5         0         0         0         0         0
*SECTION_SPH
$#   secid      cslh      hmin      hmax    sphini     death     start   sphkern
         5       1.0       1.0       1.0      12.0       0.0       0.0         0
*MAT_SPH_INCOMPRESSIBLE_STRUCTURE
$#     mid        ro      beta     rough       adh
         51.00000E-9       0.0       0.0       0.0
*PART
$#                                                                         title
SPH Water
$#     pid     secid       mid     eosid      hgid      grav    adpopt      tmid
         6         6         6         0         0         0         0         0
*SECTION_SPH
$#   secid      cslh      hmin      hmax    sphini     death     start   sphkern
         6       1.0       1.0       1.0      12.0       0.0       0.0         0
*MAT_SPH_INCOMPRESSIBLE_FLUID
$#     mid        ro        mu    gamma1    gamma2     stens
         61.00000E-91.00000E-9   1000000    1000.0       0.0
*PART_INERTIA
$#                                                                         title
Moving cube mesh
$#     pid     secid       mid     eosid      hgid      grav    adpopt      tmid
         7         2         2         0         0         0         0         0
$#      xc        yc        zc        tm      ircs    nodeid
    -672.0       0.0     274.08.64000E-4         0         0
$#     ixx       ixy       ixz       iyy       iyz       izz
    2.0736       0.0       0.0    2.0736       0.0    2.0736
$#     vtx       vty       vtz       vrx       vry       vrz
    2000.0       0.0       0.0       0.0       0.0       0.0
*SECTION_SHELL
$#   secid    elform      shrf       nip     propt   qr/irid     icomp     setyp
         2         2       1.0         5       1.0         0         0         1
$#      t1        t2        t3        t4      nloc     marea      idof    edgset
       0.1       0.1       0.1       0.1       0.0       0.0       0.0         0
*MAT_RIGID
$#     mid        ro         e        pr         n    couple         m     alias
         21.00000E-9      10.0       0.3       0.0       0.0       0.0                    
$#     cmo      con1      con2
       1.0         5         4
$#lco or a1        a2        a3        v1        v2        v3
       0.0       0.0       0.0       0.0       0.0       0.0
*PART
$#                                                                         title
Walls Mesh
$#     pid     secid       mid     eosid      hgid      grav    adpopt      tmid
         8         3         3         0         0         0         0         0
*SECTION_SHELL
$#   secid    elform      shrf       nip     propt   qr/irid     icomp     setyp
         3         2       1.0         5       1.0         0         0         1
$#      t1        t2        t3        t4      nloc     marea      idof    edgset
       0.1       0.1       0.1       0.1       0.0       0.0       0.0         0
*MAT_RIGID
$#     mid        ro         e        pr         n    couple         m     alias
         31.00000E-9      10.0       0.3       0.0       0.0       0.0                    
$#     cmo      con1      con2
       1.0         7         7
$#lco or a1        a2        a3        v1        v2        v3
       0.0       0.0       0.0       0.0       0.0       0.0
*SECTION_SHELL
$#   secid    elform      shrf       nip     propt   qr/irid     icomp     setyp
         1         2       1.0         5       1.0         0         0         1
$#      t1        t2        t3        t4      nloc     marea      idof    edgset
       0.1       0.1       0.1       0.1       0.0       0.0       0.0         0
*MAT_RIGID
$#     mid        ro         e        pr         n    couple         m     alias
         11.00000E-9      10.0       0.3       0.0       0.0       0.0                    
$#     cmo      con1      con2
       1.0         7         7
$#lco or a1        a2        a3        v1        v2        v3
       0.0       0.0       0.0       0.0       0.0       0.0
*DEFINE_BOX
$#   boxid       xmn       xmx       ymn       ymx       zmn       zmx
         1    -750.0     800.0    -800.0     800.0    -100.0    3000.0
*DEFINE_SPH_MESH_SURFACE
$#     sid      type    sphpid    sphxid      nsid     space      iout
         8         1         4         0         0      12.0         0
*DEFINE_SPH_MESH_SURFACE
         7         1         5         0         0      12.0         0
*DEFINE_SPH_MESH_BOX
$#    xmin      ymin      zmin      xlen      ylen      zlen
    -588.0    -588.0       9.0    1176.0    1176.0     204.0
$#    ipid        nx        ny        nz               idseg      sfsp
         6        98        98        17         0         0       0.0
*DEFINE_SPH_MASSFLOW_PLANE
$#prtclsid   surfsid     ptype     stype
         6         1         3         1
*DEFINE_CURVE
$#    lcid      sidr       sfa       sfo      offa      offo    dattyp     lcint
         1         0       1.0       1.0       0.0       0.0         0         0
$#                a1                  o1
                 0.0                 0.5
                0.04                 0.5
                0.05                 1.0
                 0.1                 1.0
               100.0                 1.0
*DEFINE_CURVE
$#    lcid      sidr       sfa       sfo      offa      offo    dattyp     lcint
         2         0       1.0       1.0       0.0       0.0         0         0
$#                a1                  o1
                 0.0              3000.0
                 0.1              3000.0
                0.11                 0.0
                20.0                 0.0
*DEFINE_CURVE
$#    lcid      sidr       sfa       sfo      offa      offo    dattyp     lcint
         3         0       1.0       1.0       0.0       0.0         0         0
$#                a1                  o1
                 0.0               500.0
                 0.1               500.0
                0.11               500.0
                20.0               500.0
*DEFINE_CURVE
$#    lcid      sidr       sfa       sfo      offa      offo    dattyp     lcint
         4         0       1.0       1.0       0.0       0.0         0         0
$#                a1                  o1
                 0.0              9810.0
               100.0              9810.0
*SET_PART_LIST
$#     sid       da1       da2       da3       da4    solver
         1                                        MECH
$#    pid1      pid2      pid3      pid4      pid5      pid6      pid7      pid8
         8         0         0         0         0         0         0         0
*SET_PART_LIST
$#     sid       da1       da2       da3       da4    solver
         2                                        MECH
$#    pid1      pid2      pid3      pid4      pid5      pid6      pid7      pid8
         7         0         0         0         0         0         0         0
*SET_PART_LIST
$#     sid       da1       da2       da3       da4    solver
         3                                        MECH
$#    pid1      pid2      pid3      pid4      pid5      pid6      pid7      pid8
         6         0         0         0         0         0         0         0
*SET_PART_LIST
$#     sid       da1       da2       da3       da4    solver
         4                                        MECH
$#    pid1      pid2      pid3      pid4      pid5      pid6      pid7      pid8
         1         0         0         0         0         0         0         0
*SET_PART_LIST
$#     sid       da1       da2       da3       da4    solver
         5                                        MECH
$#    pid1      pid2      pid3      pid4      pid5      pid6      pid7      pid8
         7         0         0         0         0         0         0         0
*SET_PART_LIST
$#     sid       da1       da2       da3       da4    solver
         6                                        MECH
$#    pid1      pid2      pid3      pid4      pid5      pid6      pid7      pid8
         7         0         0         0         0         0         0         0
*CONSTRAINED_RIGID_BODIES
$#    pidl      pidc     iflag
         7         1         0
*END
