# Copyright (C) 2021 - 2024 ANSYS, Inc. and/or its affiliates.
# SPDX-License-Identifier: MIT
#
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

test_deck_004_string = """*INCLUDE
$#                                                                      filename
                                                                 /path/to/test.k

*DEFINE_CONTACT_VOLUME
$#    cvid       cid      type        xc        yc        zc
                             0                              
$#     xmn       xmx       ymn       ymx       zmn       zmx
       0.0       0.0       0.0       0.0       0.0       0.0
*END
*DEFINE_CONTACT_VOLUME
$#    cvid       cid      type        xc        yc        zc
                             1                              
$#  length    rinner    router    d_angc
       0.0       0.0       0.0       0.0
*END
    """

test_deck_006_string_1 = """*INCLUDE
$#                                                                      filename
                                                                 /path/to/test.k

*DEFINE_CONTACT_VOLUME
$#    cvid       cid      type        xc        yc        zc
                             0                              
$#     xmn       xmx       ymn       ymx       zmn       zmx
       0.0       0.0       0.0       0.0       0.0       0.0
*END
"""

test_deck_006_string_2 = """*DEFINE_CONTACT_VOLUME
$#    cvid       cid      type        xc        yc        zc
                             1                              
$#  length    rinner    router    d_angc
       0.0       0.0       0.0       0.0
*END
"""

test_deck_006_string_sum = """*INCLUDE
$#                                                                      filename
                                                                 /path/to/test.k

*DEFINE_CONTACT_VOLUME
$#    cvid       cid      type        xc        yc        zc
                             0                              
$#     xmn       xmx       ymn       ymx       zmn       zmx
       0.0       0.0       0.0       0.0       0.0       0.0
*DEFINE_CONTACT_VOLUME
$#    cvid       cid      type        xc        yc        zc
                             1                              
$#  length    rinner    router    d_angc
       0.0       0.0       0.0       0.0
*END
"""

test_kwdeck_basic_001_string = ["""*ALE_SMOOTHING
$#    dnid      nid1      nid2      ipre       xco       yco       zco
                             3         0       0.0       0.0       0.0""",
"""*BOUNDARY_PRECRACK
$#     pid     ctype        np
                   1          
$#       x         y         z
                              """,
"""*BOUNDARY_ACOUSTIC_COUPLING
$#    ssid
          """,
"""*BOUNDARY_TEMPERATURE_SET
$#    nsid      lcid     cmult       loc    tdeath    tbirth
                   0       1.0         0     1e+20       0.0"""]


test_kwlist_string = ["""*ALE_SMOOTHING
$#    dnid      nid1      nid2      ipre       xco       yco       zco
                             3         0       0.0       0.0       0.0""",
"""*BOUNDARY_PRECRACK
$#     pid     ctype        np
                   1          
$#       x         y         z
                              """]


test_title_string = """*Keyword
*DEFINE_CURVE_TITLE
title
$#    lcid      sidr       sfa       sfo      offa      offo    dattyp     lcint
         1               
$#                a1                  o1  
                 0.0                 0.0
                 1.0                 1.0"""

test_node_long_id = """*NODE
$#   nid               x               y               z      tc      rc
69000001 683.94961562637 -477.9565044024           -65.1       0       0"""

test_set_node_title = """*SET_NODE_TITLE
nodeset
  69000017                                        
  70332693  70540826  70540837  70540840  70540846  70540853  70540857  70540869
  70540871  70540875  70540887  70540888  70540890  70563790  70563792  70563794
  70573162"""

test_mat_piecewise_linear_plasticity_title = """*MAT_PIECEWISE_LINEAR_PLASTICITY_TITLE
mat24
  690000022.24507E-6        1.       0.3       0.1        0.     1.E30          
        0.        0.  69000001         0        0.
        0.        0.        0.        0.        0.        0.        0.        0.
        0.        0.        0.        0.        0.        0.        0.        0."""

test_constrained_nodal_rigid_body_inertia_title = """*CONSTRAINED_NODAL_RIGID_BODY_INERTIA_TITLE
Rigid Body Connection Element
  69002781            69000269                   0         0         0
                                      0.         0          
        0.        0.        0.        0.        0.        0.
                                                            """

test_contact_tied_shell_edge_to_surface_id = """*CONTACT_TIED_SHELL_EDGE_TO_SURFACE_ID
69000005  TIED_TEST3                                                    
  69000268  69000267         2         2                             0         0
                                                           0                    
                           -2.       -2.                                        
         0       0.1               1.025        0.         2         0         0
        0.         0         0         0         0         0        0.        0.
                   0        0.        0.                            0.         0
                                                           0         0          
         0         0         1         0                   0         0"""

test_section_solid_title_deck_string = """$
*KEYWORD
*SECTION_SOLID_TITLE
$#                                                                         title
section3                                                                        
$#   secid    elform       aet    unused    unused    unused    cohoff   gaskeit
  69000314        13                                                            
*END"""

test_hourglass_title = """*HOURGLASS_TITLE
$#                                                                         title
hello                                                                           
$#    hgid       ihq        qm       ibq        q1        q2    qb/vdc        qw
         0         0       0.1                 1.5      0.06       0.1       0.1"""

test_mesh_string = """*NODE
$#   nid               x               y               z      tc      rc
     100      -0.2969848       0.2969848             0.0                
     101      -0.2687006       0.2687006             0.0                
     104       -0.160727       0.3880294             0.0                
     105      -0.1454197       0.3510742             0.0                
     106      -0.2969848       0.2969848            0.25                
     107      -0.2687006       0.2687006            0.25                
     108      -0.1454197       0.3510742            0.25                
     109       -0.160727       0.3880294            0.25                
     110      -0.2969848       0.2969848             0.5                
     111      -0.2687006       0.2687006             0.5                
     112      -0.1454197       0.3510742             0.5                
     113       -0.160727       0.3880294             0.5                """

test_mesh_string_long = """$#               nid                   x                   y                   z                  tc                  rc
             2000000                                                                                                    
             2000001-2772.16528319999998 643.809570300000019 376.799041699999975                                        
             2000002-3093.88916019999988         685.0078125 811.224670400000036                   2                   5"""

test_section_shell_long = """*SECTION_TSHELL+
$#             secid              elform                shrf                 nip               propt                  qr               icomp              tshear
                                       1                 1.0                   2                 1.0                   0                   0                   0"""

test_text_card_long = "$#                                                                                                                                                      function"

test_duplicate_card_group_long = """                   1                   2                   1                   2                   3                   4                   5                   6                   7                   8
                 0.1                 0.2                 0.3
                 0.3                 0.4                 0.5
                   1                   2                   5                   6                   7                   8                   1                   3                   2                   4
                 0.2                 0.3                 0.4
                 0.4                 0.5                 0.6"""

test_read_segment_string = """*SET_SEGMENT
$#     sid       da1       da2       da3       da4    solver      
         2       0.0       0.0       0.0       0.0MECH
$#      n1        n2        n3        n4        a1        a2        a3        a4
      2145      2124      2004      2045       0.0       0.0       0.0       0.0
       262       265       264       263       0.0       0.0       0.0       0.0
       304       262       263       305       0.0       0.0       0.0       0.0
       263       264       385       384       0.0       0.0       0.0       0.0
       344       345       265       262       0.0       0.0       0.0       0.0
       265       445       444       264       0.0       0.0       0.0       0.0
      1265      1285       405       525       0.0       0.0       0.0       0.0
      1865      1844      1784      1805       0.0       0.0       0.0       0.0
       444       584       665       545       0.0       0.0       0.0       0.0
       905       845       803       344       0.0       0.0       0.0       0.0
       182      2204      2084      2165       0.0       0.0       0.0       0.0"""

test_read_nodes_string = """*NODE
$#   nid               x               y               z      tc      rc
 2000000                                                                
 2000001   -2772.1652832     643.8095703     376.7990417                
 2000002   -3093.8891602     685.0078125     811.2246704       1       5"""


test_load_segment_string = """*LOAD_SEGMENT
$#    lcid        sf        at        n1        n2        n3        n4        n5
                 1.0       0.0                                                  
$#      n6        n7        n8
                              """

test_load_segment_id_string = """*LOAD_SEGMENT_ID
$#      id                                                               heading
                                                                                
$#    lcid        sf        at        n1        n2        n3        n4        n5
                 1.0       0.0                                                  
$#      n6        n7        n8
                              """

test_ss_string = """*SECTION_SOLID
$#   secid    elform       aet    unused    unused    unused    cohoff   gaskeit
                   1         0                                                  """

test_ss_elform_101_string = """*SECTION_SOLID
$#   secid    elform       aet    unused    unused    unused    cohoff   gaskeit
                 101         0                                                  
$#     nip     nxdof      ihgf      itaj       lmc      nhsv
         0         0         0         0         0         0"""

test_ss_elform_101_nip_2_string = """*SECTION_SOLID
$#   secid    elform       aet    unused    unused    unused    cohoff   gaskeit
                 101         0                                                  
$#     nip     nxdof      ihgf      itaj       lmc      nhsv
         2         0         0         0         0         0
$#      xi       eta      zeta       wgt
       1.0       2.0       3.0          
       0.0                 3.0       5.0"""

test_ss_elform_101_nip_2_lmc_9_string = """*SECTION_SOLID
$#   secid    elform       aet    unused    unused    unused    cohoff   gaskeit
                 101         0                                                  
$#     nip     nxdof      ihgf      itaj       lmc      nhsv
         2         0         0         0         9         0
$#      xi       eta      zeta       wgt
       1.0       2.0       3.0          
       0.0                 3.0       5.0
$#      pi        pi        pi        pi        pi        pi        pi        pi
      22.0                                                                      
       3.7"""

test_repr_truncate = """*NODE
$#   nid               x               y               z      tc      rc
       1             0.1             0.2             0.3                
       2             0.1             0.2             0.3                
       3             0.1             0.2             0.3                
       4             0.1             0.2             0.3                
       5             0.1             0.2             0.3                
       6             0.1             0.2             0.3                
       7             0.1             0.2             0.3                
       8             0.1             0.2             0.3                
       9             0.1             0.2             0.3                
      10             0.1             0.2             0.3                
      11             0.1             0.2             0.3                
      12             0.1             0.2             0.3                
      13             0.1             0.2             0.3                
      14             0.1             0.2             0.3                
      15             0.1             0.2             0.3                
      16             0.1             0.2             0.3                
      17             0.1             0.2             0.3                
      18             0.1             0.2             0.3                
      19             0.1             0.2             0.3                
      20             0.1             0.2             0.3                
      21             0.1             0.2             0.3                
      22             0.1             0.2             0.3                
      23             0.1             0.2             0.3                
      24             0.1             0.2             0.3                
      25             0.1             0.2             0.3                
      26             0.1             0.2             0.3                
      27             0.1             0.2             0.3                
      28             0.1             0.2             0.3                
      29             0.1             0.2             0.3                
      30             0.1             0.2             0.3                
      31             0.1             0.2             0.3                
      32             0.1             0.2             0.3                
      33             0.1             0.2             0.3                
      34             0.1             0.2             0.3                
      35             0.1             0.2             0.3                
      36             0.1             0.2             0.3                
      37             0.1             0.2             0.3                
      38             0.1             0.2             0.3                
      39             0.1             0.2             0.3                
      40             0.1             0.2             0.3                
      41             0.1             0.2             0.3                
      42             0.1             0.2             0.3                
      43             0.1             0.2             0.3                
      44             0.1             0.2             0.3                
      45             0.1             0.2             0.3                
      46             0.1             0.2             0.3                
      47             0.1             0.2             0.3                
      48             0.1             0.2             0.3                
      49             0.1             0.2             0.3                
      50             0.1             0.2             0.3                
      51             0.1             0.2             0.3                
      52             0.1             0.2             0.3                
      53             0.1             0.2             0.3                
      54             0.1             0.2             0.3                
      55             0.1             0.2             0.3                
      56             0.1             0.2             0.3                
      57             0.1             0.2             0.3                
      58             0.1             0.2             0.3                
      59             0.1             0.2             0.3                
...console output truncated at 60 rows"""

test_control_timestep_string = """*CONTROL_TIMESTEP
$#  dtinit    tssfac      isdo    tslimt     dt2ms      lctm     erode     ms1st
     0.000  1.000000         0     0.000         1         0         0         0
$#  dt2msf   dt2mslc     imscl    unused    unused     rmscl
     0.000         0         0         0         0     0.000"""

element_shell_thickness_string = """*ELEMENT_SHELL_THICKNESS
$#   eid     pid      n1      n2      n3      n4      n5      n6      n7      n8
       1       1       1     105       2       2                                
$#         thic1           thic2           thic3           thic4            beta
             2.0      1.97992622      1.97992622      1.97992622      149.965326
       2       1     136     133    2834    2834                                
      1.98166233      1.98166233      1.98296441      1.98296441      146.006557
       3       1     141     146     135     135                                
      1.98187934      1.97949219      1.98280165      1.98280165      90.0245614"""

element_solid_ortho_legacy = """*ELEMENT_SOLID_ORTHO
$#   eid     pid      n1      n2      n3      n4      n5      n6      n7      n8
       1       1     100     101     105     104     106     107     108     109
$#            a1              a2              a3
             0.4             0.3             0.1
$#            d1              d2              d3
             0.1             0.8             0.2
       2       1     106     107     108     109     110     111     112     113
             0.1             0.9             0.6
             0.0             0.0             0.1"""

element_solid_ortho = """*ELEMENT_SOLID_ORTHO
       2       1
  113460   84468  108513   93160   93160   93160   93160   93160
 -0.38202947E+00 -0.54167800E+00 -0.74875793E+00
 -0.69952126E+00 -0.35575810E+00  0.61978420E+00
       4       1
  120411  117416  107358   95326   95326   95326   95326   95326
 -0.38362982E+00 -0.60972085E+00 -0.69359112E+00
 -0.66810543E+00 -0.33064264E+00  0.66629140E+00
      13       1
  137596  111105   86994   73710   73710   73710   73710   73710
 -0.42550327E+00  0.82862371E+00  0.36377153E+00
  0.91675701E+00  0.47040764E+00 -0.20517038E+00
      18       1
   88443   89157   11329   75544   75544   75544   75544   75544
 -0.30579024E+00  0.61987494E+00 -0.72266686E+00
 -0.76423011E+00 -0.59932774E+00 -0.20244976E+00
      22       1
  150916   13334   97846   13266   13266   13266   13266   13266
 -0.97424306E+00 -0.84574605E-01  0.20903969E+00
  0.13655429E+00 -0.93923419E+00  0.29649258E+00
      25       1
  135033   73847   97135  103790  103790  103790  103790  103790
  0.22365008E+00 -0.25664759E+00 -0.94027265E+00
 -0.68651675E+00 -0.72919995E+00  0.48139922E-01
      26       1
   91937  112774   23012   84735   84735   84735   84735   84735
  0.70414946E+00  0.70112248E+00  0.11225330E+00
  0.46290576E+00 -0.22225871E+00 -0.83740122E+00
      32       1
  133811   17236   17212   93623   93623   93623   93623   93623
 -0.13305977E+00  0.95968095E+00 -0.24760367E+00
 -0.49610372E+00 -0.28415181E+00 -0.82298215E+00
      33       1
  144681  117038  105023  109628  109628  109628  109628  109628
 -0.33819581E+00 -0.31573994E+00 -0.88652799E+00
 -0.87251946E+00 -0.23142307E+00  0.42387370E+00
      35       1
  101955   21559   82864  147841  147841  147841  147841  147841
 -0.17810912E-01 -0.84876699E+00 -0.52846701E+00
 -0.90909081E+00  0.65187542E+00  0.29298234E+00
      36       1
  105239   95883   12707   76218   76218   76218   76218   76218
 -0.85562496E+00 -0.45162384E+00  0.25286722E+00
  0.25206356E+00 -0.79120031E+00 -0.55517463E+00
      39       1
  139037  111414  115163  129585  129585  129585  129585  129585
 -0.20323083E+00  0.56606251E+00  0.79891831E+00
  0.12017049E+01  0.12070042E+00 -0.10234373E+00
      40       1
  149782   21384   21293   21341   21341   21341   21341   21341
 -0.73838156E-01 -0.96973688E+00 -0.23271939E+00
 -0.97928824E+00  0.37993690E-01  0.21714321E+00
      41       1
   75238   44648   62782   95224   95224   95224   95224   95224
 -0.39247078E+00  0.76295983E+00 -0.51367206E+00
  0.42604186E+00 -0.21652302E+00 -0.80781553E+00
      42       1
  149388    9991  102057  105002  105002  105002  105002  105002
 -0.31492819E+00 -0.89028936E+00  0.32894542E+00
  0.24494516E+00 -0.31474117E+00 -0.37481684E+00
      43       1
  140819  107613  104959  116934  116934  116934  116934  116934
  0.11852383E+00  0.25962342E+00 -0.95840898E+00
 -0.90649154E+00 -0.36575259E+00 -0.21347239E+00
      50       1
   25200   25107   25053   87191   87191   87191   87191   87191
  0.42532909E+00 -0.34368036E+00 -0.83724487E+00
 -0.76251208E+00 -0.64664040E+00 -0.99452525E-01
      55       1
  140506  116408  117768  105768  105768  105768  105768  105768
  0.42266038E+00  0.90056089E+00  0.10172658E+00
  0.11288187E+00  0.59722057E-01 -0.99234141E+00
      58       1
   77215   23362   23263   89535   89535   89535   89535   89535
  0.68015335E+00  0.49033793E-01 -0.73142813E+00
 -0.72779464E+00 -0.10228868E+00 -0.67930969E+00
      61       1
  150863   17503   90262   17332   17332   17332   17332   17332
 -0.99851596E+00  0.10192491E-01 -0.53497602E-01
  0.14054508E-01 -0.65924820E+00 -0.73557459E+00
      89       1
  135402   93139  107339  104073  104073  104073  104073  104073
  0.64308696E+00  0.69647747E+00 -0.31836817E+00
 -0.41275050E+00 -0.17576379E-01 -0.91063548E+00
      92       1
  148523   83927  112660   10967   10967   10967   10967   10967
  0.35467294E-02  0.64626611E+00  0.76310388E+00
  0.66636303E+00  0.56410311E+00 -0.46429149E+00
      93       1
  115624  112781  100995  131602  131602  131602  131602  131602
  0.48485856E+00  0.87458968E+00 -0.22521597E-02
  0.13657631E+00 -0.55675866E-01 -0.98928195E+00
     100       1
   81043   23878   23945   92222   92222   92222   92222   92222
  0.74361308E+00  0.20384523E+00 -0.63677838E+00
 -0.66461622E+00  0.20578186E+00 -0.71680878E+00
     104       1
   66790   83928   26832  150823  150823  150823  150823  150823
  0.99598687E+00  0.82739953E-01 -0.34121223E-01
 -0.98113061E-01  0.95291686E+00 -0.29432579E+00"""

test_initial_strain_shell_string = """*INITIAL_STRAIN_SHELL
$#     eid    nplane    nthick     large    unused    unused    unused    ilocal
         1         1         5         0                                       0
$#   epsxx     epsyy     epszz     epsxy     epsyz     epszx         t
       1.0                                                            
       1.0                                                            
       1.0                                                            
       1.0                                                            
       1.0                                                            
         2         1         5         0                                       0
                                    22.0                              
                                     2.0                              
                                     2.0                              
                                     2.0                              
                                     2.0                              """

test_initial_stress_shell_string_single_element_single_layer = """*INITIAL_STRESS_SHELL
         1         1         1        19         0         0         0         0
      -1.0       0.0       0.0       0.0       0.0       0.0       0.0     0.194
       0.0       0.0       0.0       0.0       0.0    0.0968       0.0       0.0
      0.44       0.0       0.0       0.0       0.0     0.119       0.0       0.0
       0.0    1.0E-4     0.311"""

test_initial_stress_shell_string_single_element_multiple_layers = """*INITIAL_STRESS_SHELL
         1         1         5        19         0         0         0         0
      -1.0       0.0       0.0       0.0       0.0       0.0       0.0     0.194
       0.0       0.0       0.0       0.0       0.0    0.0968       0.0       0.0
      0.44       0.0       0.0       0.0       0.0     0.119       0.0       0.0
       0.0    1.0E-4     0.311
      -0.5       0.0       0.0       0.0       0.0       0.0       0.0     0.192
       0.0       0.0       0.0       0.0       0.0     0.102       0.0       0.0
     0.448       0.0       0.0       0.0       0.0     0.126       0.0       0.0
       0.0    1.0E-4      0.32
       0.0       0.0       0.0       0.0       0.0       0.0       0.0     0.191
       0.0       0.0       0.0       0.0       0.0     0.107       0.0       0.0
     0.455       0.0       0.0       0.0       0.0     0.132       0.0       0.0
       0.0    1.0E-4     0.327
       0.5       0.0       0.0       0.0       0.0       0.0       0.0     0.192
       0.0       0.0       0.0       0.0       0.0     0.109       0.0       0.0
     0.455       0.0       0.0       0.0       0.0     0.135       0.0       0.0
       0.0    1.0E-4      0.33
       1.0       0.0       0.0       0.0       0.0       0.0       0.0     0.193
       0.0       0.0       0.0       0.0       0.0     0.111       0.0       0.0
     0.455       0.0       0.0       0.0       0.0     0.137       0.0       0.0
       0.0    1.0E-4     0.333"""

test_initial_stress_shell_string = """*INITIAL_STRESS_SHELL
         1         1         5        19         0         0         0         0
      -1.0       0.0       0.0       0.0       0.0       0.0       0.0     0.194
       0.0       0.0       0.0       0.0       0.0    0.0968       0.0       0.0
      0.44       0.0       0.0       0.0       0.0     0.119       0.0       0.0
       0.0    1.0E-4     0.311
      -0.5       0.0       0.0       0.0       0.0       0.0       0.0     0.192
       0.0       0.0       0.0       0.0       0.0     0.102       0.0       0.0
     0.448       0.0       0.0       0.0       0.0     0.126       0.0       0.0
       0.0    1.0E-4      0.32
       0.0       0.0       0.0       0.0       0.0       0.0       0.0     0.191
       0.0       0.0       0.0       0.0       0.0     0.107       0.0       0.0
     0.455       0.0       0.0       0.0       0.0     0.132       0.0       0.0
       0.0    1.0E-4     0.327
       0.5       0.0       0.0       0.0       0.0       0.0       0.0     0.192
       0.0       0.0       0.0       0.0       0.0     0.109       0.0       0.0
     0.455       0.0       0.0       0.0       0.0     0.135       0.0       0.0
       0.0    1.0E-4      0.33
       1.0       0.0       0.0       0.0       0.0       0.0       0.0     0.193
       0.0       0.0       0.0       0.0       0.0     0.111       0.0       0.0
     0.455       0.0       0.0       0.0       0.0     0.137       0.0       0.0
       0.0    1.0E-4     0.333
         2         1         5        19         0         0         0         0
      -1.0       0.0       0.0       0.0       0.0       0.0       0.0     0.208
       0.0       0.0       0.0       0.0       0.0     0.104       0.0       0.0
     0.433       0.0       0.0       0.0       0.0     0.129       0.0       0.0
       0.0    1.0E-4     0.323
      -0.5       0.0       0.0       0.0       0.0       0.0       0.0     0.207
       0.0       0.0       0.0       0.0       0.0     0.122       0.0       0.0
     0.451       0.0       0.0       0.0       0.0     0.151       0.0       0.0
       0.0    1.0E-4      0.35
       0.0       0.0       0.0       0.0       0.0       0.0       0.0     0.205
       0.0       0.0       0.0       0.0       0.0     0.141       0.0       0.0
     0.468       0.0       0.0       0.0       0.0     0.175       0.0       0.0
       0.0    1.0E-4     0.376
       0.5       0.0       0.0       0.0       0.0       0.0       0.0     0.212
       0.0       0.0       0.0       0.0       0.0     0.152       0.0       0.0
     0.469       0.0       0.0       0.0       0.0     0.188       0.0       0.0
       0.0    1.0E-4      0.39
       1.0       0.0       0.0       0.0       0.0       0.0       0.0     0.219
       0.0       0.0       0.0       0.0       0.0     0.163       0.0       0.0
     0.469       0.0       0.0       0.0       0.0     0.201       0.0       0.0
       0.0    1.0E-4     0.404
         3         1         5        19         0         0         0         0
      -1.0       0.0       0.0       0.0       0.0       0.0       0.0     0.723
       0.0       0.0       0.0       0.0       0.0    0.0209       0.0       0.0
    -0.137       0.0       0.0       0.0       0.0    0.0698       0.0       0.0
       0.0    1.0E-4     0.145
      -0.5       0.0       0.0       0.0       0.0       0.0       0.0     0.636
       0.0       0.0       0.0       0.0       0.0    0.0332       0.0       0.0
   -0.0591       0.0       0.0       0.0       0.0    0.0753       0.0       0.0
       0.0    1.0E-4     0.182
       0.0       0.0       0.0       0.0       0.0       0.0       0.0     0.564
       0.0       0.0       0.0       0.0       0.0     0.292       0.0       0.0
    0.0423       0.0       0.0       0.0       0.0     0.361       0.0       0.0
       0.0    1.0E-4     0.541
       0.5       0.0       0.0       0.0       0.0       0.0       0.0     0.582
       0.0       0.0       0.0       0.0       0.0       0.7       0.0       0.0
      0.13       0.0       0.0       0.0       0.0     0.974       0.0       0.0
       0.0    1.0E-4     0.837
       1.0       0.0       0.0       0.0       0.0       0.0       0.0     0.508
       0.0       0.0       0.0       0.0       0.0       0.7      0.63       1.0
     0.209       0.0       0.0       0.0       0.0       1.0       0.0       0.0
       0.0    1.0E-4     0.837"""

test_define_function_string = """*DEFINE_FUNCTION
$#     fid                                                               heading
                                                                                
$#                                                                      function
1,x-velo
x(t)=1000*sin(100*t)
*DEFINE_FUNCTION
2,z-velo
a(t)=x(t)+200"""

test_set_shell_intersect_ref_1 = """*SET_SHELL_INTERSECT
$#     sid
          """

test_set_shell_intersect_ref_2 = """*SET_SHELL_INTERSECT_TITLE
$#                                                                         title
                                                                                
$#     sid
          """

test_set_shell_intersect_ref_3 = """*SET_SHELL_INTERSECT_TITLE
$#                                                                         title
hello                                                                           
$#     sid
          """

test_parameter_expression_ref = """*PARAMETER_EXPRESSION
$#    prmr                                                            expression
R PE_200  1+1+1                                                                 
R PE_300  1+1+1                                                                 
R PE_400  1+1+1                                                                 """

test_define_transformation_ref = """*DEFINE_TRANSFORMATION
$#  tranid
         1
$#  option        a1        a2        a3        a4        a5        a6        a7
TRANSL           1.0       0.0       0.0                                        
TRANSL           2.0       0.0       0.0                                        
TRANSL           3.0       0.0       0.0                                        """

test_mat_plastic_kinematic_ref = """*MAT_PLASTIC_KINEMATIC
$#     mid        ro         e        pr      sigy      etan      beta
         1       0.0       0.0       0.0       0.0       0.0       0.0
$#     src       srp        fs        vp
       0.0       0.0       0.0       0.0"""

test_mat_null_ref = """*MAT_NULL
$#     mid        ro        pc        mu     terod     cerod        ym        pr
         2       0.0       0.0       0.0       0.0       0.0       0.0       0.0"""

test_mat_piecewise_linear_plasticity_ref = """*MAT_PIECEWISE_LINEAR_PLASTICITY
$#     mid        ro         e        pr      sigy      etan      fail      tdel
         3       0.0       0.0       0.0       0.0       0.0     1e+21       0.0
$#       c         p      lcss      lcsr        vp
       0.0       0.0         0         0       0.0
$#    eps1      eps2      eps3      eps4      eps5      eps6      eps7      eps8
       0.0       0.0       0.0       0.0       0.0       0.0       0.0       0.0
$#     es1       es2       es3       es4       es5       es6       es7       es8
       0.0       0.0       0.0       0.0       0.0       0.0       0.0       0.0"""

test_mat_piecewise_linear_plasticity_2d_ref = """*MAT_PIECEWISE_LINEAR_PLASTICITY_2D
$#     mid        ro         e        pr      sigy      etan      fail      tdel
         4       0.0       0.0       0.0       0.0       0.0     1e+21       0.0
$#       c         p      lcss      lcsr        vp
       0.0       0.0         0         0       0.0
$#    eps1      eps2      eps3      eps4      eps5      eps6      eps7      eps8
       0.0       0.0       0.0       0.0       0.0       0.0       0.0       0.0
$#     es1       es2       es3       es4       es5       es6       es7       es8
       0.0       0.0       0.0       0.0       0.0       0.0       0.0       0.0"""

test_mat_piecewise_linear_plasticity_haz_ref = """*MAT_PIECEWISE_LINEAR_PLASTICITY_HAZ
$#     mid        ro         e        pr      sigy      etan      fail      tdel
         5       0.0       0.0       0.0       0.0       0.0     1e+21       0.0
$#       c         p      lcss      lcsr        vp
       0.0       0.0         0         0       0.0
$#    eps1      eps2      eps3      eps4      eps5      eps6      eps7      eps8
       0.0       0.0       0.0       0.0       0.0       0.0       0.0       0.0
$#     es1       es2       es3       es4       es5       es6       es7       es8
       0.0       0.0       0.0       0.0       0.0       0.0       0.0       0.0"""

test_mat_piecewise_linear_plasticity_log_interpolation_ref = """*MAT_PIECEWISE_LINEAR_PLASTICITY_LOG_INTERPOLATION
$#     mid        ro         e        pr      sigy      etan      fail      tdel
         6       0.0       0.0       0.0       0.0       0.0     1e+21       0.0
$#       c         p      lcss      lcsr        vp
       0.0       0.0         0         0       0.0
$#    eps1      eps2      eps3      eps4      eps5      eps6      eps7      eps8
       0.0       0.0       0.0       0.0       0.0       0.0       0.0       0.0
$#     es1       es2       es3       es4       es5       es6       es7       es8
       0.0       0.0       0.0       0.0       0.0       0.0       0.0       0.0"""

test_mat_piecewise_linear_plasticity_midfail_ref = """*MAT_PIECEWISE_LINEAR_PLASTICITY_MIDFAIL
$#     mid        ro         e        pr      sigy      etan      fail      tdel
         7       0.0       0.0       0.0       0.0       0.0     1e+21       0.0
$#       c         p      lcss      lcsr        vp
       0.0       0.0         0         0       0.0
$#    eps1      eps2      eps3      eps4      eps5      eps6      eps7      eps8
       0.0       0.0       0.0       0.0       0.0       0.0       0.0       0.0
$#     es1       es2       es3       es4       es5       es6       es7       es8
       0.0       0.0       0.0       0.0       0.0       0.0       0.0       0.0"""

test_mat_piecewise_linear_plasticity_stochastic_ref = """*MAT_PIECEWISE_LINEAR_PLASTICITY_STOCHASTIC
$#     mid        ro         e        pr      sigy      etan      fail      tdel
         8       0.0       0.0       0.0       0.0       0.0     1e+21       0.0
$#       c         p      lcss      lcsr        vp
       0.0       0.0         0         0       0.0
$#    eps1      eps2      eps3      eps4      eps5      eps6      eps7      eps8
       0.0       0.0       0.0       0.0       0.0       0.0       0.0       0.0
$#     es1       es2       es3       es4       es5       es6       es7       es8
       0.0       0.0       0.0       0.0       0.0       0.0       0.0       0.0"""

test_mat_laminated_composite_fabric_ref = """*MAT_LAMINATED_COMPOSITE_FABRIC
$#     mid        ro        ea        eb        ec      prba      tau1    gamma1
         9       0.0       0.0       0.0       0.0       0.0       0.0       0.0
$#     gab       gbc       gca    slimt1    slimc1    slimt2    slimc2     slims
       0.0       0.0       0.0       0.0       0.0       0.0       0.0       0.0
$#    aopt     tsize     erods      soft        fs      epsf      epsr      tsmd
       0.0       0.0       0.0       0.0       0.0       0.0       0.0       0.9
$#      xp        yp        zp        a1        a2        a3      prca      prcb
       0.0       0.0       0.0       0.0       0.0       0.0       0.0       0.0
$#      v1        v2        v3        d1        d2        d3      beta   lcdfail
       0.0       0.0       0.0       0.0       0.0       0.0       0.0         0
$#    e11c      e11t      e22c      e22t       gms
       0.0       0.0       0.0       0.0       0.0
$#      xc        xt        yc        yt        sc
       0.0       0.0       0.0       0.0       0.0
$#    lcxc      lcxt      lcyc      lcyt      lcsc     lctau     lcgam        dt
         0         0         0         0         0         0         0       0.0
$#  lce11c    lce11t    lce22c    lce22t     lcgms     lcefs
         0         0         0         0         0         0"""

test_mat_laminated_composite_fabric_solid_ref = """*MAT_LAMINATED_COMPOSITE_FABRIC_SOLID
$#     mid        ro        ea        eb        ec      prba      tau1    gamma1
        10       0.0       0.0       0.0       0.0       0.0       0.0       0.0
$#     gab       gbc       gca    slimt1    slimc1    slimt2    slimc2     slims
       0.0       0.0       0.0       0.0       0.0       0.0       0.0       0.0
$#    aopt     tsize     erods      soft        fs      epsf      epsr      tsmd
       0.0       0.0       0.0       0.0       0.0       0.0       0.0       0.9
$#      xp        yp        zp        a1        a2        a3      prca      prcb
       0.0       0.0       0.0       0.0       0.0       0.0       0.0       0.0
$#      v1        v2        v3        d1        d2        d3      beta   lcdfail
       0.0       0.0       0.0       0.0       0.0       0.0       0.0         0
$#    e11c      e11t      e22c      e22t       gms
       0.0       0.0       0.0       0.0       0.0
$#      xc        xt        yc        yt        sc
       0.0       0.0       0.0       0.0       0.0
$#    e33c      e33t      gm23      gm31
       0.0       0.0       0.0       0.0
$#      zc        zt      sc23      sc31
       0.0       0.0       0.0       0.0
$#  slimt3    slimc3   slims23   lsims31      tau2    gamma2      tau3    gamma3
       0.0       0.0       0.0       0.0       0.0       0.0       0.0       0.0
$#    lcxc      lcxt      lcyc      lcyt      lcsc     lctau     lcgam        dt
         0         0         0         0         0         0         0       0.0
$#  lce11c    lce11t    lce22c    lce22t     lcgms     lcefs
         0         0         0         0         0         0
$#    lczc      lczt    lcsc23    lcsc31    lctau2    lcgam2    lctau3    lcgam3
         0         0         0         0         0         0         0         0
$#  lce33c    lce33t   lcgms23   lcgms31
         0         0         0         0"""

test_mat_hyperelastic_rubber_ref = """*MAT_HYPERELASTIC_RUBBER
$#     mid        ro        pr         n        nv         g      sigf       ref
        11       0.0       0.0         0         0       0.0       0.0       0.0
$#     c10       c01       c11       c20       c02       c30    therml
       0.0       0.0       0.0       0.0       0.0       0.0       0.0"""

test_mat_ogden_rubber_ref = """*MAT_OGDEN_RUBBER
$#     mid        ro        pr         n        nv         g      sigf       ref
        12       0.0       0.0         0         6       0.0       0.0       0.0
$#     mu1       mu2       mu3       mu4       mu5       mu6       mu7       mu8
       0.0       0.0       0.0       0.0       0.0       0.0       0.0       0.0
$#  alpha1    alpha2    alpha3    alpha4    alpha5    alpha6    alpha7    alpha8
       0.0       0.0       0.0       0.0       0.0       0.0       0.0       0.0"""

test_mat_fu_chang_foam_ref = """*MAT_FU_CHANG_FOAM
$#     mid        ro         e      kcon        tc      fail      damp      tbid
        13       0.0       0.0       0.0     1e+20       0.0       0.0         0
$#  bvflag     sflag     rflag     tflag      pvid      sraf       ref        hu
       0.0       0.0       0.0       0.0         0       0.0       0.0       0.0
$#      d0        n0        n1        n2        n3        c0        c1        c2
       0.0       0.0       0.0       0.0       0.0       0.0       0.0       0.0
$#      c3        c4        c5       aij       sij      minr      maxr     shape
       0.0       0.0       0.0       0.0       0.0       0.0       0.0       0.0
$#   expon     riuld
       1.0       0.0"""

test_mat_fu_chang_foam_damage_decay_ref = """*MAT_FU_CHANG_FOAM_DAMAGE_DECAY
$#     mid        ro         e      kcon        tc      fail      damp      tbid
        14       0.0       0.0       0.0     1e+20       0.0       0.0         0
$#  bvflag     sflag     rflag     tflag      pvid      sraf       ref        hu
       0.0       0.0       0.0       0.0         0       0.0       0.0       0.0
$#    minr      maxr     shape     betat     betac
       0.0       0.0       0.0       0.0       0.0
$#   expon     riuld
       1.0       0.0"""

test_mat_fu_chang_foam_log_log_interpolation_ref = """*MAT_FU_CHANG_FOAM_LOG_LOG_INTERPOLATION
$#     mid        ro         e      kcon        tc      fail      damp      tbid
        15       0.0       0.0       0.0     1e+20       0.0       0.0         0
$#  bvflag     sflag     rflag     tflag      pvid      sraf       ref        hu
       0.0       0.0       0.0       0.0         0       0.0       0.0       0.0
$#      d0        n0        n1        n2        n3        c0        c1        c2
       0.0       0.0       0.0       0.0       0.0       0.0       0.0       0.0
$#      c3        c4        c5       aij       sij      minr      maxr     shape
       0.0       0.0       0.0       0.0       0.0       0.0       0.0       0.0
$#   expon     riuld
       1.0       0.0"""

test_mat_modified_johnson_cook_ref = """*MAT_MODIFIED_JOHNSON_COOK
$#     mid        ro         e        pr      beta       xsi        cp     alpha
        16       0.0       0.0       0.0       0.0       0.0       0.0       0.0
$#   e0dot        tr        tm        t0     flag1     flag2
       0.0       0.0       0.0       0.0       0.0       0.0
$#  a/siga       b/b   n/beta0   c/beta1      m/na
       0.0       0.0       0.0       0.0       0.0
$#    q1/a      c1/n q2/alpha0 c2/alpha1
       0.0       0.0       0.0       0.0
$#   dc/dc     pd/wc     d1/na     d2/na     d3/na     d4/na     d5/na
       0.0       0.0       0.0       0.0       0.0       0.0       0.0
$#      tc      tauc
       0.0       0.0"""

test_mat_modified_piecewise_linear_plasticity_ref = """*MAT_MODIFIED_PIECEWISE_LINEAR_PLASTICITY
$#     mid        ro         e        pr      sigy      etan      fail      tdel
        17       0.0       0.0       0.0       0.0       0.0     1e+21       0.0
$#       c         p      lcss      lcsr        vp   epsthin    epsmaj    numint
       0.0       0.0         0         0       0.0       0.0       0.0       0.0
$#    eps1      eps2      eps3      eps4      eps5      eps6      eps7      eps8
       0.0       0.0       0.0       0.0       0.0       0.0       0.0       0.0
$#     es1       es2       es3       es4       es5       es6       es7       es8
       0.0       0.0       0.0       0.0       0.0       0.0       0.0       0.0"""

test_mat_modified_piecewise_linear_plasticity_log_interpolation_ref = """*MAT_MODIFIED_PIECEWISE_LINEAR_PLASTICITY_LOG_INTERPOLATION
$#     mid        ro         e        pr      sigy      etan      fail      tdel
        18       0.0       0.0       0.0       0.0       0.0     1e+21       0.0
$#       c         p      lcss      lcsr        vp   epsthin    epsmaj    numint
       0.0       0.0         0         0       0.0       0.0       0.0       0.0
$#    eps1      eps2      eps3      eps4      eps5      eps6      eps7      eps8
       0.0       0.0       0.0       0.0       0.0       0.0       0.0       0.0
$#     es1       es2       es3       es4       es5       es6       es7       es8
       0.0       0.0       0.0       0.0       0.0       0.0       0.0       0.0"""

test_mat_modified_piecewise_linear_plasticity_prestrain_ref = """*MAT_MODIFIED_PIECEWISE_LINEAR_PLASTICITY_PRESTRAIN
$#     mid        ro         e        pr      sigy      etan      fail      tdel
        19       0.0       0.0       0.0       0.0       0.0     1e+21       0.0
$#       c         p      lcss      lcsr        vp   epsthin    epsmaj    numint
       0.0       0.0         0         0       0.0       0.0       0.0       0.0
$#    eps1      eps2      eps3      eps4      eps5      eps6      eps7      eps8
       0.0       0.0       0.0       0.0       0.0       0.0       0.0       0.0
$#     es1       es2       es3       es4       es5       es6       es7       es8
       0.0       0.0       0.0       0.0       0.0       0.0       0.0       0.0
$#  lctsrf      eps0     triax       ips    lcemod      beta    rfiltf
         0       0.0       0.0         0         0       0.0       0.0"""

test_mat_modified_piecewise_linear_plasticity_rate_ref = """*MAT_MODIFIED_PIECEWISE_LINEAR_PLASTICITY_RATE
$#     mid        ro         e        pr      sigy      etan      fail      tdel
        20       0.0       0.0       0.0       0.0       0.0     1e+21       0.0
$#       c         p      lcss      lcsr        vp   epsthin    epsmaj    numint
       0.0       0.0         0         0       0.0       0.0       0.0       0.0
$#    eps1      eps2      eps3      eps4      eps5      eps6      eps7      eps8
       0.0       0.0       0.0       0.0       0.0       0.0       0.0       0.0
$#     es1       es2       es3       es4       es5       es6       es7       es8
       0.0       0.0       0.0       0.0       0.0       0.0       0.0       0.0
$#  lctsrf      eps0     triax       ips    lcemod      beta    rfiltf
         0       0.0       0.0         0         0       0.0       0.0"""

test_mat_modified_piecewise_linear_plasticity_rtcl_ref = """*MAT_MODIFIED_PIECEWISE_LINEAR_PLASTICITY_RTCL
$#     mid        ro         e        pr      sigy      etan      fail      tdel
        21       0.0       0.0       0.0       0.0       0.0     1e+21       0.0
$#       c         p      lcss      lcsr        vp   epsthin    epsmaj    numint
       0.0       0.0         0         0       0.0       0.0       0.0       0.0
$#    eps1      eps2      eps3      eps4      eps5      eps6      eps7      eps8
       0.0       0.0       0.0       0.0       0.0       0.0       0.0       0.0
$#     es1       es2       es3       es4       es5       es6       es7       es8
       0.0       0.0       0.0       0.0       0.0       0.0       0.0       0.0
$#  lctsrf      eps0     triax       ips    lcemod      beta    rfiltf
         0       0.0       0.0         0         0       0.0       0.0"""

test_mat_modified_piecewise_linear_plasticity_stochastic_ref = """*MAT_MODIFIED_PIECEWISE_LINEAR_PLASTICITY_STOCHASTIC
$#     mid        ro         e        pr      sigy      etan      fail      tdel
        22       0.0       0.0       0.0       0.0       0.0     1e+21       0.0
$#       c         p      lcss      lcsr        vp   epsthin    epsmaj    numint
       0.0       0.0         0         0       0.0       0.0       0.0       0.0
$#    eps1      eps2      eps3      eps4      eps5      eps6      eps7      eps8
       0.0       0.0       0.0       0.0       0.0       0.0       0.0       0.0
$#     es1       es2       es3       es4       es5       es6       es7       es8
       0.0       0.0       0.0       0.0       0.0       0.0       0.0       0.0"""

test_mat_plasticity_compression_tension_ref = """*MAT_PLASTICITY_COMPRESSION_TENSION
$#     mid        ro         e        pr         c         p      fail      tdel
        23       0.0       0.0       0.0       0.0       0.0     1e+20       0.0
$#   lcidc     lcidt     lcsrc     lcsrt    srflag    lcfail        ec      rpct
         0         0         0         0       0.0         0       0.0       0.0
$#      pc        pt     pcutc     pcutt     pcutf    unused    unused    srfilt
       0.0       0.0       0.0       0.0       0.0                           0.0
$#       k
       0.0"""

test_mat_cohesive_mixed_mode_ref = """*MAT_COHESIVE_MIXED_MODE
$#     mid        ro     roflg   intfail        en        et       gic      giic
        24       0.0         0       0.0       0.0       0.0       0.0       0.0
$#     xmu         t         s       und       utd     gamma
       0.0       0.0       0.0       0.0       0.0       1.0"""

test_mat_simplified_rubber_foam_ref = """*MAT_SIMPLIFIED_RUBBER/FOAM
$#     mid        ro        km        mu         g      sigf       ref     prten
        25       0.0       0.0       0.1       0.0       0.0       0.0       0.0
$#     sgl        sw        st   lc/tbid   tension     rtype    avgopt       pra
       0.0       0.0       0.0         0      -1.0       0.0       0.0       0.0
$#  lcunld        hu     shape      stol     visco    hisout
         0       1.0       0.0       0.0       0.0       0.0"""

test_mat_simplified_rubber_foam_log_log_interpolation_ref = """*MAT_SIMPLIFIED_RUBBER/FOAM_LOG_LOG_INTERPOLATION
$#     mid        ro        km        mu         g      sigf       ref     prten
        26       0.0       0.0       0.1       0.0       0.0       0.0       0.0
$#     sgl        sw        st   lc/tbid   tension     rtype    avgopt       pra
       0.0       0.0       0.0         0      -1.0       0.0       0.0       0.0
$#  lcunld        hu     shape      stol     visco    hisout
         0       1.0       0.0       0.0       0.0       0.0"""

test_mat_simplified_rubber_foam_with_failure_ref = """*MAT_SIMPLIFIED_RUBBER/FOAM_WITH_FAILURE
$#     mid        ro        km        mu         g      sigf       ref     prten
        27       0.0       0.0       0.1       0.0       0.0       0.0       0.0
$#     sgl        sw        st   lc/tbid   tension     rtype    avgopt       pra
       0.0       0.0       0.0         0      -1.0       0.0       0.0       0.0
$#       k     gama1     gama2        eh
       0.0       0.0       0.0       0.0
$#  lcunld        hu     shape      stol     visco    hisout
         0       1.0       0.0       0.0       0.0       0.0"""

test_mat_simplified_rubber_foam_with_failure_log_log_interpolation_ref = """*MAT_SIMPLIFIED_RUBBER/FOAM_WITH_FAILURE_LOG_LOG_INTERPOLATION
$#     mid        ro        km        mu         g      sigf       ref     prten
        28       0.0       0.0       0.1       0.0       0.0       0.0       0.0
$#     sgl        sw        st   lc/tbid   tension     rtype    avgopt       pra
       0.0       0.0       0.0         0      -1.0       0.0       0.0       0.0
$#       k     gama1     gama2        eh
       0.0       0.0       0.0       0.0
$#  lcunld        hu     shape      stol     visco    hisout
         0       1.0       0.0       0.0       0.0       0.0"""

test_mat_295_ref = """*MAT_295
$#     mid       rho      aopt
         1     0.001       2.0
$#   title     itype      beta        nu
ISO               -3       0.0     0.499
$#      k1        k2
   0.00236      1.75
$#   title     atype    intype        nf
ANISO             -1         0         1
$#   theta         a         b
       0.0       0.0       1.0
$#   ftype      fcid        k1        k2
         1         0   0.00049      9.01
$#   title    actype     acdir      acid     acthr        sf        ss        sn
ACTIVE             1         1         2     2.175       1.0       0.0       0.0
$#      t0    ca2ion   ca2ionm         n    taumax       stf         b        l0
                          4.35       2.0     0.125       0.0      4.75      1.58
$#       l     dtmax        mr        tr
      1.85     150.0    1048.9   -1429.0
$#      xp        yp        zp        a1        a2        a3      macf    unused
                                     1.0       0.0       0.0         1          
$#      v1        v2        v3        d1        d2        d3      beta       ref
                                     0.0       1.0       0.0                    """

test_set_part_list_ref = """*SET_PART_LIST
$#     sid       da1       da2       da3       da4    solver
         1       0.0       0.0       0.0       0.0MECH      
$#   parts     parts     parts
         1         2         3"""

test_contact_1d_id_mpp1_mpp2 = """*CONTACT_1D_ID_MPP
$#     cid                                                               heading
                                                                                
$#  ignore      bckt    lcbckt    ns2trk   inititr    parmax    unused    cparm8
         0       200                   3         2    1.0005                   0
$#    mpp2   chksegs     pensf   grpable
&                  0       1.0         0
$#   nsidr     nsidc       err      sigc        gb      smax       exp
                           0.0       0.0       0.0       0.0       0.0"""

test_contact_1d_mpp1 = """*CONTACT_1D_MPP
$#  ignore      bckt    lcbckt    ns2trk   inititr    parmax    unused    cparm8
         0       200                   3         2    1.0005                   0
$#   nsidr     nsidc       err      sigc        gb      smax       exp
                           0.0       0.0       0.0       0.0       0.0"""

test_contact_automatic_single_surface = """*CONTACT_AUTOMATIC_SINGLE_SURFACE
$#    ssid      msid     sstyp     mstyp    sboxid    mboxid       spr       mpr
         1                                                                      
$#      fs        fd        dc        vc       vdc    penchk        bt        dt
       0.0       0.0       0.0       0.0       0.0         0       0.0       0.0
$#     sfs       sfm       sst       mst      sfst      sfmt       fsf       vsf
       1.0       1.0                           1.0       1.0       1.0       1.0"""

test_em_control_string = """*EM_CONTROL
$#   emsol     numls   macrodt   dimtype    nperio    unused   ncylfem   ncylbem
        -1       100                   0         2                5000      5000"""

test_long_deck_standard_keyword_string = """*KEYWORD LONG=Y
*SECTION_SEATBELT-
$#   secid      area     thick
                              
*END"""

test_standard_deck_string = """*KEYWORD LONG=S
*SECTION_SEATBELT
$#   secid      area     thick
                              
*END"""

test_long_deck_string = """*KEYWORD LONG=Y
*SECTION_SEATBELT
$#             secid                area               thick
                                                            
*END"""

test_variable_card_string = """$#                bi                  bi                  bi                  bi                  bi                  bi                  bi                  bi
                 0.0                 1.0                 2.0                 3.0                 4.0                 5.0                 6.0                 7.0
                 8.0                 9.0"""

test_deck_with_unknown_keywords = """*KEYWORD
*NOT_REAL_KEYWORD
$#    what        is      this
                              
*END"""

test_variable_card_sets_string = """$
*KEYWORD
*SET_PART_LIST_TITLE
$#                                                                         title
test                                                                            
$#     sid       da1       da2       da3       da4    solver
         1                                                  
$#   parts
         1
*SET_PART_LIST_TITLE
$#                                                                         title
test2                                                                           
$#     sid       da1       da2       da3       da4    solver
         2                                                  
$#   parts     parts     parts     parts     parts     parts     parts     parts
         1         2         3         4         5         6         7         8
*END"""

test_default_card_em_isopotential_connect_string = """*EM_ISOPOTENTIAL_CONNECT
$#   conid   contype    isoid1    isoid2       vallcid/rdlid      psid
                   1                                                  """

test_conditional_card_em_isopotential_connect_string = """*EM_ISOPOTENTIAL_CONNECT
$#   conid   contype    isoid1    isoid2       vallcid/rdlid      psid
                   6                                                  
$#       l         c        v0
                              """
test_contact_tied_shell_edge_to_surface_beam_offset_opt_cards1 = """*CONTACT_TIED_SHELL_EDGE_TO_SURFACE_BEAM_OFFSET
$#   surfa     surfb  surfatyp  surfbtyp   saboxid   sbboxid      sapr      sbpr
         1         2         3         3         0         0         0         0
$#      fs        fd        dc        vc       vdc    penchk        bt        dt
       0.3       0.0       0.0      20.0       0.0         0       0.0     1e+20
$#    sfsa      sfsb      sast      sbst     sfsat     sfsbt       fsf       vsf
       1.0       1.0       0.0       0.0       1.0       1.0       1.0       1.0
$#    soft    sofscl    lcidab    maxpar     sbopt     depth     bsort    frcfrq
         2       0.1         0     1.025         2         2         0         1
$#  penmax    thkopt    shlthk     snlog      isym     i2d3d    sldthk    sldstf
       0.0         0         0         0         0         0       0.0       0.0
$#    igap    ignore    dprfac    dtstif     edgek              flangl   cid_rcf
         1         2       0.0       0.0       0.0                 0.0         0
$#   q2tri    dtpchk     sfnbr    fnlscl    dnlscl      tcso    tiedid    shledg
         0       0.0       0.0       0.0       0.0         0         1         0
$#  sharec    cparm8    ipback     srnde    fricsf      icor     ftorq    region
         0         0         0         0       1.0         0         0         0
$#  pstiff   ignroff               fstol    2dbinr    ssftyp     swtpr    tetfac
         1         0                 2.0         0         0         0       0.0
$#            shloff
                 0.0"""

test_contact_tied_shell_edge_to_surface_beam_offset_opt_cards2 = """*CONTACT_TIED_SHELL_EDGE_TO_SURFACE_BEAM_OFFSET
$#   surfa     surfb  surfatyp  surfbtyp   saboxid   sbboxid      sapr      sbpr
         1         2         3         3         0         0         0         0
$#      fs        fd        dc        vc       vdc    penchk        bt        dt
       0.3       0.0       0.0      20.0       0.0         0       0.0     1e+20
$#    sfsa      sfsb      sast      sbst     sfsat     sfsbt       fsf       vsf
       1.0       1.0       0.0       0.0       1.0       1.0       1.0       1.0
$#    soft    sofscl    lcidab    maxpar     sbopt     depth     bsort    frcfrq
         2       0.1         0     1.025         2         2         0         1
$#  penmax    thkopt    shlthk     snlog      isym     i2d3d    sldthk    sldstf
       0.0         0         0         0         0         0       0.0       0.0
$#    igap    ignore    dprfac    dtstif     edgek              flangl   cid_rcf
         1         2       0.0       0.0       0.0                 0.0         0
$#   q2tri    dtpchk     sfnbr    fnlscl    dnlscl      tcso    tiedid    shledg
         0       0.0       0.0       0.0       0.0         0         1         0
$#  sharec    cparm8    ipback     srnde    fricsf      icor     ftorq    region
         0         0         0         0       1.0         0         0         0"""

test_deck_contact_tied_shell_edge_to_surface_id2 = """$
*KEYWORD
*CONTACT_TIED_SHELL_EDGE_TO_SURFACE_BEAM_OFFSET_ID
$#     cid                                                               heading
    999999TEST_CONTACT_WITH_ID                                                  
$#   surfa     surfb  surfatyp  surfbtyp   saboxid   sbboxid      sapr      sbpr
         0         0         0         0         0         0         0         0
$#      fs        fd        dc        vc       vdc    penchk        bt        dt
       0.0       0.0       0.0       0.0       0.0         0       0.0     1e+20
$#    sfsa      sfsb      sast      sbst     sfsat     sfsbt       fsf       vsf
       1.0       1.0       0.0       0.0       1.0       1.0       1.0       1.0
*END"""

test_deck_contact_tied_shell_edge_to_surface_id3 = """$
*KEYWORD
*CONTACT_TIED_SHELL_EDGE_TO_SURFACE_BEAM_OFFSET_ID
$#     cid                                                               heading
    999999TEST_CONTACT_WITH_OPTIONAL_CARDS_AND_ID                               
$#   surfa     surfb  surfatyp  surfbtyp   saboxid   sbboxid      sapr      sbpr
         0         0         0         0         0         0         0         0
$#      fs        fd        dc        vc       vdc    penchk        bt        dt
       0.0       0.0       0.0       0.0       0.0         0       0.0     1e+20
$#    sfsa      sfsb      sast      sbst     sfsat     sfsbt       fsf       vsf
       1.0       1.0       0.0       0.0       1.0       1.0       1.0       1.0
$#    soft    sofscl    lcidab    maxpar     sbopt     depth     bsort    frcfrq
         0       0.1         0     1.025         2         2         0         1
$#  penmax    thkopt    shlthk     snlog      isym     i2d3d    sldthk    sldstf
       0.0         0         0         0         0         0       0.0       0.0
$#    igap    ignore    dprfac    dtstif     edgek              flangl   cid_rcf
         1         0       0.0       0.0       0.0                 0.0         0
$#   q2tri    dtpchk     sfnbr    fnlscl    dnlscl      tcso    tiedid    shledg
         0       0.0       0.0       0.0       0.0         0         0         0
$#  sharec    cparm8    ipback     srnde    fricsf      icor     ftorq    region
         0         0         0         0       1.0         0         0         0
$#  pstiff   ignroff               fstol    2dbinr    ssftyp     swtpr    tetfac
         0         0                 2.0         0         0         0       0.0
$#            shloff
                 0.0
*END"""

test_em_randles_batmac_rdltype_0_1 = """*EM_RANDLES_BATMAC
$#   rdlid   rdltype   rdlarea      psid
                   1         1          
$#       q        cq   socinit    soctou
                                        
$#   r0cha     r0dis    r10cha    r10dis    c10cha    c10dis
                                                            
$#    temp    frther    r0toth      dudt     tempu
       0.0         0         0                   0
$# usesocs       tau     flcid
         0                    """

test_em_randles_batmac_rdltype_2_3 = """*EM_RANDLES_BATMAC
$#   rdlid   rdltype   rdlarea      psid
                   3         1          
$#       q        cq   socinit    soctou
                                        
$#   r0cha     r0dis    r10cha    r10dis    c10cha    c10dis
                                                            
$#  r20cha    r20dis    c20cha    c20dis    r30cha    r30dis    c30cha    c30dis
                                                                                
$#    temp    frther    r0toth      dudt     tempu
       0.0         0         0                   0
$# usesocs       tau     flcid
         0                    """

test_em_randles_solid_rdltype_0_1 = """*EM_RANDLES_SOLID
$#   rdlid   rdltype   rdlarea   ccppart   ccnpart   seppart   pelpart   nelpart
                   1         2                                                  
$#       q        cq   socinit    soctou
                                        
$#   r0cha     r0dis    r10cha    r10dis    c10cha    c10dis
                                                            
$#    temp    frther    r0toth      dudt     tempu
       0.0         0         0       0.0         0
$# usesocs       tau     flcid
         0                    """

test_em_randles_solid_rdltype_2_3 = """*EM_RANDLES_SOLID
$#   rdlid   rdltype   rdlarea   ccppart   ccnpart   seppart   pelpart   nelpart
                   3         2                                                  
$#       q        cq   socinit    soctou
                                        
$#   r0cha     r0dis    r10cha    r10dis    c10cha    c10dis
                                                            
$#  r20cha    r20dis    c20cha    c20dis    r30cha    r30dis    c30cha    c30dis
                                                                                
$#    temp    frther    r0toth      dudt     tempu
       0.0         0         0       0.0         0
$# usesocs       tau     flcid
         0                    """

test_em_randles_tshell_rdltype_0_1 = """*EM_RANDLES_TSHELL
$#   rdlid   rdltype   rdlarea      psid
                   1         2          
$#       q        cq   socinit    soctou
                                        
$#   r0cha     r0dis    r10cha    r10dis    c10cha    c10dis
                                                            
$#    temp    frther    r0toth      dudt     tempu
       0.0         0         0       0.0         0
$# usesocs       tau     flcid
         0                    """

test_em_randles_tshell_rdltype_2_3 = """*EM_RANDLES_TSHELL
$#   rdlid   rdltype   rdlarea      psid
                   3         2          
$#       q        cq   socinit    soctou
                                        
$#   r0cha     r0dis    r10cha    r10dis    c10cha    c10dis
                                                            
$#  r20cha    r20dis    c20cha    c20dis    r30cha    r30dis    c30cha    c30dis
                                                                                
$#    temp    frther    r0toth      dudt     tempu
       0.0         0         0       0.0         0
$# usesocs       tau     flcid
         0                    """