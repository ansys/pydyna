*KEYWORD
*TITLE
Skew Plate with Normal Pressure (thick shell)
*CONTROL_IMPLICIT_GENERAL
$# imflag dt0 imform nsbs igs cnstn form
1 0.0 2 1 2
*CONTROL_IMPLICIT_SOLUTION
$# nsolvr ilimit maxref dctol ectol rctol lstol abstol
1 11 15 0.001000 0.010000 0.0 0.900000 1.000000
$# dnorm diverg istif nlprint
2 1 1 2
$# arcctl arcdir arclen arcmth arcdmp
0 1 0.0 1 2
*CONTROL_SHELL
$# wrpang esort irnxx istupd theory bwc miter proj
20.00000 0 0 0 2 2 1
$# rotascl intgrd lamsht cstyp6 tshell nfail1 nfail4
0.0 1
*CONTROL_TERMINATION
$# endtim endcyc dtmin endeng endmas
1.000000 0 0.0 0.0 0.0
*DATABASE_ELOUT
$# dt/cycl
1.0E-01
*DATABASE_BINARY_D3PLOT
$# dt/cycl
0.100000
*DATABASE_HISTORY_TSHELL
$# eid1 eid2 eid3 eid4 ei5 eid6 eid7 eid8
6 7 10 11
*DEFINE_CURVE
12
$# lcid sdir sfa sfo offa offo dattyp
1 0 0.0 0.0 0.0 0.0
$# a1 o1
0.0 0.0
1.00000000 700.0000000
*ELEMENT_TSHELL
$# eid pid n1 n2 n3 n4 n5 n6 n7 n8
1 1 1 6 7 2 101 106 107 102
16 1 19 24 25 20 119 124 125 120
*NODE
$# nid x y z tc rc
1 0.0 0.0 0.0 3
125 1.86602540 0.50000000 0.010
*PART
$# title
material type # 1 (Elastic)
$# pid secid mid eosid hgid grav adpopt tmid
1 1 1
*SECTION_TSHELL
$# secid elform shrf nip propt qr/irid icomp tshear
1 2 0.0 5 0 0.0
$ 1 3 0.0 5 0 0.0
$ 1 5 0.0 5 0 0.0
*MAT_ELASTIC
$# mid ro e pr da db not used
1 7800.0002.1000e+11 0.300000 0.0 0.0 0.0
*LOAD_SEGMENT
$# lcid sf at n1 n2 n3 n4
1 1.000000 0.0 101 106 107 102
1 1.000000 0.0 102 107 108 103
1 1.000000 0.0 103 108 109 104
1 1.000000 0.0 104 109 110 105
1 1.000000 0.0 106 111 112 107
1 1.000000 0.0 107 112 113 108
1 1.000000 0.0 108 113 114 109
1 1.000000 0.0 109 114 115 110
1 1.000000 0.0 111 116 117 112
1 1.000000 0.0 112 117 118 113
1 1.000000 0.0 113 118 119 114
1 1.000000 0.0 114 119 120 115
1 1.000000 0.0 116 121 122 117
1 1.000000 0.0 117 122 123 118
1 1.000000 0.0 118 123 124 119
1 1.000000 0.0 119 124 125 120
*END