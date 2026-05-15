*KEYWORD
*COMMENT
DISABLED: self-contact removed for this load case
Re-enable if buckling is observed
*CONTACT_AUTOMATIC_SINGLE_SURFACE
$#     ssid      msid    sstyp    mstyp    sboxid    mboxid       spr       mpr
         0         0         0         0         0         0         0         0
$#       fs        fd        dc        vc       vdc    penchk        bt        dt
      0.00      0.00      0.00      0.00      0.00         0     0.000  1.00e+20
*COMMENT
Contact definition: impactor vs plate
Automatic surface-to-surface, friction 0.2
*CONTACT_AUTOMATIC_SURFACE_TO_SURFACE
$#    surfa     surfb  surfatyp  surfbtyp   saboxid   sbboxid      sapr      sbpr
         1         2         0         0         0         0         0         0
$#       fs        fd        dc        vc       vdc    penchk        bt        dt
      0.20      0.00      0.00      0.00      0.00         0     0.000  1.00e+20
$#     sfsa      sfsb      sast      sbst     sfsat     sfsbt       fsf       vsf
      1.00      1.00      0.00      0.00      1.00      1.00      1.00      1.00
$#     soft    sofscl    lcidab    maxpar     sbopt     depth     bsort    frcfrq
         2      0.10         0     1.025      0.00         2        10         1
*COMMENT
Segment sets for contact surfaces
Surface 1: impactor face (nodes 2842-2846)
Surface 2: plate top face (nodes 626-3242)
*SET_SEGMENT
$#      sid       da1       da2       da3       da4    solver
         1     0.000     0.000     0.000     0.000      MECH
$#       n1        n2        n3        n4        a1        a2        a3        a4
      2842       626      3232      3242     0.000     0.000     0.000     0.000
      2846      2842       627      2843     0.000     0.000     0.000     0.000
*SET_SEGMENT
$#      sid       da1       da2       da3       da4    solver
         2     0.000     0.000     0.000     0.000      MECH
$#       n1        n2        n3        n4        a1        a2        a3        a4
       626      2842      3242       627     0.000     0.000     0.000     0.000
*END
