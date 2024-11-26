*KEYWORD
*TITLE
Basic 001
*DEFINE_CURVE
$My first curve
$#    lcid      sidr       sfa       sfo      offa      offo    dattyp     lcint
                   0       1.0       1.0       0.0       0.0         0         0
*DEFINE_CURVE
$#    lcid      sidr       sfa       sfo      offa      offo    dattyp     lcint
                   0       1.0       1.0       2.0       0.0         0         0
$#                a1                  o1
                 1.0                 2.0
                 3.0                 4.0
                 5.0                 6.0
*INCLUDE
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
*DEFINE_CONTACT_VOLUME
$#    cvid       cid      type        xc        yc        zc
                             2                              
$#  rinner    router    d_angs
       0.0       0.0       0.0
*SECTION_TSHELL
$#   secid    elform      shrf       nip     propt        qr     icomp    tshear
         4         1       1.0         9       1.0         0         1         0
$#      bi        bi        bi        bi        bi        bi        bi        bi
                                                                                
          
*SECTION_SEATBELT
$#   secid      area     thick
                              
*ALE_SMOOTHING
$#    dnid      nid1      nid2      ipre       xco       yco       zco
                             3         0       0.0       0.0       0.0
*BOUNDARY_PRECRACK
$#     pid     ctype        np
                   1          
$#       x         y         z
                              
*BOUNDARY_ACOUSTIC_COUPLING
$#    ssid
          
*BOUNDARY_TEMPERATURE_SET
$#    nsid      lcid     cmult       loc    tdeath    tbirth
                   0       1.0         0     1e+20       0.0
*END