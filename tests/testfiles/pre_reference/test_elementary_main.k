$# LS-DYNA Keyword file created by LS-KwdReader(R) V1.0 (Beta) - 07May2020
$# Created on Jan-9-2026 (15:29:26)
*KEYWORD
*TITLE
$#                                                                         title
LS-DYNA keyword deck by LS-PrePost
*CONTROL_ACCURACY
$#     osu       inn    pidosu      iacc     exacc
         1         4         0         1       0.0
*CONTROL_BULK_VISCOSITY
$#      q1        q2      type     btype    tstype
       1.5      0.06        -2         0         0
*CONTROL_ENERGY
$#    hgen      rwen    slnten     rylen     irgen     maten     drlen     disen
         2         2         2         1         2         1         1         1
*CONTROL_HOURGLASS
$#     ihq        qh
         2       0.0
*CONTROL_TERMINATION
$#  endtim    endcyc     dtmin    endeng    endmas     nosol
       0.1         0       0.0       0.01.000000E8         0
*CONTROL_TIMESTEP
$#  dtinit    tssfac      isdo    tslimt     dt2ms      lctm     erode     ms1st
       0.0       0.9         0       0.0       0.0         0         0         0
*DEFINE_TRANSFORMATION
$#  tranid
         1
$#  option        a1        a2        a3        a4        a5        a6        a7
MIRROR          -4.0       0.0       0.0      -5.0       0.0       0.0       0.0
*INCLUDE_TRANSFORM
$#                                                                      filename
transform.k
$#  idnoff    ideoff    idpoff    idmoff    idsoff    idfoff    iddoff
       100       100       100       100       100       100         0
$#  idroff         -    prefix    suffix
                                                            
$#  fctmas    fcttim    fctlen    fcttem   incout1    fctchg
                              1.0                           
$#  tranid
         1
*END
