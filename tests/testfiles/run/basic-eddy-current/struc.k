*KEYWORD
*CONTROL_TERMINATION
$#  endtim    endcyc     dtmin    endeng    endmas
    &T_end
*CONTROL_TIMESTEP
$#  dtinit    tssfac      isdo    tslimt     dt2ms      lctm     erode     ms1st
     0.000  1.000000         0     0.000 &dt_struc         0         0         0
$#  dt2msf   dt2mslc     imscl    unused    unused     rmscl
     0.000         0         0         0         0     0.000
*PART
$# title
                                                                                
$#     pid     secid       mid     eosid      hgid      grav    adpopt      tmid
         1         1         1         0         0         0         0         0
*SECTION_SOLID
$#   secid    elform       aet
         1         1         0
*MAT_RIGID
$#     mid        ro         e        pr         n    couple         m     alias
         1&rho_struc  &E_struc &nu_struc     0.000     0.000     0.000          
$#     cmo      con1      con2
  1.000000         7         7
$# lco or a1      a2        a3        v1        v2        v3
     0.000     0.000     0.000     0.000     0.000     0.000
*END

