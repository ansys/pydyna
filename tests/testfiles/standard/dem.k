*KEYWORD
*CONTROL_CONTACT
$#  slsfac    rwpnal    islchk    shlthk    penopt    thkchg     orien    enmass
       0.0       1.0         0         0         0         0         1         0
$#  usrstr    usrfrc     nsbcs    interm     xpene     ssthk      ecdt   tiedprj
         0         0         0         0       0.0         0         0         0
$#   sfric     dfric       edc       vfc        th     th_sf    pen_sf     ptscl
                                                                                
$#  ignore    frceng   skiprwg    outseg   spotstp   spotdel   spothin       
         1                                                            
$#    isym    nserod    rwgaps    rwgdth     rwksf      icov    swradf    ithoff
                                                                                
$#  shledg    pstiff    ithcnt    tdcnof     ftall    unused    shltrw    igactc
         0         0         0         0         0                 0.0         0
*CONTROL_DISCRETE_ELEMENT
$#   ndamp     tdamp     frics     fricr     normk    sheark       cap       vtk
      0.99      0.99       0.9       0.9      0.01    0.2857         0         0
$#   gamma       vol       ang       gap    unused    ignore      nbuf  parallel

$#   lnorm    lshear    unused     fricd        dc      ncrb        bt        dt
         0         0                 0.0       0.0         0       0.01.00000E20
$#      cp        tc      tfac    
       0.0       0.0       0.0
$# idesoft    sofscl    unused     iskip    maxnei      
         0       0.1                   0        20
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
