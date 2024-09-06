$# LS-DYNA Keyword file created by LS-KwdReader(R) V1.0 (Beta) - 07May2020
$# Created on Sep-6-2024 (21:42:02)
*KEYWORD
*TITLE
$#                                                                         title
LS-DYNA keyword deck by LS-PrePost
*CONTROL_ACCURACY
$#     osu       inn    pidosu      iacc     exacc
         0         1         0         0       0.0
*CONTROL_ADAPSTEP
$#  factin    dfactr
       1.0      0.01
*CONTROL_ADAPTIVE
$# adpfreq    adptol    adptyp    maxlvl    tbirth    tdeath     lcadp    ioflag
2.50000E-4       5.0         2         3       0.01.00000E20         0         1
$# adpsize    adpass    ireflg    adpene     adpth    memory    orient     maxel
      1.83         1         0       5.0       0.0         0         0         0
$#  adpscl         -         -    adpene    adpdam    memory         -         -
        -1                           0.0         0         0                    
$#    dam1
          
*CONTROL_BULK_VISCOSITY
$#      q1        q2      type     btype    tstype
       1.5      0.06        -1         0         0
*CONTROL_CONTACT
$#  slsfac    rwpnal    islchk    shlthk    penopt    thkchg     orien    enmass
       0.1       0.0         2         1         4         0         4         0
$#  usrstr    usrfrc     nsbcs    interm     xpene     ssthk      ecdt   tiedprj
         0         0        10         0       1.0         0         0         0
$#   sfric     dfric       edc       vfc        th     th_sf    pen_sf     ptscl
                                                                                
$#  ignore    frceng   skiprwg    outseg   spotstp   spotdel   spothin   dir_tie
         0                                                                      
$#    isym    nserod    rwgaps    rwgdth     rwksf      icov    swradf    ithoff
                                                                                
$#  shledg    pstiff    ithcnt    tdcnof     ftall    unused    shltrw    igactc
         0         0         0         0         0                 0.0         0
*CONTROL_ENERGY
$#    hgen      rwen    slnten     rylen     irgen     maten     drlen     disen
         2         1         2         1         2         1         1         1
*CONTROL_HOURGLASS
$#     ihq        qh
         3       0.1
*CONTROL_OUTPUT
$#   npopt    neecho    nrefup    iaccop     opifs    ipnint    ikedit    iflush
         1         0         0         0       0.0         0         0         0
*CONTROL_RIGID
$#     lmf      jntf    orthmd     partm    sparse    metalf    plotel     rbsms
         0         0         0         0         0         1         0         0
$#  norbic   gjadstf   gjadvsc    tjastf   tjadvsc   rcvlr2d
         0       0.0       0.0       0.0       0.0         0
*CONTROL_SHELL
$#  wrpang     esort     irnxx    istupd    theory       bwc     miter      proj
      20.0         1        -1         1         2         2         1         0
$# rotascl    intgrd    lamsht    cstyp6    thshel
                                                  
$# psstupd   sidt4tu     cntco    itsflg    irquad    w-mode   stretch      icrq
         0         0         0         0         3       0.0       0.0         0
*CONTROL_TERMINATION
$#  endtim    endcyc     dtmin    endeng    endmas     nosol
      0.05         0       0.0       0.01.000000E8         0
*CONTROL_TIMESTEP
$#  dtinit    tssfac      isdo    tslimt     dt2ms      lctm     erode     ms1st
       0.0       0.9         0       0.0-7.0000E-7         0         0         0
*DATABASE_GLSTAT
$#      dt    binary      lcur     ioopt
2.50000E-4         1         0         1
*DATABASE_MATSUM
$#      dt    binary      lcur     ioopt
2.50000E-4         1         0         1
*DATABASE_RCFORC
$#      dt    binary      lcur     ioopt
2.50000E-4         1         0         1
*DATABASE_BINARY_D3PLOT
$#      dt      lcdt      beam     npltc    psetid
5.00000E-4         0         0         0         0
*BOUNDARY_PRESCRIBED_MOTION_RIGID
$#     pid       dof       vad      lcid        sf       vid     death     birth
         2         6         0         2      -1.0         0       0.0       0.0
$#     pid       dof       vad      lcid        sf       vid     death     birth
         3         6         0         3       1.0         0       0.0       0.0
*BOUNDARY_SPC_SET
$#    nsid       cid      dofx      dofy      dofz     dofrx     dofry     dofrz
         1         0         0         1         0         1         0         1
*SET_NODE_LIST
$#     sid       da1       da2       da3       da4    solver       its         -
         1                                        MECH                          
$#    nid1      nid2      nid3      nid4      nid5      nid6      nid7      nid8
       600       593         0         0         0         0         0         0
*LOAD_NODE_SET
$#    nsid       dof      lcid        sf       cid        m1        m2        m3
         2         1         4       1.0         0         0         0         0
*CONTACT_FORMING_ONE_WAY_SURFACE_TO_SURFACE
$#     cid                                                                 title
$#   surfa     surfb  surfatyp  surfbtyp   saboxid   sbboxid      sapr      sbpr
         1         2         3         3         0         0         1         1
$#      fs        fd        dc        vc       vdc    penchk        bt        dt
     0.125       0.0       0.0       0.0      20.0         0       0.0       0.0
$#    sfsa      sfsb      sast      sbst     sfsat     sfsbt       fsf       vsf
       1.0       1.0       0.0       0.0       1.0       1.0       1.0       1.0
$#    soft    sofscl    lcidab    maxpar     sbopt     depth     bsort    frcfrq
         0       0.1         0     1.025         2         2       100         1
$#  penmax    thkopt    shlthk     snlog      isym     i2d3d    sldthk    sldstf
       0.0         0         0         1         0         0       0.0       0.0
$#    igap    ignore    dprfac    dtstif     edgek    unused    flangl   cid_rcf
         1         0       0.0       0.0       0.0                 0.0         0
*CONTACT_FORMING_ONE_WAY_SURFACE_TO_SURFACE
$#     cid                                                                 title
$#   surfa     surfb  surfatyp  surfbtyp   saboxid   sbboxid      sapr      sbpr
         1         3         3         3         0         0         1         1
$#      fs        fd        dc        vc       vdc    penchk        bt        dt
     0.125       0.0       0.0       0.0      20.0         0       0.0       0.0
$#    sfsa      sfsb      sast      sbst     sfsat     sfsbt       fsf       vsf
       1.0       1.0       0.0       0.0       1.0       1.0       1.0       1.0
$#    soft    sofscl    lcidab    maxpar     sbopt     depth     bsort    frcfrq
         0       0.1         0     1.025         2         2       100         1
$#  penmax    thkopt    shlthk     snlog      isym     i2d3d    sldthk    sldstf
       0.0         0         0         1         0         0       0.0       0.0
$#    igap    ignore    dprfac    dtstif     edgek    unused    flangl   cid_rcf
         1         0       0.0       0.0       0.0                 0.0         0
*CONTACT_FORMING_ONE_WAY_SURFACE_TO_SURFACE
$#     cid                                                                 title
$#   surfa     surfb  surfatyp  surfbtyp   saboxid   sbboxid      sapr      sbpr
         1         4         3         3         0         0         1         1
$#      fs        fd        dc        vc       vdc    penchk        bt        dt
     0.125       0.0       0.0       0.0      20.0         0       0.0       0.0
$#    sfsa      sfsb      sast      sbst     sfsat     sfsbt       fsf       vsf
       1.0       1.0       0.0       0.0       1.0       1.0       1.0       1.0
$#    soft    sofscl    lcidab    maxpar     sbopt     depth     bsort    frcfrq
         0       0.1         0     1.025         2         2       100         1
$#  penmax    thkopt    shlthk     snlog      isym     i2d3d    sldthk    sldstf
       0.0         0         0         1         0         0       0.0       0.0
$#    igap    ignore    dprfac    dtstif     edgek    unused    flangl   cid_rcf
         1         0       0.0       0.0       0.0                 0.0         0
*CONTACT_FORMING_ONE_WAY_SURFACE_TO_SURFACE
$#     cid                                                                 title
$#   surfa     surfb  surfatyp  surfbtyp   saboxid   sbboxid      sapr      sbpr
         1         5         3         3         0         0         1         1
$#      fs        fd        dc        vc       vdc    penchk        bt        dt
     0.125       0.0       0.0       0.0      20.0         0       0.0       0.0
$#    sfsa      sfsb      sast      sbst     sfsat     sfsbt       fsf       vsf
       1.0       1.0       0.0       0.0       1.0       1.0       1.0       1.0
$#    soft    sofscl    lcidab    maxpar     sbopt     depth     bsort    frcfrq
         0       0.1         0     1.025         2         2       100         1
$#  penmax    thkopt    shlthk     snlog      isym     i2d3d    sldthk    sldstf
       0.0         0         0         1         0         0       0.0       0.0
$#    igap    ignore    dprfac    dtstif     edgek    unused    flangl   cid_rcf
         1         0       0.0       0.0       0.0                 0.0         0
*SECTION_SHELL
$#   secid    elform      shrf       nip     propt   qr/irid     icomp     setyp
         1        16     0.833         5       1.0         0         0         1
$#      t1        t2        t3        t4      nloc     marea      idof    edgset
       1.5       1.5       1.5       1.5       0.0       0.0       0.0         0
*SECTION_SHELL
$#   secid    elform      shrf       nip     propt   qr/irid     icomp     setyp
         2         2       1.0         3       1.0         0         0         1
$#      t1        t2        t3        t4      nloc     marea      idof    edgset
       1.0       1.0       1.0       1.0       0.0       0.0       0.0         0
*SECTION_SHELL
$#   secid    elform      shrf       nip     propt   qr/irid     icomp     setyp
         3         2       1.0         3       1.0         0         0         1
$#      t1        t2        t3        t4      nloc     marea      idof    edgset
       1.0       1.0       1.0       1.0       0.0       0.0       0.0         0
*SECTION_SHELL
$#   secid    elform      shrf       nip     propt   qr/irid     icomp     setyp
         4         2       1.0         3       1.0         0         0         1
$#      t1        t2        t3        t4      nloc     marea      idof    edgset
       1.0       1.0       1.0       1.0       0.0       0.0       0.0         0
*SECTION_SHELL
$#   secid    elform      shrf       nip     propt   qr/irid     icomp     setyp
         5         2       1.0         3       1.0         0         0         1
$#      t1        t2        t3        t4      nloc     marea      idof    edgset
       1.0       1.0       1.0       1.0       0.0       0.0       0.0         0
*MAT_TRANSVERSELY_ANISOTROPIC_ELASTIC_PLASTIC
$#     mid        ro         e        pr      sigy      etan         r     hlcid
         17.90000E-9  207000.0       0.3     201.3       0.0    -1.593         1
*MAT_RIGID
$#     mid        ro         e        pr         n    couple         m     alias
         27.83000E-9  207000.0      0.28       0.0       0.0       0.0                    
$#     cmo      con1      con2    spcnid      xspc      yspc      zspc
       1.0         7         6         0       0.0       0.0       0.0
$#lco or a1        a2        a3        v1        v2        v3
       0.0       0.0       0.0       0.0       0.0       0.0
*MAT_RIGID
$#     mid        ro         e        pr         n    couple         m     alias
         37.83000E-9  207000.0      0.28       0.0       0.0       0.0                    
$#     cmo      con1      con2    spcnid      xspc      yspc      zspc
       1.0         7         6         0       0.0       0.0       0.0
$#lco or a1        a2        a3        v1        v2        v3
       0.0       0.0       0.0       0.0       0.0       0.0
*MAT_RIGID
$#     mid        ro         e        pr         n    couple         m     alias
         47.83000E-9  207000.0      0.28       0.0       0.0       0.0                    
$#     cmo      con1      con2    spcnid      xspc      yspc      zspc
       1.0         7         7         0       0.0       0.0       0.0
$#lco or a1        a2        a3        v1        v2        v3
       0.0       0.0       0.0       0.0       0.0       0.0
*MAT_RIGID
$#     mid        ro         e        pr         n    couple         m     alias
         57.83000E-9  207000.0      0.28       0.0       0.0       0.0                    
$#     cmo      con1      con2    spcnid      xspc      yspc      zspc
       1.0         7         7         0       0.0       0.0       0.0
$#lco or a1        a2        a3        v1        v2        v3
       0.0       0.0       0.0       0.0       0.0       0.0
*DEFINE_CURVE
$#    lcid      sidr       sfa       sfo      offa      offo    dattyp     lcint
         1         0       1.0       1.0       0.0       0.0         0         0
$#                a1                  o1
                 0.0                 0.0
                 1.0                13.0
*DEFINE_CURVE
$#    lcid      sidr       sfa       sfo      offa      offo    dattyp     lcint
         2         0       1.0       1.0       0.0       0.0         0         0
$#                a1                  o1
                 0.0               600.0
               100.0               600.0
*DEFINE_CURVE
$#    lcid      sidr       sfa       sfo      offa      offo    dattyp     lcint
         3         0       1.0       1.0       0.0       0.0         0         0
$#                a1                  o1
                 0.0               600.0
               100.0               600.0
*DEFINE_CURVE
$#    lcid      sidr       sfa       sfo      offa      offo    dattyp     lcint
         4         0       1.0       1.0       0.0       0.0         0         0
$#                a1                  o1
                 0.0               100.0
               0.015               100.0
               0.016                 0.0
               100.0                 0.0
*SET_NODE_LIST
$#     sid       da1       da2       da3       da4    solver       its         -
         2                                        MECH                          
$#    nid1      nid2      nid3      nid4      nid5      nid6      nid7      nid8
       695       696       697       698       694       693       692         0
*SET_PART_LIST
$#     sid       da1       da2       da3       da4    solver
         1                                        MECH
$#    pid1      pid2      pid3      pid4      pid5      pid6      pid7      pid8
         1         0         0         0         0         0         0         0
*SET_PART_LIST
$#     sid       da1       da2       da3       da4    solver
         2                                        MECH
$#    pid1      pid2      pid3      pid4      pid5      pid6      pid7      pid8
         2         0         0         0         0         0         0         0
*SET_PART_LIST
$#     sid       da1       da2       da3       da4    solver
         3                                        MECH
$#    pid1      pid2      pid3      pid4      pid5      pid6      pid7      pid8
         1         0         0         0         0         0         0         0
*SET_PART_LIST
$#     sid       da1       da2       da3       da4    solver
         4                                        MECH
$#    pid1      pid2      pid3      pid4      pid5      pid6      pid7      pid8
         3         0         0         0         0         0         0         0
*SET_PART_LIST
$#     sid       da1       da2       da3       da4    solver
         5                                        MECH
$#    pid1      pid2      pid3      pid4      pid5      pid6      pid7      pid8
         1         0         0         0         0         0         0         0
*SET_PART_LIST
$#     sid       da1       da2       da3       da4    solver
         6                                        MECH
$#    pid1      pid2      pid3      pid4      pid5      pid6      pid7      pid8
         4         0         0         0         0         0         0         0
*SET_PART_LIST
$#     sid       da1       da2       da3       da4    solver
         7                                        MECH
$#    pid1      pid2      pid3      pid4      pid5      pid6      pid7      pid8
         1         0         0         0         0         0         0         0
*SET_PART_LIST
$#     sid       da1       da2       da3       da4    solver
         8                                        MECH
$#    pid1      pid2      pid3      pid4      pid5      pid6      pid7      pid8
         5         0         0         0         0         0         0         0
*SET_PART_LIST
$#     sid       da1       da2       da3       da4    solver
         9                                        MECH
$#    pid1      pid2      pid3      pid4      pid5      pid6      pid7      pid8
         2         0         0         0         0         0         0         0
*SET_PART_LIST
$#     sid       da1       da2       da3       da4    solver
        10                                        MECH
$#    pid1      pid2      pid3      pid4      pid5      pid6      pid7      pid8
         3         0         0         0         0         0         0         0
*SET_PART_LIST
$#     sid       da1       da2       da3       da4    solver
        11                                        MECH
$#    pid1      pid2      pid3      pid4      pid5      pid6      pid7      pid8
         1         0         0         0         0         0         0         0
*INTERFACE_SPRINGBACK_LSDYNA
$#    psid      nshv     ftype         -    ftensr    nthhsv     rflag   intstrn
        11       100         0                   0         0         0         0
*ELEMENT_SHELL
$#   eid     pid      n1      n2      n3      n4      n5      n6      n7      n8
       1       4      51      73      74      52       0       0       0       0
       2       4      52      74      75      53       0       0       0       0
       3       4      53      75      76      54       0       0       0       0
       4       4      54      76      77      55       0       0       0       0
       5       4      55      77      78      56       0       0       0       0
       6       4      56      78      79      57       0       0       0       0
       7       4      57      79      80      58       0       0       0       0
       8       4      58      80      81      59       0       0       0       0
       9       4      59      81      82      60       0       0       0       0
      10       4      60      82      83      61       0       0       0       0
      11       4      61      83      84      62       0       0       0       0
      12       4      62      84      85      63       0       0       0       0
      13       4      63      85      86      64       0       0       0       0
      14       4      64      86      87      65       0       0       0       0
      15       4      65      87      88      66       0       0       0       0
      16       4      66      88      89      67       0       0       0       0
      17       4      67      89      90      68       0       0       0       0
      18       4      68      90      91      69       0       0       0       0
      19       4      69      91      92      70       0       0       0       0
      20       4      70      92      93      71       0       0       0       0
      21       4      71      93      94      72       0       0       0       0
      22       4       2      51      25       1       0       0       0       0
      23       4       2       3      73      51       0       0       0       0
      24       4      23      24      52      53       0       0       0       0
      25       4       5      71      72       4       0       0       0       0
      26       4       6      70      71       5       0       0       0       0
      27       4       7      69      70       6       0       0       0       0
      28       4       8      68      69       7       0       0       0       0
      29       4       9      67      68       8       0       0       0       0
      30       4      10      66      67       9       0       0       0       0
      31       4      10      11      65      66       0       0       0       0
      32       4      12      64      65      11       0       0       0       0
      33       4      13      63      64      12       0       0       0       0
      34       4      13      14      62      63       0       0       0       0
      35       4      14      15      61      62       0       0       0       0
      36       4      15      16      60      61       0       0       0       0
      37       4      16      17      59      60       0       0       0       0
      38       4      17      18      58      59       0       0       0       0
      39       4      18      19      57      58       0       0       0       0
      40       4      19      20      56      57       0       0       0       0
      41       4      20      21      55      56       0       0       0       0
      42       4      21      22      54      55       0       0       0       0
      43       4      22      23      53      54       0       0       0       0
      44       4      24      25      51      52       0       0       0       0
      45       5      95     117     118      96       0       0       0       0
      46       5      96     118     119      97       0       0       0       0
      47       5      97     119     120      98       0       0       0       0
      48       5      98     120     121      99       0       0       0       0
      49       5      99     121     122     100       0       0       0       0
      50       5     100     122     123     101       0       0       0       0
      51       5     101     123     124     102       0       0       0       0
      52       5     102     124     125     103       0       0       0       0
      53       5     103     125     126     104       0       0       0       0
      54       5     104     126     127     105       0       0       0       0
      55       5     105     127     128     106       0       0       0       0
      56       5     106     128     129     107       0       0       0       0
      57       5     107     129     130     108       0       0       0       0
      58       5     108     130     131     109       0       0       0       0
      59       5     109     131     132     110       0       0       0       0
      60       5     110     132     133     111       0       0       0       0
      61       5     111     133     134     112       0       0       0       0
      62       5     112     134     135     113       0       0       0       0
      63       5     113     135     136     114       0       0       0       0
      64       5     114     136     137     115       0       0       0       0
      65       5     115     137     138     116       0       0       0       0
      66       5      27      95      50      26       0       0       0       0
      67       5      27      28     117      95       0       0       0       0
      68       5      48      49      96      97       0       0       0       0
      69       5      30     115     116      29       0       0       0       0
      70       5      31     114     115      30       0       0       0       0
      71       5      32     113     114      31       0       0       0       0
      72       5      33     112     113      32       0       0       0       0
      73       5      34     111     112      33       0       0       0       0
      74       5      35     110     111      34       0       0       0       0
      75       5      35      36     109     110       0       0       0       0
      76       5      37     108     109      36       0       0       0       0
      77       5      38     107     108      37       0       0       0       0
      78       5      38      39     106     107       0       0       0       0
      79       5      39      40     105     106       0       0       0       0
      80       5      40      41     104     105       0       0       0       0
      81       5      41      42     103     104       0       0       0       0
      82       5      42      43     102     103       0       0       0       0
      83       5      43      44     101     102       0       0       0       0
      84       5      44      45     100     101       0       0       0       0
      85       5      45      46      99     100       0       0       0       0
      86       5      46      47      98      99       0       0       0       0
      87       5      47      48      97      98       0       0       0       0
      88       5      49      50      95      96       0       0       0       0
      89       1     139     146     147     140       0       0       0       0
      90       1     140     147     148     141       0       0       0       0
      91       1     141     148     149     142       0       0       0       0
      92       1     142     149     150     143       0       0       0       0
      93       1     143     150     151     144       0       0       0       0
      94       1     144     151     152     145       0       0       0       0
      95       1     146     153     154     147       0       0       0       0
      96       1     147     154     155     148       0       0       0       0
      97       1     148     155     156     149       0       0       0       0
      98       1     149     156     157     150       0       0       0       0
      99       1     150     157     158     151       0       0       0       0
     100       1     151     158     159     152       0       0       0       0
     101       1     153     160     161     154       0       0       0       0
     102       1     154     161     162     155       0       0       0       0
     103       1     155     162     163     156       0       0       0       0
     104       1     156     163     164     157       0       0       0       0
     105       1     157     164     165     158       0       0       0       0
     106       1     158     165     166     159       0       0       0       0
     107       1     160     167     168     161       0       0       0       0
     108       1     161     168     169     162       0       0       0       0
     109       1     162     169     170     163       0       0       0       0
     110       1     163     170     171     164       0       0       0       0
     111       1     164     171     172     165       0       0       0       0
     112       1     165     172     173     166       0       0       0       0
     113       1     167     174     175     168       0       0       0       0
     114       1     168     175     176     169       0       0       0       0
     115       1     169     176     177     170       0       0       0       0
     116       1     170     177     178     171       0       0       0       0
     117       1     171     178     179     172       0       0       0       0
     118       1     172     179     180     173       0       0       0       0
     119       1     174     181     182     175       0       0       0       0
     120       1     175     182     183     176       0       0       0       0
     121       1     176     183     184     177       0       0       0       0
     122       1     177     184     185     178       0       0       0       0
     123       1     178     185     186     179       0       0       0       0
     124       1     179     186     187     180       0       0       0       0
     125       1     181     188     189     182       0       0       0       0
     126       1     182     189     190     183       0       0       0       0
     127       1     183     190     191     184       0       0       0       0
     128       1     184     191     192     185       0       0       0       0
     129       1     185     192     193     186       0       0       0       0
     130       1     186     193     194     187       0       0       0       0
     131       1     188     195     196     189       0       0       0       0
     132       1     189     196     197     190       0       0       0       0
     133       1     190     197     198     191       0       0       0       0
     134       1     191     198     199     192       0       0       0       0
     135       1     192     199     200     193       0       0       0       0
     136       1     193     200     201     194       0       0       0       0
     137       1     195     202     203     196       0       0       0       0
     138       1     196     203     204     197       0       0       0       0
     139       1     197     204     205     198       0       0       0       0
     140       1     198     205     206     199       0       0       0       0
     141       1     199     206     207     200       0       0       0       0
     142       1     200     207     208     201       0       0       0       0
     143       1     202     209     210     203       0       0       0       0
     144       1     203     210     211     204       0       0       0       0
     145       1     204     211     212     205       0       0       0       0
     146       1     205     212     213     206       0       0       0       0
     147       1     206     213     214     207       0       0       0       0
     148       1     207     214     215     208       0       0       0       0
     149       1     209     216     217     210       0       0       0       0
     150       1     210     217     218     211       0       0       0       0
     151       1     211     218     219     212       0       0       0       0
     152       1     212     219     220     213       0       0       0       0
     153       1     213     220     221     214       0       0       0       0
     154       1     214     221     222     215       0       0       0       0
     155       1     216     223     224     217       0       0       0       0
     156       1     217     224     225     218       0       0       0       0
     157       1     218     225     226     219       0       0       0       0
     158       1     219     226     227     220       0       0       0       0
     159       1     220     227     228     221       0       0       0       0
     160       1     221     228     229     222       0       0       0       0
     161       1     223     230     231     224       0       0       0       0
     162       1     224     231     232     225       0       0       0       0
     163       1     225     232     233     226       0       0       0       0
     164       1     226     233     234     227       0       0       0       0
     165       1     227     234     235     228       0       0       0       0
     166       1     228     235     236     229       0       0       0       0
     167       1     230     237     238     231       0       0       0       0
     168       1     231     238     239     232       0       0       0       0
     169       1     232     239     240     233       0       0       0       0
     170       1     233     240     241     234       0       0       0       0
     171       1     234     241     242     235       0       0       0       0
     172       1     235     242     243     236       0       0       0       0
     173       1     237     244     245     238       0       0       0       0
     174       1     238     245     246     239       0       0       0       0
     175       1     239     246     247     240       0       0       0       0
     176       1     240     247     248     241       0       0       0       0
     177       1     241     248     249     242       0       0       0       0
     178       1     242     249     250     243       0       0       0       0
     179       1     244     251     252     245       0       0       0       0
     180       1     245     252     253     246       0       0       0       0
     181       1     246     253     254     247       0       0       0       0
     182       1     247     254     255     248       0       0       0       0
     183       1     248     255     256     249       0       0       0       0
     184       1     249     256     257     250       0       0       0       0
     185       1     251     258     259     252       0       0       0       0
     186       1     252     259     260     253       0       0       0       0
     187       1     253     260     261     254       0       0       0       0
     188       1     254     261     262     255       0       0       0       0
     189       1     255     262     263     256       0       0       0       0
     190       1     256     263     264     257       0       0       0       0
     191       1     258     265     266     259       0       0       0       0
     192       1     259     266     267     260       0       0       0       0
     193       1     260     267     268     261       0       0       0       0
     194       1     261     268     269     262       0       0       0       0
     195       1     262     269     270     263       0       0       0       0
     196       1     263     270     271     264       0       0       0       0
     197       1     265     272     273     266       0       0       0       0
     198       1     266     273     274     267       0       0       0       0
     199       1     267     274     275     268       0       0       0       0
     200       1     268     275     276     269       0       0       0       0
     201       1     269     276     277     270       0       0       0       0
     202       1     270     277     278     271       0       0       0       0
     203       1     272     279     280     273       0       0       0       0
     204       1     273     280     281     274       0       0       0       0
     205       1     274     281     282     275       0       0       0       0
     206       1     275     282     283     276       0       0       0       0
     207       1     276     283     284     277       0       0       0       0
     208       1     277     284     285     278       0       0       0       0
     209       1     279     286     287     280       0       0       0       0
     210       1     280     287     288     281       0       0       0       0
     211       1     281     288     289     282       0       0       0       0
     212       1     282     289     290     283       0       0       0       0
     213       1     283     290     291     284       0       0       0       0
     214       1     284     291     292     285       0       0       0       0
     215       1     286     293     294     287       0       0       0       0
     216       1     287     294     295     288       0       0       0       0
     217       1     288     295     296     289       0       0       0       0
     218       1     289     296     297     290       0       0       0       0
     219       1     290     297     298     291       0       0       0       0
     220       1     291     298     299     292       0       0       0       0
     221       1     293     300     301     294       0       0       0       0
     222       1     294     301     302     295       0       0       0       0
     223       1     295     302     303     296       0       0       0       0
     224       1     296     303     304     297       0       0       0       0
     225       1     297     304     305     298       0       0       0       0
     226       1     298     305     306     299       0       0       0       0
     227       1     300     307     308     301       0       0       0       0
     228       1     301     308     309     302       0       0       0       0
     229       1     302     309     310     303       0       0       0       0
     230       1     303     310     311     304       0       0       0       0
     231       1     304     311     312     305       0       0       0       0
     232       1     305     312     313     306       0       0       0       0
     233       1     307     314     315     308       0       0       0       0
     234       1     308     315     316     309       0       0       0       0
     235       1     309     316     317     310       0       0       0       0
     236       1     310     317     318     311       0       0       0       0
     237       1     311     318     319     312       0       0       0       0
     238       1     312     319     320     313       0       0       0       0
     239       1     314     321     322     315       0       0       0       0
     240       1     315     322     323     316       0       0       0       0
     241       1     316     323     324     317       0       0       0       0
     242       1     317     324     325     318       0       0       0       0
     243       1     318     325     326     319       0       0       0       0
     244       1     319     326     327     320       0       0       0       0
     245       1     321     328     329     322       0       0       0       0
     246       1     322     329     330     323       0       0       0       0
     247       1     323     330     331     324       0       0       0       0
     248       1     324     331     332     325       0       0       0       0
     249       1     325     332     333     326       0       0       0       0
     250       1     326     333     334     327       0       0       0       0
     251       1     328     335     336     329       0       0       0       0
     252       1     329     336     337     330       0       0       0       0
     253       1     330     337     338     331       0       0       0       0
     254       1     331     338     339     332       0       0       0       0
     255       1     332     339     340     333       0       0       0       0
     256       1     333     340     341     334       0       0       0       0
     257       1     335     342     343     336       0       0       0       0
     258       1     336     343     344     337       0       0       0       0
     259       1     337     344     345     338       0       0       0       0
     260       1     338     345     346     339       0       0       0       0
     261       1     339     346     347     340       0       0       0       0
     262       1     340     347     348     341       0       0       0       0
     263       1     342     349     350     343       0       0       0       0
     264       1     343     350     351     344       0       0       0       0
     265       1     344     351     352     345       0       0       0       0
     266       1     345     352     353     346       0       0       0       0
     267       1     346     353     354     347       0       0       0       0
     268       1     347     354     355     348       0       0       0       0
     269       1     349     356     357     350       0       0       0       0
     270       1     350     357     358     351       0       0       0       0
     271       1     351     358     359     352       0       0       0       0
     272       1     352     359     360     353       0       0       0       0
     273       1     353     360     361     354       0       0       0       0
     274       1     354     361     362     355       0       0       0       0
     275       1     356     363     364     357       0       0       0       0
     276       1     357     364     365     358       0       0       0       0
     277       1     358     365     366     359       0       0       0       0
     278       1     359     366     367     360       0       0       0       0
     279       1     360     367     368     361       0       0       0       0
     280       1     361     368     369     362       0       0       0       0
     281       1     363     370     371     364       0       0       0       0
     282       1     364     371     372     365       0       0       0       0
     283       1     365     372     373     366       0       0       0       0
     284       1     366     373     374     367       0       0       0       0
     285       1     367     374     375     368       0       0       0       0
     286       1     368     375     376     369       0       0       0       0
     287       1     370     377     378     371       0       0       0       0
     288       1     371     378     379     372       0       0       0       0
     289       1     372     379     380     373       0       0       0       0
     290       1     373     380     381     374       0       0       0       0
     291       1     374     381     382     375       0       0       0       0
     292       1     375     382     383     376       0       0       0       0
     293       1     377     384     385     378       0       0       0       0
     294       1     378     385     386     379       0       0       0       0
     295       1     379     386     387     380       0       0       0       0
     296       1     380     387     388     381       0       0       0       0
     297       1     381     388     389     382       0       0       0       0
     298       1     382     389     390     383       0       0       0       0
     299       1     384     391     392     385       0       0       0       0
     300       1     385     392     393     386       0       0       0       0
     301       1     386     393     394     387       0       0       0       0
     302       1     387     394     395     388       0       0       0       0
     303       1     388     395     396     389       0       0       0       0
     304       1     389     396     397     390       0       0       0       0
     305       1     391     398     399     392       0       0       0       0
     306       1     392     399     400     393       0       0       0       0
     307       1     393     400     401     394       0       0       0       0
     308       1     394     401     402     395       0       0       0       0
     309       1     395     402     403     396       0       0       0       0
     310       1     396     403     404     397       0       0       0       0
     311       1     398     405     406     399       0       0       0       0
     312       1     399     406     407     400       0       0       0       0
     313       1     400     407     408     401       0       0       0       0
     314       1     401     408     409     402       0       0       0       0
     315       1     402     409     410     403       0       0       0       0
     316       1     403     410     411     404       0       0       0       0
     317       1     405     412     413     406       0       0       0       0
     318       1     406     413     414     407       0       0       0       0
     319       1     407     414     415     408       0       0       0       0
     320       1     408     415     416     409       0       0       0       0
     321       1     409     416     417     410       0       0       0       0
     322       1     410     417     418     411       0       0       0       0
     323       1     412     419     420     413       0       0       0       0
     324       1     413     420     421     414       0       0       0       0
     325       1     414     421     422     415       0       0       0       0
     326       1     415     422     423     416       0       0       0       0
     327       1     416     423     424     417       0       0       0       0
     328       1     417     424     425     418       0       0       0       0
     329       1     419     426     427     420       0       0       0       0
     330       1     420     427     428     421       0       0       0       0
     331       1     421     428     429     422       0       0       0       0
     332       1     422     429     430     423       0       0       0       0
     333       1     423     430     431     424       0       0       0       0
     334       1     424     431     432     425       0       0       0       0
     335       1     426     433     434     427       0       0       0       0
     336       1     427     434     435     428       0       0       0       0
     337       1     428     435     436     429       0       0       0       0
     338       1     429     436     437     430       0       0       0       0
     339       1     430     437     438     431       0       0       0       0
     340       1     431     438     439     432       0       0       0       0
     341       1     433     440     441     434       0       0       0       0
     342       1     434     441     442     435       0       0       0       0
     343       1     435     442     443     436       0       0       0       0
     344       1     436     443     444     437       0       0       0       0
     345       1     437     444     445     438       0       0       0       0
     346       1     438     445     446     439       0       0       0       0
     347       1     440     447     448     441       0       0       0       0
     348       1     441     448     449     442       0       0       0       0
     349       1     442     449     450     443       0       0       0       0
     350       1     443     450     451     444       0       0       0       0
     351       1     444     451     452     445       0       0       0       0
     352       1     445     452     453     446       0       0       0       0
     353       1     447     454     455     448       0       0       0       0
     354       1     448     455     456     449       0       0       0       0
     355       1     449     456     457     450       0       0       0       0
     356       1     450     457     458     451       0       0       0       0
     357       1     451     458     459     452       0       0       0       0
     358       1     452     459     460     453       0       0       0       0
     359       1     454     461     462     455       0       0       0       0
     360       1     455     462     463     456       0       0       0       0
     361       1     456     463     464     457       0       0       0       0
     362       1     457     464     465     458       0       0       0       0
     363       1     458     465     466     459       0       0       0       0
     364       1     459     466     467     460       0       0       0       0
     365       1     461     468     469     462       0       0       0       0
     366       1     462     469     470     463       0       0       0       0
     367       1     463     470     471     464       0       0       0       0
     368       1     464     471     472     465       0       0       0       0
     369       1     465     472     473     466       0       0       0       0
     370       1     466     473     474     467       0       0       0       0
     371       1     468     475     476     469       0       0       0       0
     372       1     469     476     477     470       0       0       0       0
     373       1     470     477     478     471       0       0       0       0
     374       1     471     478     479     472       0       0       0       0
     375       1     472     479     480     473       0       0       0       0
     376       1     473     480     481     474       0       0       0       0
     377       1     475     482     483     476       0       0       0       0
     378       1     476     483     484     477       0       0       0       0
     379       1     477     484     485     478       0       0       0       0
     380       1     478     485     486     479       0       0       0       0
     381       1     479     486     487     480       0       0       0       0
     382       1     480     487     488     481       0       0       0       0
     383       1     482     489     490     483       0       0       0       0
     384       1     483     490     491     484       0       0       0       0
     385       1     484     491     492     485       0       0       0       0
     386       1     485     492     493     486       0       0       0       0
     387       1     486     493     494     487       0       0       0       0
     388       1     487     494     495     488       0       0       0       0
     389       1     489     496     497     490       0       0       0       0
     390       1     490     497     498     491       0       0       0       0
     391       1     491     498     499     492       0       0       0       0
     392       1     492     499     500     493       0       0       0       0
     393       1     493     500     501     494       0       0       0       0
     394       1     494     501     502     495       0       0       0       0
     395       1     496     503     504     497       0       0       0       0
     396       1     497     504     505     498       0       0       0       0
     397       1     498     505     506     499       0       0       0       0
     398       1     499     506     507     500       0       0       0       0
     399       1     500     507     508     501       0       0       0       0
     400       1     501     508     509     502       0       0       0       0
     401       1     503     510     511     504       0       0       0       0
     402       1     504     511     512     505       0       0       0       0
     403       1     505     512     513     506       0       0       0       0
     404       1     506     513     514     507       0       0       0       0
     405       1     507     514     515     508       0       0       0       0
     406       1     508     515     516     509       0       0       0       0
     407       1     510     517     518     511       0       0       0       0
     408       1     511     518     519     512       0       0       0       0
     409       1     512     519     520     513       0       0       0       0
     410       1     513     520     521     514       0       0       0       0
     411       1     514     521     522     515       0       0       0       0
     412       1     515     522     523     516       0       0       0       0
     413       1     517     524     525     518       0       0       0       0
     414       1     518     525     526     519       0       0       0       0
     415       1     519     526     527     520       0       0       0       0
     416       1     520     527     528     521       0       0       0       0
     417       1     521     528     529     522       0       0       0       0
     418       1     522     529     530     523       0       0       0       0
     419       1     524     531     532     525       0       0       0       0
     420       1     525     532     533     526       0       0       0       0
     421       1     526     533     534     527       0       0       0       0
     422       1     527     534     535     528       0       0       0       0
     423       1     528     535     536     529       0       0       0       0
     424       1     529     536     537     530       0       0       0       0
     425       1     531     538     539     532       0       0       0       0
     426       1     532     539     540     533       0       0       0       0
     427       1     533     540     541     534       0       0       0       0
     428       1     534     541     542     535       0       0       0       0
     429       1     535     542     543     536       0       0       0       0
     430       1     536     543     544     537       0       0       0       0
     431       1     538     545     546     539       0       0       0       0
     432       1     539     546     547     540       0       0       0       0
     433       1     540     547     548     541       0       0       0       0
     434       1     541     548     549     542       0       0       0       0
     435       1     542     549     550     543       0       0       0       0
     436       1     543     550     551     544       0       0       0       0
     437       1     545     552     553     546       0       0       0       0
     438       1     546     553     554     547       0       0       0       0
     439       1     547     554     555     548       0       0       0       0
     440       1     548     555     556     549       0       0       0       0
     441       1     549     556     557     550       0       0       0       0
     442       1     550     557     558     551       0       0       0       0
     443       1     552     559     560     553       0       0       0       0
     444       1     553     560     561     554       0       0       0       0
     445       1     554     561     562     555       0       0       0       0
     446       1     555     562     563     556       0       0       0       0
     447       1     556     563     564     557       0       0       0       0
     448       1     557     564     565     558       0       0       0       0
     449       1     559     566     567     560       0       0       0       0
     450       1     560     567     568     561       0       0       0       0
     451       1     561     568     569     562       0       0       0       0
     452       1     562     569     570     563       0       0       0       0
     453       1     563     570     571     564       0       0       0       0
     454       1     564     571     572     565       0       0       0       0
     455       1     566     573     574     567       0       0       0       0
     456       1     567     574     575     568       0       0       0       0
     457       1     568     575     576     569       0       0       0       0
     458       1     569     576     577     570       0       0       0       0
     459       1     570     577     578     571       0       0       0       0
     460       1     571     578     579     572       0       0       0       0
     461       1     573     580     581     574       0       0       0       0
     462       1     574     581     582     575       0       0       0       0
     463       1     575     582     583     576       0       0       0       0
     464       1     576     583     584     577       0       0       0       0
     465       1     577     584     585     578       0       0       0       0
     466       1     578     585     586     579       0       0       0       0
     467       1     580     587     588     581       0       0       0       0
     468       1     581     588     589     582       0       0       0       0
     469       1     582     589     590     583       0       0       0       0
     470       1     583     590     591     584       0       0       0       0
     471       1     584     591     592     585       0       0       0       0
     472       1     585     592     593     586       0       0       0       0
     473       1     587     594     595     588       0       0       0       0
     474       1     588     595     596     589       0       0       0       0
     475       1     589     596     597     590       0       0       0       0
     476       1     590     597     598     591       0       0       0       0
     477       1     591     598     599     592       0       0       0       0
     478       1     592     599     600     593       0       0       0       0
     479       1     594     601     602     595       0       0       0       0
     480       1     595     602     603     596       0       0       0       0
     481       1     596     603     604     597       0       0       0       0
     482       1     597     604     605     598       0       0       0       0
     483       1     598     605     606     599       0       0       0       0
     484       1     599     606     607     600       0       0       0       0
     485       1     601     608     609     602       0       0       0       0
     486       1     602     609     610     603       0       0       0       0
     487       1     603     610     611     604       0       0       0       0
     488       1     604     611     612     605       0       0       0       0
     489       1     605     612     613     606       0       0       0       0
     490       1     606     613     614     607       0       0       0       0
     491       1     608     615     616     609       0       0       0       0
     492       1     609     616     617     610       0       0       0       0
     493       1     610     617     618     611       0       0       0       0
     494       1     611     618     619     612       0       0       0       0
     495       1     612     619     620     613       0       0       0       0
     496       1     613     620     621     614       0       0       0       0
     497       1     615     622     623     616       0       0       0       0
     498       1     616     623     624     617       0       0       0       0
     499       1     617     624     625     618       0       0       0       0
     500       1     618     625     626     619       0       0       0       0
     501       1     619     626     627     620       0       0       0       0
     502       1     620     627     628     621       0       0       0       0
     503       1     622     629     630     623       0       0       0       0
     504       1     623     630     631     624       0       0       0       0
     505       1     624     631     632     625       0       0       0       0
     506       1     625     632     633     626       0       0       0       0
     507       1     626     633     634     627       0       0       0       0
     508       1     627     634     635     628       0       0       0       0
     509       1     629     636     637     630       0       0       0       0
     510       1     630     637     638     631       0       0       0       0
     511       1     631     638     639     632       0       0       0       0
     512       1     632     639     640     633       0       0       0       0
     513       1     633     640     641     634       0       0       0       0
     514       1     634     641     642     635       0       0       0       0
     515       1     636     643     644     637       0       0       0       0
     516       1     637     644     645     638       0       0       0       0
     517       1     638     645     646     639       0       0       0       0
     518       1     639     646     647     640       0       0       0       0
     519       1     640     647     648     641       0       0       0       0
     520       1     641     648     649     642       0       0       0       0
     521       1     643     650     651     644       0       0       0       0
     522       1     644     651     652     645       0       0       0       0
     523       1     645     652     653     646       0       0       0       0
     524       1     646     653     654     647       0       0       0       0
     525       1     647     654     655     648       0       0       0       0
     526       1     648     655     656     649       0       0       0       0
     527       1     650     657     658     651       0       0       0       0
     528       1     651     658     659     652       0       0       0       0
     529       1     652     659     660     653       0       0       0       0
     530       1     653     660     661     654       0       0       0       0
     531       1     654     661     662     655       0       0       0       0
     532       1     655     662     663     656       0       0       0       0
     533       1     657     664     665     658       0       0       0       0
     534       1     658     665     666     659       0       0       0       0
     535       1     659     666     667     660       0       0       0       0
     536       1     660     667     668     661       0       0       0       0
     537       1     661     668     669     662       0       0       0       0
     538       1     662     669     670     663       0       0       0       0
     539       1     664     671     672     665       0       0       0       0
     540       1     665     672     673     666       0       0       0       0
     541       1     666     673     674     667       0       0       0       0
     542       1     667     674     675     668       0       0       0       0
     543       1     668     675     676     669       0       0       0       0
     544       1     669     676     677     670       0       0       0       0
     545       1     671     678     679     672       0       0       0       0
     546       1     672     679     680     673       0       0       0       0
     547       1     673     680     681     674       0       0       0       0
     548       1     674     681     682     675       0       0       0       0
     549       1     675     682     683     676       0       0       0       0
     550       1     676     683     684     677       0       0       0       0
     551       1     678     685     686     679       0       0       0       0
     552       1     679     686     687     680       0       0       0       0
     553       1     680     687     688     681       0       0       0       0
     554       1     681     688     689     682       0       0       0       0
     555       1     682     689     690     683       0       0       0       0
     556       1     683     690     691     684       0       0       0       0
     557       1     685     692     693     686       0       0       0       0
     558       1     686     693     694     687       0       0       0       0
     559       1     687     694     695     688       0       0       0       0
     560       1     688     695     696     689       0       0       0       0
     561       1     689     696     697     690       0       0       0       0
     562       1     690     697     698     691       0       0       0       0
    2063       3    2367    2368    2370    2369       0       0       0       0
    2064       3    2369    2370    2372    2371       0       0       0       0
    2065       3    2371    2372    2374    2373       0       0       0       0
    2066       3    2373    2374    2376    2375       0       0       0       0
    2067       3    2375    2376    2378    2377       0       0       0       0
    2068       3    2377    2378    2380    2379       0       0       0       0
    2069       3    2379    2380    2382    2381       0       0       0       0
    2070       3    2381    2382    2384    2383       0       0       0       0
    2071       3    2383    2384    2386    2385       0       0       0       0
    2072       3    2385    2386    2388    2387       0       0       0       0
    2073       3    2387    2388    2390    2389       0       0       0       0
    2074       3    2389    2390    2392    2391       0       0       0       0
    2075       3    2391    2392    2394    2393       0       0       0       0
    2076       3    2393    2394    2396    2395       0       0       0       0
    2077       3    2395    2396    2398    2397       0       0       0       0
    2078       3    2397    2398    2400    2399       0       0       0       0
    2079       3    2399    2400    2402    2401       0       0       0       0
    2080       3    2401    2402    2404    2403       0       0       0       0
    2081       3    2403    2404    2406    2405       0       0       0       0
    2082       3    2405    2406    2408    2407       0       0       0       0
    2083       3    2407    2408    2410    2409       0       0       0       0
    2084       3    2409    2410    2412    2411       0       0       0       0
    2085       3    2411    2412    2414    2413       0       0       0       0
    2086       3    2413    2414    2416    2415       0       0       0       0
    2087       3    2415    2416    2418    2417       0       0       0       0
    2088       3    2417    2418    2420    2419       0       0       0       0
    2089       3    2419    2420    2422    2421       0       0       0       0
    2090       3    2421    2422    2424    2423       0       0       0       0
    2091       3    2423    2424    2426    2425       0       0       0       0
    2092       3    2425    2426    2428    2427       0       0       0       0
    2093       3    2427    2428    2430    2429       0       0       0       0
    2094       3    2429    2430    2432    2431       0       0       0       0
    2095       3    2431    2432    2434    2433       0       0       0       0
    2096       3    2433    2434    2436    2435       0       0       0       0
    2097       3    2435    2436    2438    2437       0       0       0       0
    2098       3    2437    2438    2440    2439       0       0       0       0
    2099       3    2439    2440    2442    2441       0       0       0       0
    2100       3    2441    2442    2444    2443       0       0       0       0
    2101       3    2443    2444    2446    2445       0       0       0       0
    2102       3    2445    2446    2448    2447       0       0       0       0
    2103       3    2447    2448    2450    2449       0       0       0       0
    2104       3    2449    2450    2452    2451       0       0       0       0
    2105       3    2451    2452    2454    2453       0       0       0       0
    2106       3    2453    2454    2456    2455       0       0       0       0
    2107       3    2455    2456    2458    2457       0       0       0       0
    2108       3    2457    2458    2460    2459       0       0       0       0
    2109       3    2459    2460    2462    2461       0       0       0       0
    2110       3    2461    2462    2464    2463       0       0       0       0
    2111       3    2463    2464    2466    2465       0       0       0       0
    2112       3    2465    2466    2468    2467       0       0       0       0
    2113       3    2467    2468    2470    2469       0       0       0       0
    2114       3    2469    2470    2472    2471       0       0       0       0
    2115       3    2471    2472    2474    2473       0       0       0       0
    2116       3    2473    2474    2476    2475       0       0       0       0
    2117       3    2475    2476    2478    2477       0       0       0       0
    2118       3    2477    2478    2480    2479       0       0       0       0
    2119       3    2479    2480    2482    2481       0       0       0       0
    2120       3    2481    2482    2484    2483       0       0       0       0
    2121       3    2483    2484    2486    2485       0       0       0       0
    2122       3    2485    2486    2488    2487       0       0       0       0
    2123       3    2487    2488    2490    2489       0       0       0       0
    2124       3    2489    2490    2492    2491       0       0       0       0
    2125       3    2491    2492    2494    2493       0       0       0       0
    2126       3    2493    2494    2496    2495       0       0       0       0
    2127       3    2495    2496    2498    2497       0       0       0       0
    2128       3    2497    2498    2500    2499       0       0       0       0
    2129       3    2499    2500    2502    2501       0       0       0       0
    2130       3    2501    2502    2504    2503       0       0       0       0
    2131       3    2503    2504    2506    2505       0       0       0       0
    2132       3    2505    2506    2508    2507       0       0       0       0
    2133       3    2507    2508    2510    2509       0       0       0       0
    2134       3    2509    2510    2512    2511       0       0       0       0
    2135       3    2511    2512    2514    2513       0       0       0       0
    2136       3    2513    2514    2516    2515       0       0       0       0
    2137       3    2515    2516    2518    2517       0       0       0       0
    2138       3    2517    2518    2520    2519       0       0       0       0
    2139       3    2519    2520    2522    2521       0       0       0       0
    2140       3    2521    2522    2524    2523       0       0       0       0
    2141       3    2523    2524    2526    2525       0       0       0       0
    2142       3    2525    2526    2528    2527       0       0       0       0
    2143       3    2527    2528    2530    2529       0       0       0       0
    2144       3    2529    2530    2532    2531       0       0       0       0
    2145       3    2531    2532    2534    2533       0       0       0       0
    2146       3    2533    2534    2536    2535       0       0       0       0
    2147       3    2535    2536    2538    2537       0       0       0       0
    2148       3    2537    2538    2540    2539       0       0       0       0
    2149       3    2539    2540    2542    2541       0       0       0       0
    2150       3    2541    2542    2544    2543       0       0       0       0
    2151       3    2543    2544    2546    2545       0       0       0       0
    2152       3    2545    2546    2548    2547       0       0       0       0
    2153       3    2547    2548    2550    2549       0       0       0       0
    2154       3    2549    2550    2552    2551       0       0       0       0
    2155       3    2551    2552    2554    2553       0       0       0       0
    2156       3    2553    2554    2556    2555       0       0       0       0
    2157       3    2555    2556    2558    2557       0       0       0       0
    2158       3    2557    2558    2560    2559       0       0       0       0
    2159       3    2559    2560    2562    2561       0       0       0       0
    2160       3    2561    2562    2564    2563       0       0       0       0
    2161       3    2563    2564    2566    2565       0       0       0       0
    2162       3    2565    2566    2568    2567       0       0       0       0
    2163       3    2567    2568    2570    2569       0       0       0       0
    2164       3    2569    2570    2572    2571       0       0       0       0
    2165       3    2571    2572    2574    2573       0       0       0       0
    2166       3    2573    2574    2576    2575       0       0       0       0
    2167       3    2575    2576    2578    2577       0       0       0       0
    2168       3    2577    2578    2580    2579       0       0       0       0
    2169       3    2579    2580    2582    2581       0       0       0       0
    2170       3    2581    2582    2584    2583       0       0       0       0
    2171       3    2583    2584    2586    2585       0       0       0       0
    2172       3    2585    2586    2588    2587       0       0       0       0
    2173       3    2587    2588    2590    2589       0       0       0       0
    2174       3    2589    2590    2592    2591       0       0       0       0
    2175       3    2591    2592    2594    2593       0       0       0       0
    2176       3    2593    2594    2596    2595       0       0       0       0
    2177       3    2595    2596    2598    2597       0       0       0       0
    2178       3    2597    2598    2600    2599       0       0       0       0
    2179       3    2599    2600    2602    2601       0       0       0       0
    2180       3    2601    2602    2604    2603       0       0       0       0
    2181       3    2603    2604    2606    2605       0       0       0       0
    2182       3    2605    2606    2608    2607       0       0       0       0
    2183       3    2607    2608    2610    2609       0       0       0       0
    2184       3    2609    2610    2612    2611       0       0       0       0
    2185       3    2611    2612    2614    2613       0       0       0       0
    2186       3    2613    2614    2616    2615       0       0       0       0
    2187       3    2615    2616    2618    2617       0       0       0       0
    2188       3    2617    2618    2620    2619       0       0       0       0
    2189       3    2619    2620    2622    2621       0       0       0       0
    2190       3    2621    2622    2624    2623       0       0       0       0
    2191       3    2623    2624    2626    2625       0       0       0       0
    2192       3    2625    2626    2628    2627       0       0       0       0
    2193       3    2627    2628    2630    2629       0       0       0       0
    2194       3    2629    2630    2632    2631       0       0       0       0
    2195       3    2631    2632    2634    2633       0       0       0       0
    2196       3    2633    2634    2636    2635       0       0       0       0
    2197       3    2635    2636    2638    2637       0       0       0       0
    2198       3    2637    2638    2640    2639       0       0       0       0
    2199       3    2639    2640    2642    2641       0       0       0       0
    2200       3    2641    2642    2644    2643       0       0       0       0
    2201       3    2643    2644    2646    2645       0       0       0       0
    2202       3    2645    2646    2648    2647       0       0       0       0
    2203       3    2647    2648    2650    2649       0       0       0       0
    2204       3    2649    2650    2652    2651       0       0       0       0
    2205       3    2651    2652    2654    2653       0       0       0       0
    2206       3    2653    2654    2656    2655       0       0       0       0
    2207       3    2655    2656    2658    2657       0       0       0       0
    2208       3    2657    2658    2660    2659       0       0       0       0
    2209       3    2659    2660    2662    2661       0       0       0       0
    2210       3    2661    2662    2664    2663       0       0       0       0
    2211       3    2663    2664    2666    2665       0       0       0       0
    2212       3    2665    2666    2668    2667       0       0       0       0
    2213       3    2368    2669    2670    2370       0       0       0       0
    2214       3    2370    2670    2671    2372       0       0       0       0
    2215       3    2372    2671    2672    2374       0       0       0       0
    2216       3    2374    2672    2673    2376       0       0       0       0
    2217       3    2376    2673    2674    2378       0       0       0       0
    2218       3    2378    2674    2675    2380       0       0       0       0
    2219       3    2380    2675    2676    2382       0       0       0       0
    2220       3    2382    2676    2677    2384       0       0       0       0
    2221       3    2384    2677    2678    2386       0       0       0       0
    2222       3    2386    2678    2679    2388       0       0       0       0
    2223       3    2388    2679    2680    2390       0       0       0       0
    2224       3    2390    2680    2681    2392       0       0       0       0
    2225       3    2392    2681    2682    2394       0       0       0       0
    2226       3    2394    2682    2683    2396       0       0       0       0
    2227       3    2396    2683    2684    2398       0       0       0       0
    2228       3    2398    2684    2685    2400       0       0       0       0
    2229       3    2400    2685    2686    2402       0       0       0       0
    2230       3    2402    2686    2687    2404       0       0       0       0
    2231       3    2404    2687    2688    2406       0       0       0       0
    2232       3    2406    2688    2689    2408       0       0       0       0
    2233       3    2408    2689    2690    2410       0       0       0       0
    2234       3    2410    2690    2691    2412       0       0       0       0
    2235       3    2412    2691    2692    2414       0       0       0       0
    2236       3    2414    2692    2693    2416       0       0       0       0
    2237       3    2416    2693    2694    2418       0       0       0       0
    2238       3    2418    2694    2695    2420       0       0       0       0
    2239       3    2420    2695    2696    2422       0       0       0       0
    2240       3    2422    2696    2697    2424       0       0       0       0
    2241       3    2424    2697    2698    2426       0       0       0       0
    2242       3    2426    2698    2699    2428       0       0       0       0
    2243       3    2428    2699    2700    2430       0       0       0       0
    2244       3    2430    2700    2701    2432       0       0       0       0
    2245       3    2432    2701    2702    2434       0       0       0       0
    2246       3    2434    2702    2703    2436       0       0       0       0
    2247       3    2436    2703    2704    2438       0       0       0       0
    2248       3    2438    2704    2705    2440       0       0       0       0
    2249       3    2440    2705    2706    2442       0       0       0       0
    2250       3    2442    2706    2707    2444       0       0       0       0
    2251       3    2444    2707    2708    2446       0       0       0       0
    2252       3    2446    2708    2709    2448       0       0       0       0
    2253       3    2448    2709    2710    2450       0       0       0       0
    2254       3    2450    2710    2711    2452       0       0       0       0
    2255       3    2452    2711    2712    2454       0       0       0       0
    2256       3    2454    2712    2713    2456       0       0       0       0
    2257       3    2456    2713    2714    2458       0       0       0       0
    2258       3    2458    2714    2715    2460       0       0       0       0
    2259       3    2460    2715    2716    2462       0       0       0       0
    2260       3    2462    2716    2717    2464       0       0       0       0
    2261       3    2464    2717    2718    2466       0       0       0       0
    2262       3    2466    2718    2719    2468       0       0       0       0
    2263       3    2468    2719    2720    2470       0       0       0       0
    2264       3    2470    2720    2721    2472       0       0       0       0
    2265       3    2472    2721    2722    2474       0       0       0       0
    2266       3    2474    2722    2723    2476       0       0       0       0
    2267       3    2476    2723    2724    2478       0       0       0       0
    2268       3    2478    2724    2725    2480       0       0       0       0
    2269       3    2480    2725    2726    2482       0       0       0       0
    2270       3    2482    2726    2727    2484       0       0       0       0
    2271       3    2484    2727    2728    2486       0       0       0       0
    2272       3    2486    2728    2729    2488       0       0       0       0
    2273       3    2488    2729    2730    2490       0       0       0       0
    2274       3    2490    2730    2731    2492       0       0       0       0
    2275       3    2492    2731    2732    2494       0       0       0       0
    2276       3    2494    2732    2733    2496       0       0       0       0
    2277       3    2496    2733    2734    2498       0       0       0       0
    2278       3    2498    2734    2735    2500       0       0       0       0
    2279       3    2500    2735    2736    2502       0       0       0       0
    2280       3    2502    2736    2737    2504       0       0       0       0
    2281       3    2504    2737    2738    2506       0       0       0       0
    2282       3    2506    2738    2739    2508       0       0       0       0
    2283       3    2508    2739    2740    2510       0       0       0       0
    2284       3    2510    2740    2741    2512       0       0       0       0
    2285       3    2512    2741    2742    2514       0       0       0       0
    2286       3    2514    2742    2743    2516       0       0       0       0
    2287       3    2516    2743    2744    2518       0       0       0       0
    2288       3    2518    2744    2745    2520       0       0       0       0
    2289       3    2520    2745    2746    2522       0       0       0       0
    2290       3    2522    2746    2747    2524       0       0       0       0
    2291       3    2524    2747    2748    2526       0       0       0       0
    2292       3    2526    2748    2749    2528       0       0       0       0
    2293       3    2528    2749    2750    2530       0       0       0       0
    2294       3    2530    2750    2751    2532       0       0       0       0
    2295       3    2532    2751    2752    2534       0       0       0       0
    2296       3    2534    2752    2753    2536       0       0       0       0
    2297       3    2536    2753    2754    2538       0       0       0       0
    2298       3    2538    2754    2755    2540       0       0       0       0
    2299       3    2540    2755    2756    2542       0       0       0       0
    2300       3    2542    2756    2757    2544       0       0       0       0
    2301       3    2544    2757    2758    2546       0       0       0       0
    2302       3    2546    2758    2759    2548       0       0       0       0
    2303       3    2548    2759    2760    2550       0       0       0       0
    2304       3    2550    2760    2761    2552       0       0       0       0
    2305       3    2552    2761    2762    2554       0       0       0       0
    2306       3    2554    2762    2763    2556       0       0       0       0
    2307       3    2556    2763    2764    2558       0       0       0       0
    2308       3    2558    2764    2765    2560       0       0       0       0
    2309       3    2560    2765    2766    2562       0       0       0       0
    2310       3    2562    2766    2767    2564       0       0       0       0
    2311       3    2564    2767    2768    2566       0       0       0       0
    2312       3    2566    2768    2769    2568       0       0       0       0
    2313       3    2568    2769    2770    2570       0       0       0       0
    2314       3    2570    2770    2771    2572       0       0       0       0
    2315       3    2572    2771    2772    2574       0       0       0       0
    2316       3    2574    2772    2773    2576       0       0       0       0
    2317       3    2576    2773    2774    2578       0       0       0       0
    2318       3    2578    2774    2775    2580       0       0       0       0
    2319       3    2580    2775    2776    2582       0       0       0       0
    2320       3    2582    2776    2777    2584       0       0       0       0
    2321       3    2584    2777    2778    2586       0       0       0       0
    2322       3    2586    2778    2779    2588       0       0       0       0
    2323       3    2588    2779    2780    2590       0       0       0       0
    2324       3    2590    2780    2781    2592       0       0       0       0
    2325       3    2592    2781    2782    2594       0       0       0       0
    2326       3    2594    2782    2783    2596       0       0       0       0
    2327       3    2596    2783    2784    2598       0       0       0       0
    2328       3    2598    2784    2785    2600       0       0       0       0
    2329       3    2600    2785    2786    2602       0       0       0       0
    2330       3    2602    2786    2787    2604       0       0       0       0
    2331       3    2604    2787    2788    2606       0       0       0       0
    2332       3    2606    2788    2789    2608       0       0       0       0
    2333       3    2608    2789    2790    2610       0       0       0       0
    2334       3    2610    2790    2791    2612       0       0       0       0
    2335       3    2612    2791    2792    2614       0       0       0       0
    2336       3    2614    2792    2793    2616       0       0       0       0
    2337       3    2616    2793    2794    2618       0       0       0       0
    2338       3    2618    2794    2795    2620       0       0       0       0
    2339       3    2620    2795    2796    2622       0       0       0       0
    2340       3    2622    2796    2797    2624       0       0       0       0
    2341       3    2624    2797    2798    2626       0       0       0       0
    2342       3    2626    2798    2799    2628       0       0       0       0
    2343       3    2628    2799    2800    2630       0       0       0       0
    2344       3    2630    2800    2801    2632       0       0       0       0
    2345       3    2632    2801    2802    2634       0       0       0       0
    2346       3    2634    2802    2803    2636       0       0       0       0
    2347       3    2636    2803    2804    2638       0       0       0       0
    2348       3    2638    2804    2805    2640       0       0       0       0
    2349       3    2640    2805    2806    2642       0       0       0       0
    2350       3    2642    2806    2807    2644       0       0       0       0
    2351       3    2644    2807    2808    2646       0       0       0       0
    2352       3    2646    2808    2809    2648       0       0       0       0
    2353       3    2648    2809    2810    2650       0       0       0       0
    2354       3    2650    2810    2811    2652       0       0       0       0
    2355       3    2652    2811    2812    2654       0       0       0       0
    2356       3    2654    2812    2813    2656       0       0       0       0
    2357       3    2656    2813    2814    2658       0       0       0       0
    2358       3    2658    2814    2815    2660       0       0       0       0
    2359       3    2660    2815    2816    2662       0       0       0       0
    2360       3    2662    2816    2817    2664       0       0       0       0
    2361       3    2664    2817    2818    2666       0       0       0       0
    2362       3    2666    2818    2819    2668       0       0       0       0
    2363       3    2669    2820    2821    2670       0       0       0       0
    2364       3    2670    2821    2822    2671       0       0       0       0
    2365       3    2671    2822    2823    2672       0       0       0       0
    2366       3    2672    2823    2824    2673       0       0       0       0
    2367       3    2673    2824    2825    2674       0       0       0       0
    2368       3    2674    2825    2826    2675       0       0       0       0
    2369       3    2675    2826    2827    2676       0       0       0       0
    2370       3    2676    2827    2828    2677       0       0       0       0
    2371       3    2677    2828    2829    2678       0       0       0       0
    2372       3    2678    2829    2830    2679       0       0       0       0
    2373       3    2679    2830    2831    2680       0       0       0       0
    2374       3    2680    2831    2832    2681       0       0       0       0
    2375       3    2681    2832    2833    2682       0       0       0       0
    2376       3    2682    2833    2834    2683       0       0       0       0
    2377       3    2683    2834    2835    2684       0       0       0       0
    2378       3    2684    2835    2836    2685       0       0       0       0
    2379       3    2685    2836    2837    2686       0       0       0       0
    2380       3    2686    2837    2838    2687       0       0       0       0
    2381       3    2687    2838    2839    2688       0       0       0       0
    2382       3    2688    2839    2840    2689       0       0       0       0
    2383       3    2689    2840    2841    2690       0       0       0       0
    2384       3    2690    2841    2842    2691       0       0       0       0
    2385       3    2691    2842    2843    2692       0       0       0       0
    2386       3    2692    2843    2844    2693       0       0       0       0
    2387       3    2693    2844    2845    2694       0       0       0       0
    2388       3    2694    2845    2846    2695       0       0       0       0
    2389       3    2695    2846    2847    2696       0       0       0       0
    2390       3    2696    2847    2848    2697       0       0       0       0
    2391       3    2697    2848    2849    2698       0       0       0       0
    2392       3    2698    2849    2850    2699       0       0       0       0
    2393       3    2699    2850    2851    2700       0       0       0       0
    2394       3    2700    2851    2852    2701       0       0       0       0
    2395       3    2701    2852    2853    2702       0       0       0       0
    2396       3    2702    2853    2854    2703       0       0       0       0
    2397       3    2703    2854    2855    2704       0       0       0       0
    2398       3    2704    2855    2856    2705       0       0       0       0
    2399       3    2705    2856    2857    2706       0       0       0       0
    2400       3    2706    2857    2858    2707       0       0       0       0
    2401       3    2707    2858    2859    2708       0       0       0       0
    2402       3    2708    2859    2860    2709       0       0       0       0
    2403       3    2709    2860    2861    2710       0       0       0       0
    2404       3    2710    2861    2862    2711       0       0       0       0
    2405       3    2711    2862    2863    2712       0       0       0       0
    2406       3    2712    2863    2864    2713       0       0       0       0
    2407       3    2713    2864    2865    2714       0       0       0       0
    2408       3    2714    2865    2866    2715       0       0       0       0
    2409       3    2715    2866    2867    2716       0       0       0       0
    2410       3    2716    2867    2868    2717       0       0       0       0
    2411       3    2717    2868    2869    2718       0       0       0       0
    2412       3    2718    2869    2870    2719       0       0       0       0
    2413       3    2719    2870    2871    2720       0       0       0       0
    2414       3    2720    2871    2872    2721       0       0       0       0
    2415       3    2721    2872    2873    2722       0       0       0       0
    2416       3    2722    2873    2874    2723       0       0       0       0
    2417       3    2723    2874    2875    2724       0       0       0       0
    2418       3    2724    2875    2876    2725       0       0       0       0
    2419       3    2725    2876    2877    2726       0       0       0       0
    2420       3    2726    2877    2878    2727       0       0       0       0
    2421       3    2727    2878    2879    2728       0       0       0       0
    2422       3    2728    2879    2880    2729       0       0       0       0
    2423       3    2729    2880    2881    2730       0       0       0       0
    2424       3    2730    2881    2882    2731       0       0       0       0
    2425       3    2731    2882    2883    2732       0       0       0       0
    2426       3    2732    2883    2884    2733       0       0       0       0
    2427       3    2733    2884    2885    2734       0       0       0       0
    2428       3    2734    2885    2886    2735       0       0       0       0
    2429       3    2735    2886    2887    2736       0       0       0       0
    2430       3    2736    2887    2888    2737       0       0       0       0
    2431       3    2737    2888    2889    2738       0       0       0       0
    2432       3    2738    2889    2890    2739       0       0       0       0
    2433       3    2739    2890    2891    2740       0       0       0       0
    2434       3    2740    2891    2892    2741       0       0       0       0
    2435       3    2741    2892    2893    2742       0       0       0       0
    2436       3    2742    2893    2894    2743       0       0       0       0
    2437       3    2743    2894    2895    2744       0       0       0       0
    2438       3    2744    2895    2896    2745       0       0       0       0
    2439       3    2745    2896    2897    2746       0       0       0       0
    2440       3    2746    2897    2898    2747       0       0       0       0
    2441       3    2747    2898    2899    2748       0       0       0       0
    2442       3    2748    2899    2900    2749       0       0       0       0
    2443       3    2749    2900    2901    2750       0       0       0       0
    2444       3    2750    2901    2902    2751       0       0       0       0
    2445       3    2751    2902    2903    2752       0       0       0       0
    2446       3    2752    2903    2904    2753       0       0       0       0
    2447       3    2753    2904    2905    2754       0       0       0       0
    2448       3    2754    2905    2906    2755       0       0       0       0
    2449       3    2755    2906    2907    2756       0       0       0       0
    2450       3    2756    2907    2908    2757       0       0       0       0
    2451       3    2757    2908    2909    2758       0       0       0       0
    2452       3    2758    2909    2910    2759       0       0       0       0
    2453       3    2759    2910    2911    2760       0       0       0       0
    2454       3    2760    2911    2912    2761       0       0       0       0
    2455       3    2761    2912    2913    2762       0       0       0       0
    2456       3    2762    2913    2914    2763       0       0       0       0
    2457       3    2763    2914    2915    2764       0       0       0       0
    2458       3    2764    2915    2916    2765       0       0       0       0
    2459       3    2765    2916    2917    2766       0       0       0       0
    2460       3    2766    2917    2918    2767       0       0       0       0
    2461       3    2767    2918    2919    2768       0       0       0       0
    2462       3    2768    2919    2920    2769       0       0       0       0
    2463       3    2769    2920    2921    2770       0       0       0       0
    2464       3    2770    2921    2922    2771       0       0       0       0
    2465       3    2771    2922    2923    2772       0       0       0       0
    2466       3    2772    2923    2924    2773       0       0       0       0
    2467       3    2773    2924    2925    2774       0       0       0       0
    2468       3    2774    2925    2926    2775       0       0       0       0
    2469       3    2775    2926    2927    2776       0       0       0       0
    2470       3    2776    2927    2928    2777       0       0       0       0
    2471       3    2777    2928    2929    2778       0       0       0       0
    2472       3    2778    2929    2930    2779       0       0       0       0
    2473       3    2779    2930    2931    2780       0       0       0       0
    2474       3    2780    2931    2932    2781       0       0       0       0
    2475       3    2781    2932    2933    2782       0       0       0       0
    2476       3    2782    2933    2934    2783       0       0       0       0
    2477       3    2783    2934    2935    2784       0       0       0       0
    2478       3    2784    2935    2936    2785       0       0       0       0
    2479       3    2785    2936    2937    2786       0       0       0       0
    2480       3    2786    2937    2938    2787       0       0       0       0
    2481       3    2787    2938    2939    2788       0       0       0       0
    2482       3    2788    2939    2940    2789       0       0       0       0
    2483       3    2789    2940    2941    2790       0       0       0       0
    2484       3    2790    2941    2942    2791       0       0       0       0
    2485       3    2791    2942    2943    2792       0       0       0       0
    2486       3    2792    2943    2944    2793       0       0       0       0
    2487       3    2793    2944    2945    2794       0       0       0       0
    2488       3    2794    2945    2946    2795       0       0       0       0
    2489       3    2795    2946    2947    2796       0       0       0       0
    2490       3    2796    2947    2948    2797       0       0       0       0
    2491       3    2797    2948    2949    2798       0       0       0       0
    2492       3    2798    2949    2950    2799       0       0       0       0
    2493       3    2799    2950    2951    2800       0       0       0       0
    2494       3    2800    2951    2952    2801       0       0       0       0
    2495       3    2801    2952    2953    2802       0       0       0       0
    2496       3    2802    2953    2954    2803       0       0       0       0
    2497       3    2803    2954    2955    2804       0       0       0       0
    2498       3    2804    2955    2956    2805       0       0       0       0
    2499       3    2805    2956    2957    2806       0       0       0       0
    2500       3    2806    2957    2958    2807       0       0       0       0
    2501       3    2807    2958    2959    2808       0       0       0       0
    2502       3    2808    2959    2960    2809       0       0       0       0
    2503       3    2809    2960    2961    2810       0       0       0       0
    2504       3    2810    2961    2962    2811       0       0       0       0
    2505       3    2811    2962    2963    2812       0       0       0       0
    2506       3    2812    2963    2964    2813       0       0       0       0
    2507       3    2813    2964    2965    2814       0       0       0       0
    2508       3    2814    2965    2966    2815       0       0       0       0
    2509       3    2815    2966    2967    2816       0       0       0       0
    2510       3    2816    2967    2968    2817       0       0       0       0
    2511       3    2817    2968    2969    2818       0       0       0       0
    2512       3    2818    2969    2970    2819       0       0       0       0
    2513       3    2820    2971    2972    2821       0       0       0       0
    2514       3    2821    2972    2973    2822       0       0       0       0
    2515       3    2822    2973    2974    2823       0       0       0       0
    2516       3    2823    2974    2975    2824       0       0       0       0
    2517       3    2824    2975    2976    2825       0       0       0       0
    2518       3    2825    2976    2977    2826       0       0       0       0
    2519       3    2826    2977    2978    2827       0       0       0       0
    2520       3    2827    2978    2979    2828       0       0       0       0
    2521       3    2828    2979    2980    2829       0       0       0       0
    2522       3    2829    2980    2981    2830       0       0       0       0
    2523       3    2830    2981    2982    2831       0       0       0       0
    2524       3    2831    2982    2983    2832       0       0       0       0
    2525       3    2832    2983    2984    2833       0       0       0       0
    2526       3    2833    2984    2985    2834       0       0       0       0
    2527       3    2834    2985    2986    2835       0       0       0       0
    2528       3    2835    2986    2987    2836       0       0       0       0
    2529       3    2836    2987    2988    2837       0       0       0       0
    2530       3    2837    2988    2989    2838       0       0       0       0
    2531       3    2838    2989    2990    2839       0       0       0       0
    2532       3    2839    2990    2991    2840       0       0       0       0
    2533       3    2840    2991    2992    2841       0       0       0       0
    2534       3    2841    2992    2993    2842       0       0       0       0
    2535       3    2842    2993    2994    2843       0       0       0       0
    2536       3    2843    2994    2995    2844       0       0       0       0
    2537       3    2844    2995    2996    2845       0       0       0       0
    2538       3    2845    2996    2997    2846       0       0       0       0
    2539       3    2846    2997    2998    2847       0       0       0       0
    2540       3    2847    2998    2999    2848       0       0       0       0
    2541       3    2848    2999    3000    2849       0       0       0       0
    2542       3    2849    3000    3001    2850       0       0       0       0
    2543       3    2850    3001    3002    2851       0       0       0       0
    2544       3    2851    3002    3003    2852       0       0       0       0
    2545       3    2852    3003    3004    2853       0       0       0       0
    2546       3    2853    3004    3005    2854       0       0       0       0
    2547       3    2854    3005    3006    2855       0       0       0       0
    2548       3    2855    3006    3007    2856       0       0       0       0
    2549       3    2856    3007    3008    2857       0       0       0       0
    2550       3    2857    3008    3009    2858       0       0       0       0
    2551       3    2858    3009    3010    2859       0       0       0       0
    2552       3    2859    3010    3011    2860       0       0       0       0
    2553       3    2860    3011    3012    2861       0       0       0       0
    2554       3    2861    3012    3013    2862       0       0       0       0
    2555       3    2862    3013    3014    2863       0       0       0       0
    2556       3    2863    3014    3015    2864       0       0       0       0
    2557       3    2864    3015    3016    2865       0       0       0       0
    2558       3    2865    3016    3017    2866       0       0       0       0
    2559       3    2866    3017    3018    2867       0       0       0       0
    2560       3    2867    3018    3019    2868       0       0       0       0
    2561       3    2868    3019    3020    2869       0       0       0       0
    2562       3    2869    3020    3021    2870       0       0       0       0
    2563       3    2870    3021    3022    2871       0       0       0       0
    2564       3    2871    3022    3023    2872       0       0       0       0
    2565       3    2872    3023    3024    2873       0       0       0       0
    2566       3    2873    3024    3025    2874       0       0       0       0
    2567       3    2874    3025    3026    2875       0       0       0       0
    2568       3    2875    3026    3027    2876       0       0       0       0
    2569       3    2876    3027    3028    2877       0       0       0       0
    2570       3    2877    3028    3029    2878       0       0       0       0
    2571       3    2878    3029    3030    2879       0       0       0       0
    2572       3    2879    3030    3031    2880       0       0       0       0
    2573       3    2880    3031    3032    2881       0       0       0       0
    2574       3    2881    3032    3033    2882       0       0       0       0
    2575       3    2882    3033    3034    2883       0       0       0       0
    2576       3    2883    3034    3035    2884       0       0       0       0
    2577       3    2884    3035    3036    2885       0       0       0       0
    2578       3    2885    3036    3037    2886       0       0       0       0
    2579       3    2886    3037    3038    2887       0       0       0       0
    2580       3    2887    3038    3039    2888       0       0       0       0
    2581       3    2888    3039    3040    2889       0       0       0       0
    2582       3    2889    3040    3041    2890       0       0       0       0
    2583       3    2890    3041    3042    2891       0       0       0       0
    2584       3    2891    3042    3043    2892       0       0       0       0
    2585       3    2892    3043    3044    2893       0       0       0       0
    2586       3    2893    3044    3045    2894       0       0       0       0
    2587       3    2894    3045    3046    2895       0       0       0       0
    2588       3    2895    3046    3047    2896       0       0       0       0
    2589       3    2896    3047    3048    2897       0       0       0       0
    2590       3    2897    3048    3049    2898       0       0       0       0
    2591       3    2898    3049    3050    2899       0       0       0       0
    2592       3    2899    3050    3051    2900       0       0       0       0
    2593       3    2900    3051    3052    2901       0       0       0       0
    2594       3    2901    3052    3053    2902       0       0       0       0
    2595       3    2902    3053    3054    2903       0       0       0       0
    2596       3    2903    3054    3055    2904       0       0       0       0
    2597       3    2904    3055    3056    2905       0       0       0       0
    2598       3    2905    3056    3057    2906       0       0       0       0
    2599       3    2906    3057    3058    2907       0       0       0       0
    2600       3    2907    3058    3059    2908       0       0       0       0
    2601       3    2908    3059    3060    2909       0       0       0       0
    2602       3    2909    3060    3061    2910       0       0       0       0
    2603       3    2910    3061    3062    2911       0       0       0       0
    2604       3    2911    3062    3063    2912       0       0       0       0
    2605       3    2912    3063    3064    2913       0       0       0       0
    2606       3    2913    3064    3065    2914       0       0       0       0
    2607       3    2914    3065    3066    2915       0       0       0       0
    2608       3    2915    3066    3067    2916       0       0       0       0
    2609       3    2916    3067    3068    2917       0       0       0       0
    2610       3    2917    3068    3069    2918       0       0       0       0
    2611       3    2918    3069    3070    2919       0       0       0       0
    2612       3    2919    3070    3071    2920       0       0       0       0
    2613       3    2920    3071    3072    2921       0       0       0       0
    2614       3    2921    3072    3073    2922       0       0       0       0
    2615       3    2922    3073    3074    2923       0       0       0       0
    2616       3    2923    3074    3075    2924       0       0       0       0
    2617       3    2924    3075    3076    2925       0       0       0       0
    2618       3    2925    3076    3077    2926       0       0       0       0
    2619       3    2926    3077    3078    2927       0       0       0       0
    2620       3    2927    3078    3079    2928       0       0       0       0
    2621       3    2928    3079    3080    2929       0       0       0       0
    2622       3    2929    3080    3081    2930       0       0       0       0
    2623       3    2930    3081    3082    2931       0       0       0       0
    2624       3    2931    3082    3083    2932       0       0       0       0
    2625       3    2932    3083    3084    2933       0       0       0       0
    2626       3    2933    3084    3085    2934       0       0       0       0
    2627       3    2934    3085    3086    2935       0       0       0       0
    2628       3    2935    3086    3087    2936       0       0       0       0
    2629       3    2936    3087    3088    2937       0       0       0       0
    2630       3    2937    3088    3089    2938       0       0       0       0
    2631       3    2938    3089    3090    2939       0       0       0       0
    2632       3    2939    3090    3091    2940       0       0       0       0
    2633       3    2940    3091    3092    2941       0       0       0       0
    2634       3    2941    3092    3093    2942       0       0       0       0
    2635       3    2942    3093    3094    2943       0       0       0       0
    2636       3    2943    3094    3095    2944       0       0       0       0
    2637       3    2944    3095    3096    2945       0       0       0       0
    2638       3    2945    3096    3097    2946       0       0       0       0
    2639       3    2946    3097    3098    2947       0       0       0       0
    2640       3    2947    3098    3099    2948       0       0       0       0
    2641       3    2948    3099    3100    2949       0       0       0       0
    2642       3    2949    3100    3101    2950       0       0       0       0
    2643       3    2950    3101    3102    2951       0       0       0       0
    2644       3    2951    3102    3103    2952       0       0       0       0
    2645       3    2952    3103    3104    2953       0       0       0       0
    2646       3    2953    3104    3105    2954       0       0       0       0
    2647       3    2954    3105    3106    2955       0       0       0       0
    2648       3    2955    3106    3107    2956       0       0       0       0
    2649       3    2956    3107    3108    2957       0       0       0       0
    2650       3    2957    3108    3109    2958       0       0       0       0
    2651       3    2958    3109    3110    2959       0       0       0       0
    2652       3    2959    3110    3111    2960       0       0       0       0
    2653       3    2960    3111    3112    2961       0       0       0       0
    2654       3    2961    3112    3113    2962       0       0       0       0
    2655       3    2962    3113    3114    2963       0       0       0       0
    2656       3    2963    3114    3115    2964       0       0       0       0
    2657       3    2964    3115    3116    2965       0       0       0       0
    2658       3    2965    3116    3117    2966       0       0       0       0
    2659       3    2966    3117    3118    2967       0       0       0       0
    2660       3    2967    3118    3119    2968       0       0       0       0
    2661       3    2968    3119    3120    2969       0       0       0       0
    2662       3    2969    3120    3121    2970       0       0       0       0
    2663       3    2971    3122    3123    2972       0       0       0       0
    2664       3    2972    3123    3124    2973       0       0       0       0
    2665       3    2973    3124    3125    2974       0       0       0       0
    2666       3    2974    3125    3126    2975       0       0       0       0
    2667       3    2975    3126    3127    2976       0       0       0       0
    2668       3    2976    3127    3128    2977       0       0       0       0
    2669       3    2977    3128    3129    2978       0       0       0       0
    2670       3    2978    3129    3130    2979       0       0       0       0
    2671       3    2979    3130    3131    2980       0       0       0       0
    2672       3    2980    3131    3132    2981       0       0       0       0
    2673       3    2981    3132    3133    2982       0       0       0       0
    2674       3    2982    3133    3134    2983       0       0       0       0
    2675       3    2983    3134    3135    2984       0       0       0       0
    2676       3    2984    3135    3136    2985       0       0       0       0
    2677       3    2985    3136    3137    2986       0       0       0       0
    2678       3    2986    3137    3138    2987       0       0       0       0
    2679       3    2987    3138    3139    2988       0       0       0       0
    2680       3    2988    3139    3140    2989       0       0       0       0
    2681       3    2989    3140    3141    2990       0       0       0       0
    2682       3    2990    3141    3142    2991       0       0       0       0
    2683       3    2991    3142    3143    2992       0       0       0       0
    2684       3    2992    3143    3144    2993       0       0       0       0
    2685       3    2993    3144    3145    2994       0       0       0       0
    2686       3    2994    3145    3146    2995       0       0       0       0
    2687       3    2995    3146    3147    2996       0       0       0       0
    2688       3    2996    3147    3148    2997       0       0       0       0
    2689       3    2997    3148    3149    2998       0       0       0       0
    2690       3    2998    3149    3150    2999       0       0       0       0
    2691       3    2999    3150    3151    3000       0       0       0       0
    2692       3    3000    3151    3152    3001       0       0       0       0
    2693       3    3001    3152    3153    3002       0       0       0       0
    2694       3    3002    3153    3154    3003       0       0       0       0
    2695       3    3003    3154    3155    3004       0       0       0       0
    2696       3    3004    3155    3156    3005       0       0       0       0
    2697       3    3005    3156    3157    3006       0       0       0       0
    2698       3    3006    3157    3158    3007       0       0       0       0
    2699       3    3007    3158    3159    3008       0       0       0       0
    2700       3    3008    3159    3160    3009       0       0       0       0
    2701       3    3009    3160    3161    3010       0       0       0       0
    2702       3    3010    3161    3162    3011       0       0       0       0
    2703       3    3011    3162    3163    3012       0       0       0       0
    2704       3    3012    3163    3164    3013       0       0       0       0
    2705       3    3013    3164    3165    3014       0       0       0       0
    2706       3    3014    3165    3166    3015       0       0       0       0
    2707       3    3015    3166    3167    3016       0       0       0       0
    2708       3    3016    3167    3168    3017       0       0       0       0
    2709       3    3017    3168    3169    3018       0       0       0       0
    2710       3    3018    3169    3170    3019       0       0       0       0
    2711       3    3019    3170    3171    3020       0       0       0       0
    2712       3    3020    3171    3172    3021       0       0       0       0
    2713       3    3021    3172    3173    3022       0       0       0       0
    2714       3    3022    3173    3174    3023       0       0       0       0
    2715       3    3023    3174    3175    3024       0       0       0       0
    2716       3    3024    3175    3176    3025       0       0       0       0
    2717       3    3025    3176    3177    3026       0       0       0       0
    2718       3    3026    3177    3178    3027       0       0       0       0
    2719       3    3027    3178    3179    3028       0       0       0       0
    2720       3    3028    3179    3180    3029       0       0       0       0
    2721       3    3029    3180    3181    3030       0       0       0       0
    2722       3    3030    3181    3182    3031       0       0       0       0
    2723       3    3031    3182    3183    3032       0       0       0       0
    2724       3    3032    3183    3184    3033       0       0       0       0
    2725       3    3033    3184    3185    3034       0       0       0       0
    2726       3    3034    3185    3186    3035       0       0       0       0
    2727       3    3035    3186    3187    3036       0       0       0       0
    2728       3    3036    3187    3188    3037       0       0       0       0
    2729       3    3037    3188    3189    3038       0       0       0       0
    2730       3    3038    3189    3190    3039       0       0       0       0
    2731       3    3039    3190    3191    3040       0       0       0       0
    2732       3    3040    3191    3192    3041       0       0       0       0
    2733       3    3041    3192    3193    3042       0       0       0       0
    2734       3    3042    3193    3194    3043       0       0       0       0
    2735       3    3043    3194    3195    3044       0       0       0       0
    2736       3    3044    3195    3196    3045       0       0       0       0
    2737       3    3045    3196    3197    3046       0       0       0       0
    2738       3    3046    3197    3198    3047       0       0       0       0
    2739       3    3047    3198    3199    3048       0       0       0       0
    2740       3    3048    3199    3200    3049       0       0       0       0
    2741       3    3049    3200    3201    3050       0       0       0       0
    2742       3    3050    3201    3202    3051       0       0       0       0
    2743       3    3051    3202    3203    3052       0       0       0       0
    2744       3    3052    3203    3204    3053       0       0       0       0
    2745       3    3053    3204    3205    3054       0       0       0       0
    2746       3    3054    3205    3206    3055       0       0       0       0
    2747       3    3055    3206    3207    3056       0       0       0       0
    2748       3    3056    3207    3208    3057       0       0       0       0
    2749       3    3057    3208    3209    3058       0       0       0       0
    2750       3    3058    3209    3210    3059       0       0       0       0
    2751       3    3059    3210    3211    3060       0       0       0       0
    2752       3    3060    3211    3212    3061       0       0       0       0
    2753       3    3061    3212    3213    3062       0       0       0       0
    2754       3    3062    3213    3214    3063       0       0       0       0
    2755       3    3063    3214    3215    3064       0       0       0       0
    2756       3    3064    3215    3216    3065       0       0       0       0
    2757       3    3065    3216    3217    3066       0       0       0       0
    2758       3    3066    3217    3218    3067       0       0       0       0
    2759       3    3067    3218    3219    3068       0       0       0       0
    2760       3    3068    3219    3220    3069       0       0       0       0
    2761       3    3069    3220    3221    3070       0       0       0       0
    2762       3    3070    3221    3222    3071       0       0       0       0
    2763       3    3071    3222    3223    3072       0       0       0       0
    2764       3    3072    3223    3224    3073       0       0       0       0
    2765       3    3073    3224    3225    3074       0       0       0       0
    2766       3    3074    3225    3226    3075       0       0       0       0
    2767       3    3075    3226    3227    3076       0       0       0       0
    2768       3    3076    3227    3228    3077       0       0       0       0
    2769       3    3077    3228    3229    3078       0       0       0       0
    2770       3    3078    3229    3230    3079       0       0       0       0
    2771       3    3079    3230    3231    3080       0       0       0       0
    2772       3    3080    3231    3232    3081       0       0       0       0
    2773       3    3081    3232    3233    3082       0       0       0       0
    2774       3    3082    3233    3234    3083       0       0       0       0
    2775       3    3083    3234    3235    3084       0       0       0       0
    2776       3    3084    3235    3236    3085       0       0       0       0
    2777       3    3085    3236    3237    3086       0       0       0       0
    2778       3    3086    3237    3238    3087       0       0       0       0
    2779       3    3087    3238    3239    3088       0       0       0       0
    2780       3    3088    3239    3240    3089       0       0       0       0
    2781       3    3089    3240    3241    3090       0       0       0       0
    2782       3    3090    3241    3242    3091       0       0       0       0
    2783       3    3091    3242    3243    3092       0       0       0       0
    2784       3    3092    3243    3244    3093       0       0       0       0
    2785       3    3093    3244    3245    3094       0       0       0       0
    2786       3    3094    3245    3246    3095       0       0       0       0
    2787       3    3095    3246    3247    3096       0       0       0       0
    2788       3    3096    3247    3248    3097       0       0       0       0
    2789       3    3097    3248    3249    3098       0       0       0       0
    2790       3    3098    3249    3250    3099       0       0       0       0
    2791       3    3099    3250    3251    3100       0       0       0       0
    2792       3    3100    3251    3252    3101       0       0       0       0
    2793       3    3101    3252    3253    3102       0       0       0       0
    2794       3    3102    3253    3254    3103       0       0       0       0
    2795       3    3103    3254    3255    3104       0       0       0       0
    2796       3    3104    3255    3256    3105       0       0       0       0
    2797       3    3105    3256    3257    3106       0       0       0       0
    2798       3    3106    3257    3258    3107       0       0       0       0
    2799       3    3107    3258    3259    3108       0       0       0       0
    2800       3    3108    3259    3260    3109       0       0       0       0
    2801       3    3109    3260    3261    3110       0       0       0       0
    2802       3    3110    3261    3262    3111       0       0       0       0
    2803       3    3111    3262    3263    3112       0       0       0       0
    2804       3    3112    3263    3264    3113       0       0       0       0
    2805       3    3113    3264    3265    3114       0       0       0       0
    2806       3    3114    3265    3266    3115       0       0       0       0
    2807       3    3115    3266    3267    3116       0       0       0       0
    2808       3    3116    3267    3268    3117       0       0       0       0
    2809       3    3117    3268    3269    3118       0       0       0       0
    2810       3    3118    3269    3270    3119       0       0       0       0
    2811       3    3119    3270    3271    3120       0       0       0       0
    2812       3    3120    3271    3272    3121       0       0       0       0
    2813       3    3122    3273    3274    3123       0       0       0       0
    2814       3    3123    3274    3275    3124       0       0       0       0
    2815       3    3124    3275    3276    3125       0       0       0       0
    2816       3    3125    3276    3277    3126       0       0       0       0
    2817       3    3126    3277    3278    3127       0       0       0       0
    2818       3    3127    3278    3279    3128       0       0       0       0
    2819       3    3128    3279    3280    3129       0       0       0       0
    2820       3    3129    3280    3281    3130       0       0       0       0
    2821       3    3130    3281    3282    3131       0       0       0       0
    2822       3    3131    3282    3283    3132       0       0       0       0
    2823       3    3132    3283    3284    3133       0       0       0       0
    2824       3    3133    3284    3285    3134       0       0       0       0
    2825       3    3134    3285    3286    3135       0       0       0       0
    2826       3    3135    3286    3287    3136       0       0       0       0
    2827       3    3136    3287    3288    3137       0       0       0       0
    2828       3    3137    3288    3289    3138       0       0       0       0
    2829       3    3138    3289    3290    3139       0       0       0       0
    2830       3    3139    3290    3291    3140       0       0       0       0
    2831       3    3140    3291    3292    3141       0       0       0       0
    2832       3    3141    3292    3293    3142       0       0       0       0
    2833       3    3142    3293    3294    3143       0       0       0       0
    2834       3    3143    3294    3295    3144       0       0       0       0
    2835       3    3144    3295    3296    3145       0       0       0       0
    2836       3    3145    3296    3297    3146       0       0       0       0
    2837       3    3146    3297    3298    3147       0       0       0       0
    2838       3    3147    3298    3299    3148       0       0       0       0
    2839       3    3148    3299    3300    3149       0       0       0       0
    2840       3    3149    3300    3301    3150       0       0       0       0
    2841       3    3150    3301    3302    3151       0       0       0       0
    2842       3    3151    3302    3303    3152       0       0       0       0
    2843       3    3152    3303    3304    3153       0       0       0       0
    2844       3    3153    3304    3305    3154       0       0       0       0
    2845       3    3154    3305    3306    3155       0       0       0       0
    2846       3    3155    3306    3307    3156       0       0       0       0
    2847       3    3156    3307    3308    3157       0       0       0       0
    2848       3    3157    3308    3309    3158       0       0       0       0
    2849       3    3158    3309    3310    3159       0       0       0       0
    2850       3    3159    3310    3311    3160       0       0       0       0
    2851       3    3160    3311    3312    3161       0       0       0       0
    2852       3    3161    3312    3313    3162       0       0       0       0
    2853       3    3162    3313    3314    3163       0       0       0       0
    2854       3    3163    3314    3315    3164       0       0       0       0
    2855       3    3164    3315    3316    3165       0       0       0       0
    2856       3    3165    3316    3317    3166       0       0       0       0
    2857       3    3166    3317    3318    3167       0       0       0       0
    2858       3    3167    3318    3319    3168       0       0       0       0
    2859       3    3168    3319    3320    3169       0       0       0       0
    2860       3    3169    3320    3321    3170       0       0       0       0
    2861       3    3170    3321    3322    3171       0       0       0       0
    2862       3    3171    3322    3323    3172       0       0       0       0
    2863       3    3172    3323    3324    3173       0       0       0       0
    2864       3    3173    3324    3325    3174       0       0       0       0
    2865       3    3174    3325    3326    3175       0       0       0       0
    2866       3    3175    3326    3327    3176       0       0       0       0
    2867       3    3176    3327    3328    3177       0       0       0       0
    2868       3    3177    3328    3329    3178       0       0       0       0
    2869       3    3178    3329    3330    3179       0       0       0       0
    2870       3    3179    3330    3331    3180       0       0       0       0
    2871       3    3180    3331    3332    3181       0       0       0       0
    2872       3    3181    3332    3333    3182       0       0       0       0
    2873       3    3182    3333    3334    3183       0       0       0       0
    2874       3    3183    3334    3335    3184       0       0       0       0
    2875       3    3184    3335    3336    3185       0       0       0       0
    2876       3    3185    3336    3337    3186       0       0       0       0
    2877       3    3186    3337    3338    3187       0       0       0       0
    2878       3    3187    3338    3339    3188       0       0       0       0
    2879       3    3188    3339    3340    3189       0       0       0       0
    2880       3    3189    3340    3341    3190       0       0       0       0
    2881       3    3190    3341    3342    3191       0       0       0       0
    2882       3    3191    3342    3343    3192       0       0       0       0
    2883       3    3192    3343    3344    3193       0       0       0       0
    2884       3    3193    3344    3345    3194       0       0       0       0
    2885       3    3194    3345    3346    3195       0       0       0       0
    2886       3    3195    3346    3347    3196       0       0       0       0
    2887       3    3196    3347    3348    3197       0       0       0       0
    2888       3    3197    3348    3349    3198       0       0       0       0
    2889       3    3198    3349    3350    3199       0       0       0       0
    2890       3    3199    3350    3351    3200       0       0       0       0
    2891       3    3200    3351    3352    3201       0       0       0       0
    2892       3    3201    3352    3353    3202       0       0       0       0
    2893       3    3202    3353    3354    3203       0       0       0       0
    2894       3    3203    3354    3355    3204       0       0       0       0
    2895       3    3204    3355    3356    3205       0       0       0       0
    2896       3    3205    3356    3357    3206       0       0       0       0
    2897       3    3206    3357    3358    3207       0       0       0       0
    2898       3    3207    3358    3359    3208       0       0       0       0
    2899       3    3208    3359    3360    3209       0       0       0       0
    2900       3    3209    3360    3361    3210       0       0       0       0
    2901       3    3210    3361    3362    3211       0       0       0       0
    2902       3    3211    3362    3363    3212       0       0       0       0
    2903       3    3212    3363    3364    3213       0       0       0       0
    2904       3    3213    3364    3365    3214       0       0       0       0
    2905       3    3214    3365    3366    3215       0       0       0       0
    2906       3    3215    3366    3367    3216       0       0       0       0
    2907       3    3216    3367    3368    3217       0       0       0       0
    2908       3    3217    3368    3369    3218       0       0       0       0
    2909       3    3218    3369    3370    3219       0       0       0       0
    2910       3    3219    3370    3371    3220       0       0       0       0
    2911       3    3220    3371    3372    3221       0       0       0       0
    2912       3    3221    3372    3373    3222       0       0       0       0
    2913       3    3222    3373    3374    3223       0       0       0       0
    2914       3    3223    3374    3375    3224       0       0       0       0
    2915       3    3224    3375    3376    3225       0       0       0       0
    2916       3    3225    3376    3377    3226       0       0       0       0
    2917       3    3226    3377    3378    3227       0       0       0       0
    2918       3    3227    3378    3379    3228       0       0       0       0
    2919       3    3228    3379    3380    3229       0       0       0       0
    2920       3    3229    3380    3381    3230       0       0       0       0
    2921       3    3230    3381    3382    3231       0       0       0       0
    2922       3    3231    3382    3383    3232       0       0       0       0
    2923       3    3232    3383    3384    3233       0       0       0       0
    2924       3    3233    3384    3385    3234       0       0       0       0
    2925       3    3234    3385    3386    3235       0       0       0       0
    2926       3    3235    3386    3387    3236       0       0       0       0
    2927       3    3236    3387    3388    3237       0       0       0       0
    2928       3    3237    3388    3389    3238       0       0       0       0
    2929       3    3238    3389    3390    3239       0       0       0       0
    2930       3    3239    3390    3391    3240       0       0       0       0
    2931       3    3240    3391    3392    3241       0       0       0       0
    2932       3    3241    3392    3393    3242       0       0       0       0
    2933       3    3242    3393    3394    3243       0       0       0       0
    2934       3    3243    3394    3395    3244       0       0       0       0
    2935       3    3244    3395    3396    3245       0       0       0       0
    2936       3    3245    3396    3397    3246       0       0       0       0
    2937       3    3246    3397    3398    3247       0       0       0       0
    2938       3    3247    3398    3399    3248       0       0       0       0
    2939       3    3248    3399    3400    3249       0       0       0       0
    2940       3    3249    3400    3401    3250       0       0       0       0
    2941       3    3250    3401    3402    3251       0       0       0       0
    2942       3    3251    3402    3403    3252       0       0       0       0
    2943       3    3252    3403    3404    3253       0       0       0       0
    2944       3    3253    3404    3405    3254       0       0       0       0
    2945       3    3254    3405    3406    3255       0       0       0       0
    2946       3    3255    3406    3407    3256       0       0       0       0
    2947       3    3256    3407    3408    3257       0       0       0       0
    2948       3    3257    3408    3409    3258       0       0       0       0
    2949       3    3258    3409    3410    3259       0       0       0       0
    2950       3    3259    3410    3411    3260       0       0       0       0
    2951       3    3260    3411    3412    3261       0       0       0       0
    2952       3    3261    3412    3413    3262       0       0       0       0
    2953       3    3262    3413    3414    3263       0       0       0       0
    2954       3    3263    3414    3415    3264       0       0       0       0
    2955       3    3264    3415    3416    3265       0       0       0       0
    2956       3    3265    3416    3417    3266       0       0       0       0
    2957       3    3266    3417    3418    3267       0       0       0       0
    2958       3    3267    3418    3419    3268       0       0       0       0
    2959       3    3268    3419    3420    3269       0       0       0       0
    2960       3    3269    3420    3421    3270       0       0       0       0
    2961       3    3270    3421    3422    3271       0       0       0       0
    2962       3    3271    3422    3423    3272       0       0       0       0
    2963       3    3273    3424    3425    3274       0       0       0       0
    2964       3    3274    3425    3426    3275       0       0       0       0
    2965       3    3275    3426    3427    3276       0       0       0       0
    2966       3    3276    3427    3428    3277       0       0       0       0
    2967       3    3277    3428    3429    3278       0       0       0       0
    2968       3    3278    3429    3430    3279       0       0       0       0
    2969       3    3279    3430    3431    3280       0       0       0       0
    2970       3    3280    3431    3432    3281       0       0       0       0
    2971       3    3281    3432    3433    3282       0       0       0       0
    2972       3    3282    3433    3434    3283       0       0       0       0
    2973       3    3283    3434    3435    3284       0       0       0       0
    2974       3    3284    3435    3436    3285       0       0       0       0
    2975       3    3285    3436    3437    3286       0       0       0       0
    2976       3    3286    3437    3438    3287       0       0       0       0
    2977       3    3287    3438    3439    3288       0       0       0       0
    2978       3    3288    3439    3440    3289       0       0       0       0
    2979       3    3289    3440    3441    3290       0       0       0       0
    2980       3    3290    3441    3442    3291       0       0       0       0
    2981       3    3291    3442    3443    3292       0       0       0       0
    2982       3    3292    3443    3444    3293       0       0       0       0
    2983       3    3293    3444    3445    3294       0       0       0       0
    2984       3    3294    3445    3446    3295       0       0       0       0
    2985       3    3295    3446    3447    3296       0       0       0       0
    2986       3    3296    3447    3448    3297       0       0       0       0
    2987       3    3297    3448    3449    3298       0       0       0       0
    2988       3    3298    3449    3450    3299       0       0       0       0
    2989       3    3299    3450    3451    3300       0       0       0       0
    2990       3    3300    3451    3452    3301       0       0       0       0
    2991       3    3301    3452    3453    3302       0       0       0       0
    2992       3    3302    3453    3454    3303       0       0       0       0
    2993       3    3303    3454    3455    3304       0       0       0       0
    2994       3    3304    3455    3456    3305       0       0       0       0
    2995       3    3305    3456    3457    3306       0       0       0       0
    2996       3    3306    3457    3458    3307       0       0       0       0
    2997       3    3307    3458    3459    3308       0       0       0       0
    2998       3    3308    3459    3460    3309       0       0       0       0
    2999       3    3309    3460    3461    3310       0       0       0       0
    3000       3    3310    3461    3462    3311       0       0       0       0
    3001       3    3311    3462    3463    3312       0       0       0       0
    3002       3    3312    3463    3464    3313       0       0       0       0
    3003       3    3313    3464    3465    3314       0       0       0       0
    3004       3    3314    3465    3466    3315       0       0       0       0
    3005       3    3315    3466    3467    3316       0       0       0       0
    3006       3    3316    3467    3468    3317       0       0       0       0
    3007       3    3317    3468    3469    3318       0       0       0       0
    3008       3    3318    3469    3470    3319       0       0       0       0
    3009       3    3319    3470    3471    3320       0       0       0       0
    3010       3    3320    3471    3472    3321       0       0       0       0
    3011       3    3321    3472    3473    3322       0       0       0       0
    3012       3    3322    3473    3474    3323       0       0       0       0
    3013       3    3323    3474    3475    3324       0       0       0       0
    3014       3    3324    3475    3476    3325       0       0       0       0
    3015       3    3325    3476    3477    3326       0       0       0       0
    3016       3    3326    3477    3478    3327       0       0       0       0
    3017       3    3327    3478    3479    3328       0       0       0       0
    3018       3    3328    3479    3480    3329       0       0       0       0
    3019       3    3329    3480    3481    3330       0       0       0       0
    3020       3    3330    3481    3482    3331       0       0       0       0
    3021       3    3331    3482    3483    3332       0       0       0       0
    3022       3    3332    3483    3484    3333       0       0       0       0
    3023       3    3333    3484    3485    3334       0       0       0       0
    3024       3    3334    3485    3486    3335       0       0       0       0
    3025       3    3335    3486    3487    3336       0       0       0       0
    3026       3    3336    3487    3488    3337       0       0       0       0
    3027       3    3337    3488    3489    3338       0       0       0       0
    3028       3    3338    3489    3490    3339       0       0       0       0
    3029       3    3339    3490    3491    3340       0       0       0       0
    3030       3    3340    3491    3492    3341       0       0       0       0
    3031       3    3341    3492    3493    3342       0       0       0       0
    3032       3    3342    3493    3494    3343       0       0       0       0
    3033       3    3343    3494    3495    3344       0       0       0       0
    3034       3    3344    3495    3496    3345       0       0       0       0
    3035       3    3345    3496    3497    3346       0       0       0       0
    3036       3    3346    3497    3498    3347       0       0       0       0
    3037       3    3347    3498    3499    3348       0       0       0       0
    3038       3    3348    3499    3500    3349       0       0       0       0
    3039       3    3349    3500    3501    3350       0       0       0       0
    3040       3    3350    3501    3502    3351       0       0       0       0
    3041       3    3351    3502    3503    3352       0       0       0       0
    3042       3    3352    3503    3504    3353       0       0       0       0
    3043       3    3353    3504    3505    3354       0       0       0       0
    3044       3    3354    3505    3506    3355       0       0       0       0
    3045       3    3355    3506    3507    3356       0       0       0       0
    3046       3    3356    3507    3508    3357       0       0       0       0
    3047       3    3357    3508    3509    3358       0       0       0       0
    3048       3    3358    3509    3510    3359       0       0       0       0
    3049       3    3359    3510    3511    3360       0       0       0       0
    3050       3    3360    3511    3512    3361       0       0       0       0
    3051       3    3361    3512    3513    3362       0       0       0       0
    3052       3    3362    3513    3514    3363       0       0       0       0
    3053       3    3363    3514    3515    3364       0       0       0       0
    3054       3    3364    3515    3516    3365       0       0       0       0
    3055       3    3365    3516    3517    3366       0       0       0       0
    3056       3    3366    3517    3518    3367       0       0       0       0
    3057       3    3367    3518    3519    3368       0       0       0       0
    3058       3    3368    3519    3520    3369       0       0       0       0
    3059       3    3369    3520    3521    3370       0       0       0       0
    3060       3    3370    3521    3522    3371       0       0       0       0
    3061       3    3371    3522    3523    3372       0       0       0       0
    3062       3    3372    3523    3524    3373       0       0       0       0
    3063       3    3373    3524    3525    3374       0       0       0       0
    3064       3    3374    3525    3526    3375       0       0       0       0
    3065       3    3375    3526    3527    3376       0       0       0       0
    3066       3    3376    3527    3528    3377       0       0       0       0
    3067       3    3377    3528    3529    3378       0       0       0       0
    3068       3    3378    3529    3530    3379       0       0       0       0
    3069       3    3379    3530    3531    3380       0       0       0       0
    3070       3    3380    3531    3532    3381       0       0       0       0
    3071       3    3381    3532    3533    3382       0       0       0       0
    3072       3    3382    3533    3534    3383       0       0       0       0
    3073       3    3383    3534    3535    3384       0       0       0       0
    3074       3    3384    3535    3536    3385       0       0       0       0
    3075       3    3385    3536    3537    3386       0       0       0       0
    3076       3    3386    3537    3538    3387       0       0       0       0
    3077       3    3387    3538    3539    3388       0       0       0       0
    3078       3    3388    3539    3540    3389       0       0       0       0
    3079       3    3389    3540    3541    3390       0       0       0       0
    3080       3    3390    3541    3542    3391       0       0       0       0
    3081       3    3391    3542    3543    3392       0       0       0       0
    3082       3    3392    3543    3544    3393       0       0       0       0
    3083       3    3393    3544    3545    3394       0       0       0       0
    3084       3    3394    3545    3546    3395       0       0       0       0
    3085       3    3395    3546    3547    3396       0       0       0       0
    3086       3    3396    3547    3548    3397       0       0       0       0
    3087       3    3397    3548    3549    3398       0       0       0       0
    3088       3    3398    3549    3550    3399       0       0       0       0
    3089       3    3399    3550    3551    3400       0       0       0       0
    3090       3    3400    3551    3552    3401       0       0       0       0
    3091       3    3401    3552    3553    3402       0       0       0       0
    3092       3    3402    3553    3554    3403       0       0       0       0
    3093       3    3403    3554    3555    3404       0       0       0       0
    3094       3    3404    3555    3556    3405       0       0       0       0
    3095       3    3405    3556    3557    3406       0       0       0       0
    3096       3    3406    3557    3558    3407       0       0       0       0
    3097       3    3407    3558    3559    3408       0       0       0       0
    3098       3    3408    3559    3560    3409       0       0       0       0
    3099       3    3409    3560    3561    3410       0       0       0       0
    3100       3    3410    3561    3562    3411       0       0       0       0
    3101       3    3411    3562    3563    3412       0       0       0       0
    3102       3    3412    3563    3564    3413       0       0       0       0
    3103       3    3413    3564    3565    3414       0       0       0       0
    3104       3    3414    3565    3566    3415       0       0       0       0
    3105       3    3415    3566    3567    3416       0       0       0       0
    3106       3    3416    3567    3568    3417       0       0       0       0
    3107       3    3417    3568    3569    3418       0       0       0       0
    3108       3    3418    3569    3570    3419       0       0       0       0
    3109       3    3419    3570    3571    3420       0       0       0       0
    3110       3    3420    3571    3572    3421       0       0       0       0
    3111       3    3421    3572    3573    3422       0       0       0       0
    3112       3    3422    3573    3574    3423       0       0       0       0
    3113       3    3424    3575    3576    3425       0       0       0       0
    3114       3    3425    3576    3577    3426       0       0       0       0
    3115       3    3426    3577    3578    3427       0       0       0       0
    3116       3    3427    3578    3579    3428       0       0       0       0
    3117       3    3428    3579    3580    3429       0       0       0       0
    3118       3    3429    3580    3581    3430       0       0       0       0
    3119       3    3430    3581    3582    3431       0       0       0       0
    3120       3    3431    3582    3583    3432       0       0       0       0
    3121       3    3432    3583    3584    3433       0       0       0       0
    3122       3    3433    3584    3585    3434       0       0       0       0
    3123       3    3434    3585    3586    3435       0       0       0       0
    3124       3    3435    3586    3587    3436       0       0       0       0
    3125       3    3436    3587    3588    3437       0       0       0       0
    3126       3    3437    3588    3589    3438       0       0       0       0
    3127       3    3438    3589    3590    3439       0       0       0       0
    3128       3    3439    3590    3591    3440       0       0       0       0
    3129       3    3440    3591    3592    3441       0       0       0       0
    3130       3    3441    3592    3593    3442       0       0       0       0
    3131       3    3442    3593    3594    3443       0       0       0       0
    3132       3    3443    3594    3595    3444       0       0       0       0
    3133       3    3444    3595    3596    3445       0       0       0       0
    3134       3    3445    3596    3597    3446       0       0       0       0
    3135       3    3446    3597    3598    3447       0       0       0       0
    3136       3    3447    3598    3599    3448       0       0       0       0
    3137       3    3448    3599    3600    3449       0       0       0       0
    3138       3    3449    3600    3601    3450       0       0       0       0
    3139       3    3450    3601    3602    3451       0       0       0       0
    3140       3    3451    3602    3603    3452       0       0       0       0
    3141       3    3452    3603    3604    3453       0       0       0       0
    3142       3    3453    3604    3605    3454       0       0       0       0
    3143       3    3454    3605    3606    3455       0       0       0       0
    3144       3    3455    3606    3607    3456       0       0       0       0
    3145       3    3456    3607    3608    3457       0       0       0       0
    3146       3    3457    3608    3609    3458       0       0       0       0
    3147       3    3458    3609    3610    3459       0       0       0       0
    3148       3    3459    3610    3611    3460       0       0       0       0
    3149       3    3460    3611    3612    3461       0       0       0       0
    3150       3    3461    3612    3613    3462       0       0       0       0
    3151       3    3462    3613    3614    3463       0       0       0       0
    3152       3    3463    3614    3615    3464       0       0       0       0
    3153       3    3464    3615    3616    3465       0       0       0       0
    3154       3    3465    3616    3617    3466       0       0       0       0
    3155       3    3466    3617    3618    3467       0       0       0       0
    3156       3    3467    3618    3619    3468       0       0       0       0
    3157       3    3468    3619    3620    3469       0       0       0       0
    3158       3    3469    3620    3621    3470       0       0       0       0
    3159       3    3470    3621    3622    3471       0       0       0       0
    3160       3    3471    3622    3623    3472       0       0       0       0
    3161       3    3472    3623    3624    3473       0       0       0       0
    3162       3    3473    3624    3625    3474       0       0       0       0
    3163       3    3474    3625    3626    3475       0       0       0       0
    3164       3    3475    3626    3627    3476       0       0       0       0
    3165       3    3476    3627    3628    3477       0       0       0       0
    3166       3    3477    3628    3629    3478       0       0       0       0
    3167       3    3478    3629    3630    3479       0       0       0       0
    3168       3    3479    3630    3631    3480       0       0       0       0
    3169       3    3480    3631    3632    3481       0       0       0       0
    3170       3    3481    3632    3633    3482       0       0       0       0
    3171       3    3482    3633    3634    3483       0       0       0       0
    3172       3    3483    3634    3635    3484       0       0       0       0
    3173       3    3484    3635    3636    3485       0       0       0       0
    3174       3    3485    3636    3637    3486       0       0       0       0
    3175       3    3486    3637    3638    3487       0       0       0       0
    3176       3    3487    3638    3639    3488       0       0       0       0
    3177       3    3488    3639    3640    3489       0       0       0       0
    3178       3    3489    3640    3641    3490       0       0       0       0
    3179       3    3490    3641    3642    3491       0       0       0       0
    3180       3    3491    3642    3643    3492       0       0       0       0
    3181       3    3492    3643    3644    3493       0       0       0       0
    3182       3    3493    3644    3645    3494       0       0       0       0
    3183       3    3494    3645    3646    3495       0       0       0       0
    3184       3    3495    3646    3647    3496       0       0       0       0
    3185       3    3496    3647    3648    3497       0       0       0       0
    3186       3    3497    3648    3649    3498       0       0       0       0
    3187       3    3498    3649    3650    3499       0       0       0       0
    3188       3    3499    3650    3651    3500       0       0       0       0
    3189       3    3500    3651    3652    3501       0       0       0       0
    3190       3    3501    3652    3653    3502       0       0       0       0
    3191       3    3502    3653    3654    3503       0       0       0       0
    3192       3    3503    3654    3655    3504       0       0       0       0
    3193       3    3504    3655    3656    3505       0       0       0       0
    3194       3    3505    3656    3657    3506       0       0       0       0
    3195       3    3506    3657    3658    3507       0       0       0       0
    3196       3    3507    3658    3659    3508       0       0       0       0
    3197       3    3508    3659    3660    3509       0       0       0       0
    3198       3    3509    3660    3661    3510       0       0       0       0
    3199       3    3510    3661    3662    3511       0       0       0       0
    3200       3    3511    3662    3663    3512       0       0       0       0
    3201       3    3512    3663    3664    3513       0       0       0       0
    3202       3    3513    3664    3665    3514       0       0       0       0
    3203       3    3514    3665    3666    3515       0       0       0       0
    3204       3    3515    3666    3667    3516       0       0       0       0
    3205       3    3516    3667    3668    3517       0       0       0       0
    3206       3    3517    3668    3669    3518       0       0       0       0
    3207       3    3518    3669    3670    3519       0       0       0       0
    3208       3    3519    3670    3671    3520       0       0       0       0
    3209       3    3520    3671    3672    3521       0       0       0       0
    3210       3    3521    3672    3673    3522       0       0       0       0
    3211       3    3522    3673    3674    3523       0       0       0       0
    3212       3    3523    3674    3675    3524       0       0       0       0
    3213       3    3524    3675    3676    3525       0       0       0       0
    3214       3    3525    3676    3677    3526       0       0       0       0
    3215       3    3526    3677    3678    3527       0       0       0       0
    3216       3    3527    3678    3679    3528       0       0       0       0
    3217       3    3528    3679    3680    3529       0       0       0       0
    3218       3    3529    3680    3681    3530       0       0       0       0
    3219       3    3530    3681    3682    3531       0       0       0       0
    3220       3    3531    3682    3683    3532       0       0       0       0
    3221       3    3532    3683    3684    3533       0       0       0       0
    3222       3    3533    3684    3685    3534       0       0       0       0
    3223       3    3534    3685    3686    3535       0       0       0       0
    3224       3    3535    3686    3687    3536       0       0       0       0
    3225       3    3536    3687    3688    3537       0       0       0       0
    3226       3    3537    3688    3689    3538       0       0       0       0
    3227       3    3538    3689    3690    3539       0       0       0       0
    3228       3    3539    3690    3691    3540       0       0       0       0
    3229       3    3540    3691    3692    3541       0       0       0       0
    3230       3    3541    3692    3693    3542       0       0       0       0
    3231       3    3542    3693    3694    3543       0       0       0       0
    3232       3    3543    3694    3695    3544       0       0       0       0
    3233       3    3544    3695    3696    3545       0       0       0       0
    3234       3    3545    3696    3697    3546       0       0       0       0
    3235       3    3546    3697    3698    3547       0       0       0       0
    3236       3    3547    3698    3699    3548       0       0       0       0
    3237       3    3548    3699    3700    3549       0       0       0       0
    3238       3    3549    3700    3701    3550       0       0       0       0
    3239       3    3550    3701    3702    3551       0       0       0       0
    3240       3    3551    3702    3703    3552       0       0       0       0
    3241       3    3552    3703    3704    3553       0       0       0       0
    3242       3    3553    3704    3705    3554       0       0       0       0
    3243       3    3554    3705    3706    3555       0       0       0       0
    3244       3    3555    3706    3707    3556       0       0       0       0
    3245       3    3556    3707    3708    3557       0       0       0       0
    3246       3    3557    3708    3709    3558       0       0       0       0
    3247       3    3558    3709    3710    3559       0       0       0       0
    3248       3    3559    3710    3711    3560       0       0       0       0
    3249       3    3560    3711    3712    3561       0       0       0       0
    3250       3    3561    3712    3713    3562       0       0       0       0
    3251       3    3562    3713    3714    3563       0       0       0       0
    3252       3    3563    3714    3715    3564       0       0       0       0
    3253       3    3564    3715    3716    3565       0       0       0       0
    3254       3    3565    3716    3717    3566       0       0       0       0
    3255       3    3566    3717    3718    3567       0       0       0       0
    3256       3    3567    3718    3719    3568       0       0       0       0
    3257       3    3568    3719    3720    3569       0       0       0       0
    3258       3    3569    3720    3721    3570       0       0       0       0
    3259       3    3570    3721    3722    3571       0       0       0       0
    3260       3    3571    3722    3723    3572       0       0       0       0
    3261       3    3572    3723    3724    3573       0       0       0       0
    3262       3    3573    3724    3725    3574       0       0       0       0
    3263       3    3575    3726    3727    3576       0       0       0       0
    3264       3    3576    3727    3728    3577       0       0       0       0
    3265       3    3577    3728    3729    3578       0       0       0       0
    3266       3    3578    3729    3730    3579       0       0       0       0
    3267       3    3579    3730    3731    3580       0       0       0       0
    3268       3    3580    3731    3732    3581       0       0       0       0
    3269       3    3581    3732    3733    3582       0       0       0       0
    3270       3    3582    3733    3734    3583       0       0       0       0
    3271       3    3583    3734    3735    3584       0       0       0       0
    3272       3    3584    3735    3736    3585       0       0       0       0
    3273       3    3585    3736    3737    3586       0       0       0       0
    3274       3    3586    3737    3738    3587       0       0       0       0
    3275       3    3587    3738    3739    3588       0       0       0       0
    3276       3    3588    3739    3740    3589       0       0       0       0
    3277       3    3589    3740    3741    3590       0       0       0       0
    3278       3    3590    3741    3742    3591       0       0       0       0
    3279       3    3591    3742    3743    3592       0       0       0       0
    3280       3    3592    3743    3744    3593       0       0       0       0
    3281       3    3593    3744    3745    3594       0       0       0       0
    3282       3    3594    3745    3746    3595       0       0       0       0
    3283       3    3595    3746    3747    3596       0       0       0       0
    3284       3    3596    3747    3748    3597       0       0       0       0
    3285       3    3597    3748    3749    3598       0       0       0       0
    3286       3    3598    3749    3750    3599       0       0       0       0
    3287       3    3599    3750    3751    3600       0       0       0       0
    3288       3    3600    3751    3752    3601       0       0       0       0
    3289       3    3601    3752    3753    3602       0       0       0       0
    3290       3    3602    3753    3754    3603       0       0       0       0
    3291       3    3603    3754    3755    3604       0       0       0       0
    3292       3    3604    3755    3756    3605       0       0       0       0
    3293       3    3605    3756    3757    3606       0       0       0       0
    3294       3    3606    3757    3758    3607       0       0       0       0
    3295       3    3607    3758    3759    3608       0       0       0       0
    3296       3    3608    3759    3760    3609       0       0       0       0
    3297       3    3609    3760    3761    3610       0       0       0       0
    3298       3    3610    3761    3762    3611       0       0       0       0
    3299       3    3611    3762    3763    3612       0       0       0       0
    3300       3    3612    3763    3764    3613       0       0       0       0
    3301       3    3613    3764    3765    3614       0       0       0       0
    3302       3    3614    3765    3766    3615       0       0       0       0
    3303       3    3615    3766    3767    3616       0       0       0       0
    3304       3    3616    3767    3768    3617       0       0       0       0
    3305       3    3617    3768    3769    3618       0       0       0       0
    3306       3    3618    3769    3770    3619       0       0       0       0
    3307       3    3619    3770    3771    3620       0       0       0       0
    3308       3    3620    3771    3772    3621       0       0       0       0
    3309       3    3621    3772    3773    3622       0       0       0       0
    3310       3    3622    3773    3774    3623       0       0       0       0
    3311       3    3623    3774    3775    3624       0       0       0       0
    3312       3    3624    3775    3776    3625       0       0       0       0
    3313       3    3625    3776    3777    3626       0       0       0       0
    3314       3    3626    3777    3778    3627       0       0       0       0
    3315       3    3627    3778    3779    3628       0       0       0       0
    3316       3    3628    3779    3780    3629       0       0       0       0
    3317       3    3629    3780    3781    3630       0       0       0       0
    3318       3    3630    3781    3782    3631       0       0       0       0
    3319       3    3631    3782    3783    3632       0       0       0       0
    3320       3    3632    3783    3784    3633       0       0       0       0
    3321       3    3633    3784    3785    3634       0       0       0       0
    3322       3    3634    3785    3786    3635       0       0       0       0
    3323       3    3635    3786    3787    3636       0       0       0       0
    3324       3    3636    3787    3788    3637       0       0       0       0
    3325       3    3637    3788    3789    3638       0       0       0       0
    3326       3    3638    3789    3790    3639       0       0       0       0
    3327       3    3639    3790    3791    3640       0       0       0       0
    3328       3    3640    3791    3792    3641       0       0       0       0
    3329       3    3641    3792    3793    3642       0       0       0       0
    3330       3    3642    3793    3794    3643       0       0       0       0
    3331       3    3643    3794    3795    3644       0       0       0       0
    3332       3    3644    3795    3796    3645       0       0       0       0
    3333       3    3645    3796    3797    3646       0       0       0       0
    3334       3    3646    3797    3798    3647       0       0       0       0
    3335       3    3647    3798    3799    3648       0       0       0       0
    3336       3    3648    3799    3800    3649       0       0       0       0
    3337       3    3649    3800    3801    3650       0       0       0       0
    3338       3    3650    3801    3802    3651       0       0       0       0
    3339       3    3651    3802    3803    3652       0       0       0       0
    3340       3    3652    3803    3804    3653       0       0       0       0
    3341       3    3653    3804    3805    3654       0       0       0       0
    3342       3    3654    3805    3806    3655       0       0       0       0
    3343       3    3655    3806    3807    3656       0       0       0       0
    3344       3    3656    3807    3808    3657       0       0       0       0
    3345       3    3657    3808    3809    3658       0       0       0       0
    3346       3    3658    3809    3810    3659       0       0       0       0
    3347       3    3659    3810    3811    3660       0       0       0       0
    3348       3    3660    3811    3812    3661       0       0       0       0
    3349       3    3661    3812    3813    3662       0       0       0       0
    3350       3    3662    3813    3814    3663       0       0       0       0
    3351       3    3663    3814    3815    3664       0       0       0       0
    3352       3    3664    3815    3816    3665       0       0       0       0
    3353       3    3665    3816    3817    3666       0       0       0       0
    3354       3    3666    3817    3818    3667       0       0       0       0
    3355       3    3667    3818    3819    3668       0       0       0       0
    3356       3    3668    3819    3820    3669       0       0       0       0
    3357       3    3669    3820    3821    3670       0       0       0       0
    3358       3    3670    3821    3822    3671       0       0       0       0
    3359       3    3671    3822    3823    3672       0       0       0       0
    3360       3    3672    3823    3824    3673       0       0       0       0
    3361       3    3673    3824    3825    3674       0       0       0       0
    3362       3    3674    3825    3826    3675       0       0       0       0
    3363       3    3675    3826    3827    3676       0       0       0       0
    3364       3    3676    3827    3828    3677       0       0       0       0
    3365       3    3677    3828    3829    3678       0       0       0       0
    3366       3    3678    3829    3830    3679       0       0       0       0
    3367       3    3679    3830    3831    3680       0       0       0       0
    3368       3    3680    3831    3832    3681       0       0       0       0
    3369       3    3681    3832    3833    3682       0       0       0       0
    3370       3    3682    3833    3834    3683       0       0       0       0
    3371       3    3683    3834    3835    3684       0       0       0       0
    3372       3    3684    3835    3836    3685       0       0       0       0
    3373       3    3685    3836    3837    3686       0       0       0       0
    3374       3    3686    3837    3838    3687       0       0       0       0
    3375       3    3687    3838    3839    3688       0       0       0       0
    3376       3    3688    3839    3840    3689       0       0       0       0
    3377       3    3689    3840    3841    3690       0       0       0       0
    3378       3    3690    3841    3842    3691       0       0       0       0
    3379       3    3691    3842    3843    3692       0       0       0       0
    3380       3    3692    3843    3844    3693       0       0       0       0
    3381       3    3693    3844    3845    3694       0       0       0       0
    3382       3    3694    3845    3846    3695       0       0       0       0
    3383       3    3695    3846    3847    3696       0       0       0       0
    3384       3    3696    3847    3848    3697       0       0       0       0
    3385       3    3697    3848    3849    3698       0       0       0       0
    3386       3    3698    3849    3850    3699       0       0       0       0
    3387       3    3699    3850    3851    3700       0       0       0       0
    3388       3    3700    3851    3852    3701       0       0       0       0
    3389       3    3701    3852    3853    3702       0       0       0       0
    3390       3    3702    3853    3854    3703       0       0       0       0
    3391       3    3703    3854    3855    3704       0       0       0       0
    3392       3    3704    3855    3856    3705       0       0       0       0
    3393       3    3705    3856    3857    3706       0       0       0       0
    3394       3    3706    3857    3858    3707       0       0       0       0
    3395       3    3707    3858    3859    3708       0       0       0       0
    3396       3    3708    3859    3860    3709       0       0       0       0
    3397       3    3709    3860    3861    3710       0       0       0       0
    3398       3    3710    3861    3862    3711       0       0       0       0
    3399       3    3711    3862    3863    3712       0       0       0       0
    3400       3    3712    3863    3864    3713       0       0       0       0
    3401       3    3713    3864    3865    3714       0       0       0       0
    3402       3    3714    3865    3866    3715       0       0       0       0
    3403       3    3715    3866    3867    3716       0       0       0       0
    3404       3    3716    3867    3868    3717       0       0       0       0
    3405       3    3717    3868    3869    3718       0       0       0       0
    3406       3    3718    3869    3870    3719       0       0       0       0
    3407       3    3719    3870    3871    3720       0       0       0       0
    3408       3    3720    3871    3872    3721       0       0       0       0
    3409       3    3721    3872    3873    3722       0       0       0       0
    3410       3    3722    3873    3874    3723       0       0       0       0
    3411       3    3723    3874    3875    3724       0       0       0       0
    3412       3    3724    3875    3876    3725       0       0       0       0
    3413       3    3726    3877    3878    3727       0       0       0       0
    3414       3    3727    3878    3879    3728       0       0       0       0
    3415       3    3728    3879    3880    3729       0       0       0       0
    3416       3    3729    3880    3881    3730       0       0       0       0
    3417       3    3730    3881    3882    3731       0       0       0       0
    3418       3    3731    3882    3883    3732       0       0       0       0
    3419       3    3732    3883    3884    3733       0       0       0       0
    3420       3    3733    3884    3885    3734       0       0       0       0
    3421       3    3734    3885    3886    3735       0       0       0       0
    3422       3    3735    3886    3887    3736       0       0       0       0
    3423       3    3736    3887    3888    3737       0       0       0       0
    3424       3    3737    3888    3889    3738       0       0       0       0
    3425       3    3738    3889    3890    3739       0       0       0       0
    3426       3    3739    3890    3891    3740       0       0       0       0
    3427       3    3740    3891    3892    3741       0       0       0       0
    3428       3    3741    3892    3893    3742       0       0       0       0
    3429       3    3742    3893    3894    3743       0       0       0       0
    3430       3    3743    3894    3895    3744       0       0       0       0
    3431       3    3744    3895    3896    3745       0       0       0       0
    3432       3    3745    3896    3897    3746       0       0       0       0
    3433       3    3746    3897    3898    3747       0       0       0       0
    3434       3    3747    3898    3899    3748       0       0       0       0
    3435       3    3748    3899    3900    3749       0       0       0       0
    3436       3    3749    3900    3901    3750       0       0       0       0
    3437       3    3750    3901    3902    3751       0       0       0       0
    3438       3    3751    3902    3903    3752       0       0       0       0
    3439       3    3752    3903    3904    3753       0       0       0       0
    3440       3    3753    3904    3905    3754       0       0       0       0
    3441       3    3754    3905    3906    3755       0       0       0       0
    3442       3    3755    3906    3907    3756       0       0       0       0
    3443       3    3756    3907    3908    3757       0       0       0       0
    3444       3    3757    3908    3909    3758       0       0       0       0
    3445       3    3758    3909    3910    3759       0       0       0       0
    3446       3    3759    3910    3911    3760       0       0       0       0
    3447       3    3760    3911    3912    3761       0       0       0       0
    3448       3    3761    3912    3913    3762       0       0       0       0
    3449       3    3762    3913    3914    3763       0       0       0       0
    3450       3    3763    3914    3915    3764       0       0       0       0
    3451       3    3764    3915    3916    3765       0       0       0       0
    3452       3    3765    3916    3917    3766       0       0       0       0
    3453       3    3766    3917    3918    3767       0       0       0       0
    3454       3    3767    3918    3919    3768       0       0       0       0
    3455       3    3768    3919    3920    3769       0       0       0       0
    3456       3    3769    3920    3921    3770       0       0       0       0
    3457       3    3770    3921    3922    3771       0       0       0       0
    3458       3    3771    3922    3923    3772       0       0       0       0
    3459       3    3772    3923    3924    3773       0       0       0       0
    3460       3    3773    3924    3925    3774       0       0       0       0
    3461       3    3774    3925    3926    3775       0       0       0       0
    3462       3    3775    3926    3927    3776       0       0       0       0
    3463       3    3776    3927    3928    3777       0       0       0       0
    3464       3    3777    3928    3929    3778       0       0       0       0
    3465       3    3778    3929    3930    3779       0       0       0       0
    3466       3    3779    3930    3931    3780       0       0       0       0
    3467       3    3780    3931    3932    3781       0       0       0       0
    3468       3    3781    3932    3933    3782       0       0       0       0
    3469       3    3782    3933    3934    3783       0       0       0       0
    3470       3    3783    3934    3935    3784       0       0       0       0
    3471       3    3784    3935    3936    3785       0       0       0       0
    3472       3    3785    3936    3937    3786       0       0       0       0
    3473       3    3786    3937    3938    3787       0       0       0       0
    3474       3    3787    3938    3939    3788       0       0       0       0
    3475       3    3788    3939    3940    3789       0       0       0       0
    3476       3    3789    3940    3941    3790       0       0       0       0
    3477       3    3790    3941    3942    3791       0       0       0       0
    3478       3    3791    3942    3943    3792       0       0       0       0
    3479       3    3792    3943    3944    3793       0       0       0       0
    3480       3    3793    3944    3945    3794       0       0       0       0
    3481       3    3794    3945    3946    3795       0       0       0       0
    3482       3    3795    3946    3947    3796       0       0       0       0
    3483       3    3796    3947    3948    3797       0       0       0       0
    3484       3    3797    3948    3949    3798       0       0       0       0
    3485       3    3798    3949    3950    3799       0       0       0       0
    3486       3    3799    3950    3951    3800       0       0       0       0
    3487       3    3800    3951    3952    3801       0       0       0       0
    3488       3    3801    3952    3953    3802       0       0       0       0
    3489       3    3802    3953    3954    3803       0       0       0       0
    3490       3    3803    3954    3955    3804       0       0       0       0
    3491       3    3804    3955    3956    3805       0       0       0       0
    3492       3    3805    3956    3957    3806       0       0       0       0
    3493       3    3806    3957    3958    3807       0       0       0       0
    3494       3    3807    3958    3959    3808       0       0       0       0
    3495       3    3808    3959    3960    3809       0       0       0       0
    3496       3    3809    3960    3961    3810       0       0       0       0
    3497       3    3810    3961    3962    3811       0       0       0       0
    3498       3    3811    3962    3963    3812       0       0       0       0
    3499       3    3812    3963    3964    3813       0       0       0       0
    3500       3    3813    3964    3965    3814       0       0       0       0
    3501       3    3814    3965    3966    3815       0       0       0       0
    3502       3    3815    3966    3967    3816       0       0       0       0
    3503       3    3816    3967    3968    3817       0       0       0       0
    3504       3    3817    3968    3969    3818       0       0       0       0
    3505       3    3818    3969    3970    3819       0       0       0       0
    3506       3    3819    3970    3971    3820       0       0       0       0
    3507       3    3820    3971    3972    3821       0       0       0       0
    3508       3    3821    3972    3973    3822       0       0       0       0
    3509       3    3822    3973    3974    3823       0       0       0       0
    3510       3    3823    3974    3975    3824       0       0       0       0
    3511       3    3824    3975    3976    3825       0       0       0       0
    3512       3    3825    3976    3977    3826       0       0       0       0
    3513       3    3826    3977    3978    3827       0       0       0       0
    3514       3    3827    3978    3979    3828       0       0       0       0
    3515       3    3828    3979    3980    3829       0       0       0       0
    3516       3    3829    3980    3981    3830       0       0       0       0
    3517       3    3830    3981    3982    3831       0       0       0       0
    3518       3    3831    3982    3983    3832       0       0       0       0
    3519       3    3832    3983    3984    3833       0       0       0       0
    3520       3    3833    3984    3985    3834       0       0       0       0
    3521       3    3834    3985    3986    3835       0       0       0       0
    3522       3    3835    3986    3987    3836       0       0       0       0
    3523       3    3836    3987    3988    3837       0       0       0       0
    3524       3    3837    3988    3989    3838       0       0       0       0
    3525       3    3838    3989    3990    3839       0       0       0       0
    3526       3    3839    3990    3991    3840       0       0       0       0
    3527       3    3840    3991    3992    3841       0       0       0       0
    3528       3    3841    3992    3993    3842       0       0       0       0
    3529       3    3842    3993    3994    3843       0       0       0       0
    3530       3    3843    3994    3995    3844       0       0       0       0
    3531       3    3844    3995    3996    3845       0       0       0       0
    3532       3    3845    3996    3997    3846       0       0       0       0
    3533       3    3846    3997    3998    3847       0       0       0       0
    3534       3    3847    3998    3999    3848       0       0       0       0
    3535       3    3848    3999    4000    3849       0       0       0       0
    3536       3    3849    4000    4001    3850       0       0       0       0
    3537       3    3850    4001    4002    3851       0       0       0       0
    3538       3    3851    4002    4003    3852       0       0       0       0
    3539       3    3852    4003    4004    3853       0       0       0       0
    3540       3    3853    4004    4005    3854       0       0       0       0
    3541       3    3854    4005    4006    3855       0       0       0       0
    3542       3    3855    4006    4007    3856       0       0       0       0
    3543       3    3856    4007    4008    3857       0       0       0       0
    3544       3    3857    4008    4009    3858       0       0       0       0
    3545       3    3858    4009    4010    3859       0       0       0       0
    3546       3    3859    4010    4011    3860       0       0       0       0
    3547       3    3860    4011    4012    3861       0       0       0       0
    3548       3    3861    4012    4013    3862       0       0       0       0
    3549       3    3862    4013    4014    3863       0       0       0       0
    3550       3    3863    4014    4015    3864       0       0       0       0
    3551       3    3864    4015    4016    3865       0       0       0       0
    3552       3    3865    4016    4017    3866       0       0       0       0
    3553       3    3866    4017    4018    3867       0       0       0       0
    3554       3    3867    4018    4019    3868       0       0       0       0
    3555       3    3868    4019    4020    3869       0       0       0       0
    3556       3    3869    4020    4021    3870       0       0       0       0
    3557       3    3870    4021    4022    3871       0       0       0       0
    3558       3    3871    4022    4023    3872       0       0       0       0
    3559       3    3872    4023    4024    3873       0       0       0       0
    3560       3    3873    4024    4025    3874       0       0       0       0
    3561       3    3874    4025    4026    3875       0       0       0       0
    3562       3    3875    4026    4027    3876       0       0       0       0
   11063       2   12340   12341   12343   12342       0       0       0       0
   11064       2   12342   12343   12345   12344       0       0       0       0
   11065       2   12344   12345   12347   12346       0       0       0       0
   11066       2   12346   12347   12349   12348       0       0       0       0
   11067       2   12348   12349   12351   12350       0       0       0       0
   11068       2   12350   12351   12353   12352       0       0       0       0
   11069       2   12352   12353   12355   12354       0       0       0       0
   11070       2   12354   12355   12357   12356       0       0       0       0
   11071       2   12356   12357   12359   12358       0       0       0       0
   11072       2   12358   12359   12361   12360       0       0       0       0
   11073       2   12360   12361   12363   12362       0       0       0       0
   11074       2   12362   12363   12365   12364       0       0       0       0
   11075       2   12364   12365   12367   12366       0       0       0       0
   11076       2   12366   12367   12369   12368       0       0       0       0
   11077       2   12368   12369   12371   12370       0       0       0       0
   11078       2   12370   12371   12373   12372       0       0       0       0
   11079       2   12372   12373   12375   12374       0       0       0       0
   11080       2   12374   12375   12377   12376       0       0       0       0
   11081       2   12376   12377   12379   12378       0       0       0       0
   11082       2   12378   12379   12381   12380       0       0       0       0
   11083       2   12380   12381   12383   12382       0       0       0       0
   11084       2   12382   12383   12385   12384       0       0       0       0
   11085       2   12384   12385   12387   12386       0       0       0       0
   11086       2   12386   12387   12389   12388       0       0       0       0
   11087       2   12388   12389   12391   12390       0       0       0       0
   11088       2   12390   12391   12393   12392       0       0       0       0
   11089       2   12392   12393   12395   12394       0       0       0       0
   11090       2   12394   12395   12397   12396       0       0       0       0
   11091       2   12396   12397   12399   12398       0       0       0       0
   11092       2   12398   12399   12401   12400       0       0       0       0
   11093       2   12400   12401   12403   12402       0       0       0       0
   11094       2   12402   12403   12405   12404       0       0       0       0
   11095       2   12404   12405   12407   12406       0       0       0       0
   11096       2   12406   12407   12409   12408       0       0       0       0
   11097       2   12408   12409   12411   12410       0       0       0       0
   11098       2   12410   12411   12413   12412       0       0       0       0
   11099       2   12412   12413   12415   12414       0       0       0       0
   11100       2   12414   12415   12417   12416       0       0       0       0
   11101       2   12416   12417   12419   12418       0       0       0       0
   11102       2   12418   12419   12421   12420       0       0       0       0
   11103       2   12420   12421   12423   12422       0       0       0       0
   11104       2   12422   12423   12425   12424       0       0       0       0
   11105       2   12424   12425   12427   12426       0       0       0       0
   11106       2   12426   12427   12429   12428       0       0       0       0
   11107       2   12428   12429   12431   12430       0       0       0       0
   11108       2   12430   12431   12433   12432       0       0       0       0
   11109       2   12432   12433   12435   12434       0       0       0       0
   11110       2   12434   12435   12437   12436       0       0       0       0
   11111       2   12436   12437   12439   12438       0       0       0       0
   11112       2   12438   12439   12441   12440       0       0       0       0
   11113       2   12440   12441   12443   12442       0       0       0       0
   11114       2   12442   12443   12445   12444       0       0       0       0
   11115       2   12444   12445   12447   12446       0       0       0       0
   11116       2   12446   12447   12449   12448       0       0       0       0
   11117       2   12448   12449   12451   12450       0       0       0       0
   11118       2   12450   12451   12453   12452       0       0       0       0
   11119       2   12452   12453   12455   12454       0       0       0       0
   11120       2   12454   12455   12457   12456       0       0       0       0
   11121       2   12456   12457   12459   12458       0       0       0       0
   11122       2   12458   12459   12461   12460       0       0       0       0
   11123       2   12460   12461   12463   12462       0       0       0       0
   11124       2   12462   12463   12465   12464       0       0       0       0
   11125       2   12464   12465   12467   12466       0       0       0       0
   11126       2   12466   12467   12469   12468       0       0       0       0
   11127       2   12468   12469   12471   12470       0       0       0       0
   11128       2   12470   12471   12473   12472       0       0       0       0
   11129       2   12472   12473   12475   12474       0       0       0       0
   11130       2   12474   12475   12477   12476       0       0       0       0
   11131       2   12476   12477   12479   12478       0       0       0       0
   11132       2   12478   12479   12481   12480       0       0       0       0
   11133       2   12480   12481   12483   12482       0       0       0       0
   11134       2   12482   12483   12485   12484       0       0       0       0
   11135       2   12484   12485   12487   12486       0       0       0       0
   11136       2   12486   12487   12489   12488       0       0       0       0
   11137       2   12488   12489   12491   12490       0       0       0       0
   11138       2   12490   12491   12493   12492       0       0       0       0
   11139       2   12492   12493   12495   12494       0       0       0       0
   11140       2   12494   12495   12497   12496       0       0       0       0
   11141       2   12496   12497   12499   12498       0       0       0       0
   11142       2   12498   12499   12501   12500       0       0       0       0
   11143       2   12500   12501   12503   12502       0       0       0       0
   11144       2   12502   12503   12505   12504       0       0       0       0
   11145       2   12504   12505   12507   12506       0       0       0       0
   11146       2   12506   12507   12509   12508       0       0       0       0
   11147       2   12508   12509   12511   12510       0       0       0       0
   11148       2   12510   12511   12513   12512       0       0       0       0
   11149       2   12512   12513   12515   12514       0       0       0       0
   11150       2   12514   12515   12517   12516       0       0       0       0
   11151       2   12516   12517   12519   12518       0       0       0       0
   11152       2   12518   12519   12521   12520       0       0       0       0
   11153       2   12520   12521   12523   12522       0       0       0       0
   11154       2   12522   12523   12525   12524       0       0       0       0
   11155       2   12524   12525   12527   12526       0       0       0       0
   11156       2   12526   12527   12529   12528       0       0       0       0
   11157       2   12528   12529   12531   12530       0       0       0       0
   11158       2   12530   12531   12533   12532       0       0       0       0
   11159       2   12532   12533   12535   12534       0       0       0       0
   11160       2   12534   12535   12537   12536       0       0       0       0
   11161       2   12536   12537   12539   12538       0       0       0       0
   11162       2   12538   12539   12541   12540       0       0       0       0
   11163       2   12540   12541   12543   12542       0       0       0       0
   11164       2   12542   12543   12545   12544       0       0       0       0
   11165       2   12544   12545   12547   12546       0       0       0       0
   11166       2   12546   12547   12549   12548       0       0       0       0
   11167       2   12548   12549   12551   12550       0       0       0       0
   11168       2   12550   12551   12553   12552       0       0       0       0
   11169       2   12552   12553   12555   12554       0       0       0       0
   11170       2   12554   12555   12557   12556       0       0       0       0
   11171       2   12556   12557   12559   12558       0       0       0       0
   11172       2   12558   12559   12561   12560       0       0       0       0
   11173       2   12560   12561   12563   12562       0       0       0       0
   11174       2   12562   12563   12565   12564       0       0       0       0
   11175       2   12564   12565   12567   12566       0       0       0       0
   11176       2   12566   12567   12569   12568       0       0       0       0
   11177       2   12568   12569   12571   12570       0       0       0       0
   11178       2   12570   12571   12573   12572       0       0       0       0
   11179       2   12572   12573   12575   12574       0       0       0       0
   11180       2   12574   12575   12577   12576       0       0       0       0
   11181       2   12576   12577   12579   12578       0       0       0       0
   11182       2   12578   12579   12581   12580       0       0       0       0
   11183       2   12580   12581   12583   12582       0       0       0       0
   11184       2   12582   12583   12585   12584       0       0       0       0
   11185       2   12584   12585   12587   12586       0       0       0       0
   11186       2   12586   12587   12589   12588       0       0       0       0
   11187       2   12588   12589   12591   12590       0       0       0       0
   11188       2   12590   12591   12593   12592       0       0       0       0
   11189       2   12592   12593   12595   12594       0       0       0       0
   11190       2   12594   12595   12597   12596       0       0       0       0
   11191       2   12596   12597   12599   12598       0       0       0       0
   11192       2   12598   12599   12601   12600       0       0       0       0
   11193       2   12600   12601   12603   12602       0       0       0       0
   11194       2   12602   12603   12605   12604       0       0       0       0
   11195       2   12604   12605   12607   12606       0       0       0       0
   11196       2   12606   12607   12609   12608       0       0       0       0
   11197       2   12608   12609   12611   12610       0       0       0       0
   11198       2   12610   12611   12613   12612       0       0       0       0
   11199       2   12612   12613   12615   12614       0       0       0       0
   11200       2   12614   12615   12617   12616       0       0       0       0
   11201       2   12616   12617   12619   12618       0       0       0       0
   11202       2   12618   12619   12621   12620       0       0       0       0
   11203       2   12620   12621   12623   12622       0       0       0       0
   11204       2   12622   12623   12625   12624       0       0       0       0
   11205       2   12624   12625   12627   12626       0       0       0       0
   11206       2   12626   12627   12629   12628       0       0       0       0
   11207       2   12628   12629   12631   12630       0       0       0       0
   11208       2   12630   12631   12633   12632       0       0       0       0
   11209       2   12632   12633   12635   12634       0       0       0       0
   11210       2   12634   12635   12637   12636       0       0       0       0
   11211       2   12636   12637   12639   12638       0       0       0       0
   11212       2   12638   12639   12641   12640       0       0       0       0
   11213       2   12341   12642   12643   12343       0       0       0       0
   11214       2   12343   12643   12644   12345       0       0       0       0
   11215       2   12345   12644   12645   12347       0       0       0       0
   11216       2   12347   12645   12646   12349       0       0       0       0
   11217       2   12349   12646   12647   12351       0       0       0       0
   11218       2   12351   12647   12648   12353       0       0       0       0
   11219       2   12353   12648   12649   12355       0       0       0       0
   11220       2   12355   12649   12650   12357       0       0       0       0
   11221       2   12357   12650   12651   12359       0       0       0       0
   11222       2   12359   12651   12652   12361       0       0       0       0
   11223       2   12361   12652   12653   12363       0       0       0       0
   11224       2   12363   12653   12654   12365       0       0       0       0
   11225       2   12365   12654   12655   12367       0       0       0       0
   11226       2   12367   12655   12656   12369       0       0       0       0
   11227       2   12369   12656   12657   12371       0       0       0       0
   11228       2   12371   12657   12658   12373       0       0       0       0
   11229       2   12373   12658   12659   12375       0       0       0       0
   11230       2   12375   12659   12660   12377       0       0       0       0
   11231       2   12377   12660   12661   12379       0       0       0       0
   11232       2   12379   12661   12662   12381       0       0       0       0
   11233       2   12381   12662   12663   12383       0       0       0       0
   11234       2   12383   12663   12664   12385       0       0       0       0
   11235       2   12385   12664   12665   12387       0       0       0       0
   11236       2   12387   12665   12666   12389       0       0       0       0
   11237       2   12389   12666   12667   12391       0       0       0       0
   11238       2   12391   12667   12668   12393       0       0       0       0
   11239       2   12393   12668   12669   12395       0       0       0       0
   11240       2   12395   12669   12670   12397       0       0       0       0
   11241       2   12397   12670   12671   12399       0       0       0       0
   11242       2   12399   12671   12672   12401       0       0       0       0
   11243       2   12401   12672   12673   12403       0       0       0       0
   11244       2   12403   12673   12674   12405       0       0       0       0
   11245       2   12405   12674   12675   12407       0       0       0       0
   11246       2   12407   12675   12676   12409       0       0       0       0
   11247       2   12409   12676   12677   12411       0       0       0       0
   11248       2   12411   12677   12678   12413       0       0       0       0
   11249       2   12413   12678   12679   12415       0       0       0       0
   11250       2   12415   12679   12680   12417       0       0       0       0
   11251       2   12417   12680   12681   12419       0       0       0       0
   11252       2   12419   12681   12682   12421       0       0       0       0
   11253       2   12421   12682   12683   12423       0       0       0       0
   11254       2   12423   12683   12684   12425       0       0       0       0
   11255       2   12425   12684   12685   12427       0       0       0       0
   11256       2   12427   12685   12686   12429       0       0       0       0
   11257       2   12429   12686   12687   12431       0       0       0       0
   11258       2   12431   12687   12688   12433       0       0       0       0
   11259       2   12433   12688   12689   12435       0       0       0       0
   11260       2   12435   12689   12690   12437       0       0       0       0
   11261       2   12437   12690   12691   12439       0       0       0       0
   11262       2   12439   12691   12692   12441       0       0       0       0
   11263       2   12441   12692   12693   12443       0       0       0       0
   11264       2   12443   12693   12694   12445       0       0       0       0
   11265       2   12445   12694   12695   12447       0       0       0       0
   11266       2   12447   12695   12696   12449       0       0       0       0
   11267       2   12449   12696   12697   12451       0       0       0       0
   11268       2   12451   12697   12698   12453       0       0       0       0
   11269       2   12453   12698   12699   12455       0       0       0       0
   11270       2   12455   12699   12700   12457       0       0       0       0
   11271       2   12457   12700   12701   12459       0       0       0       0
   11272       2   12459   12701   12702   12461       0       0       0       0
   11273       2   12461   12702   12703   12463       0       0       0       0
   11274       2   12463   12703   12704   12465       0       0       0       0
   11275       2   12465   12704   12705   12467       0       0       0       0
   11276       2   12467   12705   12706   12469       0       0       0       0
   11277       2   12469   12706   12707   12471       0       0       0       0
   11278       2   12471   12707   12708   12473       0       0       0       0
   11279       2   12473   12708   12709   12475       0       0       0       0
   11280       2   12475   12709   12710   12477       0       0       0       0
   11281       2   12477   12710   12711   12479       0       0       0       0
   11282       2   12479   12711   12712   12481       0       0       0       0
   11283       2   12481   12712   12713   12483       0       0       0       0
   11284       2   12483   12713   12714   12485       0       0       0       0
   11285       2   12485   12714   12715   12487       0       0       0       0
   11286       2   12487   12715   12716   12489       0       0       0       0
   11287       2   12489   12716   12717   12491       0       0       0       0
   11288       2   12491   12717   12718   12493       0       0       0       0
   11289       2   12493   12718   12719   12495       0       0       0       0
   11290       2   12495   12719   12720   12497       0       0       0       0
   11291       2   12497   12720   12721   12499       0       0       0       0
   11292       2   12499   12721   12722   12501       0       0       0       0
   11293       2   12501   12722   12723   12503       0       0       0       0
   11294       2   12503   12723   12724   12505       0       0       0       0
   11295       2   12505   12724   12725   12507       0       0       0       0
   11296       2   12507   12725   12726   12509       0       0       0       0
   11297       2   12509   12726   12727   12511       0       0       0       0
   11298       2   12511   12727   12728   12513       0       0       0       0
   11299       2   12513   12728   12729   12515       0       0       0       0
   11300       2   12515   12729   12730   12517       0       0       0       0
   11301       2   12517   12730   12731   12519       0       0       0       0
   11302       2   12519   12731   12732   12521       0       0       0       0
   11303       2   12521   12732   12733   12523       0       0       0       0
   11304       2   12523   12733   12734   12525       0       0       0       0
   11305       2   12525   12734   12735   12527       0       0       0       0
   11306       2   12527   12735   12736   12529       0       0       0       0
   11307       2   12529   12736   12737   12531       0       0       0       0
   11308       2   12531   12737   12738   12533       0       0       0       0
   11309       2   12533   12738   12739   12535       0       0       0       0
   11310       2   12535   12739   12740   12537       0       0       0       0
   11311       2   12537   12740   12741   12539       0       0       0       0
   11312       2   12539   12741   12742   12541       0       0       0       0
   11313       2   12541   12742   12743   12543       0       0       0       0
   11314       2   12543   12743   12744   12545       0       0       0       0
   11315       2   12545   12744   12745   12547       0       0       0       0
   11316       2   12547   12745   12746   12549       0       0       0       0
   11317       2   12549   12746   12747   12551       0       0       0       0
   11318       2   12551   12747   12748   12553       0       0       0       0
   11319       2   12553   12748   12749   12555       0       0       0       0
   11320       2   12555   12749   12750   12557       0       0       0       0
   11321       2   12557   12750   12751   12559       0       0       0       0
   11322       2   12559   12751   12752   12561       0       0       0       0
   11323       2   12561   12752   12753   12563       0       0       0       0
   11324       2   12563   12753   12754   12565       0       0       0       0
   11325       2   12565   12754   12755   12567       0       0       0       0
   11326       2   12567   12755   12756   12569       0       0       0       0
   11327       2   12569   12756   12757   12571       0       0       0       0
   11328       2   12571   12757   12758   12573       0       0       0       0
   11329       2   12573   12758   12759   12575       0       0       0       0
   11330       2   12575   12759   12760   12577       0       0       0       0
   11331       2   12577   12760   12761   12579       0       0       0       0
   11332       2   12579   12761   12762   12581       0       0       0       0
   11333       2   12581   12762   12763   12583       0       0       0       0
   11334       2   12583   12763   12764   12585       0       0       0       0
   11335       2   12585   12764   12765   12587       0       0       0       0
   11336       2   12587   12765   12766   12589       0       0       0       0
   11337       2   12589   12766   12767   12591       0       0       0       0
   11338       2   12591   12767   12768   12593       0       0       0       0
   11339       2   12593   12768   12769   12595       0       0       0       0
   11340       2   12595   12769   12770   12597       0       0       0       0
   11341       2   12597   12770   12771   12599       0       0       0       0
   11342       2   12599   12771   12772   12601       0       0       0       0
   11343       2   12601   12772   12773   12603       0       0       0       0
   11344       2   12603   12773   12774   12605       0       0       0       0
   11345       2   12605   12774   12775   12607       0       0       0       0
   11346       2   12607   12775   12776   12609       0       0       0       0
   11347       2   12609   12776   12777   12611       0       0       0       0
   11348       2   12611   12777   12778   12613       0       0       0       0
   11349       2   12613   12778   12779   12615       0       0       0       0
   11350       2   12615   12779   12780   12617       0       0       0       0
   11351       2   12617   12780   12781   12619       0       0       0       0
   11352       2   12619   12781   12782   12621       0       0       0       0
   11353       2   12621   12782   12783   12623       0       0       0       0
   11354       2   12623   12783   12784   12625       0       0       0       0
   11355       2   12625   12784   12785   12627       0       0       0       0
   11356       2   12627   12785   12786   12629       0       0       0       0
   11357       2   12629   12786   12787   12631       0       0       0       0
   11358       2   12631   12787   12788   12633       0       0       0       0
   11359       2   12633   12788   12789   12635       0       0       0       0
   11360       2   12635   12789   12790   12637       0       0       0       0
   11361       2   12637   12790   12791   12639       0       0       0       0
   11362       2   12639   12791   12792   12641       0       0       0       0
   11363       2   12642   12793   12794   12643       0       0       0       0
   11364       2   12643   12794   12795   12644       0       0       0       0
   11365       2   12644   12795   12796   12645       0       0       0       0
   11366       2   12645   12796   12797   12646       0       0       0       0
   11367       2   12646   12797   12798   12647       0       0       0       0
   11368       2   12647   12798   12799   12648       0       0       0       0
   11369       2   12648   12799   12800   12649       0       0       0       0
   11370       2   12649   12800   12801   12650       0       0       0       0
   11371       2   12650   12801   12802   12651       0       0       0       0
   11372       2   12651   12802   12803   12652       0       0       0       0
   11373       2   12652   12803   12804   12653       0       0       0       0
   11374       2   12653   12804   12805   12654       0       0       0       0
   11375       2   12654   12805   12806   12655       0       0       0       0
   11376       2   12655   12806   12807   12656       0       0       0       0
   11377       2   12656   12807   12808   12657       0       0       0       0
   11378       2   12657   12808   12809   12658       0       0       0       0
   11379       2   12658   12809   12810   12659       0       0       0       0
   11380       2   12659   12810   12811   12660       0       0       0       0
   11381       2   12660   12811   12812   12661       0       0       0       0
   11382       2   12661   12812   12813   12662       0       0       0       0
   11383       2   12662   12813   12814   12663       0       0       0       0
   11384       2   12663   12814   12815   12664       0       0       0       0
   11385       2   12664   12815   12816   12665       0       0       0       0
   11386       2   12665   12816   12817   12666       0       0       0       0
   11387       2   12666   12817   12818   12667       0       0       0       0
   11388       2   12667   12818   12819   12668       0       0       0       0
   11389       2   12668   12819   12820   12669       0       0       0       0
   11390       2   12669   12820   12821   12670       0       0       0       0
   11391       2   12670   12821   12822   12671       0       0       0       0
   11392       2   12671   12822   12823   12672       0       0       0       0
   11393       2   12672   12823   12824   12673       0       0       0       0
   11394       2   12673   12824   12825   12674       0       0       0       0
   11395       2   12674   12825   12826   12675       0       0       0       0
   11396       2   12675   12826   12827   12676       0       0       0       0
   11397       2   12676   12827   12828   12677       0       0       0       0
   11398       2   12677   12828   12829   12678       0       0       0       0
   11399       2   12678   12829   12830   12679       0       0       0       0
   11400       2   12679   12830   12831   12680       0       0       0       0
   11401       2   12680   12831   12832   12681       0       0       0       0
   11402       2   12681   12832   12833   12682       0       0       0       0
   11403       2   12682   12833   12834   12683       0       0       0       0
   11404       2   12683   12834   12835   12684       0       0       0       0
   11405       2   12684   12835   12836   12685       0       0       0       0
   11406       2   12685   12836   12837   12686       0       0       0       0
   11407       2   12686   12837   12838   12687       0       0       0       0
   11408       2   12687   12838   12839   12688       0       0       0       0
   11409       2   12688   12839   12840   12689       0       0       0       0
   11410       2   12689   12840   12841   12690       0       0       0       0
   11411       2   12690   12841   12842   12691       0       0       0       0
   11412       2   12691   12842   12843   12692       0       0       0       0
   11413       2   12692   12843   12844   12693       0       0       0       0
   11414       2   12693   12844   12845   12694       0       0       0       0
   11415       2   12694   12845   12846   12695       0       0       0       0
   11416       2   12695   12846   12847   12696       0       0       0       0
   11417       2   12696   12847   12848   12697       0       0       0       0
   11418       2   12697   12848   12849   12698       0       0       0       0
   11419       2   12698   12849   12850   12699       0       0       0       0
   11420       2   12699   12850   12851   12700       0       0       0       0
   11421       2   12700   12851   12852   12701       0       0       0       0
   11422       2   12701   12852   12853   12702       0       0       0       0
   11423       2   12702   12853   12854   12703       0       0       0       0
   11424       2   12703   12854   12855   12704       0       0       0       0
   11425       2   12704   12855   12856   12705       0       0       0       0
   11426       2   12705   12856   12857   12706       0       0       0       0
   11427       2   12706   12857   12858   12707       0       0       0       0
   11428       2   12707   12858   12859   12708       0       0       0       0
   11429       2   12708   12859   12860   12709       0       0       0       0
   11430       2   12709   12860   12861   12710       0       0       0       0
   11431       2   12710   12861   12862   12711       0       0       0       0
   11432       2   12711   12862   12863   12712       0       0       0       0
   11433       2   12712   12863   12864   12713       0       0       0       0
   11434       2   12713   12864   12865   12714       0       0       0       0
   11435       2   12714   12865   12866   12715       0       0       0       0
   11436       2   12715   12866   12867   12716       0       0       0       0
   11437       2   12716   12867   12868   12717       0       0       0       0
   11438       2   12717   12868   12869   12718       0       0       0       0
   11439       2   12718   12869   12870   12719       0       0       0       0
   11440       2   12719   12870   12871   12720       0       0       0       0
   11441       2   12720   12871   12872   12721       0       0       0       0
   11442       2   12721   12872   12873   12722       0       0       0       0
   11443       2   12722   12873   12874   12723       0       0       0       0
   11444       2   12723   12874   12875   12724       0       0       0       0
   11445       2   12724   12875   12876   12725       0       0       0       0
   11446       2   12725   12876   12877   12726       0       0       0       0
   11447       2   12726   12877   12878   12727       0       0       0       0
   11448       2   12727   12878   12879   12728       0       0       0       0
   11449       2   12728   12879   12880   12729       0       0       0       0
   11450       2   12729   12880   12881   12730       0       0       0       0
   11451       2   12730   12881   12882   12731       0       0       0       0
   11452       2   12731   12882   12883   12732       0       0       0       0
   11453       2   12732   12883   12884   12733       0       0       0       0
   11454       2   12733   12884   12885   12734       0       0       0       0
   11455       2   12734   12885   12886   12735       0       0       0       0
   11456       2   12735   12886   12887   12736       0       0       0       0
   11457       2   12736   12887   12888   12737       0       0       0       0
   11458       2   12737   12888   12889   12738       0       0       0       0
   11459       2   12738   12889   12890   12739       0       0       0       0
   11460       2   12739   12890   12891   12740       0       0       0       0
   11461       2   12740   12891   12892   12741       0       0       0       0
   11462       2   12741   12892   12893   12742       0       0       0       0
   11463       2   12742   12893   12894   12743       0       0       0       0
   11464       2   12743   12894   12895   12744       0       0       0       0
   11465       2   12744   12895   12896   12745       0       0       0       0
   11466       2   12745   12896   12897   12746       0       0       0       0
   11467       2   12746   12897   12898   12747       0       0       0       0
   11468       2   12747   12898   12899   12748       0       0       0       0
   11469       2   12748   12899   12900   12749       0       0       0       0
   11470       2   12749   12900   12901   12750       0       0       0       0
   11471       2   12750   12901   12902   12751       0       0       0       0
   11472       2   12751   12902   12903   12752       0       0       0       0
   11473       2   12752   12903   12904   12753       0       0       0       0
   11474       2   12753   12904   12905   12754       0       0       0       0
   11475       2   12754   12905   12906   12755       0       0       0       0
   11476       2   12755   12906   12907   12756       0       0       0       0
   11477       2   12756   12907   12908   12757       0       0       0       0
   11478       2   12757   12908   12909   12758       0       0       0       0
   11479       2   12758   12909   12910   12759       0       0       0       0
   11480       2   12759   12910   12911   12760       0       0       0       0
   11481       2   12760   12911   12912   12761       0       0       0       0
   11482       2   12761   12912   12913   12762       0       0       0       0
   11483       2   12762   12913   12914   12763       0       0       0       0
   11484       2   12763   12914   12915   12764       0       0       0       0
   11485       2   12764   12915   12916   12765       0       0       0       0
   11486       2   12765   12916   12917   12766       0       0       0       0
   11487       2   12766   12917   12918   12767       0       0       0       0
   11488       2   12767   12918   12919   12768       0       0       0       0
   11489       2   12768   12919   12920   12769       0       0       0       0
   11490       2   12769   12920   12921   12770       0       0       0       0
   11491       2   12770   12921   12922   12771       0       0       0       0
   11492       2   12771   12922   12923   12772       0       0       0       0
   11493       2   12772   12923   12924   12773       0       0       0       0
   11494       2   12773   12924   12925   12774       0       0       0       0
   11495       2   12774   12925   12926   12775       0       0       0       0
   11496       2   12775   12926   12927   12776       0       0       0       0
   11497       2   12776   12927   12928   12777       0       0       0       0
   11498       2   12777   12928   12929   12778       0       0       0       0
   11499       2   12778   12929   12930   12779       0       0       0       0
   11500       2   12779   12930   12931   12780       0       0       0       0
   11501       2   12780   12931   12932   12781       0       0       0       0
   11502       2   12781   12932   12933   12782       0       0       0       0
   11503       2   12782   12933   12934   12783       0       0       0       0
   11504       2   12783   12934   12935   12784       0       0       0       0
   11505       2   12784   12935   12936   12785       0       0       0       0
   11506       2   12785   12936   12937   12786       0       0       0       0
   11507       2   12786   12937   12938   12787       0       0       0       0
   11508       2   12787   12938   12939   12788       0       0       0       0
   11509       2   12788   12939   12940   12789       0       0       0       0
   11510       2   12789   12940   12941   12790       0       0       0       0
   11511       2   12790   12941   12942   12791       0       0       0       0
   11512       2   12791   12942   12943   12792       0       0       0       0
   11513       2   12793   12944   12945   12794       0       0       0       0
   11514       2   12794   12945   12946   12795       0       0       0       0
   11515       2   12795   12946   12947   12796       0       0       0       0
   11516       2   12796   12947   12948   12797       0       0       0       0
   11517       2   12797   12948   12949   12798       0       0       0       0
   11518       2   12798   12949   12950   12799       0       0       0       0
   11519       2   12799   12950   12951   12800       0       0       0       0
   11520       2   12800   12951   12952   12801       0       0       0       0
   11521       2   12801   12952   12953   12802       0       0       0       0
   11522       2   12802   12953   12954   12803       0       0       0       0
   11523       2   12803   12954   12955   12804       0       0       0       0
   11524       2   12804   12955   12956   12805       0       0       0       0
   11525       2   12805   12956   12957   12806       0       0       0       0
   11526       2   12806   12957   12958   12807       0       0       0       0
   11527       2   12807   12958   12959   12808       0       0       0       0
   11528       2   12808   12959   12960   12809       0       0       0       0
   11529       2   12809   12960   12961   12810       0       0       0       0
   11530       2   12810   12961   12962   12811       0       0       0       0
   11531       2   12811   12962   12963   12812       0       0       0       0
   11532       2   12812   12963   12964   12813       0       0       0       0
   11533       2   12813   12964   12965   12814       0       0       0       0
   11534       2   12814   12965   12966   12815       0       0       0       0
   11535       2   12815   12966   12967   12816       0       0       0       0
   11536       2   12816   12967   12968   12817       0       0       0       0
   11537       2   12817   12968   12969   12818       0       0       0       0
   11538       2   12818   12969   12970   12819       0       0       0       0
   11539       2   12819   12970   12971   12820       0       0       0       0
   11540       2   12820   12971   12972   12821       0       0       0       0
   11541       2   12821   12972   12973   12822       0       0       0       0
   11542       2   12822   12973   12974   12823       0       0       0       0
   11543       2   12823   12974   12975   12824       0       0       0       0
   11544       2   12824   12975   12976   12825       0       0       0       0
   11545       2   12825   12976   12977   12826       0       0       0       0
   11546       2   12826   12977   12978   12827       0       0       0       0
   11547       2   12827   12978   12979   12828       0       0       0       0
   11548       2   12828   12979   12980   12829       0       0       0       0
   11549       2   12829   12980   12981   12830       0       0       0       0
   11550       2   12830   12981   12982   12831       0       0       0       0
   11551       2   12831   12982   12983   12832       0       0       0       0
   11552       2   12832   12983   12984   12833       0       0       0       0
   11553       2   12833   12984   12985   12834       0       0       0       0
   11554       2   12834   12985   12986   12835       0       0       0       0
   11555       2   12835   12986   12987   12836       0       0       0       0
   11556       2   12836   12987   12988   12837       0       0       0       0
   11557       2   12837   12988   12989   12838       0       0       0       0
   11558       2   12838   12989   12990   12839       0       0       0       0
   11559       2   12839   12990   12991   12840       0       0       0       0
   11560       2   12840   12991   12992   12841       0       0       0       0
   11561       2   12841   12992   12993   12842       0       0       0       0
   11562       2   12842   12993   12994   12843       0       0       0       0
   11563       2   12843   12994   12995   12844       0       0       0       0
   11564       2   12844   12995   12996   12845       0       0       0       0
   11565       2   12845   12996   12997   12846       0       0       0       0
   11566       2   12846   12997   12998   12847       0       0       0       0
   11567       2   12847   12998   12999   12848       0       0       0       0
   11568       2   12848   12999   13000   12849       0       0       0       0
   11569       2   12849   13000   13001   12850       0       0       0       0
   11570       2   12850   13001   13002   12851       0       0       0       0
   11571       2   12851   13002   13003   12852       0       0       0       0
   11572       2   12852   13003   13004   12853       0       0       0       0
   11573       2   12853   13004   13005   12854       0       0       0       0
   11574       2   12854   13005   13006   12855       0       0       0       0
   11575       2   12855   13006   13007   12856       0       0       0       0
   11576       2   12856   13007   13008   12857       0       0       0       0
   11577       2   12857   13008   13009   12858       0       0       0       0
   11578       2   12858   13009   13010   12859       0       0       0       0
   11579       2   12859   13010   13011   12860       0       0       0       0
   11580       2   12860   13011   13012   12861       0       0       0       0
   11581       2   12861   13012   13013   12862       0       0       0       0
   11582       2   12862   13013   13014   12863       0       0       0       0
   11583       2   12863   13014   13015   12864       0       0       0       0
   11584       2   12864   13015   13016   12865       0       0       0       0
   11585       2   12865   13016   13017   12866       0       0       0       0
   11586       2   12866   13017   13018   12867       0       0       0       0
   11587       2   12867   13018   13019   12868       0       0       0       0
   11588       2   12868   13019   13020   12869       0       0       0       0
   11589       2   12869   13020   13021   12870       0       0       0       0
   11590       2   12870   13021   13022   12871       0       0       0       0
   11591       2   12871   13022   13023   12872       0       0       0       0
   11592       2   12872   13023   13024   12873       0       0       0       0
   11593       2   12873   13024   13025   12874       0       0       0       0
   11594       2   12874   13025   13026   12875       0       0       0       0
   11595       2   12875   13026   13027   12876       0       0       0       0
   11596       2   12876   13027   13028   12877       0       0       0       0
   11597       2   12877   13028   13029   12878       0       0       0       0
   11598       2   12878   13029   13030   12879       0       0       0       0
   11599       2   12879   13030   13031   12880       0       0       0       0
   11600       2   12880   13031   13032   12881       0       0       0       0
   11601       2   12881   13032   13033   12882       0       0       0       0
   11602       2   12882   13033   13034   12883       0       0       0       0
   11603       2   12883   13034   13035   12884       0       0       0       0
   11604       2   12884   13035   13036   12885       0       0       0       0
   11605       2   12885   13036   13037   12886       0       0       0       0
   11606       2   12886   13037   13038   12887       0       0       0       0
   11607       2   12887   13038   13039   12888       0       0       0       0
   11608       2   12888   13039   13040   12889       0       0       0       0
   11609       2   12889   13040   13041   12890       0       0       0       0
   11610       2   12890   13041   13042   12891       0       0       0       0
   11611       2   12891   13042   13043   12892       0       0       0       0
   11612       2   12892   13043   13044   12893       0       0       0       0
   11613       2   12893   13044   13045   12894       0       0       0       0
   11614       2   12894   13045   13046   12895       0       0       0       0
   11615       2   12895   13046   13047   12896       0       0       0       0
   11616       2   12896   13047   13048   12897       0       0       0       0
   11617       2   12897   13048   13049   12898       0       0       0       0
   11618       2   12898   13049   13050   12899       0       0       0       0
   11619       2   12899   13050   13051   12900       0       0       0       0
   11620       2   12900   13051   13052   12901       0       0       0       0
   11621       2   12901   13052   13053   12902       0       0       0       0
   11622       2   12902   13053   13054   12903       0       0       0       0
   11623       2   12903   13054   13055   12904       0       0       0       0
   11624       2   12904   13055   13056   12905       0       0       0       0
   11625       2   12905   13056   13057   12906       0       0       0       0
   11626       2   12906   13057   13058   12907       0       0       0       0
   11627       2   12907   13058   13059   12908       0       0       0       0
   11628       2   12908   13059   13060   12909       0       0       0       0
   11629       2   12909   13060   13061   12910       0       0       0       0
   11630       2   12910   13061   13062   12911       0       0       0       0
   11631       2   12911   13062   13063   12912       0       0       0       0
   11632       2   12912   13063   13064   12913       0       0       0       0
   11633       2   12913   13064   13065   12914       0       0       0       0
   11634       2   12914   13065   13066   12915       0       0       0       0
   11635       2   12915   13066   13067   12916       0       0       0       0
   11636       2   12916   13067   13068   12917       0       0       0       0
   11637       2   12917   13068   13069   12918       0       0       0       0
   11638       2   12918   13069   13070   12919       0       0       0       0
   11639       2   12919   13070   13071   12920       0       0       0       0
   11640       2   12920   13071   13072   12921       0       0       0       0
   11641       2   12921   13072   13073   12922       0       0       0       0
   11642       2   12922   13073   13074   12923       0       0       0       0
   11643       2   12923   13074   13075   12924       0       0       0       0
   11644       2   12924   13075   13076   12925       0       0       0       0
   11645       2   12925   13076   13077   12926       0       0       0       0
   11646       2   12926   13077   13078   12927       0       0       0       0
   11647       2   12927   13078   13079   12928       0       0       0       0
   11648       2   12928   13079   13080   12929       0       0       0       0
   11649       2   12929   13080   13081   12930       0       0       0       0
   11650       2   12930   13081   13082   12931       0       0       0       0
   11651       2   12931   13082   13083   12932       0       0       0       0
   11652       2   12932   13083   13084   12933       0       0       0       0
   11653       2   12933   13084   13085   12934       0       0       0       0
   11654       2   12934   13085   13086   12935       0       0       0       0
   11655       2   12935   13086   13087   12936       0       0       0       0
   11656       2   12936   13087   13088   12937       0       0       0       0
   11657       2   12937   13088   13089   12938       0       0       0       0
   11658       2   12938   13089   13090   12939       0       0       0       0
   11659       2   12939   13090   13091   12940       0       0       0       0
   11660       2   12940   13091   13092   12941       0       0       0       0
   11661       2   12941   13092   13093   12942       0       0       0       0
   11662       2   12942   13093   13094   12943       0       0       0       0
   11663       2   12944   13095   13096   12945       0       0       0       0
   11664       2   12945   13096   13097   12946       0       0       0       0
   11665       2   12946   13097   13098   12947       0       0       0       0
   11666       2   12947   13098   13099   12948       0       0       0       0
   11667       2   12948   13099   13100   12949       0       0       0       0
   11668       2   12949   13100   13101   12950       0       0       0       0
   11669       2   12950   13101   13102   12951       0       0       0       0
   11670       2   12951   13102   13103   12952       0       0       0       0
   11671       2   12952   13103   13104   12953       0       0       0       0
   11672       2   12953   13104   13105   12954       0       0       0       0
   11673       2   12954   13105   13106   12955       0       0       0       0
   11674       2   12955   13106   13107   12956       0       0       0       0
   11675       2   12956   13107   13108   12957       0       0       0       0
   11676       2   12957   13108   13109   12958       0       0       0       0
   11677       2   12958   13109   13110   12959       0       0       0       0
   11678       2   12959   13110   13111   12960       0       0       0       0
   11679       2   12960   13111   13112   12961       0       0       0       0
   11680       2   12961   13112   13113   12962       0       0       0       0
   11681       2   12962   13113   13114   12963       0       0       0       0
   11682       2   12963   13114   13115   12964       0       0       0       0
   11683       2   12964   13115   13116   12965       0       0       0       0
   11684       2   12965   13116   13117   12966       0       0       0       0
   11685       2   12966   13117   13118   12967       0       0       0       0
   11686       2   12967   13118   13119   12968       0       0       0       0
   11687       2   12968   13119   13120   12969       0       0       0       0
   11688       2   12969   13120   13121   12970       0       0       0       0
   11689       2   12970   13121   13122   12971       0       0       0       0
   11690       2   12971   13122   13123   12972       0       0       0       0
   11691       2   12972   13123   13124   12973       0       0       0       0
   11692       2   12973   13124   13125   12974       0       0       0       0
   11693       2   12974   13125   13126   12975       0       0       0       0
   11694       2   12975   13126   13127   12976       0       0       0       0
   11695       2   12976   13127   13128   12977       0       0       0       0
   11696       2   12977   13128   13129   12978       0       0       0       0
   11697       2   12978   13129   13130   12979       0       0       0       0
   11698       2   12979   13130   13131   12980       0       0       0       0
   11699       2   12980   13131   13132   12981       0       0       0       0
   11700       2   12981   13132   13133   12982       0       0       0       0
   11701       2   12982   13133   13134   12983       0       0       0       0
   11702       2   12983   13134   13135   12984       0       0       0       0
   11703       2   12984   13135   13136   12985       0       0       0       0
   11704       2   12985   13136   13137   12986       0       0       0       0
   11705       2   12986   13137   13138   12987       0       0       0       0
   11706       2   12987   13138   13139   12988       0       0       0       0
   11707       2   12988   13139   13140   12989       0       0       0       0
   11708       2   12989   13140   13141   12990       0       0       0       0
   11709       2   12990   13141   13142   12991       0       0       0       0
   11710       2   12991   13142   13143   12992       0       0       0       0
   11711       2   12992   13143   13144   12993       0       0       0       0
   11712       2   12993   13144   13145   12994       0       0       0       0
   11713       2   12994   13145   13146   12995       0       0       0       0
   11714       2   12995   13146   13147   12996       0       0       0       0
   11715       2   12996   13147   13148   12997       0       0       0       0
   11716       2   12997   13148   13149   12998       0       0       0       0
   11717       2   12998   13149   13150   12999       0       0       0       0
   11718       2   12999   13150   13151   13000       0       0       0       0
   11719       2   13000   13151   13152   13001       0       0       0       0
   11720       2   13001   13152   13153   13002       0       0       0       0
   11721       2   13002   13153   13154   13003       0       0       0       0
   11722       2   13003   13154   13155   13004       0       0       0       0
   11723       2   13004   13155   13156   13005       0       0       0       0
   11724       2   13005   13156   13157   13006       0       0       0       0
   11725       2   13006   13157   13158   13007       0       0       0       0
   11726       2   13007   13158   13159   13008       0       0       0       0
   11727       2   13008   13159   13160   13009       0       0       0       0
   11728       2   13009   13160   13161   13010       0       0       0       0
   11729       2   13010   13161   13162   13011       0       0       0       0
   11730       2   13011   13162   13163   13012       0       0       0       0
   11731       2   13012   13163   13164   13013       0       0       0       0
   11732       2   13013   13164   13165   13014       0       0       0       0
   11733       2   13014   13165   13166   13015       0       0       0       0
   11734       2   13015   13166   13167   13016       0       0       0       0
   11735       2   13016   13167   13168   13017       0       0       0       0
   11736       2   13017   13168   13169   13018       0       0       0       0
   11737       2   13018   13169   13170   13019       0       0       0       0
   11738       2   13019   13170   13171   13020       0       0       0       0
   11739       2   13020   13171   13172   13021       0       0       0       0
   11740       2   13021   13172   13173   13022       0       0       0       0
   11741       2   13022   13173   13174   13023       0       0       0       0
   11742       2   13023   13174   13175   13024       0       0       0       0
   11743       2   13024   13175   13176   13025       0       0       0       0
   11744       2   13025   13176   13177   13026       0       0       0       0
   11745       2   13026   13177   13178   13027       0       0       0       0
   11746       2   13027   13178   13179   13028       0       0       0       0
   11747       2   13028   13179   13180   13029       0       0       0       0
   11748       2   13029   13180   13181   13030       0       0       0       0
   11749       2   13030   13181   13182   13031       0       0       0       0
   11750       2   13031   13182   13183   13032       0       0       0       0
   11751       2   13032   13183   13184   13033       0       0       0       0
   11752       2   13033   13184   13185   13034       0       0       0       0
   11753       2   13034   13185   13186   13035       0       0       0       0
   11754       2   13035   13186   13187   13036       0       0       0       0
   11755       2   13036   13187   13188   13037       0       0       0       0
   11756       2   13037   13188   13189   13038       0       0       0       0
   11757       2   13038   13189   13190   13039       0       0       0       0
   11758       2   13039   13190   13191   13040       0       0       0       0
   11759       2   13040   13191   13192   13041       0       0       0       0
   11760       2   13041   13192   13193   13042       0       0       0       0
   11761       2   13042   13193   13194   13043       0       0       0       0
   11762       2   13043   13194   13195   13044       0       0       0       0
   11763       2   13044   13195   13196   13045       0       0       0       0
   11764       2   13045   13196   13197   13046       0       0       0       0
   11765       2   13046   13197   13198   13047       0       0       0       0
   11766       2   13047   13198   13199   13048       0       0       0       0
   11767       2   13048   13199   13200   13049       0       0       0       0
   11768       2   13049   13200   13201   13050       0       0       0       0
   11769       2   13050   13201   13202   13051       0       0       0       0
   11770       2   13051   13202   13203   13052       0       0       0       0
   11771       2   13052   13203   13204   13053       0       0       0       0
   11772       2   13053   13204   13205   13054       0       0       0       0
   11773       2   13054   13205   13206   13055       0       0       0       0
   11774       2   13055   13206   13207   13056       0       0       0       0
   11775       2   13056   13207   13208   13057       0       0       0       0
   11776       2   13057   13208   13209   13058       0       0       0       0
   11777       2   13058   13209   13210   13059       0       0       0       0
   11778       2   13059   13210   13211   13060       0       0       0       0
   11779       2   13060   13211   13212   13061       0       0       0       0
   11780       2   13061   13212   13213   13062       0       0       0       0
   11781       2   13062   13213   13214   13063       0       0       0       0
   11782       2   13063   13214   13215   13064       0       0       0       0
   11783       2   13064   13215   13216   13065       0       0       0       0
   11784       2   13065   13216   13217   13066       0       0       0       0
   11785       2   13066   13217   13218   13067       0       0       0       0
   11786       2   13067   13218   13219   13068       0       0       0       0
   11787       2   13068   13219   13220   13069       0       0       0       0
   11788       2   13069   13220   13221   13070       0       0       0       0
   11789       2   13070   13221   13222   13071       0       0       0       0
   11790       2   13071   13222   13223   13072       0       0       0       0
   11791       2   13072   13223   13224   13073       0       0       0       0
   11792       2   13073   13224   13225   13074       0       0       0       0
   11793       2   13074   13225   13226   13075       0       0       0       0
   11794       2   13075   13226   13227   13076       0       0       0       0
   11795       2   13076   13227   13228   13077       0       0       0       0
   11796       2   13077   13228   13229   13078       0       0       0       0
   11797       2   13078   13229   13230   13079       0       0       0       0
   11798       2   13079   13230   13231   13080       0       0       0       0
   11799       2   13080   13231   13232   13081       0       0       0       0
   11800       2   13081   13232   13233   13082       0       0       0       0
   11801       2   13082   13233   13234   13083       0       0       0       0
   11802       2   13083   13234   13235   13084       0       0       0       0
   11803       2   13084   13235   13236   13085       0       0       0       0
   11804       2   13085   13236   13237   13086       0       0       0       0
   11805       2   13086   13237   13238   13087       0       0       0       0
   11806       2   13087   13238   13239   13088       0       0       0       0
   11807       2   13088   13239   13240   13089       0       0       0       0
   11808       2   13089   13240   13241   13090       0       0       0       0
   11809       2   13090   13241   13242   13091       0       0       0       0
   11810       2   13091   13242   13243   13092       0       0       0       0
   11811       2   13092   13243   13244   13093       0       0       0       0
   11812       2   13093   13244   13245   13094       0       0       0       0
   11813       2   13095   13246   13247   13096       0       0       0       0
   11814       2   13096   13247   13248   13097       0       0       0       0
   11815       2   13097   13248   13249   13098       0       0       0       0
   11816       2   13098   13249   13250   13099       0       0       0       0
   11817       2   13099   13250   13251   13100       0       0       0       0
   11818       2   13100   13251   13252   13101       0       0       0       0
   11819       2   13101   13252   13253   13102       0       0       0       0
   11820       2   13102   13253   13254   13103       0       0       0       0
   11821       2   13103   13254   13255   13104       0       0       0       0
   11822       2   13104   13255   13256   13105       0       0       0       0
   11823       2   13105   13256   13257   13106       0       0       0       0
   11824       2   13106   13257   13258   13107       0       0       0       0
   11825       2   13107   13258   13259   13108       0       0       0       0
   11826       2   13108   13259   13260   13109       0       0       0       0
   11827       2   13109   13260   13261   13110       0       0       0       0
   11828       2   13110   13261   13262   13111       0       0       0       0
   11829       2   13111   13262   13263   13112       0       0       0       0
   11830       2   13112   13263   13264   13113       0       0       0       0
   11831       2   13113   13264   13265   13114       0       0       0       0
   11832       2   13114   13265   13266   13115       0       0       0       0
   11833       2   13115   13266   13267   13116       0       0       0       0
   11834       2   13116   13267   13268   13117       0       0       0       0
   11835       2   13117   13268   13269   13118       0       0       0       0
   11836       2   13118   13269   13270   13119       0       0       0       0
   11837       2   13119   13270   13271   13120       0       0       0       0
   11838       2   13120   13271   13272   13121       0       0       0       0
   11839       2   13121   13272   13273   13122       0       0       0       0
   11840       2   13122   13273   13274   13123       0       0       0       0
   11841       2   13123   13274   13275   13124       0       0       0       0
   11842       2   13124   13275   13276   13125       0       0       0       0
   11843       2   13125   13276   13277   13126       0       0       0       0
   11844       2   13126   13277   13278   13127       0       0       0       0
   11845       2   13127   13278   13279   13128       0       0       0       0
   11846       2   13128   13279   13280   13129       0       0       0       0
   11847       2   13129   13280   13281   13130       0       0       0       0
   11848       2   13130   13281   13282   13131       0       0       0       0
   11849       2   13131   13282   13283   13132       0       0       0       0
   11850       2   13132   13283   13284   13133       0       0       0       0
   11851       2   13133   13284   13285   13134       0       0       0       0
   11852       2   13134   13285   13286   13135       0       0       0       0
   11853       2   13135   13286   13287   13136       0       0       0       0
   11854       2   13136   13287   13288   13137       0       0       0       0
   11855       2   13137   13288   13289   13138       0       0       0       0
   11856       2   13138   13289   13290   13139       0       0       0       0
   11857       2   13139   13290   13291   13140       0       0       0       0
   11858       2   13140   13291   13292   13141       0       0       0       0
   11859       2   13141   13292   13293   13142       0       0       0       0
   11860       2   13142   13293   13294   13143       0       0       0       0
   11861       2   13143   13294   13295   13144       0       0       0       0
   11862       2   13144   13295   13296   13145       0       0       0       0
   11863       2   13145   13296   13297   13146       0       0       0       0
   11864       2   13146   13297   13298   13147       0       0       0       0
   11865       2   13147   13298   13299   13148       0       0       0       0
   11866       2   13148   13299   13300   13149       0       0       0       0
   11867       2   13149   13300   13301   13150       0       0       0       0
   11868       2   13150   13301   13302   13151       0       0       0       0
   11869       2   13151   13302   13303   13152       0       0       0       0
   11870       2   13152   13303   13304   13153       0       0       0       0
   11871       2   13153   13304   13305   13154       0       0       0       0
   11872       2   13154   13305   13306   13155       0       0       0       0
   11873       2   13155   13306   13307   13156       0       0       0       0
   11874       2   13156   13307   13308   13157       0       0       0       0
   11875       2   13157   13308   13309   13158       0       0       0       0
   11876       2   13158   13309   13310   13159       0       0       0       0
   11877       2   13159   13310   13311   13160       0       0       0       0
   11878       2   13160   13311   13312   13161       0       0       0       0
   11879       2   13161   13312   13313   13162       0       0       0       0
   11880       2   13162   13313   13314   13163       0       0       0       0
   11881       2   13163   13314   13315   13164       0       0       0       0
   11882       2   13164   13315   13316   13165       0       0       0       0
   11883       2   13165   13316   13317   13166       0       0       0       0
   11884       2   13166   13317   13318   13167       0       0       0       0
   11885       2   13167   13318   13319   13168       0       0       0       0
   11886       2   13168   13319   13320   13169       0       0       0       0
   11887       2   13169   13320   13321   13170       0       0       0       0
   11888       2   13170   13321   13322   13171       0       0       0       0
   11889       2   13171   13322   13323   13172       0       0       0       0
   11890       2   13172   13323   13324   13173       0       0       0       0
   11891       2   13173   13324   13325   13174       0       0       0       0
   11892       2   13174   13325   13326   13175       0       0       0       0
   11893       2   13175   13326   13327   13176       0       0       0       0
   11894       2   13176   13327   13328   13177       0       0       0       0
   11895       2   13177   13328   13329   13178       0       0       0       0
   11896       2   13178   13329   13330   13179       0       0       0       0
   11897       2   13179   13330   13331   13180       0       0       0       0
   11898       2   13180   13331   13332   13181       0       0       0       0
   11899       2   13181   13332   13333   13182       0       0       0       0
   11900       2   13182   13333   13334   13183       0       0       0       0
   11901       2   13183   13334   13335   13184       0       0       0       0
   11902       2   13184   13335   13336   13185       0       0       0       0
   11903       2   13185   13336   13337   13186       0       0       0       0
   11904       2   13186   13337   13338   13187       0       0       0       0
   11905       2   13187   13338   13339   13188       0       0       0       0
   11906       2   13188   13339   13340   13189       0       0       0       0
   11907       2   13189   13340   13341   13190       0       0       0       0
   11908       2   13190   13341   13342   13191       0       0       0       0
   11909       2   13191   13342   13343   13192       0       0       0       0
   11910       2   13192   13343   13344   13193       0       0       0       0
   11911       2   13193   13344   13345   13194       0       0       0       0
   11912       2   13194   13345   13346   13195       0       0       0       0
   11913       2   13195   13346   13347   13196       0       0       0       0
   11914       2   13196   13347   13348   13197       0       0       0       0
   11915       2   13197   13348   13349   13198       0       0       0       0
   11916       2   13198   13349   13350   13199       0       0       0       0
   11917       2   13199   13350   13351   13200       0       0       0       0
   11918       2   13200   13351   13352   13201       0       0       0       0
   11919       2   13201   13352   13353   13202       0       0       0       0
   11920       2   13202   13353   13354   13203       0       0       0       0
   11921       2   13203   13354   13355   13204       0       0       0       0
   11922       2   13204   13355   13356   13205       0       0       0       0
   11923       2   13205   13356   13357   13206       0       0       0       0
   11924       2   13206   13357   13358   13207       0       0       0       0
   11925       2   13207   13358   13359   13208       0       0       0       0
   11926       2   13208   13359   13360   13209       0       0       0       0
   11927       2   13209   13360   13361   13210       0       0       0       0
   11928       2   13210   13361   13362   13211       0       0       0       0
   11929       2   13211   13362   13363   13212       0       0       0       0
   11930       2   13212   13363   13364   13213       0       0       0       0
   11931       2   13213   13364   13365   13214       0       0       0       0
   11932       2   13214   13365   13366   13215       0       0       0       0
   11933       2   13215   13366   13367   13216       0       0       0       0
   11934       2   13216   13367   13368   13217       0       0       0       0
   11935       2   13217   13368   13369   13218       0       0       0       0
   11936       2   13218   13369   13370   13219       0       0       0       0
   11937       2   13219   13370   13371   13220       0       0       0       0
   11938       2   13220   13371   13372   13221       0       0       0       0
   11939       2   13221   13372   13373   13222       0       0       0       0
   11940       2   13222   13373   13374   13223       0       0       0       0
   11941       2   13223   13374   13375   13224       0       0       0       0
   11942       2   13224   13375   13376   13225       0       0       0       0
   11943       2   13225   13376   13377   13226       0       0       0       0
   11944       2   13226   13377   13378   13227       0       0       0       0
   11945       2   13227   13378   13379   13228       0       0       0       0
   11946       2   13228   13379   13380   13229       0       0       0       0
   11947       2   13229   13380   13381   13230       0       0       0       0
   11948       2   13230   13381   13382   13231       0       0       0       0
   11949       2   13231   13382   13383   13232       0       0       0       0
   11950       2   13232   13383   13384   13233       0       0       0       0
   11951       2   13233   13384   13385   13234       0       0       0       0
   11952       2   13234   13385   13386   13235       0       0       0       0
   11953       2   13235   13386   13387   13236       0       0       0       0
   11954       2   13236   13387   13388   13237       0       0       0       0
   11955       2   13237   13388   13389   13238       0       0       0       0
   11956       2   13238   13389   13390   13239       0       0       0       0
   11957       2   13239   13390   13391   13240       0       0       0       0
   11958       2   13240   13391   13392   13241       0       0       0       0
   11959       2   13241   13392   13393   13242       0       0       0       0
   11960       2   13242   13393   13394   13243       0       0       0       0
   11961       2   13243   13394   13395   13244       0       0       0       0
   11962       2   13244   13395   13396   13245       0       0       0       0
   11963       2   13246   13397   13398   13247       0       0       0       0
   11964       2   13247   13398   13399   13248       0       0       0       0
   11965       2   13248   13399   13400   13249       0       0       0       0
   11966       2   13249   13400   13401   13250       0       0       0       0
   11967       2   13250   13401   13402   13251       0       0       0       0
   11968       2   13251   13402   13403   13252       0       0       0       0
   11969       2   13252   13403   13404   13253       0       0       0       0
   11970       2   13253   13404   13405   13254       0       0       0       0
   11971       2   13254   13405   13406   13255       0       0       0       0
   11972       2   13255   13406   13407   13256       0       0       0       0
   11973       2   13256   13407   13408   13257       0       0       0       0
   11974       2   13257   13408   13409   13258       0       0       0       0
   11975       2   13258   13409   13410   13259       0       0       0       0
   11976       2   13259   13410   13411   13260       0       0       0       0
   11977       2   13260   13411   13412   13261       0       0       0       0
   11978       2   13261   13412   13413   13262       0       0       0       0
   11979       2   13262   13413   13414   13263       0       0       0       0
   11980       2   13263   13414   13415   13264       0       0       0       0
   11981       2   13264   13415   13416   13265       0       0       0       0
   11982       2   13265   13416   13417   13266       0       0       0       0
   11983       2   13266   13417   13418   13267       0       0       0       0
   11984       2   13267   13418   13419   13268       0       0       0       0
   11985       2   13268   13419   13420   13269       0       0       0       0
   11986       2   13269   13420   13421   13270       0       0       0       0
   11987       2   13270   13421   13422   13271       0       0       0       0
   11988       2   13271   13422   13423   13272       0       0       0       0
   11989       2   13272   13423   13424   13273       0       0       0       0
   11990       2   13273   13424   13425   13274       0       0       0       0
   11991       2   13274   13425   13426   13275       0       0       0       0
   11992       2   13275   13426   13427   13276       0       0       0       0
   11993       2   13276   13427   13428   13277       0       0       0       0
   11994       2   13277   13428   13429   13278       0       0       0       0
   11995       2   13278   13429   13430   13279       0       0       0       0
   11996       2   13279   13430   13431   13280       0       0       0       0
   11997       2   13280   13431   13432   13281       0       0       0       0
   11998       2   13281   13432   13433   13282       0       0       0       0
   11999       2   13282   13433   13434   13283       0       0       0       0
   12000       2   13283   13434   13435   13284       0       0       0       0
   12001       2   13284   13435   13436   13285       0       0       0       0
   12002       2   13285   13436   13437   13286       0       0       0       0
   12003       2   13286   13437   13438   13287       0       0       0       0
   12004       2   13287   13438   13439   13288       0       0       0       0
   12005       2   13288   13439   13440   13289       0       0       0       0
   12006       2   13289   13440   13441   13290       0       0       0       0
   12007       2   13290   13441   13442   13291       0       0       0       0
   12008       2   13291   13442   13443   13292       0       0       0       0
   12009       2   13292   13443   13444   13293       0       0       0       0
   12010       2   13293   13444   13445   13294       0       0       0       0
   12011       2   13294   13445   13446   13295       0       0       0       0
   12012       2   13295   13446   13447   13296       0       0       0       0
   12013       2   13296   13447   13448   13297       0       0       0       0
   12014       2   13297   13448   13449   13298       0       0       0       0
   12015       2   13298   13449   13450   13299       0       0       0       0
   12016       2   13299   13450   13451   13300       0       0       0       0
   12017       2   13300   13451   13452   13301       0       0       0       0
   12018       2   13301   13452   13453   13302       0       0       0       0
   12019       2   13302   13453   13454   13303       0       0       0       0
   12020       2   13303   13454   13455   13304       0       0       0       0
   12021       2   13304   13455   13456   13305       0       0       0       0
   12022       2   13305   13456   13457   13306       0       0       0       0
   12023       2   13306   13457   13458   13307       0       0       0       0
   12024       2   13307   13458   13459   13308       0       0       0       0
   12025       2   13308   13459   13460   13309       0       0       0       0
   12026       2   13309   13460   13461   13310       0       0       0       0
   12027       2   13310   13461   13462   13311       0       0       0       0
   12028       2   13311   13462   13463   13312       0       0       0       0
   12029       2   13312   13463   13464   13313       0       0       0       0
   12030       2   13313   13464   13465   13314       0       0       0       0
   12031       2   13314   13465   13466   13315       0       0       0       0
   12032       2   13315   13466   13467   13316       0       0       0       0
   12033       2   13316   13467   13468   13317       0       0       0       0
   12034       2   13317   13468   13469   13318       0       0       0       0
   12035       2   13318   13469   13470   13319       0       0       0       0
   12036       2   13319   13470   13471   13320       0       0       0       0
   12037       2   13320   13471   13472   13321       0       0       0       0
   12038       2   13321   13472   13473   13322       0       0       0       0
   12039       2   13322   13473   13474   13323       0       0       0       0
   12040       2   13323   13474   13475   13324       0       0       0       0
   12041       2   13324   13475   13476   13325       0       0       0       0
   12042       2   13325   13476   13477   13326       0       0       0       0
   12043       2   13326   13477   13478   13327       0       0       0       0
   12044       2   13327   13478   13479   13328       0       0       0       0
   12045       2   13328   13479   13480   13329       0       0       0       0
   12046       2   13329   13480   13481   13330       0       0       0       0
   12047       2   13330   13481   13482   13331       0       0       0       0
   12048       2   13331   13482   13483   13332       0       0       0       0
   12049       2   13332   13483   13484   13333       0       0       0       0
   12050       2   13333   13484   13485   13334       0       0       0       0
   12051       2   13334   13485   13486   13335       0       0       0       0
   12052       2   13335   13486   13487   13336       0       0       0       0
   12053       2   13336   13487   13488   13337       0       0       0       0
   12054       2   13337   13488   13489   13338       0       0       0       0
   12055       2   13338   13489   13490   13339       0       0       0       0
   12056       2   13339   13490   13491   13340       0       0       0       0
   12057       2   13340   13491   13492   13341       0       0       0       0
   12058       2   13341   13492   13493   13342       0       0       0       0
   12059       2   13342   13493   13494   13343       0       0       0       0
   12060       2   13343   13494   13495   13344       0       0       0       0
   12061       2   13344   13495   13496   13345       0       0       0       0
   12062       2   13345   13496   13497   13346       0       0       0       0
   12063       2   13346   13497   13498   13347       0       0       0       0
   12064       2   13347   13498   13499   13348       0       0       0       0
   12065       2   13348   13499   13500   13349       0       0       0       0
   12066       2   13349   13500   13501   13350       0       0       0       0
   12067       2   13350   13501   13502   13351       0       0       0       0
   12068       2   13351   13502   13503   13352       0       0       0       0
   12069       2   13352   13503   13504   13353       0       0       0       0
   12070       2   13353   13504   13505   13354       0       0       0       0
   12071       2   13354   13505   13506   13355       0       0       0       0
   12072       2   13355   13506   13507   13356       0       0       0       0
   12073       2   13356   13507   13508   13357       0       0       0       0
   12074       2   13357   13508   13509   13358       0       0       0       0
   12075       2   13358   13509   13510   13359       0       0       0       0
   12076       2   13359   13510   13511   13360       0       0       0       0
   12077       2   13360   13511   13512   13361       0       0       0       0
   12078       2   13361   13512   13513   13362       0       0       0       0
   12079       2   13362   13513   13514   13363       0       0       0       0
   12080       2   13363   13514   13515   13364       0       0       0       0
   12081       2   13364   13515   13516   13365       0       0       0       0
   12082       2   13365   13516   13517   13366       0       0       0       0
   12083       2   13366   13517   13518   13367       0       0       0       0
   12084       2   13367   13518   13519   13368       0       0       0       0
   12085       2   13368   13519   13520   13369       0       0       0       0
   12086       2   13369   13520   13521   13370       0       0       0       0
   12087       2   13370   13521   13522   13371       0       0       0       0
   12088       2   13371   13522   13523   13372       0       0       0       0
   12089       2   13372   13523   13524   13373       0       0       0       0
   12090       2   13373   13524   13525   13374       0       0       0       0
   12091       2   13374   13525   13526   13375       0       0       0       0
   12092       2   13375   13526   13527   13376       0       0       0       0
   12093       2   13376   13527   13528   13377       0       0       0       0
   12094       2   13377   13528   13529   13378       0       0       0       0
   12095       2   13378   13529   13530   13379       0       0       0       0
   12096       2   13379   13530   13531   13380       0       0       0       0
   12097       2   13380   13531   13532   13381       0       0       0       0
   12098       2   13381   13532   13533   13382       0       0       0       0
   12099       2   13382   13533   13534   13383       0       0       0       0
   12100       2   13383   13534   13535   13384       0       0       0       0
   12101       2   13384   13535   13536   13385       0       0       0       0
   12102       2   13385   13536   13537   13386       0       0       0       0
   12103       2   13386   13537   13538   13387       0       0       0       0
   12104       2   13387   13538   13539   13388       0       0       0       0
   12105       2   13388   13539   13540   13389       0       0       0       0
   12106       2   13389   13540   13541   13390       0       0       0       0
   12107       2   13390   13541   13542   13391       0       0       0       0
   12108       2   13391   13542   13543   13392       0       0       0       0
   12109       2   13392   13543   13544   13393       0       0       0       0
   12110       2   13393   13544   13545   13394       0       0       0       0
   12111       2   13394   13545   13546   13395       0       0       0       0
   12112       2   13395   13546   13547   13396       0       0       0       0
   12113       2   13397   13548   13549   13398       0       0       0       0
   12114       2   13398   13549   13550   13399       0       0       0       0
   12115       2   13399   13550   13551   13400       0       0       0       0
   12116       2   13400   13551   13552   13401       0       0       0       0
   12117       2   13401   13552   13553   13402       0       0       0       0
   12118       2   13402   13553   13554   13403       0       0       0       0
   12119       2   13403   13554   13555   13404       0       0       0       0
   12120       2   13404   13555   13556   13405       0       0       0       0
   12121       2   13405   13556   13557   13406       0       0       0       0
   12122       2   13406   13557   13558   13407       0       0       0       0
   12123       2   13407   13558   13559   13408       0       0       0       0
   12124       2   13408   13559   13560   13409       0       0       0       0
   12125       2   13409   13560   13561   13410       0       0       0       0
   12126       2   13410   13561   13562   13411       0       0       0       0
   12127       2   13411   13562   13563   13412       0       0       0       0
   12128       2   13412   13563   13564   13413       0       0       0       0
   12129       2   13413   13564   13565   13414       0       0       0       0
   12130       2   13414   13565   13566   13415       0       0       0       0
   12131       2   13415   13566   13567   13416       0       0       0       0
   12132       2   13416   13567   13568   13417       0       0       0       0
   12133       2   13417   13568   13569   13418       0       0       0       0
   12134       2   13418   13569   13570   13419       0       0       0       0
   12135       2   13419   13570   13571   13420       0       0       0       0
   12136       2   13420   13571   13572   13421       0       0       0       0
   12137       2   13421   13572   13573   13422       0       0       0       0
   12138       2   13422   13573   13574   13423       0       0       0       0
   12139       2   13423   13574   13575   13424       0       0       0       0
   12140       2   13424   13575   13576   13425       0       0       0       0
   12141       2   13425   13576   13577   13426       0       0       0       0
   12142       2   13426   13577   13578   13427       0       0       0       0
   12143       2   13427   13578   13579   13428       0       0       0       0
   12144       2   13428   13579   13580   13429       0       0       0       0
   12145       2   13429   13580   13581   13430       0       0       0       0
   12146       2   13430   13581   13582   13431       0       0       0       0
   12147       2   13431   13582   13583   13432       0       0       0       0
   12148       2   13432   13583   13584   13433       0       0       0       0
   12149       2   13433   13584   13585   13434       0       0       0       0
   12150       2   13434   13585   13586   13435       0       0       0       0
   12151       2   13435   13586   13587   13436       0       0       0       0
   12152       2   13436   13587   13588   13437       0       0       0       0
   12153       2   13437   13588   13589   13438       0       0       0       0
   12154       2   13438   13589   13590   13439       0       0       0       0
   12155       2   13439   13590   13591   13440       0       0       0       0
   12156       2   13440   13591   13592   13441       0       0       0       0
   12157       2   13441   13592   13593   13442       0       0       0       0
   12158       2   13442   13593   13594   13443       0       0       0       0
   12159       2   13443   13594   13595   13444       0       0       0       0
   12160       2   13444   13595   13596   13445       0       0       0       0
   12161       2   13445   13596   13597   13446       0       0       0       0
   12162       2   13446   13597   13598   13447       0       0       0       0
   12163       2   13447   13598   13599   13448       0       0       0       0
   12164       2   13448   13599   13600   13449       0       0       0       0
   12165       2   13449   13600   13601   13450       0       0       0       0
   12166       2   13450   13601   13602   13451       0       0       0       0
   12167       2   13451   13602   13603   13452       0       0       0       0
   12168       2   13452   13603   13604   13453       0       0       0       0
   12169       2   13453   13604   13605   13454       0       0       0       0
   12170       2   13454   13605   13606   13455       0       0       0       0
   12171       2   13455   13606   13607   13456       0       0       0       0
   12172       2   13456   13607   13608   13457       0       0       0       0
   12173       2   13457   13608   13609   13458       0       0       0       0
   12174       2   13458   13609   13610   13459       0       0       0       0
   12175       2   13459   13610   13611   13460       0       0       0       0
   12176       2   13460   13611   13612   13461       0       0       0       0
   12177       2   13461   13612   13613   13462       0       0       0       0
   12178       2   13462   13613   13614   13463       0       0       0       0
   12179       2   13463   13614   13615   13464       0       0       0       0
   12180       2   13464   13615   13616   13465       0       0       0       0
   12181       2   13465   13616   13617   13466       0       0       0       0
   12182       2   13466   13617   13618   13467       0       0       0       0
   12183       2   13467   13618   13619   13468       0       0       0       0
   12184       2   13468   13619   13620   13469       0       0       0       0
   12185       2   13469   13620   13621   13470       0       0       0       0
   12186       2   13470   13621   13622   13471       0       0       0       0
   12187       2   13471   13622   13623   13472       0       0       0       0
   12188       2   13472   13623   13624   13473       0       0       0       0
   12189       2   13473   13624   13625   13474       0       0       0       0
   12190       2   13474   13625   13626   13475       0       0       0       0
   12191       2   13475   13626   13627   13476       0       0       0       0
   12192       2   13476   13627   13628   13477       0       0       0       0
   12193       2   13477   13628   13629   13478       0       0       0       0
   12194       2   13478   13629   13630   13479       0       0       0       0
   12195       2   13479   13630   13631   13480       0       0       0       0
   12196       2   13480   13631   13632   13481       0       0       0       0
   12197       2   13481   13632   13633   13482       0       0       0       0
   12198       2   13482   13633   13634   13483       0       0       0       0
   12199       2   13483   13634   13635   13484       0       0       0       0
   12200       2   13484   13635   13636   13485       0       0       0       0
   12201       2   13485   13636   13637   13486       0       0       0       0
   12202       2   13486   13637   13638   13487       0       0       0       0
   12203       2   13487   13638   13639   13488       0       0       0       0
   12204       2   13488   13639   13640   13489       0       0       0       0
   12205       2   13489   13640   13641   13490       0       0       0       0
   12206       2   13490   13641   13642   13491       0       0       0       0
   12207       2   13491   13642   13643   13492       0       0       0       0
   12208       2   13492   13643   13644   13493       0       0       0       0
   12209       2   13493   13644   13645   13494       0       0       0       0
   12210       2   13494   13645   13646   13495       0       0       0       0
   12211       2   13495   13646   13647   13496       0       0       0       0
   12212       2   13496   13647   13648   13497       0       0       0       0
   12213       2   13497   13648   13649   13498       0       0       0       0
   12214       2   13498   13649   13650   13499       0       0       0       0
   12215       2   13499   13650   13651   13500       0       0       0       0
   12216       2   13500   13651   13652   13501       0       0       0       0
   12217       2   13501   13652   13653   13502       0       0       0       0
   12218       2   13502   13653   13654   13503       0       0       0       0
   12219       2   13503   13654   13655   13504       0       0       0       0
   12220       2   13504   13655   13656   13505       0       0       0       0
   12221       2   13505   13656   13657   13506       0       0       0       0
   12222       2   13506   13657   13658   13507       0       0       0       0
   12223       2   13507   13658   13659   13508       0       0       0       0
   12224       2   13508   13659   13660   13509       0       0       0       0
   12225       2   13509   13660   13661   13510       0       0       0       0
   12226       2   13510   13661   13662   13511       0       0       0       0
   12227       2   13511   13662   13663   13512       0       0       0       0
   12228       2   13512   13663   13664   13513       0       0       0       0
   12229       2   13513   13664   13665   13514       0       0       0       0
   12230       2   13514   13665   13666   13515       0       0       0       0
   12231       2   13515   13666   13667   13516       0       0       0       0
   12232       2   13516   13667   13668   13517       0       0       0       0
   12233       2   13517   13668   13669   13518       0       0       0       0
   12234       2   13518   13669   13670   13519       0       0       0       0
   12235       2   13519   13670   13671   13520       0       0       0       0
   12236       2   13520   13671   13672   13521       0       0       0       0
   12237       2   13521   13672   13673   13522       0       0       0       0
   12238       2   13522   13673   13674   13523       0       0       0       0
   12239       2   13523   13674   13675   13524       0       0       0       0
   12240       2   13524   13675   13676   13525       0       0       0       0
   12241       2   13525   13676   13677   13526       0       0       0       0
   12242       2   13526   13677   13678   13527       0       0       0       0
   12243       2   13527   13678   13679   13528       0       0       0       0
   12244       2   13528   13679   13680   13529       0       0       0       0
   12245       2   13529   13680   13681   13530       0       0       0       0
   12246       2   13530   13681   13682   13531       0       0       0       0
   12247       2   13531   13682   13683   13532       0       0       0       0
   12248       2   13532   13683   13684   13533       0       0       0       0
   12249       2   13533   13684   13685   13534       0       0       0       0
   12250       2   13534   13685   13686   13535       0       0       0       0
   12251       2   13535   13686   13687   13536       0       0       0       0
   12252       2   13536   13687   13688   13537       0       0       0       0
   12253       2   13537   13688   13689   13538       0       0       0       0
   12254       2   13538   13689   13690   13539       0       0       0       0
   12255       2   13539   13690   13691   13540       0       0       0       0
   12256       2   13540   13691   13692   13541       0       0       0       0
   12257       2   13541   13692   13693   13542       0       0       0       0
   12258       2   13542   13693   13694   13543       0       0       0       0
   12259       2   13543   13694   13695   13544       0       0       0       0
   12260       2   13544   13695   13696   13545       0       0       0       0
   12261       2   13545   13696   13697   13546       0       0       0       0
   12262       2   13546   13697   13698   13547       0       0       0       0
   12263       2   13548   13699   13700   13549       0       0       0       0
   12264       2   13549   13700   13701   13550       0       0       0       0
   12265       2   13550   13701   13702   13551       0       0       0       0
   12266       2   13551   13702   13703   13552       0       0       0       0
   12267       2   13552   13703   13704   13553       0       0       0       0
   12268       2   13553   13704   13705   13554       0       0       0       0
   12269       2   13554   13705   13706   13555       0       0       0       0
   12270       2   13555   13706   13707   13556       0       0       0       0
   12271       2   13556   13707   13708   13557       0       0       0       0
   12272       2   13557   13708   13709   13558       0       0       0       0
   12273       2   13558   13709   13710   13559       0       0       0       0
   12274       2   13559   13710   13711   13560       0       0       0       0
   12275       2   13560   13711   13712   13561       0       0       0       0
   12276       2   13561   13712   13713   13562       0       0       0       0
   12277       2   13562   13713   13714   13563       0       0       0       0
   12278       2   13563   13714   13715   13564       0       0       0       0
   12279       2   13564   13715   13716   13565       0       0       0       0
   12280       2   13565   13716   13717   13566       0       0       0       0
   12281       2   13566   13717   13718   13567       0       0       0       0
   12282       2   13567   13718   13719   13568       0       0       0       0
   12283       2   13568   13719   13720   13569       0       0       0       0
   12284       2   13569   13720   13721   13570       0       0       0       0
   12285       2   13570   13721   13722   13571       0       0       0       0
   12286       2   13571   13722   13723   13572       0       0       0       0
   12287       2   13572   13723   13724   13573       0       0       0       0
   12288       2   13573   13724   13725   13574       0       0       0       0
   12289       2   13574   13725   13726   13575       0       0       0       0
   12290       2   13575   13726   13727   13576       0       0       0       0
   12291       2   13576   13727   13728   13577       0       0       0       0
   12292       2   13577   13728   13729   13578       0       0       0       0
   12293       2   13578   13729   13730   13579       0       0       0       0
   12294       2   13579   13730   13731   13580       0       0       0       0
   12295       2   13580   13731   13732   13581       0       0       0       0
   12296       2   13581   13732   13733   13582       0       0       0       0
   12297       2   13582   13733   13734   13583       0       0       0       0
   12298       2   13583   13734   13735   13584       0       0       0       0
   12299       2   13584   13735   13736   13585       0       0       0       0
   12300       2   13585   13736   13737   13586       0       0       0       0
   12301       2   13586   13737   13738   13587       0       0       0       0
   12302       2   13587   13738   13739   13588       0       0       0       0
   12303       2   13588   13739   13740   13589       0       0       0       0
   12304       2   13589   13740   13741   13590       0       0       0       0
   12305       2   13590   13741   13742   13591       0       0       0       0
   12306       2   13591   13742   13743   13592       0       0       0       0
   12307       2   13592   13743   13744   13593       0       0       0       0
   12308       2   13593   13744   13745   13594       0       0       0       0
   12309       2   13594   13745   13746   13595       0       0       0       0
   12310       2   13595   13746   13747   13596       0       0       0       0
   12311       2   13596   13747   13748   13597       0       0       0       0
   12312       2   13597   13748   13749   13598       0       0       0       0
   12313       2   13598   13749   13750   13599       0       0       0       0
   12314       2   13599   13750   13751   13600       0       0       0       0
   12315       2   13600   13751   13752   13601       0       0       0       0
   12316       2   13601   13752   13753   13602       0       0       0       0
   12317       2   13602   13753   13754   13603       0       0       0       0
   12318       2   13603   13754   13755   13604       0       0       0       0
   12319       2   13604   13755   13756   13605       0       0       0       0
   12320       2   13605   13756   13757   13606       0       0       0       0
   12321       2   13606   13757   13758   13607       0       0       0       0
   12322       2   13607   13758   13759   13608       0       0       0       0
   12323       2   13608   13759   13760   13609       0       0       0       0
   12324       2   13609   13760   13761   13610       0       0       0       0
   12325       2   13610   13761   13762   13611       0       0       0       0
   12326       2   13611   13762   13763   13612       0       0       0       0
   12327       2   13612   13763   13764   13613       0       0       0       0
   12328       2   13613   13764   13765   13614       0       0       0       0
   12329       2   13614   13765   13766   13615       0       0       0       0
   12330       2   13615   13766   13767   13616       0       0       0       0
   12331       2   13616   13767   13768   13617       0       0       0       0
   12332       2   13617   13768   13769   13618       0       0       0       0
   12333       2   13618   13769   13770   13619       0       0       0       0
   12334       2   13619   13770   13771   13620       0       0       0       0
   12335       2   13620   13771   13772   13621       0       0       0       0
   12336       2   13621   13772   13773   13622       0       0       0       0
   12337       2   13622   13773   13774   13623       0       0       0       0
   12338       2   13623   13774   13775   13624       0       0       0       0
   12339       2   13624   13775   13776   13625       0       0       0       0
   12340       2   13625   13776   13777   13626       0       0       0       0
   12341       2   13626   13777   13778   13627       0       0       0       0
   12342       2   13627   13778   13779   13628       0       0       0       0
   12343       2   13628   13779   13780   13629       0       0       0       0
   12344       2   13629   13780   13781   13630       0       0       0       0
   12345       2   13630   13781   13782   13631       0       0       0       0
   12346       2   13631   13782   13783   13632       0       0       0       0
   12347       2   13632   13783   13784   13633       0       0       0       0
   12348       2   13633   13784   13785   13634       0       0       0       0
   12349       2   13634   13785   13786   13635       0       0       0       0
   12350       2   13635   13786   13787   13636       0       0       0       0
   12351       2   13636   13787   13788   13637       0       0       0       0
   12352       2   13637   13788   13789   13638       0       0       0       0
   12353       2   13638   13789   13790   13639       0       0       0       0
   12354       2   13639   13790   13791   13640       0       0       0       0
   12355       2   13640   13791   13792   13641       0       0       0       0
   12356       2   13641   13792   13793   13642       0       0       0       0
   12357       2   13642   13793   13794   13643       0       0       0       0
   12358       2   13643   13794   13795   13644       0       0       0       0
   12359       2   13644   13795   13796   13645       0       0       0       0
   12360       2   13645   13796   13797   13646       0       0       0       0
   12361       2   13646   13797   13798   13647       0       0       0       0
   12362       2   13647   13798   13799   13648       0       0       0       0
   12363       2   13648   13799   13800   13649       0       0       0       0
   12364       2   13649   13800   13801   13650       0       0       0       0
   12365       2   13650   13801   13802   13651       0       0       0       0
   12366       2   13651   13802   13803   13652       0       0       0       0
   12367       2   13652   13803   13804   13653       0       0       0       0
   12368       2   13653   13804   13805   13654       0       0       0       0
   12369       2   13654   13805   13806   13655       0       0       0       0
   12370       2   13655   13806   13807   13656       0       0       0       0
   12371       2   13656   13807   13808   13657       0       0       0       0
   12372       2   13657   13808   13809   13658       0       0       0       0
   12373       2   13658   13809   13810   13659       0       0       0       0
   12374       2   13659   13810   13811   13660       0       0       0       0
   12375       2   13660   13811   13812   13661       0       0       0       0
   12376       2   13661   13812   13813   13662       0       0       0       0
   12377       2   13662   13813   13814   13663       0       0       0       0
   12378       2   13663   13814   13815   13664       0       0       0       0
   12379       2   13664   13815   13816   13665       0       0       0       0
   12380       2   13665   13816   13817   13666       0       0       0       0
   12381       2   13666   13817   13818   13667       0       0       0       0
   12382       2   13667   13818   13819   13668       0       0       0       0
   12383       2   13668   13819   13820   13669       0       0       0       0
   12384       2   13669   13820   13821   13670       0       0       0       0
   12385       2   13670   13821   13822   13671       0       0       0       0
   12386       2   13671   13822   13823   13672       0       0       0       0
   12387       2   13672   13823   13824   13673       0       0       0       0
   12388       2   13673   13824   13825   13674       0       0       0       0
   12389       2   13674   13825   13826   13675       0       0       0       0
   12390       2   13675   13826   13827   13676       0       0       0       0
   12391       2   13676   13827   13828   13677       0       0       0       0
   12392       2   13677   13828   13829   13678       0       0       0       0
   12393       2   13678   13829   13830   13679       0       0       0       0
   12394       2   13679   13830   13831   13680       0       0       0       0
   12395       2   13680   13831   13832   13681       0       0       0       0
   12396       2   13681   13832   13833   13682       0       0       0       0
   12397       2   13682   13833   13834   13683       0       0       0       0
   12398       2   13683   13834   13835   13684       0       0       0       0
   12399       2   13684   13835   13836   13685       0       0       0       0
   12400       2   13685   13836   13837   13686       0       0       0       0
   12401       2   13686   13837   13838   13687       0       0       0       0
   12402       2   13687   13838   13839   13688       0       0       0       0
   12403       2   13688   13839   13840   13689       0       0       0       0
   12404       2   13689   13840   13841   13690       0       0       0       0
   12405       2   13690   13841   13842   13691       0       0       0       0
   12406       2   13691   13842   13843   13692       0       0       0       0
   12407       2   13692   13843   13844   13693       0       0       0       0
   12408       2   13693   13844   13845   13694       0       0       0       0
   12409       2   13694   13845   13846   13695       0       0       0       0
   12410       2   13695   13846   13847   13696       0       0       0       0
   12411       2   13696   13847   13848   13697       0       0       0       0
   12412       2   13697   13848   13849   13698       0       0       0       0
   12413       2   13699   13850   13851   13700       0       0       0       0
   12414       2   13700   13851   13852   13701       0       0       0       0
   12415       2   13701   13852   13853   13702       0       0       0       0
   12416       2   13702   13853   13854   13703       0       0       0       0
   12417       2   13703   13854   13855   13704       0       0       0       0
   12418       2   13704   13855   13856   13705       0       0       0       0
   12419       2   13705   13856   13857   13706       0       0       0       0
   12420       2   13706   13857   13858   13707       0       0       0       0
   12421       2   13707   13858   13859   13708       0       0       0       0
   12422       2   13708   13859   13860   13709       0       0       0       0
   12423       2   13709   13860   13861   13710       0       0       0       0
   12424       2   13710   13861   13862   13711       0       0       0       0
   12425       2   13711   13862   13863   13712       0       0       0       0
   12426       2   13712   13863   13864   13713       0       0       0       0
   12427       2   13713   13864   13865   13714       0       0       0       0
   12428       2   13714   13865   13866   13715       0       0       0       0
   12429       2   13715   13866   13867   13716       0       0       0       0
   12430       2   13716   13867   13868   13717       0       0       0       0
   12431       2   13717   13868   13869   13718       0       0       0       0
   12432       2   13718   13869   13870   13719       0       0       0       0
   12433       2   13719   13870   13871   13720       0       0       0       0
   12434       2   13720   13871   13872   13721       0       0       0       0
   12435       2   13721   13872   13873   13722       0       0       0       0
   12436       2   13722   13873   13874   13723       0       0       0       0
   12437       2   13723   13874   13875   13724       0       0       0       0
   12438       2   13724   13875   13876   13725       0       0       0       0
   12439       2   13725   13876   13877   13726       0       0       0       0
   12440       2   13726   13877   13878   13727       0       0       0       0
   12441       2   13727   13878   13879   13728       0       0       0       0
   12442       2   13728   13879   13880   13729       0       0       0       0
   12443       2   13729   13880   13881   13730       0       0       0       0
   12444       2   13730   13881   13882   13731       0       0       0       0
   12445       2   13731   13882   13883   13732       0       0       0       0
   12446       2   13732   13883   13884   13733       0       0       0       0
   12447       2   13733   13884   13885   13734       0       0       0       0
   12448       2   13734   13885   13886   13735       0       0       0       0
   12449       2   13735   13886   13887   13736       0       0       0       0
   12450       2   13736   13887   13888   13737       0       0       0       0
   12451       2   13737   13888   13889   13738       0       0       0       0
   12452       2   13738   13889   13890   13739       0       0       0       0
   12453       2   13739   13890   13891   13740       0       0       0       0
   12454       2   13740   13891   13892   13741       0       0       0       0
   12455       2   13741   13892   13893   13742       0       0       0       0
   12456       2   13742   13893   13894   13743       0       0       0       0
   12457       2   13743   13894   13895   13744       0       0       0       0
   12458       2   13744   13895   13896   13745       0       0       0       0
   12459       2   13745   13896   13897   13746       0       0       0       0
   12460       2   13746   13897   13898   13747       0       0       0       0
   12461       2   13747   13898   13899   13748       0       0       0       0
   12462       2   13748   13899   13900   13749       0       0       0       0
   12463       2   13749   13900   13901   13750       0       0       0       0
   12464       2   13750   13901   13902   13751       0       0       0       0
   12465       2   13751   13902   13903   13752       0       0       0       0
   12466       2   13752   13903   13904   13753       0       0       0       0
   12467       2   13753   13904   13905   13754       0       0       0       0
   12468       2   13754   13905   13906   13755       0       0       0       0
   12469       2   13755   13906   13907   13756       0       0       0       0
   12470       2   13756   13907   13908   13757       0       0       0       0
   12471       2   13757   13908   13909   13758       0       0       0       0
   12472       2   13758   13909   13910   13759       0       0       0       0
   12473       2   13759   13910   13911   13760       0       0       0       0
   12474       2   13760   13911   13912   13761       0       0       0       0
   12475       2   13761   13912   13913   13762       0       0       0       0
   12476       2   13762   13913   13914   13763       0       0       0       0
   12477       2   13763   13914   13915   13764       0       0       0       0
   12478       2   13764   13915   13916   13765       0       0       0       0
   12479       2   13765   13916   13917   13766       0       0       0       0
   12480       2   13766   13917   13918   13767       0       0       0       0
   12481       2   13767   13918   13919   13768       0       0       0       0
   12482       2   13768   13919   13920   13769       0       0       0       0
   12483       2   13769   13920   13921   13770       0       0       0       0
   12484       2   13770   13921   13922   13771       0       0       0       0
   12485       2   13771   13922   13923   13772       0       0       0       0
   12486       2   13772   13923   13924   13773       0       0       0       0
   12487       2   13773   13924   13925   13774       0       0       0       0
   12488       2   13774   13925   13926   13775       0       0       0       0
   12489       2   13775   13926   13927   13776       0       0       0       0
   12490       2   13776   13927   13928   13777       0       0       0       0
   12491       2   13777   13928   13929   13778       0       0       0       0
   12492       2   13778   13929   13930   13779       0       0       0       0
   12493       2   13779   13930   13931   13780       0       0       0       0
   12494       2   13780   13931   13932   13781       0       0       0       0
   12495       2   13781   13932   13933   13782       0       0       0       0
   12496       2   13782   13933   13934   13783       0       0       0       0
   12497       2   13783   13934   13935   13784       0       0       0       0
   12498       2   13784   13935   13936   13785       0       0       0       0
   12499       2   13785   13936   13937   13786       0       0       0       0
   12500       2   13786   13937   13938   13787       0       0       0       0
   12501       2   13787   13938   13939   13788       0       0       0       0
   12502       2   13788   13939   13940   13789       0       0       0       0
   12503       2   13789   13940   13941   13790       0       0       0       0
   12504       2   13790   13941   13942   13791       0       0       0       0
   12505       2   13791   13942   13943   13792       0       0       0       0
   12506       2   13792   13943   13944   13793       0       0       0       0
   12507       2   13793   13944   13945   13794       0       0       0       0
   12508       2   13794   13945   13946   13795       0       0       0       0
   12509       2   13795   13946   13947   13796       0       0       0       0
   12510       2   13796   13947   13948   13797       0       0       0       0
   12511       2   13797   13948   13949   13798       0       0       0       0
   12512       2   13798   13949   13950   13799       0       0       0       0
   12513       2   13799   13950   13951   13800       0       0       0       0
   12514       2   13800   13951   13952   13801       0       0       0       0
   12515       2   13801   13952   13953   13802       0       0       0       0
   12516       2   13802   13953   13954   13803       0       0       0       0
   12517       2   13803   13954   13955   13804       0       0       0       0
   12518       2   13804   13955   13956   13805       0       0       0       0
   12519       2   13805   13956   13957   13806       0       0       0       0
   12520       2   13806   13957   13958   13807       0       0       0       0
   12521       2   13807   13958   13959   13808       0       0       0       0
   12522       2   13808   13959   13960   13809       0       0       0       0
   12523       2   13809   13960   13961   13810       0       0       0       0
   12524       2   13810   13961   13962   13811       0       0       0       0
   12525       2   13811   13962   13963   13812       0       0       0       0
   12526       2   13812   13963   13964   13813       0       0       0       0
   12527       2   13813   13964   13965   13814       0       0       0       0
   12528       2   13814   13965   13966   13815       0       0       0       0
   12529       2   13815   13966   13967   13816       0       0       0       0
   12530       2   13816   13967   13968   13817       0       0       0       0
   12531       2   13817   13968   13969   13818       0       0       0       0
   12532       2   13818   13969   13970   13819       0       0       0       0
   12533       2   13819   13970   13971   13820       0       0       0       0
   12534       2   13820   13971   13972   13821       0       0       0       0
   12535       2   13821   13972   13973   13822       0       0       0       0
   12536       2   13822   13973   13974   13823       0       0       0       0
   12537       2   13823   13974   13975   13824       0       0       0       0
   12538       2   13824   13975   13976   13825       0       0       0       0
   12539       2   13825   13976   13977   13826       0       0       0       0
   12540       2   13826   13977   13978   13827       0       0       0       0
   12541       2   13827   13978   13979   13828       0       0       0       0
   12542       2   13828   13979   13980   13829       0       0       0       0
   12543       2   13829   13980   13981   13830       0       0       0       0
   12544       2   13830   13981   13982   13831       0       0       0       0
   12545       2   13831   13982   13983   13832       0       0       0       0
   12546       2   13832   13983   13984   13833       0       0       0       0
   12547       2   13833   13984   13985   13834       0       0       0       0
   12548       2   13834   13985   13986   13835       0       0       0       0
   12549       2   13835   13986   13987   13836       0       0       0       0
   12550       2   13836   13987   13988   13837       0       0       0       0
   12551       2   13837   13988   13989   13838       0       0       0       0
   12552       2   13838   13989   13990   13839       0       0       0       0
   12553       2   13839   13990   13991   13840       0       0       0       0
   12554       2   13840   13991   13992   13841       0       0       0       0
   12555       2   13841   13992   13993   13842       0       0       0       0
   12556       2   13842   13993   13994   13843       0       0       0       0
   12557       2   13843   13994   13995   13844       0       0       0       0
   12558       2   13844   13995   13996   13845       0       0       0       0
   12559       2   13845   13996   13997   13846       0       0       0       0
   12560       2   13846   13997   13998   13847       0       0       0       0
   12561       2   13847   13998   13999   13848       0       0       0       0
   12562       2   13848   13999   14000   13849       0       0       0       0
*NODE
$#   nid               x               y               z      tc      rc
       1        689.0768        858.6819           308.4       0       0
       2        689.0768        897.1522           308.4       0       0
       3        689.0768        935.6226           308.4       0       0
       4       -117.1495         858.682           308.4       0       0
       5       -80.50343         858.682           308.4       0       0
       6       -43.85597         858.682           308.4       0       0
       7        -7.20993         858.682           308.4       0       0
       8        29.43619         858.682           308.4       0       0
       9        66.08363         858.682           308.4       0       0
      10        102.7297         858.682           308.4       0       0
      11        139.3758         858.682           308.4       0       0
      12        176.0233         858.682           308.4       0       0
      13        212.6693         858.682           308.4       0       0
      14        249.3154         858.682           308.4       0       0
      15        285.9614         858.682           308.4       0       0
      16        322.6088         858.682           308.4       0       0
      17        359.2549         858.682           308.4       0       0
      18         395.901         858.682           308.4       0       0
      19        432.5484         858.682           308.4       0       0
      20        469.1944         858.682           308.4       0       0
      21        505.8405         858.682           308.4       0       0
      22        542.4874         858.682           308.4       0       0
      23        579.1338         858.682           308.4       0       0
      24        615.7803         858.682           308.4       0       0
      25        652.4266         858.682           308.4       0       0
      26        689.0768        858.6819        306.7584       0       0
      27        689.0768        897.1522        306.7584       0       0
      28        689.0768        935.6226        306.7584       0       0
      29       -117.1495         858.682        306.7584       0       0
      30       -80.50343         858.682        306.7584       0       0
      31       -43.85597         858.682        306.7584       0       0
      32        -7.20993         858.682        306.7584       0       0
      33        29.43619         858.682        306.7584       0       0
      34        66.08363         858.682        306.7584       0       0
      35        102.7297         858.682        306.7584       0       0
      36        139.3758         858.682        306.7584       0       0
      37        176.0233         858.682        306.7584       0       0
      38        212.6693         858.682        306.7584       0       0
      39        249.3154         858.682        306.7584       0       0
      40        285.9614         858.682        306.7584       0       0
      41        322.6088         858.682        306.7584       0       0
      42        359.2549         858.682        306.7584       0       0
      43         395.901         858.682        306.7584       0       0
      44        432.5484         858.682        306.7584       0       0
      45        469.1944         858.682        306.7584       0       0
      46        505.8405         858.682        306.7584       0       0
      47        542.4874         858.682        306.7584       0       0
      48        579.1338         858.682        306.7584       0       0
      49        615.7803         858.682        306.7584       0       0
      50        652.4266         858.682        306.7584       0       0
      51        652.4301        897.1523           308.4       0       0
      52        615.7833        897.1523           308.4       0       0
      53        579.1366        897.1523           308.4       0       0
      54        542.4899        897.1523           308.4       0       0
      55        505.8433        897.1523           308.4       0       0
      56        469.1965        897.1523           308.4       0       0
      57        432.5498        897.1523           308.4       0       0
      58        395.9031        897.1523           308.4       0       0
      59        359.2564        897.1523           308.4       0       0
      60        322.6097        897.1523           308.4       0       0
      61         285.963        897.1523           308.4       0       0
      62        249.3163        897.1523           308.4       0       0
      63        212.6696        897.1523           308.4       0       0
      64        176.0229        897.1523           308.4       0       0
      65        139.3762        897.1523           308.4       0       0
      66        102.7295        897.1523           308.4       0       0
      67        66.08275        897.1523           308.4       0       0
      68        29.43604        897.1523           308.4       0       0
      69       -7.210609        897.1523           308.4       0       0
      70       -43.85732        897.1523           308.4       0       0
      71       -80.50403        897.1523           308.4       0       0
      72       -117.1508        897.1523           308.4       0       0
      73        652.4301        935.6226           308.4       0       0
      74        615.7833        935.6226           308.4       0       0
      75        579.1366        935.6226           308.4       0       0
      76        542.4899        935.6226           308.4       0       0
      77        505.8433        935.6226           308.4       0       0
      78        469.1965        935.6226           308.4       0       0
      79        432.5498        935.6226           308.4       0       0
      80        395.9031        935.6226           308.4       0       0
      81        359.2564        935.6226           308.4       0       0
      82        322.6097        935.6226           308.4       0       0
      83         285.963        935.6226           308.4       0       0
      84        249.3163        935.6226           308.4       0       0
      85        212.6696        935.6226           308.4       0       0
      86        176.0229        935.6226           308.4       0       0
      87        139.3762        935.6226           308.4       0       0
      88        102.7295        935.6226           308.4       0       0
      89        66.08275        935.6226           308.4       0       0
      90        29.43604        935.6226           308.4       0       0
      91       -7.210609        935.6226           308.4       0       0
      92       -43.85732        935.6226           308.4       0       0
      93       -80.50403        935.6226           308.4       0       0
      94       -117.1508        935.6226           308.4       0       0
      95        652.4301        897.1523        306.7584       0       0
      96        615.7833        897.1523        306.7584       0       0
      97        579.1366        897.1523        306.7584       0       0
      98        542.4899        897.1523        306.7584       0       0
      99        505.8433        897.1523        306.7584       0       0
     100        469.1965        897.1523        306.7584       0       0
     101        432.5498        897.1523        306.7584       0       0
     102        395.9031        897.1523        306.7584       0       0
     103        359.2564        897.1523        306.7584       0       0
     104        322.6097        897.1523        306.7584       0       0
     105         285.963        897.1523        306.7584       0       0
     106        249.3163        897.1523        306.7584       0       0
     107        212.6696        897.1523        306.7584       0       0
     108        176.0229        897.1523        306.7584       0       0
     109        139.3762        897.1523        306.7584       0       0
     110        102.7295        897.1523        306.7584       0       0
     111        66.08275        897.1523        306.7584       0       0
     112        29.43604        897.1523        306.7584       0       0
     113       -7.210609        897.1523        306.7584       0       0
     114       -43.85732        897.1523        306.7584       0       0
     115       -80.50403        897.1523        306.7584       0       0
     116       -117.1508        897.1523        306.7584       0       0
     117        652.4301        935.6226        306.7584       0       0
     118        615.7833        935.6226        306.7584       0       0
     119        579.1366        935.6226        306.7584       0       0
     120        542.4899        935.6226        306.7584       0       0
     121        505.8433        935.6226        306.7584       0       0
     122        469.1965        935.6226        306.7584       0       0
     123        432.5498        935.6226        306.7584       0       0
     124        395.9031        935.6226        306.7584       0       0
     125        359.2564        935.6226        306.7584       0       0
     126        322.6097        935.6226        306.7584       0       0
     127         285.963        935.6226        306.7584       0       0
     128        249.3163        935.6226        306.7584       0       0
     129        212.6696        935.6226        306.7584       0       0
     130        176.0229        935.6226        306.7584       0       0
     131        139.3762        935.6226        306.7584       0       0
     132        102.7295        935.6226        306.7584       0       0
     133        66.08275        935.6226        306.7584       0       0
     134        29.43604        935.6226        306.7584       0       0
     135       -7.210609        935.6226        306.7584       0       0
     136       -43.85732        935.6226        306.7584       0       0
     137       -80.50403        935.6226        306.7584       0       0
     138       -117.1508        935.6226        306.7584       0       0
     139        -109.037        864.8578           307.6       0       0
     140        -109.037        874.8578           307.6       0       0
     141        -109.037        884.8578           307.6       0       0
     142        -109.037        894.8578           307.6       0       0
     143        -109.037        904.8578           307.6       0       0
     144        -109.037        914.8578           307.6       0       0
     145        -109.037        924.8578           307.6       0       0
     146       -99.03705        864.8578           307.6       0       0
     147       -99.03705        874.8578           307.6       0       0
     148       -99.03705        884.8578           307.6       0       0
     149       -99.03705        894.8578           307.6       0       0
     150       -99.03705        904.8578           307.6       0       0
     151       -99.03705        914.8578           307.6       0       0
     152       -99.03705        924.8578           307.6       0       0
     153       -89.03705        864.8578           307.6       0       0
     154       -89.03705        874.8578           307.6       0       0
     155       -89.03705        884.8578           307.6       0       0
     156       -89.03705        894.8578           307.6       0       0
     157       -89.03705        904.8578           307.6       0       0
     158       -89.03705        914.8578           307.6       0       0
     159       -89.03705        924.8578           307.6       0       0
     160       -79.03705        864.8578           307.6       0       0
     161       -79.03705        874.8578           307.6       0       0
     162       -79.03705        884.8578           307.6       0       0
     163       -79.03705        894.8578           307.6       0       0
     164       -79.03705        904.8578           307.6       0       0
     165       -79.03705        914.8578           307.6       0       0
     166       -79.03705        924.8578           307.6       0       0
     167       -69.03705        864.8578           307.6       0       0
     168       -69.03705        874.8578           307.6       0       0
     169       -69.03705        884.8578           307.6       0       0
     170       -69.03705        894.8578           307.6       0       0
     171       -69.03705        904.8578           307.6       0       0
     172       -69.03705        914.8578           307.6       0       0
     173       -69.03705        924.8578           307.6       0       0
     174       -59.03705        864.8578           307.6       0       0
     175       -59.03705        874.8578           307.6       0       0
     176       -59.03705        884.8578           307.6       0       0
     177       -59.03705        894.8578           307.6       0       0
     178       -59.03705        904.8578           307.6       0       0
     179       -59.03705        914.8578           307.6       0       0
     180       -59.03705        924.8578           307.6       0       0
     181       -49.03705        864.8578           307.6       0       0
     182       -49.03705        874.8578           307.6       0       0
     183       -49.03705        884.8578           307.6       0       0
     184       -49.03705        894.8578           307.6       0       0
     185       -49.03705        904.8578           307.6       0       0
     186       -49.03705        914.8578           307.6       0       0
     187       -49.03705        924.8578           307.6       0       0
     188       -39.03705        864.8578           307.6       0       0
     189       -39.03705        874.8578           307.6       0       0
     190       -39.03705        884.8578           307.6       0       0
     191       -39.03705        894.8578           307.6       0       0
     192       -39.03705        904.8578           307.6       0       0
     193       -39.03705        914.8578           307.6       0       0
     194       -39.03705        924.8578           307.6       0       0
     195       -29.03705        864.8578           307.6       0       0
     196       -29.03705        874.8578           307.6       0       0
     197       -29.03705        884.8578           307.6       0       0
     198       -29.03705        894.8578           307.6       0       0
     199       -29.03705        904.8578           307.6       0       0
     200       -29.03705        914.8578           307.6       0       0
     201       -29.03705        924.8578           307.6       0       0
     202       -19.03705        864.8578           307.6       0       0
     203       -19.03705        874.8578           307.6       0       0
     204       -19.03705        884.8578           307.6       0       0
     205       -19.03705        894.8578           307.6       0       0
     206       -19.03705        904.8578           307.6       0       0
     207       -19.03705        914.8578           307.6       0       0
     208       -19.03705        924.8578           307.6       0       0
     209       -9.037048        864.8578           307.6       0       0
     210       -9.037048        874.8578           307.6       0       0
     211       -9.037048        884.8578           307.6       0       0
     212       -9.037048        894.8578           307.6       0       0
     213       -9.037048        904.8578           307.6       0       0
     214       -9.037048        914.8578           307.6       0       0
     215       -9.037048        924.8578           307.6       0       0
     216       0.9629517        864.8578           307.6       0       0
     217       0.9629517        874.8578           307.6       0       0
     218       0.9629517        884.8578           307.6       0       0
     219       0.9629517        894.8578           307.6       0       0
     220       0.9629517        904.8578           307.6       0       0
     221       0.9629517        914.8578           307.6       0       0
     222       0.9629517        924.8578           307.6       0       0
     223        10.96295        864.8578           307.6       0       0
     224        10.96295        874.8578           307.6       0       0
     225        10.96295        884.8578           307.6       0       0
     226        10.96295        894.8578           307.6       0       0
     227        10.96295        904.8578           307.6       0       0
     228        10.96295        914.8578           307.6       0       0
     229        10.96295        924.8578           307.6       0       0
     230        20.96295        864.8578           307.6       0       0
     231        20.96295        874.8578           307.6       0       0
     232        20.96295        884.8578           307.6       0       0
     233        20.96295        894.8578           307.6       0       0
     234        20.96295        904.8578           307.6       0       0
     235        20.96295        914.8578           307.6       0       0
     236        20.96295        924.8578           307.6       0       0
     237        30.96295        864.8578           307.6       0       0
     238        30.96295        874.8578           307.6       0       0
     239        30.96295        884.8578           307.6       0       0
     240        30.96295        894.8578           307.6       0       0
     241        30.96295        904.8578           307.6       0       0
     242        30.96295        914.8578           307.6       0       0
     243        30.96295        924.8578           307.6       0       0
     244        40.96295        864.8578           307.6       0       0
     245        40.96295        874.8578           307.6       0       0
     246        40.96295        884.8578           307.6       0       0
     247        40.96295        894.8578           307.6       0       0
     248        40.96295        904.8578           307.6       0       0
     249        40.96295        914.8578           307.6       0       0
     250        40.96295        924.8578           307.6       0       0
     251        50.96295        864.8578           307.6       0       0
     252        50.96295        874.8578           307.6       0       0
     253        50.96295        884.8578           307.6       0       0
     254        50.96295        894.8578           307.6       0       0
     255        50.96295        904.8578           307.6       0       0
     256        50.96295        914.8578           307.6       0       0
     257        50.96295        924.8578           307.6       0       0
     258        60.96295        864.8578           307.6       0       0
     259        60.96295        874.8578           307.6       0       0
     260        60.96295        884.8578           307.6       0       0
     261        60.96295        894.8578           307.6       0       0
     262        60.96295        904.8578           307.6       0       0
     263        60.96295        914.8578           307.6       0       0
     264        60.96295        924.8578           307.6       0       0
     265        70.96295        864.8578           307.6       0       0
     266        70.96295        874.8578           307.6       0       0
     267        70.96295        884.8578           307.6       0       0
     268        70.96295        894.8578           307.6       0       0
     269        70.96295        904.8578           307.6       0       0
     270        70.96295        914.8578           307.6       0       0
     271        70.96295        924.8578           307.6       0       0
     272        80.96295        864.8578           307.6       0       0
     273        80.96295        874.8578           307.6       0       0
     274        80.96295        884.8578           307.6       0       0
     275        80.96295        894.8578           307.6       0       0
     276        80.96295        904.8578           307.6       0       0
     277        80.96295        914.8578           307.6       0       0
     278        80.96295        924.8578           307.6       0       0
     279        90.96295        864.8578           307.6       0       0
     280        90.96295        874.8578           307.6       0       0
     281        90.96295        884.8578           307.6       0       0
     282        90.96295        894.8578           307.6       0       0
     283        90.96295        904.8578           307.6       0       0
     284        90.96295        914.8578           307.6       0       0
     285        90.96295        924.8578           307.6       0       0
     286         100.963        864.8578           307.6       0       0
     287         100.963        874.8578           307.6       0       0
     288         100.963        884.8578           307.6       0       0
     289         100.963        894.8578           307.6       0       0
     290         100.963        904.8578           307.6       0       0
     291         100.963        914.8578           307.6       0       0
     292         100.963        924.8578           307.6       0       0
     293         110.963        864.8578           307.6       0       0
     294         110.963        874.8578           307.6       0       0
     295         110.963        884.8578           307.6       0       0
     296         110.963        894.8578           307.6       0       0
     297         110.963        904.8578           307.6       0       0
     298         110.963        914.8578           307.6       0       0
     299         110.963        924.8578           307.6       0       0
     300         120.963        864.8578           307.6       0       0
     301         120.963        874.8578           307.6       0       0
     302         120.963        884.8578           307.6       0       0
     303         120.963        894.8578           307.6       0       0
     304         120.963        904.8578           307.6       0       0
     305         120.963        914.8578           307.6       0       0
     306         120.963        924.8578           307.6       0       0
     307         130.963        864.8578           307.6       0       0
     308         130.963        874.8578           307.6       0       0
     309         130.963        884.8578           307.6       0       0
     310         130.963        894.8578           307.6       0       0
     311         130.963        904.8578           307.6       0       0
     312         130.963        914.8578           307.6       0       0
     313         130.963        924.8578           307.6       0       0
     314         140.963        864.8578           307.6       0       0
     315         140.963        874.8578           307.6       0       0
     316         140.963        884.8578           307.6       0       0
     317         140.963        894.8578           307.6       0       0
     318         140.963        904.8578           307.6       0       0
     319         140.963        914.8578           307.6       0       0
     320         140.963        924.8578           307.6       0       0
     321         150.963        864.8578           307.6       0       0
     322         150.963        874.8578           307.6       0       0
     323         150.963        884.8578           307.6       0       0
     324         150.963        894.8578           307.6       0       0
     325         150.963        904.8578           307.6       0       0
     326         150.963        914.8578           307.6       0       0
     327         150.963        924.8578           307.6       0       0
     328         160.963        864.8578           307.6       0       0
     329         160.963        874.8578           307.6       0       0
     330         160.963        884.8578           307.6       0       0
     331         160.963        894.8578           307.6       0       0
     332         160.963        904.8578           307.6       0       0
     333         160.963        914.8578           307.6       0       0
     334         160.963        924.8578           307.6       0       0
     335         170.963        864.8578           307.6       0       0
     336         170.963        874.8578           307.6       0       0
     337         170.963        884.8578           307.6       0       0
     338         170.963        894.8578           307.6       0       0
     339         170.963        904.8578           307.6       0       0
     340         170.963        914.8578           307.6       0       0
     341         170.963        924.8578           307.6       0       0
     342         180.963        864.8578           307.6       0       0
     343         180.963        874.8578           307.6       0       0
     344         180.963        884.8578           307.6       0       0
     345         180.963        894.8578           307.6       0       0
     346         180.963        904.8578           307.6       0       0
     347         180.963        914.8578           307.6       0       0
     348         180.963        924.8578           307.6       0       0
     349         190.963        864.8578           307.6       0       0
     350         190.963        874.8578           307.6       0       0
     351         190.963        884.8578           307.6       0       0
     352         190.963        894.8578           307.6       0       0
     353         190.963        904.8578           307.6       0       0
     354         190.963        914.8578           307.6       0       0
     355         190.963        924.8578           307.6       0       0
     356         200.963        864.8578           307.6       0       0
     357         200.963        874.8578           307.6       0       0
     358         200.963        884.8578           307.6       0       0
     359         200.963        894.8578           307.6       0       0
     360         200.963        904.8578           307.6       0       0
     361         200.963        914.8578           307.6       0       0
     362         200.963        924.8578           307.6       0       0
     363         210.963        864.8578           307.6       0       0
     364         210.963        874.8578           307.6       0       0
     365         210.963        884.8578           307.6       0       0
     366         210.963        894.8578           307.6       0       0
     367         210.963        904.8578           307.6       0       0
     368         210.963        914.8578           307.6       0       0
     369         210.963        924.8578           307.6       0       0
     370         220.963        864.8578           307.6       0       0
     371         220.963        874.8578           307.6       0       0
     372         220.963        884.8578           307.6       0       0
     373         220.963        894.8578           307.6       0       0
     374         220.963        904.8578           307.6       0       0
     375         220.963        914.8578           307.6       0       0
     376         220.963        924.8578           307.6       0       0
     377         230.963        864.8578           307.6       0       0
     378         230.963        874.8578           307.6       0       0
     379         230.963        884.8578           307.6       0       0
     380         230.963        894.8578           307.6       0       0
     381         230.963        904.8578           307.6       0       0
     382         230.963        914.8578           307.6       0       0
     383         230.963        924.8578           307.6       0       0
     384         240.963        864.8578           307.6       0       0
     385         240.963        874.8578           307.6       0       0
     386         240.963        884.8578           307.6       0       0
     387         240.963        894.8578           307.6       0       0
     388         240.963        904.8578           307.6       0       0
     389         240.963        914.8578           307.6       0       0
     390         240.963        924.8578           307.6       0       0
     391         250.963        864.8578           307.6       0       0
     392         250.963        874.8578           307.6       0       0
     393         250.963        884.8578           307.6       0       0
     394         250.963        894.8578           307.6       0       0
     395         250.963        904.8578           307.6       0       0
     396         250.963        914.8578           307.6       0       0
     397         250.963        924.8578           307.6       0       0
     398         260.963        864.8578           307.6       0       0
     399         260.963        874.8578           307.6       0       0
     400         260.963        884.8578           307.6       0       0
     401         260.963        894.8578           307.6       0       0
     402         260.963        904.8578           307.6       0       0
     403         260.963        914.8578           307.6       0       0
     404         260.963        924.8578           307.6       0       0
     405         270.963        864.8578           307.6       0       0
     406         270.963        874.8578           307.6       0       0
     407         270.963        884.8578           307.6       0       0
     408         270.963        894.8578           307.6       0       0
     409         270.963        904.8578           307.6       0       0
     410         270.963        914.8578           307.6       0       0
     411         270.963        924.8578           307.6       0       0
     412         280.963        864.8578           307.6       0       0
     413         280.963        874.8578           307.6       0       0
     414         280.963        884.8578           307.6       0       0
     415         280.963        894.8578           307.6       0       0
     416         280.963        904.8578           307.6       0       0
     417         280.963        914.8578           307.6       0       0
     418         280.963        924.8578           307.6       0       0
     419         290.963        864.8578           307.6       0       0
     420         290.963        874.8578           307.6       0       0
     421         290.963        884.8578           307.6       0       0
     422         290.963        894.8578           307.6       0       0
     423         290.963        904.8578           307.6       0       0
     424         290.963        914.8578           307.6       0       0
     425         290.963        924.8578           307.6       0       0
     426         300.963        864.8578           307.6       0       0
     427         300.963        874.8578           307.6       0       0
     428         300.963        884.8578           307.6       0       0
     429         300.963        894.8578           307.6       0       0
     430         300.963        904.8578           307.6       0       0
     431         300.963        914.8578           307.6       0       0
     432         300.963        924.8578           307.6       0       0
     433         310.963        864.8578           307.6       0       0
     434         310.963        874.8578           307.6       0       0
     435         310.963        884.8578           307.6       0       0
     436         310.963        894.8578           307.6       0       0
     437         310.963        904.8578           307.6       0       0
     438         310.963        914.8578           307.6       0       0
     439         310.963        924.8578           307.6       0       0
     440         320.963        864.8578           307.6       0       0
     441         320.963        874.8578           307.6       0       0
     442         320.963        884.8578           307.6       0       0
     443         320.963        894.8578           307.6       0       0
     444         320.963        904.8578           307.6       0       0
     445         320.963        914.8578           307.6       0       0
     446         320.963        924.8578           307.6       0       0
     447         330.963        864.8578           307.6       0       0
     448         330.963        874.8578           307.6       0       0
     449         330.963        884.8578           307.6       0       0
     450         330.963        894.8578           307.6       0       0
     451         330.963        904.8578           307.6       0       0
     452         330.963        914.8578           307.6       0       0
     453         330.963        924.8578           307.6       0       0
     454         340.963        864.8578           307.6       0       0
     455         340.963        874.8578           307.6       0       0
     456         340.963        884.8578           307.6       0       0
     457         340.963        894.8578           307.6       0       0
     458         340.963        904.8578           307.6       0       0
     459         340.963        914.8578           307.6       0       0
     460         340.963        924.8578           307.6       0       0
     461         350.963        864.8578           307.6       0       0
     462         350.963        874.8578           307.6       0       0
     463         350.963        884.8578           307.6       0       0
     464         350.963        894.8578           307.6       0       0
     465         350.963        904.8578           307.6       0       0
     466         350.963        914.8578           307.6       0       0
     467         350.963        924.8578           307.6       0       0
     468         360.963        864.8578           307.6       0       0
     469         360.963        874.8578           307.6       0       0
     470         360.963        884.8578           307.6       0       0
     471         360.963        894.8578           307.6       0       0
     472         360.963        904.8578           307.6       0       0
     473         360.963        914.8578           307.6       0       0
     474         360.963        924.8578           307.6       0       0
     475         370.963        864.8578           307.6       0       0
     476         370.963        874.8578           307.6       0       0
     477         370.963        884.8578           307.6       0       0
     478         370.963        894.8578           307.6       0       0
     479         370.963        904.8578           307.6       0       0
     480         370.963        914.8578           307.6       0       0
     481         370.963        924.8578           307.6       0       0
     482         380.963        864.8578           307.6       0       0
     483         380.963        874.8578           307.6       0       0
     484         380.963        884.8578           307.6       0       0
     485         380.963        894.8578           307.6       0       0
     486         380.963        904.8578           307.6       0       0
     487         380.963        914.8578           307.6       0       0
     488         380.963        924.8578           307.6       0       0
     489         390.963        864.8578           307.6       0       0
     490         390.963        874.8578           307.6       0       0
     491         390.963        884.8578           307.6       0       0
     492         390.963        894.8578           307.6       0       0
     493         390.963        904.8578           307.6       0       0
     494         390.963        914.8578           307.6       0       0
     495         390.963        924.8578           307.6       0       0
     496         400.963        864.8578           307.6       0       0
     497         400.963        874.8578           307.6       0       0
     498         400.963        884.8578           307.6       0       0
     499         400.963        894.8578           307.6       0       0
     500         400.963        904.8578           307.6       0       0
     501         400.963        914.8578           307.6       0       0
     502         400.963        924.8578           307.6       0       0
     503         410.963        864.8578           307.6       0       0
     504         410.963        874.8578           307.6       0       0
     505         410.963        884.8578           307.6       0       0
     506         410.963        894.8578           307.6       0       0
     507         410.963        904.8578           307.6       0       0
     508         410.963        914.8578           307.6       0       0
     509         410.963        924.8578           307.6       0       0
     510         420.963        864.8578           307.6       0       0
     511         420.963        874.8578           307.6       0       0
     512         420.963        884.8578           307.6       0       0
     513         420.963        894.8578           307.6       0       0
     514         420.963        904.8578           307.6       0       0
     515         420.963        914.8578           307.6       0       0
     516         420.963        924.8578           307.6       0       0
     517         430.963        864.8578           307.6       0       0
     518         430.963        874.8578           307.6       0       0
     519         430.963        884.8578           307.6       0       0
     520         430.963        894.8578           307.6       0       0
     521         430.963        904.8578           307.6       0       0
     522         430.963        914.8578           307.6       0       0
     523         430.963        924.8578           307.6       0       0
     524         440.963        864.8578           307.6       0       0
     525         440.963        874.8578           307.6       0       0
     526         440.963        884.8578           307.6       0       0
     527         440.963        894.8578           307.6       0       0
     528         440.963        904.8578           307.6       0       0
     529         440.963        914.8578           307.6       0       0
     530         440.963        924.8578           307.6       0       0
     531         450.963        864.8578           307.6       0       0
     532         450.963        874.8578           307.6       0       0
     533         450.963        884.8578           307.6       0       0
     534         450.963        894.8578           307.6       0       0
     535         450.963        904.8578           307.6       0       0
     536         450.963        914.8578           307.6       0       0
     537         450.963        924.8578           307.6       0       0
     538         460.963        864.8578           307.6       0       0
     539         460.963        874.8578           307.6       0       0
     540         460.963        884.8578           307.6       0       0
     541         460.963        894.8578           307.6       0       0
     542         460.963        904.8578           307.6       0       0
     543         460.963        914.8578           307.6       0       0
     544         460.963        924.8578           307.6       0       0
     545         470.963        864.8578           307.6       0       0
     546         470.963        874.8578           307.6       0       0
     547         470.963        884.8578           307.6       0       0
     548         470.963        894.8578           307.6       0       0
     549         470.963        904.8578           307.6       0       0
     550         470.963        914.8578           307.6       0       0
     551         470.963        924.8578           307.6       0       0
     552         480.963        864.8578           307.6       0       0
     553         480.963        874.8578           307.6       0       0
     554         480.963        884.8578           307.6       0       0
     555         480.963        894.8578           307.6       0       0
     556         480.963        904.8578           307.6       0       0
     557         480.963        914.8578           307.6       0       0
     558         480.963        924.8578           307.6       0       0
     559         490.963        864.8578           307.6       0       0
     560         490.963        874.8578           307.6       0       0
     561         490.963        884.8578           307.6       0       0
     562         490.963        894.8578           307.6       0       0
     563         490.963        904.8578           307.6       0       0
     564         490.963        914.8578           307.6       0       0
     565         490.963        924.8578           307.6       0       0
     566         500.963        864.8578           307.6       0       0
     567         500.963        874.8578           307.6       0       0
     568         500.963        884.8578           307.6       0       0
     569         500.963        894.8578           307.6       0       0
     570         500.963        904.8578           307.6       0       0
     571         500.963        914.8578           307.6       0       0
     572         500.963        924.8578           307.6       0       0
     573         510.963        864.8578           307.6       0       0
     574         510.963        874.8578           307.6       0       0
     575         510.963        884.8578           307.6       0       0
     576         510.963        894.8578           307.6       0       0
     577         510.963        904.8578           307.6       0       0
     578         510.963        914.8578           307.6       0       0
     579         510.963        924.8578           307.6       0       0
     580         520.963        864.8578           307.6       0       0
     581         520.963        874.8578           307.6       0       0
     582         520.963        884.8578           307.6       0       0
     583         520.963        894.8578           307.6       0       0
     584         520.963        904.8578           307.6       0       0
     585         520.963        914.8578           307.6       0       0
     586         520.963        924.8578           307.6       0       0
     587         530.963        864.8578           307.6       0       0
     588         530.963        874.8578           307.6       0       0
     589         530.963        884.8578           307.6       0       0
     590         530.963        894.8578           307.6       0       0
     591         530.963        904.8578           307.6       0       0
     592         530.963        914.8578           307.6       0       0
     593         530.963        924.8578           307.6       0       0
     594         540.963        864.8578           307.6       0       0
     595         540.963        874.8578           307.6       0       0
     596         540.963        884.8578           307.6       0       0
     597         540.963        894.8578           307.6       0       0
     598         540.963        904.8578           307.6       0       0
     599         540.963        914.8578           307.6       0       0
     600         540.963        924.8578           307.6       0       0
     601         550.963        864.8578           307.6       0       0
     602         550.963        874.8578           307.6       0       0
     603         550.963        884.8578           307.6       0       0
     604         550.963        894.8578           307.6       0       0
     605         550.963        904.8578           307.6       0       0
     606         550.963        914.8578           307.6       0       0
     607         550.963        924.8578           307.6       0       0
     608         560.963        864.8578           307.6       0       0
     609         560.963        874.8578           307.6       0       0
     610         560.963        884.8578           307.6       0       0
     611         560.963        894.8578           307.6       0       0
     612         560.963        904.8578           307.6       0       0
     613         560.963        914.8578           307.6       0       0
     614         560.963        924.8578           307.6       0       0
     615         570.963        864.8578           307.6       0       0
     616         570.963        874.8578           307.6       0       0
     617         570.963        884.8578           307.6       0       0
     618         570.963        894.8578           307.6       0       0
     619         570.963        904.8578           307.6       0       0
     620         570.963        914.8578           307.6       0       0
     621         570.963        924.8578           307.6       0       0
     622         580.963        864.8578           307.6       0       0
     623         580.963        874.8578           307.6       0       0
     624         580.963        884.8578           307.6       0       0
     625         580.963        894.8578           307.6       0       0
     626         580.963        904.8578           307.6       0       0
     627         580.963        914.8578           307.6       0       0
     628         580.963        924.8578           307.6       0       0
     629         590.963        864.8578           307.6       0       0
     630         590.963        874.8578           307.6       0       0
     631         590.963        884.8578           307.6       0       0
     632         590.963        894.8578           307.6       0       0
     633         590.963        904.8578           307.6       0       0
     634         590.963        914.8578           307.6       0       0
     635         590.963        924.8578           307.6       0       0
     636         600.963        864.8578           307.6       0       0
     637         600.963        874.8578           307.6       0       0
     638         600.963        884.8578           307.6       0       0
     639         600.963        894.8578           307.6       0       0
     640         600.963        904.8578           307.6       0       0
     641         600.963        914.8578           307.6       0       0
     642         600.963        924.8578           307.6       0       0
     643         610.963        864.8578           307.6       0       0
     644         610.963        874.8578           307.6       0       0
     645         610.963        884.8578           307.6       0       0
     646         610.963        894.8578           307.6       0       0
     647         610.963        904.8578           307.6       0       0
     648         610.963        914.8578           307.6       0       0
     649         610.963        924.8578           307.6       0       0
     650         620.963        864.8578           307.6       0       0
     651         620.963        874.8578           307.6       0       0
     652         620.963        884.8578           307.6       0       0
     653         620.963        894.8578           307.6       0       0
     654         620.963        904.8578           307.6       0       0
     655         620.963        914.8578           307.6       0       0
     656         620.963        924.8578           307.6       0       0
     657         630.963        864.8578           307.6       0       0
     658         630.963        874.8578           307.6       0       0
     659         630.963        884.8578           307.6       0       0
     660         630.963        894.8578           307.6       0       0
     661         630.963        904.8578           307.6       0       0
     662         630.963        914.8578           307.6       0       0
     663         630.963        924.8578           307.6       0       0
     664         640.963        864.8578           307.6       0       0
     665         640.963        874.8578           307.6       0       0
     666         640.963        884.8578           307.6       0       0
     667         640.963        894.8578           307.6       0       0
     668         640.963        904.8578           307.6       0       0
     669         640.963        914.8578           307.6       0       0
     670         640.963        924.8578           307.6       0       0
     671         650.963        864.8578           307.6       0       0
     672         650.963        874.8578           307.6       0       0
     673         650.963        884.8578           307.6       0       0
     674         650.963        894.8578           307.6       0       0
     675         650.963        904.8578           307.6       0       0
     676         650.963        914.8578           307.6       0       0
     677         650.963        924.8578           307.6       0       0
     678         660.963        864.8578           307.6       0       0
     679         660.963        874.8578           307.6       0       0
     680         660.963        884.8578           307.6       0       0
     681         660.963        894.8578           307.6       0       0
     682         660.963        904.8578           307.6       0       0
     683         660.963        914.8578           307.6       0       0
     684         660.963        924.8578           307.6       0       0
     685         670.963        864.8578           307.6       0       0
     686         670.963        874.8578           307.6       0       0
     687         670.963        884.8578           307.6       0       0
     688         670.963        894.8578           307.6       0       0
     689         670.963        904.8578           307.6       0       0
     690         670.963        914.8578           307.6       0       0
     691         670.963        924.8578           307.6       0       0
     692         680.963        864.8578           307.6       0       0
     693         680.963        874.8578           307.6       0       0
     694         680.963        884.8578           307.6       0       0
     695         680.963        894.8578           307.6       0       0
     696         680.963        904.8578           307.6       0       0
     697         680.963        914.8578           307.6       0       0
     698         680.963        924.8578           307.6       0       0
     699         771.477          855.59         440.078       0       0
     700         771.477          855.59         440.078       0       0
     701         771.477          855.59         440.078       0       0
     702         888.644         856.116         110.609       0       0
     703         888.644         856.116         110.609       0       0
     704         771.477         855.421         194.079       0       0
     705         771.477         855.421         194.079       0       0
    2367         771.477        855.5208        339.3074       0       0
    2368        771.4768        859.2476        332.6191       0       0
    2369        777.5585        855.5208          339.18       0       0
    2370        777.2783        859.2476        332.4976       0       0
    2371        783.6294        855.5208        338.7981       0       0
    2372        783.0695        859.2476        332.1332       0       0
    2373         789.679        855.5208        338.1622       0       0
    2374        788.8405        859.2476        331.5267       0       0
    2375        795.6965        855.5208        337.2736       0       0
    2376        794.5809        859.2476         330.679       0       0
    2377        801.6717        855.5208        336.1338       0       0
    2378        800.2809        859.2476        329.5917       0       0
    2379        807.5938        855.5208        334.7448       0       0
    2380        805.9303        859.2476        328.2666       0       0
    2381        813.4526        855.5208         333.109       0       0
    2382        811.5193        859.2476        326.7062       0       0
    2383        819.2378        855.5208        331.2292       0       0
    2384         817.038        859.2476         324.913       0       0
    2385        824.9391        855.5208        329.1089       0       0
    2386        822.4768        859.2476        322.8904       0       0
    2387        830.5467        855.5208        326.7517       0       0
    2388        827.8262        859.2476        320.6417       0       0
    2389        836.0507        855.5208        324.1618       0       0
    2390        833.0766        859.2476        318.1711       0       0
    2391        841.4413        855.5208        321.3436       0       0
    2392         838.219        859.2476        315.4827       0       0
    2393        846.7092        855.5208        318.3022       0       0
    2394        843.2443        859.2476        312.5813       0       0
    2395        851.8452        855.5208        315.0428       0       0
    2396        848.1438        859.2476        309.4721       0       0
    2397        856.8401        855.5208        311.5712       0       0
    2398        852.9087        859.2476        306.1604       0       0
    2399        861.6853        855.5208        307.8936       0       0
    2400        857.5308        859.2476        302.6521       0       0
    2401        866.3722        855.5208        304.0162       0       0
    2402        862.0018        859.2476        298.9533       0       0
    2403        870.8926        855.5208         299.946       0       0
    2404        866.3141        859.2476        295.0705       0       0
    2405        875.2387        855.5208          295.69       0       0
    2406          870.46        859.2476        291.0106       0       0
    2407        879.4027        855.5208        291.2558       0       0
    2408        874.4322        859.2476        286.7805       0       0
    2409        883.3774        855.5208        286.6511       0       0
    2410        878.2238        859.2476        282.3879       0       0
    2411        887.1558        855.5208         281.884       0       0
    2412        881.8282        859.2476        277.8404       0       0
    2413        890.7312        855.5208        276.9628       0       0
    2414         885.239        859.2476        273.1459       0       0
    2415        894.0974        855.5208        271.8963       0       0
    2416        888.4501        859.2476        268.3127       0       0
    2417        897.2485        855.5208        266.6932       0       0
    2418        891.4561        859.2476        263.3492       0       0
    2419        900.1789        855.5208        261.3628       0       0
    2420        894.2516        859.2476        258.2643       0       0
    2421        902.8835        855.5208        255.9143       0       0
    2422        896.8317        859.2476        253.0667       0       0
    2423        905.3577        855.5208        250.3573       0       0
    2424        899.1919        859.2476        247.7656       0       0
    2425        907.5969        855.5208        244.7016       0       0
    2426        901.3281        859.2476        242.3704       0       0
    2427        909.5974        855.5208        238.9571       0       0
    2428        903.2363        859.2476        236.8904       0       0
    2429        911.3555        855.5208        233.1339       0       0
    2430        904.9136        859.2476        231.3354       0       0
    2431        912.8683        855.5208        227.2421       0       0
    2432        906.3566        859.2476         225.715       0       0
    2433         914.133        855.5208        221.2921       0       0
    2434        907.5631        859.2476        220.0391       0       0
    2435        915.1475        855.5208        215.2945       0       0
    2436        908.5309        859.2476        214.3176       0       0
    2437        915.9098        855.5208        209.2596       0       0
    2438        909.2581        859.2476        208.5606       0       0
    2439        916.4188        855.5208         203.198       0       0
    2440        909.7437        859.2476        202.7782       0       0
    2441        916.6735        855.5208        197.1205       0       0
    2442        909.9867        859.2476        196.9806       0       0
    2443        916.6736        855.5208        191.0376       0       0
    2444        909.9867        859.2476        191.1779       0       0
    2445        916.4188        855.5208        184.9601       0       0
    2446        909.7437        859.2476        185.3802       0       0
    2447        915.9099        855.5208        178.8986       0       0
    2448        909.2582        859.2476        179.5979       0       0
    2449        915.1475        855.5208        172.8637       0       0
    2450        908.5309        859.2476        173.8409       0       0
    2451         914.133        855.5208         166.866       0       0
    2452        907.5632        859.2476        168.1194       0       0
    2453        912.8683        855.5208         160.916       0       0
    2454        906.3568        859.2476        162.4435       0       0
    2455        911.3556        855.5208        155.0243       0       0
    2456        904.9137        859.2476        156.8231       0       0
    2457        909.5974        855.5208         149.201       0       0
    2458        903.2365        859.2476         151.268       0       0
    2459         907.597        855.5208        143.4565       0       0
    2460        901.3282        859.2476         145.788       0       0
    2461        905.3577        855.5208        137.8008       0       0
    2462        899.1921        859.2476        140.3928       0       0
    2463        902.8836        855.5208        132.2438       0       0
    2464        896.8319        859.2476        135.0917       0       0
    2465         900.179        855.5208        126.7953       0       0
    2466        894.2518        859.2476        129.8941       0       0
    2467        897.2485        855.5208        121.4648       0       0
    2468        891.4563        859.2476        124.8092       0       0
    2469        894.0974        855.5208        116.2618       0       0
    2470        888.4504        859.2476        119.8457       0       0
    2471        890.7312        855.5208        111.1952       0       0
    2472        885.2392        859.2476        115.0125       0       0
    2473        887.1558        855.5208         106.274       0       0
    2474        881.8284        859.2476        110.3179       0       0
    2475        883.3774        855.5208        101.5069       0       0
    2476        878.2241        859.2476        105.7704       0       0
    2477        879.4027        855.5208        96.90221       0       0
    2478        874.4324        859.2476        101.3777       0       0
    2479        875.2387        855.5208        92.46799       0       0
    2480        870.4602        859.2476        97.14769       0       0
    2481        870.8926        855.5208        88.21201       0       0
    2482        866.3143        859.2476        93.08772       0       0
    2483        866.3722        855.5208        84.14178       0       0
    2484         862.002        859.2476        89.20493       0       0
    2485        861.6852        855.5208         80.2644       0       0
    2486        857.5309        859.2476        85.50611       0       0
    2487          856.84        855.5208         76.5867       0       0
    2488        852.9089        859.2476        81.99778       0       0
    2489        851.8451        855.5208        73.11511       0       0
    2490        848.1439        859.2476        78.68606       0       0
    2491        846.7091        855.5208        69.85575       0       0
    2492        843.2446        859.2476         75.5768       0       0
    2493        841.4412        855.5208        66.81432       0       0
    2494        838.2192        859.2476        72.67543       0       0
    2495        836.0505        855.5208        63.99614       0       0
    2496        833.0768        859.2476        69.98704       0       0
    2497        830.5466        855.5208        61.40619       0       0
    2498        827.8264        859.2476        67.51635       0       0
    2499         824.939        855.5208        59.04898       0       0
    2500        822.4771        859.2476        65.26769       0       0
    2501        819.2376        855.5208        56.92867       0       0
    2502        817.0382        859.2476        63.24503       0       0
    2503        813.4525        855.5208        55.04895       0       0
    2504        811.5195        859.2476        61.45188       0       0
    2505        807.5936        855.5208        53.41315       0       0
    2506        805.9305        859.2476         59.8914       0       0
    2507        801.6714        855.5208        52.02412       0       0
    2508        800.2811        859.2476        58.56634       0       0
    2509        795.6963        855.5208        50.88432       0       0
    2510        794.5811        859.2476        57.47902       0       0
    2511        789.6787        855.5208        49.99573       0       0
    2512        788.8406        859.2476        56.63134       0       0
    2513        783.6292        855.5208        49.35991       0       0
    2514        783.0696        859.2476        56.02478       0       0
    2515        777.5582        855.5208        48.97797       0       0
    2516        777.2784        859.2476        55.66042       0       0
    2517        771.4767        855.5208        48.85059       0       0
    2518        771.4769        859.2476        55.53891       0       0
    2519        765.3951        855.5208        48.97798       0       0
    2520        765.6754        859.2476        55.66044       0       0
    2521        759.3243        855.5208        49.35995       0       0
    2522        759.8842        859.2476         56.0248       0       0
    2523        753.2747        855.5208        49.99579       0       0
    2524        754.1132        859.2476        56.63136       0       0
    2525        747.2571        855.5208        50.88441       0       0
    2526        748.3727        859.2476        57.47905       0       0
    2527         741.282        855.5208        52.02425       0       0
    2528        742.6727        859.2476        58.56638       0       0
    2529        735.3598        855.5208         53.4133       0       0
    2530        737.0233        859.2476        59.89145       0       0
    2531         729.501        855.5208        55.04912       0       0
    2532        731.4343        859.2476        61.45193       0       0
    2533        723.7158        855.5208        56.92885       0       0
    2534        725.9156        859.2476        63.24507       0       0
    2535        718.0145        855.5208        59.04919       0       0
    2536        720.4768        859.2476        65.26775       0       0
    2537        712.4069        855.5208        61.40642       0       0
    2538        715.1274        859.2476        67.51643       0       0
    2539         706.903        855.5208         63.9964       0       0
    2540         709.877        859.2476        69.98711       0       0
    2541        701.5123        855.5208        66.81459       0       0
    2542        704.7346        859.2476        72.67551       0       0
    2543        696.2443        855.5208        69.85605       0       0
    2544        699.7093        859.2476         75.5769       0       0
    2545        691.1084        855.5208        73.11544       0       0
    2546        694.8098        859.2476        78.68618       0       0
    2547        686.1135        855.5208        76.58704       0       0
    2548        690.0449        859.2476        81.99789       0       0
    2549        681.2683        855.5208        80.26476       0       0
    2550        685.4229        859.2476        85.50623       0       0
    2551        676.5814        855.5208        84.14215       0       0
    2552        680.9518        859.2476        89.20504       0       0
    2553        672.0609        855.5208        88.21242       0       0
    2554        676.6395        859.2476        93.08785       0       0
    2555        667.7148        855.5208         92.4684       0       0
    2556        672.4937        859.2476        97.14783       0       0
    2557        663.5509        855.5208        96.90264       0       0
    2558        668.5214        859.2476        101.3779       0       0
    2559        659.5762        855.5208        101.5074       0       0
    2560        664.7298        859.2476        105.7705       0       0
    2561        655.7979        855.5208        106.2745       0       0
    2562        661.1255        859.2476        110.3181       0       0
    2563        652.2225        855.5208        111.1956       0       0
    2564        657.7147        859.2476        115.0126       0       0
    2565        648.8563        855.5208        116.2622       0       0
    2566        654.5036        859.2476        119.8458       0       0
    2567        645.7053        855.5208        121.4652       0       0
    2568        651.4976        859.2476        124.8092       0       0
    2569        642.7748        855.5208        126.7957       0       0
    2570        648.7021        859.2476        129.8941       0       0
    2571        640.0702        855.5208        132.2442       0       0
    2572        646.1221        859.2476        135.0917       0       0
    2573        637.5961        855.5208        137.8011       0       0
    2574        643.7619        859.2476        140.3928       0       0
    2575        635.3569        855.5208        143.4568       0       0
    2576        641.6258        859.2476         145.788       0       0
    2577        633.3564        855.5208        149.2013       0       0
    2578        639.7175        859.2476        151.2679       0       0
    2579        631.5983        855.5208        155.0246       0       0
    2580        638.0403        859.2476         156.823       0       0
    2581        630.0856        855.5208        160.9163       0       0
    2582        636.5973        859.2476        162.4434       0       0
    2583        628.8209        855.5208        166.8662       0       0
    2584        635.3908        859.2476        168.1193       0       0
    2585        627.8065        855.5208        172.8639       0       0
    2586        634.4231        859.2476        173.8408       0       0
    2587        627.0441        855.5208        178.8988       0       0
    2588        633.6958        859.2476        179.5977       0       0
    2589        626.5352        855.5208        184.9603       0       0
    2590        633.2103        859.2476        185.3801       0       0
    2591        626.2804        855.5208        191.0378       0       0
    2592        632.9673        859.2476        191.1777       0       0
    2593        626.2805        855.5208        197.1207       0       0
    2594        632.9673        859.2476        196.9804       0       0
    2595        626.5352        855.5208        203.1982       0       0
    2596        633.2103        859.2476         202.778       0       0
    2597        627.0442        855.5208        209.2597       0       0
    2598        633.6959        859.2476        208.5604       0       0
    2599        627.8066        855.5208        215.2946       0       0
    2600        634.4232        859.2476        214.3173       0       0
    2601         628.821        855.5208        221.2922       0       0
    2602        635.3908        859.2476        220.0388       0       0
    2603        630.0857        855.5208        227.2422       0       0
    2604        636.5973        859.2476        225.7147       0       0
    2605        631.5984        855.5208        233.1339       0       0
    2606        638.0403        859.2476        231.3351       0       0
    2607        633.3566        855.5208        238.9571       0       0
    2608        639.7175        859.2476        236.8902       0       0
    2609        635.3571        855.5208        244.7016       0       0
    2610        641.6259        859.2476        242.3701       0       0
    2611        637.5963        855.5208        250.3573       0       0
    2612         643.762        859.2476        247.7653       0       0
    2613        640.0704        855.5208        255.9143       0       0
    2614        646.1221        859.2476        253.0663       0       0
    2615         642.775        855.5208        261.3627       0       0
    2616        648.7022        859.2476        258.2639       0       0
    2617        645.7055        855.5208        266.6932       0       0
    2618        651.4977        859.2476        263.3489       0       0
    2619        648.8566        855.5208        271.8962       0       0
    2620        654.5036        859.2476        268.3123       0       0
    2621        652.2228        855.5208        276.9628       0       0
    2622        657.7148        859.2476        273.1455       0       0
    2623        655.7982        855.5208        281.8839       0       0
    2624        661.1255        859.2476          277.84       0       0
    2625        659.5765        855.5208         286.651       0       0
    2626        664.7299        859.2476        282.3875       0       0
    2627        663.5511        855.5208        291.2557       0       0
    2628        668.5214        859.2476        286.7802       0       0
    2629        667.7151        855.5208        295.6899       0       0
    2630        672.4937        859.2476        291.0102       0       0
    2631        672.0612        855.5208        299.9459       0       0
    2632        676.6395        859.2476        295.0701       0       0
    2633        676.5816        855.5208        304.0161       0       0
    2634        680.9518        859.2476        298.9529       0       0
    2635        681.2686        855.5208        307.8934       0       0
    2636        685.4229        859.2476        302.6517       0       0
    2637        686.1137        855.5208        311.5711       0       0
    2638        690.0449        859.2476          306.16       0       0
    2639        691.1086        855.5208        315.0427       0       0
    2640        694.8098        859.2476        309.4718       0       0
    2641        696.2445        855.5208        318.3021       0       0
    2642        699.7091        859.2476         312.581       0       0
    2643        701.5125        855.5208        321.3435       0       0
    2644        704.7344        859.2476        315.4824       0       0
    2645        706.9031        855.5208        324.1617       0       0
    2646        709.8768        859.2476        318.1707       0       0
    2647         712.407        855.5208        326.7516       0       0
    2648        715.1272        859.2476        320.6415       0       0
    2649        718.0145        855.5208        329.1088       0       0
    2650        720.4765        859.2476        322.8901       0       0
    2651        723.7159        855.5208        331.2292       0       0
    2652        725.9153        859.2476        324.9128       0       0
    2653         729.501        855.5208        333.1089       0       0
    2654         731.434        859.2476        326.7059       0       0
    2655        735.3597        855.5208        334.7447       0       0
    2656        737.0229        859.2476        328.2664       0       0
    2657        741.2819        855.5208        336.1337       0       0
    2658        742.6723        859.2476        329.5915       0       0
    2659         747.257        855.5208        337.2736       0       0
    2660        748.3723        859.2476        330.6789       0       0
    2661        753.2746        855.5208        338.1622       0       0
    2662        754.1127        859.2476        331.5266       0       0
    2663        759.3241        855.5208         338.798       0       0
    2664        759.8836        859.2476        332.1331       0       0
    2665         765.395        855.5208          339.18       0       0
    2666        765.6749        859.2476        332.4975       0       0
    2667        771.4764        855.5208        339.3074       0       0
    2668        771.4763        859.2476        332.6191       0       0
    2669        771.4768        863.7776        326.4509       0       0
    2670          777.02        863.7776        326.3348       0       0
    2671        782.5534        863.7776        325.9867       0       0
    2672        788.0674        863.7776        325.4072       0       0
    2673        793.5523        863.7776        324.5972       0       0
    2674        798.9985        863.7776        323.5583       0       0
    2675        804.3964        863.7776        322.2923       0       0
    2676        809.7365        863.7776        320.8013       0       0
    2677        815.0095        863.7776         319.088       0       0
    2678        820.2062        863.7776        317.1554       0       0
    2679        825.3173        863.7776        315.0068       0       0
    2680         830.334        863.7776        312.6462       0       0
    2681        835.2475        863.7776        310.0775       0       0
    2682        840.0491        863.7776        307.3053       0       0
    2683        844.7303        863.7776        304.3345       0       0
    2684        849.2831        863.7776        301.1703       0       0
    2685        853.6994        863.7776        297.8181       0       0
    2686        857.9714        863.7776         294.284       0       0
    2687        862.0917        863.7776        290.5741       0       0
    2688         866.053        863.7776        286.6949       0       0
    2689        869.8484        863.7776        282.6533       0       0
    2690        873.4712        863.7776        278.4562       0       0
    2691         876.915        863.7776        274.1111       0       0
    2692         880.174        863.7776        269.6256       0       0
    2693        883.2422        863.7776        265.0076       0       0
    2694        886.1143        863.7776        260.2651       0       0
    2695        888.7854        863.7776        255.4066       0       0
    2696        891.2506        863.7776        250.4404       0       0
    2697        893.5057        863.7776        245.3754       0       0
    2698        895.5468        863.7776        240.2204       0       0
    2699        897.3701        863.7776        234.9844       0       0
    2700        898.9726        863.7776        229.6767       0       0
    2701        900.3514        863.7776        224.3065       0       0
    2702        901.5042        863.7776        218.8833       0       0
    2703        902.4289        863.7776        213.4165       0       0
    2704        903.1238        863.7776        207.9159       0       0
    2705        903.5877        863.7776        202.3909       0       0
    2706        903.8199        863.7776        196.8514       0       0
    2707        903.8199        863.7776        191.3071       0       0
    2708        903.5877        863.7776        185.7675       0       0
    2709        903.1238        863.7776        180.2426       0       0
    2710        902.4289        863.7776         174.742       0       0
    2711        901.5043        863.7776        169.2752       0       0
    2712        900.3516        863.7776         163.852       0       0
    2713        898.9728        863.7776        158.4818       0       0
    2714        897.3702        863.7776        153.1741       0       0
    2715        895.5469        863.7776        147.9381       0       0
    2716        893.5059        863.7776        142.7831       0       0
    2717        891.2508        863.7776         137.718       0       0
    2718        888.7856        863.7776        132.7518       0       0
    2719        886.1146        863.7776        127.8932       0       0
    2720        883.2424        863.7776        123.1508       0       0
    2721        880.1743        863.7776        118.5327       0       0
    2722        876.9153        863.7776        114.0472       0       0
    2723        873.4714        863.7776        109.7021       0       0
    2724        869.8486        863.7776         105.505       0       0
    2725        866.0532        863.7776        101.4633       0       0
    2726        862.0919        863.7776        97.58411       0       0
    2727        857.9716        863.7776        93.87419       0       0
    2728        853.6996        863.7776        90.34006       0       0
    2729        849.2833        863.7776        86.98792       0       0
    2730        844.7305        863.7776        83.82365       0       0
    2731        840.0493        863.7776        80.85282       0       0
    2732        835.2477        863.7776        78.08063       0       0
    2733        830.3343        863.7776        75.51192       0       0
    2734        825.3176        863.7776        73.15125       0       0
    2735        820.2064        863.7776         71.0027       0       0
    2736        815.0098        863.7776        69.07008       0       0
    2737        809.7367        863.7776        67.35677       0       0
    2738        804.3965        863.7776        65.86577       0       0
    2739        798.9987        863.7776        64.59971       0       0
    2740        793.5525        863.7776        63.56079       0       0
    2741        788.0676        863.7776        62.75085       0       0
    2742        782.5535        863.7776        62.17131       0       0
    2743        777.0201        863.7776        61.82317       0       0
    2744        771.4769        863.7776        61.70706       0       0
    2745        765.9337        863.7776        61.82317       0       0
    2746        760.4003        863.7776        62.17131       0       0
    2747        754.8863        863.7776        62.75086       0       0
    2748        749.4014        863.7776        63.56082       0       0
    2749        743.9552        863.7776        64.59974       0       0
    2750        738.5573        863.7776        65.86581       0       0
    2751        733.2172        863.7776        67.35681       0       0
    2752        727.9441        863.7776        69.07013       0       0
    2753        722.7474        863.7776        71.00275       0       0
    2754        717.6363        863.7776         73.1513       0       0
    2755        712.6196        863.7776        75.51199       0       0
    2756        707.7061        863.7776         78.0807       0       0
    2757        702.9045        863.7776        80.85291       0       0
    2758        698.2233        863.7776        83.82375       0       0
    2759        693.6705        863.7776        86.98801       0       0
    2760        689.2542        863.7776        90.34015       0       0
    2761        684.9822        863.7776        93.87428       0       0
    2762        680.8619        863.7776        97.58423       0       0
    2763        676.9006        863.7776        101.4634       0       0
    2764        673.1052        863.7776        105.5051       0       0
    2765        669.4824        863.7776        109.7022       0       0
    2766        666.0386        863.7776        114.0473       0       0
    2767        662.7797        863.7776        118.5328       0       0
    2768        659.7115        863.7776        123.1508       0       0
    2769        656.8394        863.7776        127.8933       0       0
    2770        654.1683        863.7776        132.7518       0       0
    2771        651.7032        863.7776         137.718       0       0
    2772        649.4481        863.7776         142.783       0       0
    2773        647.4071        863.7776         147.938       0       0
    2774        645.5837        863.7776         153.174       0       0
    2775        643.9813        863.7776        158.4817       0       0
    2776        642.6024        863.7776        163.8519       0       0
    2777        641.4497        863.7776        169.2751       0       0
    2778         640.525        863.7776        174.7418       0       0
    2779        639.8302        863.7776        180.2424       0       0
    2780        639.3662        863.7776        185.7674       0       0
    2781        639.1341        863.7776        191.3069       0       0
    2782        639.1341        863.7776        196.8512       0       0
    2783        639.3663        863.7776        202.3907       0       0
    2784        639.8302        863.7776        207.9156       0       0
    2785        640.5251        863.7776        213.4163       0       0
    2786        641.4497        863.7776         218.883       0       0
    2787        642.6024        863.7776        224.3062       0       0
    2788        643.9813        863.7776        229.6763       0       0
    2789        645.5837        863.7776        234.9841       0       0
    2790        647.4071        863.7776          240.22       0       0
    2791        649.4481        863.7776        245.3751       0       0
    2792        651.7032        863.7776        250.4401       0       0
    2793        654.1685        863.7776        255.4062       0       0
    2794        656.8395        863.7776        260.2648       0       0
    2795        659.7115        863.7776        265.0072       0       0
    2796        662.7797        863.7776        269.6253       0       0
    2797        666.0386        863.7776        274.1107       0       0
    2798        669.4825        863.7776        278.4558       0       0
    2799        673.1053        863.7776        282.6529       0       0
    2800        676.9006        863.7776        286.6946       0       0
    2801        680.8619        863.7776        290.5738       0       0
    2802        684.9822        863.7776        294.2837       0       0
    2803        689.2542        863.7776        297.8178       0       0
    2804        693.6704        863.7776        301.1699       0       0
    2805        698.2231        863.7776        304.3342       0       0
    2806        702.9044        863.7776         307.305       0       0
    2807        707.7059        863.7776        310.0772       0       0
    2808        712.6193        863.7776        312.6459       0       0
    2809         717.636        863.7776        315.0066       0       0
    2810        722.7471        863.7776        317.1551       0       0
    2811        727.9438        863.7776        319.0878       0       0
    2812        733.2168        863.7776        320.8011       0       0
    2813        738.5569        863.7776        322.2921       0       0
    2814        743.9548        863.7776        323.5582       0       0
    2815        749.4009        863.7776        324.5971       0       0
    2816        754.8858        863.7776        325.4071       0       0
    2817        760.3998        863.7776        325.9866       0       0
    2818        765.9332        863.7776        326.3348       0       0
    2819        771.4763        863.7776        326.4509       0       0
    2820        771.4768        868.9761        320.8326       0       0
    2821        776.7847        868.9761        320.7214       0       0
    2822        782.0833        868.9761        320.3881       0       0
    2823        787.3632        868.9761        319.8331       0       0
    2824        792.6154        868.9761        319.0576       0       0
    2825        797.8304        868.9761        318.0627       0       0
    2826        802.9991        868.9761        316.8504       0       0
    2827        808.1126        868.9761        315.4227       0       0
    2828        813.1619        868.9761        313.7821       0       0
    2829        818.1379        868.9761        311.9315       0       0
    2830        823.0322        868.9761        309.8742       0       0
    2831        827.8359        868.9761        307.6138       0       0
    2832        832.5408        868.9761        305.1541       0       0
    2833        837.1386        868.9761        302.4996       0       0
    2834        841.6212        868.9761        299.6548       0       0
    2835        845.9808        868.9761        296.6249       0       0
    2836        850.2096        868.9761        293.4151       0       0
    2837        854.3003        868.9761         290.031       0       0
    2838        858.2457        868.9761        286.4785       0       0
    2839        862.0388        868.9761         282.764       0       0
    2840        865.6731        868.9761        278.8938       0       0
    2841        869.1422        868.9761        274.8749       0       0
    2842        872.4399        868.9761        270.7143       0       0
    2843        875.5605        868.9761        266.4192       0       0
    2844        878.4984        868.9761        261.9971       0       0
    2845        881.2487        868.9761         257.456       0       0
    2846        883.8064        868.9761        252.8036       0       0
    2847         886.167        868.9761        248.0482       0       0
    2848        888.3264        868.9761        243.1982       0       0
    2849        890.2808        868.9761        238.2619       0       0
    2850        892.0267        868.9761        233.2482       0       0
    2851        893.5612        868.9761        228.1658       0       0
    2852        894.8815        868.9761        223.0235       0       0
    2853        895.9854        868.9761        217.8305       0       0
    2854        896.8708        868.9761        212.5958       0       0
    2855        897.5362        868.9761        207.3286       0       0
    2856        897.9805        868.9761        202.0382       0       0
    2857        898.2028        868.9761        196.7338       0       0
    2858        898.2028        868.9761        191.4247       0       0
    2859        897.9805        868.9761        186.1203       0       0
    2860        897.5362        868.9761        180.8299       0       0
    2861        896.8708        868.9761        175.5627       0       0
    2862        895.9855        868.9761         170.328       0       0
    2863        894.8817        868.9761         165.135       0       0
    2864        893.5614        868.9761        159.9927       0       0
    2865        892.0269        868.9761        154.9102       0       0
    2866        890.2809        868.9761        149.8965       0       0
    2867        888.3265        868.9761        144.9603       0       0
    2868        886.1672        868.9761        140.1102       0       0
    2869        883.8066        868.9761        135.3548       0       0
    2870        881.2489        868.9761        130.7024       0       0
    2871        878.4987        868.9761        126.1612       0       0
    2872        875.5607        868.9761        121.7392       0       0
    2873        872.4401        868.9761        117.4441       0       0
    2874        869.1425        868.9761        113.2834       0       0
    2875        865.6734        868.9761        109.2644       0       0
    2876        862.0391        868.9761        105.3943       0       0
    2877        858.2459        868.9761        101.6797       0       0
    2878        854.3005        868.9761        98.12725       0       0
    2879        850.2098        868.9761        94.74312       0       0
    2880         845.981        868.9761        91.53326       0       0
    2881        841.6214        868.9761        88.50329       0       0
    2882        837.1389        868.9761        85.65855       0       0
    2883        832.5411        868.9761        83.00402       0       0
    2884        827.8362        868.9761        80.54435       0       0
    2885        823.0323        868.9761        78.28386       0       0
    2886        818.1381        868.9761        76.22651       0       0
    2887         813.162        868.9761        74.37592       0       0
    2888        808.1129        868.9761        72.73532       0       0
    2889        802.9993        868.9761         71.3076       0       0
    2890        797.8306        868.9761        70.09528       0       0
    2891        792.6155        868.9761        69.10046       0       0
    2892        787.3634        868.9761         68.3249       0       0
    2893        782.0834        868.9761        67.76995       0       0
    2894        776.7849        868.9761        67.43659       0       0
    2895        771.4769        868.9761        67.32541       0       0
    2896         766.169        868.9761         67.4366       0       0
    2897        760.8704        868.9761        67.76996       0       0
    2898        755.5905        868.9761        68.32491       0       0
    2899        750.3383        868.9761        69.10048       0       0
    2900        745.1233        868.9761        70.09531       0       0
    2901        739.9545        868.9761        71.30764       0       0
    2902         734.841        868.9761        72.73536       0       0
    2903        729.7918        868.9761        74.37596       0       0
    2904        724.8157        868.9761        76.22655       0       0
    2905        719.9215        868.9761        78.28391       0       0
    2906        715.1177        868.9761         80.5444       0       0
    2907        710.4128        868.9761        83.00408       0       0
    2908        705.8151        868.9761        85.65863       0       0
    2909        701.3324        868.9761        88.50338       0       0
    2910        696.9729        868.9761        91.53334       0       0
    2911         692.744        868.9761         94.7432       0       0
    2912        688.6534        868.9761        98.12733       0       0
    2913        684.7079        868.9761        101.6798       0       0
    2914        680.9148        868.9761        105.3944       0       0
    2915        677.2805        868.9761        109.2645       0       0
    2916        673.8115        868.9761        113.2835       0       0
    2917        670.5138        868.9761        117.4441       0       0
    2918        667.3932        868.9761        121.7392       0       0
    2919        664.4553        868.9761        126.1613       0       0
    2920         661.705        868.9761        130.7024       0       0
    2921        659.1473        868.9761        135.3548       0       0
    2922        656.7868        868.9761        140.1101       0       0
    2923        654.6274        868.9761        144.9602       0       0
    2924        652.6731        868.9761        149.8964       0       0
    2925        650.9271        868.9761        154.9101       0       0
    2926        649.3926        868.9761        159.9926       0       0
    2927        648.0724        868.9761        165.1348       0       0
    2928        646.9686        868.9761        170.3279       0       0
    2929        646.0831        868.9761        175.5625       0       0
    2930        645.4177        868.9761        180.8297       0       0
    2931        644.9735        868.9761        186.1201       0       0
    2932        644.7512        868.9761        191.4245       0       0
    2933        644.7512        868.9761        196.7335       0       0
    2934        644.9735        868.9761        202.0379       0       0
    2935        645.4178        868.9761        207.3283       0       0
    2936        646.0832        868.9761        212.5955       0       0
    2937        646.9685        868.9761        217.8302       0       0
    2938        648.0723        868.9761        223.0232       0       0
    2939        649.3926        868.9761        228.1655       0       0
    2940        650.9271        868.9761        233.2479       0       0
    2941        652.6731        868.9761        238.2616       0       0
    2942        654.6275        868.9761        243.1979       0       0
    2943        656.7869        868.9761        248.0479       0       0
    2944        659.1475        868.9761        252.8033       0       0
    2945        661.7051        868.9761        257.4556       0       0
    2946        664.4553        868.9761        261.9968       0       0
    2947        667.3932        868.9761        266.4188       0       0
    2948        670.5138        868.9761        270.7139       0       0
    2949        673.8115        868.9761        274.8745       0       0
    2950        677.2805        868.9761        278.8935       0       0
    2951        680.9148        868.9761        282.7636       0       0
    2952        684.7079        868.9761        286.4782       0       0
    2953        688.6533        868.9761        290.0306       0       0
    2954         692.744        868.9761        293.4147       0       0
    2955        696.9728        868.9761        296.6246       0       0
    2956        701.3323        868.9761        299.6545       0       0
    2957        705.8148        868.9761        302.4993       0       0
    2958        710.4126        868.9761        305.1538       0       0
    2959        715.1175        868.9761        307.6135       0       0
    2960        719.9212        868.9761         309.874       0       0
    2961        724.8154        868.9761        311.9313       0       0
    2962        729.7915        868.9761        313.7819       0       0
    2963        734.8407        868.9761        315.4225       0       0
    2964        739.9541        868.9761        316.8503       0       0
    2965        745.1229        868.9761        318.0626       0       0
    2966        750.3379        868.9761        319.0574       0       0
    2967          755.59        868.9761         319.833       0       0
    2968        760.8699        868.9761         320.388       0       0
    2969        766.1685        868.9761        320.7214       0       0
    2970        771.4763        868.9761        320.8326       0       0
    2971        771.4768        874.7559        315.8097       0       0
    2972        776.5743        874.7559        315.7029       0       0
    2973         781.663        874.7559        315.3828       0       0
    2974        786.7337        874.7559        314.8498       0       0
    2975        791.7776        874.7559         314.105       0       0
    2976         796.786        874.7559        313.1496       0       0
    2977          801.75        874.7559        311.9853       0       0
    2978        806.6608        874.7559        310.6142       0       0
    2979        811.5099        874.7559        309.0386       0       0
    2980        816.2889        874.7559        307.2614       0       0
    2981        820.9891        874.7559        305.2856       0       0
    2982        825.6025        874.7559        303.1147       0       0
    2983         830.121        874.7559        300.7525       0       0
    2984        834.5366        874.7559        298.2032       0       0
    2985        838.8416        874.7559        295.4712       0       0
    2986        843.0284        874.7559        292.5613       0       0
    2987        847.0896        874.7559        289.4786       0       0
    2988        851.0182        874.7559        286.2286       0       0
    2989        854.8073        874.7559         282.817       0       0
    2990        858.4501        874.7559        279.2496       0       0
    2991        861.9404        874.7559        275.5328       0       0
    2992         865.272        874.7559        271.6732       0       0
    2993         868.439        874.7559        267.6774       0       0
    2994        871.4359        874.7559        263.5525       0       0
    2995        874.2574        874.7559        259.3057       0       0
    2996        876.8987        874.7559        254.9445       0       0
    2997         879.355        874.7559        250.4765       0       0
    2998        881.6221        874.7559        245.9096       0       0
    2999        883.6959        874.7559        241.2517       0       0
    3000        885.5729        874.7559        236.5111       0       0
    3001        887.2496        874.7559         231.696       0       0
    3002        888.7233        874.7559         226.815       0       0
    3003        889.9913        874.7559        221.8765       0       0
    3004        891.0514        874.7559        216.8893       0       0
    3005        891.9017        874.7559         211.862       0       0
    3006        892.5408        874.7559        206.8035       0       0
    3007        892.9675        874.7559        201.7228       0       0
    3008         893.181        874.7559        196.6286       0       0
    3009         893.181        874.7559        191.5299       0       0
    3010        892.9675        874.7559        186.4357       0       0
    3011        892.5408        874.7559        181.3549       0       0
    3012        891.9018        874.7559        176.2965       0       0
    3013        891.0515        874.7559        171.2692       0       0
    3014        889.9915        874.7559         166.282       0       0
    3015        888.7235        874.7559        161.3435       0       0
    3016        887.2498        874.7559        156.4624       0       0
    3017        885.5731        874.7559        151.6473       0       0
    3018        883.6961        874.7559        146.9067       0       0
    3019        881.6223        874.7559        142.2488       0       0
    3020        879.3552        874.7559        137.6819       0       0
    3021        876.8989        874.7559        133.2139       0       0
    3022        874.2578        874.7559        128.8526       0       0
    3023        871.4362        874.7559        124.6058       0       0
    3024        868.4392        874.7559        120.4809       0       0
    3025        865.2722        874.7559        116.4851       0       0
    3026        861.9406        874.7559        112.6254       0       0
    3027        858.4503        874.7559        108.9086       0       0
    3028        854.8075        874.7559        105.3413       0       0
    3029        851.0184        874.7559        101.9296       0       0
    3030        847.0898        874.7559        98.67955       0       0
    3031        843.0286        874.7559        95.59689       0       0
    3032        838.8418        874.7559        92.68699       0       0
    3033        834.5369        874.7559        89.95498       0       0
    3034        830.1213        874.7559        87.40564       0       0
    3035        825.6028        874.7559        85.04343       0       0
    3036        820.9894        874.7559        82.87253       0       0
    3037        816.2891        874.7559         80.8967       0       0
    3038        811.5102        874.7559        79.11944       0       0
    3039        806.6611        874.7559        77.54386       0       0
    3040        801.7502        874.7559        76.17272       0       0
    3041        796.7862        874.7559        75.00843       0       0
    3042        791.7778        874.7559        74.05304       0       0
    3043        786.7339        874.7559        73.30821       0       0
    3044        781.6631        874.7559        72.77525       0       0
    3045        776.5745        874.7559         72.4551       0       0
    3046        771.4769        874.7559        72.34833       0       0
    3047        766.3794        874.7559        72.45511       0       0
    3048        761.2908        874.7559        72.77526       0       0
    3049          756.22        874.7559        73.30822       0       0
    3050         751.176        874.7559        74.05305       0       0
    3051        746.1677        874.7559        75.00846       0       0
    3052        741.2037        874.7559        76.17275       0       0
    3053        736.2928        874.7559        77.54389       0       0
    3054        731.4437        874.7559        79.11948       0       0
    3055        726.6648        874.7559        80.89674       0       0
    3056        721.9645        874.7559        82.87257       0       0
    3057        717.3511        874.7559        85.04349       0       0
    3058        712.8326        874.7559        87.40569       0       0
    3059        708.4171        874.7559        89.95505       0       0
    3060        704.1121        874.7559        92.68707       0       0
    3061        699.9253        874.7559        95.59695       0       0
    3062         695.864        874.7559        98.67962       0       0
    3063        691.9354        874.7559        101.9296       0       0
    3064        688.1464        874.7559        105.3413       0       0
    3065        684.5035        874.7559        108.9087       0       0
    3066        681.0133        874.7559        112.6255       0       0
    3067        677.6817        874.7559        116.4852       0       0
    3068        674.5147        874.7559         120.481       0       0
    3069        671.5178        874.7559        124.6059       0       0
    3070        668.6963        874.7559        128.8527       0       0
    3071         666.055        874.7559        133.2139       0       0
    3072        663.5987        874.7559        137.6818       0       0
    3073        661.3317        874.7559        142.2488       0       0
    3074        659.2579        874.7559        146.9066       0       0
    3075         657.381        874.7559        151.6472       0       0
    3076        655.7042        874.7559        156.4623       0       0
    3077        654.2305        874.7559        161.3433       0       0
    3078        652.9626        874.7559        166.2818       0       0
    3079        651.9025        874.7559         171.269       0       0
    3080        651.0522        874.7559        176.2963       0       0
    3081        650.4131        874.7559        181.3547       0       0
    3082        649.9865        874.7559        186.4355       0       0
    3083         649.773        874.7559        191.5297       0       0
    3084         649.773        874.7559        196.6283       0       0
    3085        649.9866        874.7559        201.7225       0       0
    3086        650.4132        874.7559        206.8033       0       0
    3087        651.0522        874.7559        211.8617       0       0
    3088        651.9025        874.7559         216.889       0       0
    3089        652.9625        874.7559        221.8762       0       0
    3090        654.2305        874.7559        226.8147       0       0
    3091        655.7042        874.7559        231.6957       0       0
    3092         657.381        874.7559        236.5108       0       0
    3093        659.2579        874.7559        241.2514       0       0
    3094        661.3317        874.7559        245.9092       0       0
    3095        663.5988        874.7559        250.4762       0       0
    3096        666.0551        874.7559        254.9442       0       0
    3097        668.6962        874.7559        259.3054       0       0
    3098        671.5178        874.7559        263.5522       0       0
    3099        674.5147        874.7559         267.677       0       0
    3100        677.6817        874.7559        271.6728       0       0
    3101        681.0133        874.7559        275.5325       0       0
    3102        684.5035        874.7559        279.2493       0       0
    3103        688.1464        874.7559        282.8166       0       0
    3104        691.9354        874.7559        286.2283       0       0
    3105         695.864        874.7559        289.4783       0       0
    3106        699.9252        874.7559        292.5609       0       0
    3107         704.112        874.7559        295.4709       0       0
    3108        708.4169        874.7559        298.2029       0       0
    3109        712.8324        874.7559        300.7522       0       0
    3110        717.3508        874.7559        303.1144       0       0
    3111        721.9642        874.7559        305.2853       0       0
    3112        726.6645        874.7559        307.2611       0       0
    3113        731.4434        874.7559        309.0384       0       0
    3114        736.2925        874.7559         310.614       0       0
    3115        741.2033        874.7559        311.9851       0       0
    3116        746.1672        874.7559        313.1494       0       0
    3117        751.1755        874.7559        314.1049       0       0
    3118        756.2195        874.7559        314.8497       0       0
    3119        761.2902        874.7559        315.3827       0       0
    3120        766.3788        874.7559        315.7029       0       0
    3121        771.4764        874.7559        315.8097       0       0
    3122        771.4768        881.0677        311.4451       0       0
    3123        776.3916        881.0677        311.3422       0       0
    3124        781.2977        881.0677        311.0335       0       0
    3125        786.1867        881.0677        310.5197       0       0
    3126        791.0498        881.0677        309.8015       0       0
    3127        795.8786        881.0677        308.8804       0       0
    3128        800.6646        881.0677        307.7578       0       0
    3129        805.3994        881.0677        306.4359       0       0
    3130        810.0746        881.0677        304.9168       0       0
    3131        814.6821        881.0677        303.2033       0       0
    3132        819.2139        881.0677        301.2983       0       0
    3133        823.6619        881.0677        299.2053       0       0
    3134        828.0184        881.0677        296.9278       0       0
    3135        832.2756        881.0677        294.4698       0       0
    3136        836.4263        881.0677        291.8358       0       0
    3137         840.463        881.0677        289.0303       0       0
    3138        844.3785        881.0677        286.0582       0       0
    3139        848.1663        881.0677        282.9247       0       0
    3140        851.8195        881.0677        279.6353       0       0
    3141        855.3317        881.0677        276.1959       0       0
    3142        858.6969        881.0677        272.6124       0       0
    3143         861.909        881.0677        268.8911       0       0
    3144        864.9625        881.0677        265.0386       0       0
    3145        867.8519        881.0677        261.0616       0       0
    3146        870.5723        881.0677        256.9671       0       0
    3147        873.1189        881.0677        252.7622       0       0
    3148        875.4872        881.0677        248.4544       0       0
    3149         877.673        881.0677        244.0513       0       0
    3150        879.6724        881.0677        239.5604       0       0
    3151        881.4821        881.0677        234.9897       0       0
    3152        883.0987        881.0677        230.3473       0       0
    3153        884.5195        881.0677        225.6413       0       0
    3154        885.7421        881.0677        220.8799       0       0
    3155        886.7642        881.0677        216.0714       0       0
    3156         887.584        881.0677        211.2244       0       0
    3157        888.2001        881.0677        206.3473       0       0
    3158        888.6115        881.0677        201.4487       0       0
    3159        888.8174        881.0677        196.5372       0       0
    3160        888.8174        881.0677        191.6213       0       0
    3161        888.6115        881.0677        186.7098       0       0
    3162        888.2001        881.0677        181.8111       0       0
    3163         887.584        881.0677        176.9341       0       0
    3164        886.7643        881.0677         172.087       0       0
    3165        885.7422        881.0677        167.2786       0       0
    3166        884.5197        881.0677        162.5172       0       0
    3167        883.0989        881.0677        157.8111       0       0
    3168        881.4822        881.0677        153.1687       0       0
    3169        879.6725        881.0677         148.598       0       0
    3170        877.6731        881.0677        144.1072       0       0
    3171        875.4874        881.0677         139.704       0       0
    3172        873.1191        881.0677        135.3961       0       0
    3173        870.5726        881.0677        131.1913       0       0
    3174        867.8522        881.0677        127.0967       0       0
    3175        864.9628        881.0677        123.1197       0       0
    3176        861.9092        881.0677        119.2672       0       0
    3177        858.6971        881.0677        115.5459       0       0
    3178         855.332        881.0677        111.9623       0       0
    3179        851.8197        881.0677        108.5229       0       0
    3180        848.1665        881.0677        105.2335       0       0
    3181        844.3788        881.0677           102.1       0       0
    3182        840.4631        881.0677        99.12789       0       0
    3183        836.4265        881.0677        96.32233       0       0
    3184        832.2759        881.0677        93.68827       0       0
    3185        828.0186        881.0677        91.23034       0       0
    3186        823.6622        881.0677        88.95283       0       0
    3187        819.2142        881.0677        86.85976       0       0
    3188        814.6824        881.0677        84.95477       0       0
    3189        810.0748        881.0677        83.24123       0       0
    3190        805.3995        881.0677        81.72215       0       0
    3191        800.6648        881.0677        80.40017       0       0
    3192        795.8788        881.0677        79.27762       0       0
    3193          791.05        881.0677        78.35648       0       0
    3194        786.1868        881.0677        77.63836       0       0
    3195        781.2979        881.0677        77.12451       0       0
    3196        776.3917        881.0677        76.81584       0       0
    3197        771.4769        881.0677        76.71289       0       0
    3198        766.5621        881.0677        76.81584       0       0
    3199        761.6559        881.0677        77.12451       0       0
    3200         756.767        881.0677        77.63837       0       0
    3201        751.9039        881.0677         78.3565       0       0
    3202        747.0751        881.0677        79.27764       0       0
    3203        742.2891        881.0677        80.40019       0       0
    3204        737.5543        881.0677        81.72218       0       0
    3205        732.8791        881.0677        83.24126       0       0
    3206        728.2715        881.0677        84.95481       0       0
    3207        723.7397        881.0677        86.85979       0       0
    3208        719.2917        881.0677        88.95287       0       0
    3209        714.9353        881.0677        91.23038       0       0
    3210         710.678        881.0677        93.68833       0       0
    3211        706.5273        881.0677         96.3224       0       0
    3212        702.4907        881.0677        99.12795       0       0
    3213        698.5751        881.0677        102.1001       0       0
    3214        694.7874        881.0677        105.2336       0       0
    3215        691.1342        881.0677         108.523       0       0
    3216        687.6219        881.0677        111.9624       0       0
    3217        684.2568        881.0677        115.5459       0       0
    3218        681.0447        881.0677        119.2673       0       0
    3219        677.9912        881.0677        123.1198       0       0
    3220        675.1018        881.0677        127.0968       0       0
    3221        672.3814        881.0677        131.1913       0       0
    3222        669.8348        881.0677        135.3961       0       0
    3223        667.4666        881.0677        139.7039       0       0
    3224        665.2809        881.0677        144.1071       0       0
    3225        663.2814        881.0677         148.598       0       0
    3226        661.4718        881.0677        153.1686       0       0
    3227        659.8552        881.0677         157.811       0       0
    3228        658.4343        881.0677         162.517       0       0
    3229        657.2118        881.0677        167.2784       0       0
    3230        656.1898        881.0677        172.0869       0       0
    3231        655.3699        881.0677        176.9339       0       0
    3232        654.7538        881.0677        181.8109       0       0
    3233        654.3425        881.0677        186.7095       0       0
    3234        654.1366        881.0677        191.6211       0       0
    3235        654.1366        881.0677        196.5369       0       0
    3236        654.3425        881.0677        201.4485       0       0
    3237        654.7538        881.0677        206.3471       0       0
    3238          655.37        881.0677        211.2242       0       0
    3239        656.1897        881.0677        216.0712       0       0
    3240        657.2118        881.0677        220.8796       0       0
    3241        658.4343        881.0677         225.641       0       0
    3242        659.8552        881.0677         230.347       0       0
    3243        661.4718        881.0677        234.9894       0       0
    3244        663.2814        881.0677        239.5601       0       0
    3245        665.2809        881.0677        244.0509       0       0
    3246        667.4667        881.0677        248.4541       0       0
    3247        669.8349        881.0677        252.7619       0       0
    3248        672.3813        881.0677        256.9667       0       0
    3249        675.1017        881.0677        261.0612       0       0
    3250        677.9912        881.0677        265.0382       0       0
    3251        681.0447        881.0677        268.8907       0       0
    3252        684.2568        881.0677        272.6121       0       0
    3253        687.6219        881.0677        276.1956       0       0
    3254        691.1342        881.0677         279.635       0       0
    3255        694.7874        881.0677        282.9243       0       0
    3256        698.5751        881.0677        286.0578       0       0
    3257        702.4907        881.0677        289.0299       0       0
    3258        706.5273        881.0677        291.8355       0       0
    3259        710.6778        881.0677        294.4696       0       0
    3260        714.9351        881.0677        296.9275       0       0
    3261        719.2915        881.0677         299.205       0       0
    3262        723.7395        881.0677        301.2981       0       0
    3263        728.2712        881.0677        303.2031       0       0
    3264        732.8787        881.0677        304.9166       0       0
    3265         737.554        881.0677        306.4357       0       0
    3266        742.2887        881.0677        307.7577       0       0
    3267        747.0747        881.0677        308.8803       0       0
    3268        751.9034        881.0677        309.8014       0       0
    3269        756.7665        881.0677        310.5196       0       0
    3270        761.6555        881.0677        311.0334       0       0
    3271        766.5616        881.0677        311.3421       0       0
    3272        771.4764        881.0677        311.4451       0       0
    3273        771.4768        887.8198         307.797       0       0
    3274        776.2388        887.8198        307.6973       0       0
    3275        780.9925        887.8198        307.3982       0       0
    3276        785.7294        887.8198        306.9004       0       0
    3277        790.4414        887.8198        306.2046       0       0
    3278        795.1201        887.8198         305.312       0       0
    3279        799.7573        887.8198        304.2244       0       0
    3280        804.3449        887.8198        302.9435       0       0
    3281        808.8749        887.8198        301.4717       0       0
    3282        813.3392        887.8198        299.8114       0       0
    3283        817.7301        887.8198        297.9656       0       0
    3284        822.0399        887.8198        295.9377       0       0
    3285        826.2609        887.8198         293.731       0       0
    3286        830.3858        887.8198        291.3494       0       0
    3287        834.4075        887.8198        288.7973       0       0
    3288        838.3187        887.8198        286.0789       0       0
    3289        842.1125        887.8198        283.1992       0       0
    3290        845.7826        887.8198        280.1631       0       0
    3291        849.3222        887.8198         276.976       0       0
    3292        852.7253        887.8198        273.6435       0       0
    3293        855.9858        887.8198        270.1714       0       0
    3294        859.0981        887.8198        266.5658       0       0
    3295        862.0566        887.8198         262.833       0       0
    3296        864.8563        887.8198        258.9796       0       0
    3297        867.4922        887.8198        255.0123       0       0
    3298        869.9596        887.8198        250.9382       0       0
    3299        872.2543        887.8198        246.7643       0       0
    3300        874.3721        887.8198         242.498       0       0
    3301        876.3094        887.8198        238.1467       0       0
    3302        878.0628        887.8198        233.7181       0       0
    3303        879.6292        887.8198          229.22       0       0
    3304        881.0059        887.8198        224.6602       0       0
    3305        882.1904        887.8198        220.0468       0       0
    3306        883.1807        887.8198        215.3878       0       0
    3307        883.9751        887.8198        210.6915       0       0
    3308        884.5721        887.8198         205.966       0       0
    3309        884.9706        887.8198        201.2197       0       0
    3310        885.1701        887.8198        196.4608       0       0
    3311        885.1701        887.8198        191.6977       0       0
    3312        884.9706        887.8198        186.9388       0       0
    3313        884.5721        887.8198        182.1925       0       0
    3314        883.9751        887.8198         177.467       0       0
    3315        883.1808        887.8198        172.7706       0       0
    3316        882.1906        887.8198        168.1116       0       0
    3317         881.006        887.8198        163.4982       0       0
    3318        879.6293        887.8198        158.9384       0       0
    3319        878.0629        887.8198        154.4403       0       0
    3320        876.3096        887.8198        150.0117       0       0
    3321        874.3723        887.8198        145.6604       0       0
    3322        872.2544        887.8198        141.3941       0       0
    3323        869.9598        887.8198        137.2202       0       0
    3324        867.4924        887.8198         133.146       0       0
    3325        864.8566        887.8198        129.1787       0       0
    3326        862.0569        887.8198        125.3253       0       0
    3327        859.0984        887.8198        121.5925       0       0
    3328        855.9861        887.8198        117.9869       0       0
    3329        852.7255        887.8198        114.5148       0       0
    3330        849.3224        887.8198        111.1822       0       0
    3331        845.7828        887.8198        107.9951       0       0
    3332        842.1128        887.8198         104.959       0       0
    3333        838.3188        887.8198        102.0792       0       0
    3334        834.4077        887.8198        99.36089       0       0
    3335        830.3861        887.8198         96.8087       0       0
    3336        826.2612        887.8198        94.42717       0       0
    3337        822.0401        887.8198        92.22045       0       0
    3338        817.7303        887.8198        90.19244       0       0
    3339        813.3395        887.8198        88.34666       0       0
    3340        808.8751        887.8198        86.68639       0       0
    3341        804.3452        887.8198        85.21452       0       0
    3342        799.7576        887.8198        83.93362       0       0
    3343        795.1203        887.8198        82.84598       0       0
    3344        790.4416        887.8198        81.95347       0       0
    3345        785.7296        887.8198        81.25766       0       0
    3346        780.9927        887.8198        80.75979       0       0
    3347         776.239        887.8198        80.46071       0       0
    3348        771.4769        887.8198        80.36096       0       0
    3349        766.7149        887.8198        80.46072       0       0
    3350        761.9612        887.8198        80.75979       0       0
    3351        757.2242        887.8198        81.25767       0       0
    3352        752.5123        887.8198        81.95348       0       0
    3353        747.8336        887.8198        82.84599       0       0
    3354        743.1964        887.8198        83.93365       0       0
    3355        738.6088        887.8198        85.21454       0       0
    3356        734.0788        887.8198        86.68642       0       0
    3357        729.6144        887.8198        88.34669       0       0
    3358        725.2236        887.8198        90.19247       0       0
    3359        720.9138        887.8198        92.22048       0       0
    3360        716.6927        887.8198         94.4272       0       0
    3361        712.5679        887.8198        96.80875       0       0
    3362        708.5462        887.8198        99.36095       0       0
    3363         704.635        887.8198        102.0793       0       0
    3364        700.8411        887.8198        104.9591       0       0
    3365        697.1711        887.8198        107.9952       0       0
    3366        693.6315        887.8198        111.1823       0       0
    3367        690.2284        887.8198        114.5148       0       0
    3368        686.9678        887.8198         117.987       0       0
    3369        683.8556        887.8198        121.5926       0       0
    3370         680.897        887.8198        125.3254       0       0
    3371        678.0974        887.8198        129.1788       0       0
    3372        675.4616        887.8198         133.146       0       0
    3373        672.9941        887.8198        137.2202       0       0
    3374        670.6995        887.8198         141.394       0       0
    3375        668.5817        887.8198        145.6604       0       0
    3376        666.6445        887.8198        150.0116       0       0
    3377        664.8911        887.8198        154.4402       0       0
    3378        663.3247        887.8198        158.9383       0       0
    3379         661.948        887.8198        163.4981       0       0
    3380        660.7635        887.8198        168.1115       0       0
    3381        659.7732        887.8198        172.7704       0       0
    3382        658.9789        887.8198        177.4668       0       0
    3383        658.3819        887.8198        182.1923       0       0
    3384        657.9833        887.8198        186.9386       0       0
    3385        657.7839        887.8198        191.6975       0       0
    3386        657.7839        887.8198        196.4605       0       0
    3387        657.9834        887.8198        201.2194       0       0
    3388         658.382        887.8198        205.9657       0       0
    3389        658.9789        887.8198        210.6912       0       0
    3390        659.7732        887.8198        215.3876       0       0
    3391        660.7635        887.8198        220.0465       0       0
    3392         661.948        887.8198        224.6599       0       0
    3393        663.3247        887.8198        229.2197       0       0
    3394        664.8911        887.8198        233.7178       0       0
    3395        666.6445        887.8198        238.1464       0       0
    3396        668.5818        887.8198        242.4976       0       0
    3397        670.6996        887.8198        246.7639       0       0
    3398        672.9942        887.8198        250.9379       0       0
    3399        675.4615        887.8198         255.012       0       0
    3400        678.0974        887.8198        258.9792       0       0
    3401         680.897        887.8198        262.8326       0       0
    3402        683.8556        887.8198        266.5654       0       0
    3403        686.9678        887.8198         270.171       0       0
    3404        690.2284        887.8198        273.6431       0       0
    3405        693.6314        887.8198        276.9757       0       0
    3406        697.1711        887.8198        280.1628       0       0
    3407        700.8411        887.8198        283.1989       0       0
    3408        704.6349        887.8198        286.0786       0       0
    3409        708.5461        887.8198         288.797       0       0
    3410        712.5676        887.8198        291.3492       0       0
    3411        716.6925        887.8198        293.7307       0       0
    3412        720.9136        887.8198        295.9374       0       0
    3413        725.2233        887.8198        297.9654       0       0
    3414        729.6141        887.8198        299.8112       0       0
    3415        734.0785        887.8198        301.4715       0       0
    3416        738.6084        887.8198        302.9433       0       0
    3417         743.196        887.8198        304.2243       0       0
    3418        747.8332        887.8198        305.3119       0       0
    3419        752.5118        887.8198        306.2044       0       0
    3420        757.2238        887.8198        306.9002       0       0
    3421        761.9608        887.8198        307.3981       0       0
    3422        766.7144        887.8198        307.6973       0       0
    3423        771.4764        887.8198         307.797       0       0
    3424        771.4768        894.9169        304.9195       0       0
    3425        776.1183        894.9169        304.8223       0       0
    3426        780.7517        894.9169        304.5308       0       0
    3427        785.3688        894.9169        304.0456       0       0
    3428        789.9615        894.9169        303.3673       0       0
    3429        794.5219        894.9169        302.4974       0       0
    3430        799.0417        894.9169        301.4373       0       0
    3431        803.5132        894.9169        300.1888       0       0
    3432        807.9286        894.9169        298.7542       0       0
    3433        812.2799        894.9169         297.136       0       0
    3434        816.5597        894.9169        295.3369       0       0
    3435        820.7604        894.9169        293.3603       0       0
    3436        824.8746        894.9169        291.2094       0       0
    3437        828.8952        894.9169        288.8881       0       0
    3438        832.8151        894.9169        286.4005       0       0
    3439        836.6273        894.9169         283.751       0       0
    3440        840.3252        894.9169        280.9441       0       0
    3441        843.9023        894.9169        277.9849       0       0
    3442        847.3524        894.9169        274.8784       0       0
    3443        850.6694        894.9169        271.6302       0       0
    3444        853.8474        894.9169        268.2459       0       0
    3445         856.881        894.9169        264.7316       0       0
    3446        859.7646        894.9169        261.0933       0       0
    3447        862.4935        894.9169        257.3374       0       0
    3448        865.0626        894.9169        253.4705       0       0
    3449        867.4676        894.9169        249.4994       0       0
    3450        869.7042        894.9169        245.4312       0       0
    3451        871.7684        894.9169        241.2728       0       0
    3452        873.6567        894.9169        237.0316       0       0
    3453        875.3657        894.9169        232.7151       0       0
    3454        876.8925        894.9169        228.3308       0       0
    3455        878.2344        894.9169        223.8864       0       0
    3456        879.3889        894.9169        219.3898       0       0
    3457        880.3541        894.9169        214.8487       0       0
    3458        881.1285        894.9169        210.2711       0       0
    3459        881.7103        894.9169        205.6652       0       0
    3460        882.0988        894.9169         201.039       0       0
    3461        882.2932        894.9169        196.4005       0       0
    3462        882.2932        894.9169         191.758       0       0
    3463        882.0988        894.9169        187.1195       0       0
    3464        881.7103        894.9169        182.4933       0       0
    3465        881.1285        894.9169        177.8873       0       0
    3466        880.3543        894.9169        173.3098       0       0
    3467         879.389        894.9169        168.7687       0       0
    3468        878.2345        894.9169         164.272       0       0
    3469        876.8927        894.9169        159.8276       0       0
    3470        875.3659        894.9169        155.4433       0       0
    3471        873.6569        894.9169        151.1268       0       0
    3472        871.7686        894.9169        146.8856       0       0
    3473        869.7043        894.9169        142.7272       0       0
    3474        867.4678        894.9169        138.6589       0       0
    3475        865.0629        894.9169        134.6879       0       0
    3476        862.4938        894.9169         130.821       0       0
    3477        859.7649        894.9169        127.0651       0       0
    3478        856.8812        894.9169        123.4267       0       0
    3479        853.8477        894.9169        119.9123       0       0
    3480        850.6696        894.9169        116.5281       0       0
    3481        847.3527        894.9169        113.2798       0       0
    3482        843.9026        894.9169        110.1734       0       0
    3483        840.3254        894.9169        107.2141       0       0
    3484        836.6275        894.9169        104.4072       0       0
    3485        832.8152        894.9169        101.7576       0       0
    3486        828.8955        894.9169        99.27002       0       0
    3487        824.8749        894.9169        96.94875       0       0
    3488        820.7607        894.9169        94.79787       0       0
    3489          816.56        894.9169        92.82117       0       0
    3490        812.2802        894.9169        91.02209       0       0
    3491        807.9288        894.9169        89.40383       0       0
    3492        803.5135        894.9169        87.96921       0       0
    3493        799.0419        894.9169        86.72073       0       0
    3494         794.522        894.9169         85.6606       0       0
    3495        789.9617        894.9169        84.79067       0       0
    3496         785.369        894.9169        84.11247       0       0
    3497        780.7519        894.9169         83.6272       0       0
    3498        776.1185        894.9169        83.33569       0       0
    3499        771.4769        894.9169        83.23846       0       0
    3500        766.8354        894.9169        83.33569       0       0
    3501         762.202        894.9169         83.6272       0       0
    3502        757.5849        894.9169        84.11248       0       0
    3503        752.9922        894.9169        84.79068       0       0
    3504        748.4319        894.9169        85.66061       0       0
    3505         743.912        894.9169        86.72075       0       0
    3506        739.4404        894.9169        87.96922       0       0
    3507        735.0251        894.9169        89.40385       0       0
    3508        730.6738        894.9169        91.02213       0       0
    3509         726.394        894.9169        92.82119       0       0
    3510        722.1932        894.9169         94.7979       0       0
    3511         718.079        894.9169        96.94878       0       0
    3512        714.0585        894.9169        99.27006       0       0
    3513        710.1386        894.9169        101.7577       0       0
    3514        706.3264        894.9169        104.4072       0       0
    3515        702.6285        894.9169        107.2141       0       0
    3516        699.0513        894.9169        110.1734       0       0
    3517        695.6013        894.9169        113.2799       0       0
    3518        692.2843        894.9169        116.5281       0       0
    3519        689.1063        894.9169        119.9124       0       0
    3520        686.0728        894.9169        123.4268       0       0
    3521         683.189        894.9169        127.0651       0       0
    3522        680.4603        894.9169         130.821       0       0
    3523        677.8911        894.9169        134.6879       0       0
    3524        675.4861        894.9169        138.6589       0       0
    3525        673.2496        894.9169        142.7272       0       0
    3526        671.1854        894.9169        146.8855       0       0
    3527        669.2971        894.9169        151.1267       0       0
    3528        667.5881        894.9169        155.4432       0       0
    3529        666.0613        894.9169        159.8275       0       0
    3530        664.7195        894.9169        164.2719       0       0
    3531         663.565        894.9169        168.7685       0       0
    3532        662.5997        894.9169        173.3096       0       0
    3533        661.8255        894.9169        177.8871       0       0
    3534        661.2437        894.9169         182.493       0       0
    3535        660.8552        894.9169        187.1193       0       0
    3536        660.6608        894.9169        191.7577       0       0
    3537        660.6608        894.9169        196.4003       0       0
    3538        660.8552        894.9169        201.0387       0       0
    3539        661.2437        894.9169        205.6649       0       0
    3540        661.8256        894.9169        210.2709       0       0
    3541        662.5997        894.9169        214.8484       0       0
    3542        663.5649        894.9169        219.3894       0       0
    3543        664.7195        894.9169        223.8861       0       0
    3544        666.0613        894.9169        228.3305       0       0
    3545        667.5881        894.9169        232.7148       0       0
    3546        669.2971        894.9169        237.0313       0       0
    3547        671.1854        894.9169        241.2724       0       0
    3548        673.2496        894.9169        245.4308       0       0
    3549        675.4862        894.9169        249.4991       0       0
    3550        677.8911        894.9169        253.4702       0       0
    3551        680.4602        894.9169         257.337       0       0
    3552         683.189        894.9169        261.0929       0       0
    3553        686.0728        894.9169        264.7312       0       0
    3554        689.1063        894.9169        268.2456       0       0
    3555        692.2842        894.9169        271.6299       0       0
    3556        695.6012        894.9169        274.8781       0       0
    3557        699.0513        894.9169        277.9845       0       0
    3558        702.6284        894.9169        280.9438       0       0
    3559        706.3263        894.9169        283.7506       0       0
    3560        710.1385        894.9169        286.4002       0       0
    3561        714.0582        894.9169        288.8878       0       0
    3562        718.0788        894.9169        291.2091       0       0
    3563         722.193        894.9169          293.36       0       0
    3564        726.3937        894.9169        295.3367       0       0
    3565        730.6735        894.9169        297.1357       0       0
    3566        735.0248        894.9169         298.754       0       0
    3567        739.4401        894.9169        300.1887       0       0
    3568        743.9116        894.9169        301.4372       0       0
    3569        748.4315        894.9169        302.4973       0       0
    3570        752.9918        894.9169        303.3672       0       0
    3571        757.5845        894.9169        304.0454       0       0
    3572        762.2015        894.9169        304.5307       0       0
    3573        766.8349        894.9169        304.8223       0       0
    3574        771.4764        894.9169        304.9195       0       0
    3575        771.4768        902.2824        302.8381       0       0
    3576        776.0312        902.2824        302.7427       0       0
    3577        780.5775        902.2824        302.4567       0       0
    3578        785.1079        902.2824        301.9806       0       0
    3579        789.6144        902.2824        301.3151       0       0
    3580        794.0891        902.2824        300.4615       0       0
    3581        798.5241        902.2824        299.4213       0       0
    3582        802.9117        902.2824        298.1963       0       0
    3583        807.2441        902.2824        296.7886       0       0
    3584        811.5137        902.2824        295.2007       0       0
    3585        815.7131        902.2824        293.4355       0       0
    3586         819.835        902.2824        291.4959       0       0
    3587        823.8719        902.2824        289.3854       0       0
    3588         827.817        902.2824        287.1078       0       0
    3589        831.6633        902.2824        284.6669       0       0
    3590        835.4039        902.2824        282.0671       0       0
    3591        839.0323        902.2824         279.313       0       0
    3592        842.5424        902.2824        276.4092       0       0
    3593        845.9276        902.2824        273.3611       0       0
    3594        849.1823        902.2824        270.1739       0       0
    3595        852.3007        902.2824        266.8532       0       0
    3596        855.2772        902.2824        263.4048       0       0
    3597        858.1068        902.2824        259.8348       0       0
    3598        860.7844        902.2824        256.1495       0       0
    3599        863.3052        902.2824        252.3552       0       0
    3600         865.665        902.2824        248.4587       0       0
    3601        867.8597        902.2824        244.4669       0       0
    3602        869.8851        902.2824        240.3866       0       0
    3603         871.738        902.2824        236.2251       0       0
    3604        873.4149        902.2824        231.9896       0       0
    3605         874.913        902.2824        227.6876       0       0
    3606        876.2297        902.2824        223.3267       0       0
    3607        877.3625        902.2824        218.9145       0       0
    3608        878.3096        902.2824        214.4586       0       0
    3609        879.0694        902.2824        209.9671       0       0
    3610        879.6403        902.2824        205.4476       0       0
    3611        880.0215        902.2824        200.9083       0       0
    3612        880.2123        902.2824        196.3569       0       0
    3613        880.2123        902.2824        191.8016       0       0
    3614        880.0215        902.2824        187.2502       0       0
    3615        879.6403        902.2824        182.7108       0       0
    3616        879.0694        902.2824        178.1914       0       0
    3617        878.3098        902.2824        173.6998       0       0
    3618        877.3627        902.2824         169.244       0       0
    3619        876.2298        902.2824        164.8318       0       0
    3620        874.9131        902.2824        160.4708       0       0
    3621         873.415        902.2824        156.1688       0       0
    3622        871.7382        902.2824        151.9334       0       0
    3623        869.8853        902.2824        147.7718       0       0
    3624        867.8598        902.2824        143.6915       0       0
    3625        865.6653        902.2824        139.6996       0       0
    3626        863.3055        902.2824        135.8031       0       0
    3627        860.7846        902.2824        132.0089       0       0
    3628        858.1071        902.2824        128.3235       0       0
    3629        855.2775        902.2824        124.7534       0       0
    3630        852.3009        902.2824        121.3051       0       0
    3631        849.1826        902.2824        117.9843       0       0
    3632        845.9279        902.2824        114.7971       0       0
    3633        842.5425        902.2824         111.749       0       0
    3634        839.0326        902.2824        108.8453       0       0
    3635        835.4041        902.2824        106.0911       0       0
    3636        831.6635        902.2824        103.4913       0       0
    3637        827.8173        902.2824        101.0504       0       0
    3638        823.8722        902.2824        98.77268       0       0
    3639        819.8352        902.2824        96.66219       0       0
    3640        815.7134        902.2824        94.72261       0       0
    3641         811.514        902.2824        92.95733       0       0
    3642        807.2443        902.2824        91.36945       0       0
    3643        802.9119        902.2824        89.96176       0       0
    3644        798.5243        902.2824        88.73673       0       0
    3645        794.0893        902.2824         87.6965       0       0
    3646        789.6146        902.2824        86.84292       0       0
    3647        785.1081        902.2824        86.17745       0       0
    3648        780.5777        902.2824        85.70129       0       0
    3649        776.0313        902.2824        85.41525       0       0
    3650        771.4769        902.2824        85.31985       0       0
    3651        766.9226        902.2824        85.41526       0       0
    3652        762.3762        902.2824        85.70129       0       0
    3653        757.8458        902.2824        86.17746       0       0
    3654        753.3393        902.2824        86.84293       0       0
    3655        748.8646        902.2824        87.69652       0       0
    3656        744.4296        902.2824        88.73675       0       0
    3657        740.0421        902.2824        89.96178       0       0
    3658        735.7097        902.2824        91.36947       0       0
    3659        731.4399        902.2824        92.95735       0       0
    3660        727.2405        902.2824        94.72263       0       0
    3661        723.1187        902.2824        96.66222       0       0
    3662        719.0818        902.2824        98.77271       0       0
    3663        715.1367        902.2824        101.0504       0       0
    3664        711.2904        902.2824        103.4913       0       0
    3665        707.5498        902.2824        106.0911       0       0
    3666        703.9213        902.2824        108.8453       0       0
    3667        700.4114        902.2824         111.749       0       0
    3668        697.0261        902.2824        114.7971       0       0
    3669        693.7714        902.2824        117.9844       0       0
    3670         690.653        902.2824        121.3051       0       0
    3671        687.6765        902.2824        124.7535       0       0
    3672        684.8469        902.2824        128.3235       0       0
    3673        682.1694        902.2824        132.0089       0       0
    3674        679.6485        902.2824        135.8031       0       0
    3675        677.2887        902.2824        139.6996       0       0
    3676        675.0941        902.2824        143.6915       0       0
    3677        673.0687        902.2824        147.7717       0       0
    3678        671.2159        902.2824        151.9333       0       0
    3679        669.5389        902.2824        156.1687       0       0
    3680        668.0409        902.2824        160.4707       0       0
    3681        666.7242        902.2824        164.8316       0       0
    3682        665.5914        902.2824        169.2438       0       0
    3683        664.6443        902.2824        173.6996       0       0
    3684        663.8845        902.2824        178.1912       0       0
    3685        663.3136        902.2824        182.7106       0       0
    3686        662.9324        902.2824          187.25       0       0
    3687        662.7417        902.2824        191.8013       0       0
    3688        662.7417        902.2824        196.3567       0       0
    3689        662.9325        902.2824         200.908       0       0
    3690        663.3137        902.2824        205.4474       0       0
    3691        663.8846        902.2824        209.9668       0       0
    3692        664.6442        902.2824        214.4584       0       0
    3693        665.5914        902.2824        218.9142       0       0
    3694        666.7242        902.2824        223.3264       0       0
    3695        668.0409        902.2824        227.6873       0       0
    3696        669.5389        902.2824        231.9893       0       0
    3697        671.2159        902.2824        236.2247       0       0
    3698        673.0687        902.2824        240.3862       0       0
    3699        675.0942        902.2824        244.4665       0       0
    3700        677.2888        902.2824        248.4584       0       0
    3701        679.6485        902.2824        252.3549       0       0
    3702        682.1694        902.2824        256.1491       0       0
    3703        684.8469        902.2824        259.8345       0       0
    3704        687.6765        902.2824        263.4045       0       0
    3705         690.653        902.2824        266.8529       0       0
    3706        693.7714        902.2824        270.1736       0       0
    3707         697.026        902.2824        273.3608       0       0
    3708        700.4113        902.2824        276.4089       0       0
    3709        703.9213        902.2824        279.3126       0       0
    3710        707.5497        902.2824        282.0668       0       0
    3711        711.2903        902.2824        284.6666       0       0
    3712        715.1365        902.2824        287.1075       0       0
    3713        719.0815        902.2824        289.3852       0       0
    3714        723.1185        902.2824        291.4956       0       0
    3715        727.2402        902.2824        293.4352       0       0
    3716        731.4396        902.2824        295.2005       0       0
    3717        735.7093        902.2824        296.7884       0       0
    3718        740.0417        902.2824        298.1961       0       0
    3719        744.4292        902.2824        299.4212       0       0
    3720        748.8642        902.2824        300.4614       0       0
    3721        753.3389        902.2824         301.315       0       0
    3722        757.8453        902.2824        301.9805       0       0
    3723        762.3757        902.2824        302.4566       0       0
    3724        766.9221        902.2824        302.7427       0       0
    3725        771.4764        902.2824        302.8381       0       0
    3726        771.4768        909.8431         301.573       0       0
    3727        775.9782        909.8431        301.4787       0       0
    3728        780.4717        909.8431         301.196       0       0
    3729        784.9494        909.8431        300.7254       0       0
    3730        789.4034        909.8431        300.0677       0       0
    3731         793.826        909.8431         299.224       0       0
    3732        798.2095        909.8431        298.1959       0       0
    3733         802.546        909.8431        296.9852       0       0
    3734         806.828        909.8431        295.5938       0       0
    3735         811.048        909.8431        294.0244       0       0
    3736        815.1985        909.8431        292.2797       0       0
    3737        819.2724        909.8431        290.3627       0       0
    3738        823.2625        909.8431        288.2768       0       0
    3739        827.1616        909.8431        286.0256       0       0
    3740        830.9631        909.8431        283.6131       0       0
    3741        834.6603        909.8431        281.0436       0       0
    3742        838.2465        909.8431        278.3215       0       0
    3743        841.7156        909.8431        275.4515       0       0
    3744        845.0616        909.8431        272.4389       0       0
    3745        848.2784        909.8431        269.2888       0       0
    3746        851.3605        909.8431        266.0067       0       0
    3747        854.3024        909.8431        262.5984       0       0
    3748        857.0991        909.8431        259.0699       0       0
    3749        859.7455        909.8431        255.4275       0       0
    3750        862.2371        909.8431        251.6773       0       0
    3751        864.5694        909.8431        247.8262       0       0
    3752        866.7385        909.8431        243.8807       0       0
    3753        868.7404        909.8431        239.8479       0       0
    3754        870.5717        909.8431        235.7348       0       0
    3755        872.2291        909.8431        231.5486       0       0
    3756        873.7098        909.8431        227.2967       0       0
    3757        875.0111        909.8431        222.9865       0       0
    3758        876.1308        909.8431        218.6256       0       0
    3759        877.0669        909.8431        214.2216       0       0
    3760        877.8178        909.8431        209.7823       0       0
    3761        878.3821        909.8431        205.3154       0       0
    3762        878.7589        909.8431        200.8288       0       0
    3763        878.9474        909.8431        196.3304       0       0
    3764        878.9474        909.8431         191.828       0       0
    3765        878.7589        909.8431        187.3296       0       0
    3766        878.3821        909.8431         182.843       0       0
    3767        877.8179        909.8431        178.3762       0       0
    3768         877.067        909.8431        173.9369       0       0
    3769         876.131        909.8431        169.5329       0       0
    3770        875.0113        909.8431         165.172       0       0
    3771          873.71        909.8431        160.8618       0       0
    3772        872.2292        909.8431        156.6098       0       0
    3773        870.5718        909.8431        152.4236       0       0
    3774        868.7405        909.8431        148.3105       0       0
    3775        866.7386        909.8431        144.2777       0       0
    3776        864.5696        909.8431        140.3322       0       0
    3777        862.2373        909.8431         136.481       0       0
    3778        859.7457        909.8431        132.7309       0       0
    3779        857.0993        909.8431        129.0884       0       0
    3780        854.3027        909.8431        125.5599       0       0
    3781        851.3607        909.8431        122.1516       0       0
    3782        848.2786        909.8431        118.8695       0       0
    3783        845.0618        909.8431        115.7194       0       0
    3784        841.7159        909.8431        112.7067       0       0
    3785        838.2467        909.8431        109.8367       0       0
    3786        834.6605        909.8431        107.1146       0       0
    3787        830.9633        909.8431         104.545       0       0
    3788        827.1619        909.8431        102.1325       0       0
    3789        823.2627        909.8431        99.88133       0       0
    3790        819.2727        909.8431        97.79539       0       0
    3791        815.1989        909.8431        95.87837       0       0
    3792        811.0482        909.8431        94.13362       0       0
    3793        806.8282        909.8431        92.56422       0       0
    3794        802.5462        909.8431         91.1729       0       0
    3795        798.2097        909.8431        89.96212       0       0
    3796        793.8263        909.8431          88.934       0       0
    3797        789.4036        909.8431        88.09033       0       0
    3798        784.9496        909.8431        87.43262       0       0
    3799        780.4719        909.8431        86.96199       0       0
    3800        775.9783        909.8431        86.67928       0       0
    3801        771.4769        909.8431        86.58499       0       0
    3802        766.9756        909.8431        86.67928       0       0
    3803        762.4821        909.8431        86.96199       0       0
    3804        758.0043        909.8431        87.43262       0       0
    3805        753.5503        909.8431        88.09035       0       0
    3806        749.1276        909.8431        88.93401       0       0
    3807        744.7443        909.8431        89.96214       0       0
    3808        740.4077        909.8431        91.17292       0       0
    3809        736.1257        909.8431        92.56423       0       0
    3810        731.9057        909.8431        94.13364       0       0
    3811        727.7551        909.8431        95.87839       0       0
    3812        723.6813        909.8431        97.79541       0       0
    3813        719.6912        909.8431        99.88136       0       0
    3814        715.7921        909.8431        102.1326       0       0
    3815        711.9905        909.8431        104.5451       0       0
    3816        708.2934        909.8431        107.1146       0       0
    3817        704.7072        909.8431        109.8368       0       0
    3818         701.238        909.8431        112.7067       0       0
    3819        697.8921        909.8431        115.7194       0       0
    3820        694.6753        909.8431        118.8695       0       0
    3821        691.5932        909.8431        122.1516       0       0
    3822        688.6513        909.8431        125.5599       0       0
    3823        685.8547        909.8431        129.0884       0       0
    3824        683.2083        909.8431        132.7309       0       0
    3825        680.7167        909.8431         136.481       0       0
    3826        678.3843        909.8431        140.3322       0       0
    3827        676.2153        909.8431        144.2776       0       0
    3828        674.2134        909.8431        148.3104       0       0
    3829        672.3821        909.8431        152.4235       0       0
    3830        670.7247        909.8431        156.6097       0       0
    3831        669.2441        909.8431        160.8616       0       0
    3832        667.9427        909.8431        165.1718       0       0
    3833        666.8231        909.8431        169.5327       0       0
    3834         665.887        909.8431        173.9367       0       0
    3835        665.1361        909.8431         178.376       0       0
    3836        664.5718        909.8431        182.8428       0       0
    3837        664.1951        909.8431        187.3294       0       0
    3838        664.0066        909.8431        191.8278       0       0
    3839        664.0066        909.8431        196.3302       0       0
    3840        664.1951        909.8431        200.8286       0       0
    3841        664.5719        909.8431        205.3151       0       0
    3842        665.1362        909.8431         209.782       0       0
    3843         665.887        909.8431        214.2213       0       0
    3844        666.8231        909.8431        218.6253       0       0
    3845        667.9427        909.8431        222.9862       0       0
    3846        669.2441        909.8431        227.2964       0       0
    3847        670.7247        909.8431        231.5483       0       0
    3848        672.3822        909.8431        235.7345       0       0
    3849        674.2134        909.8431        239.8476       0       0
    3850        676.2153        909.8431        243.8804       0       0
    3851        678.3844        909.8431        247.8258       0       0
    3852        680.7167        909.8431         251.677       0       0
    3853        683.2083        909.8431        255.4271       0       0
    3854        685.8546        909.8431        259.0696       0       0
    3855        688.6512        909.8431        262.5981       0       0
    3856        691.5932        909.8431        266.0063       0       0
    3857        694.6753        909.8431        269.2884       0       0
    3858        697.8921        909.8431        272.4385       0       0
    3859         701.238        909.8431        275.4512       0       0
    3860        704.7071        909.8431        278.3211       0       0
    3861        708.2933        909.8431        281.0432       0       0
    3862        711.9904        909.8431        283.6128       0       0
    3863        715.7919        909.8431        286.0253       0       0
    3864         719.691        909.8431        288.2765       0       0
    3865         723.681        909.8431        290.3625       0       0
    3866        727.7548        909.8431        292.2795       0       0
    3867        731.9054        909.8431        294.0242       0       0
    3868        736.1254        909.8431        295.5936       0       0
    3869        740.4073        909.8431         296.985       0       0
    3870        744.7438        909.8431        298.1958       0       0
    3871        749.1273        909.8431        299.2239       0       0
    3872        753.5499        909.8431        300.0676       0       0
    3873        758.0039        909.8431        300.7253       0       0
    3874        762.4816        909.8431         301.196       0       0
    3875         766.975        909.8431        301.4787       0       0
    3876        771.4764        909.8431         301.573       0       0
    3877        771.4768        917.5197        301.1539       0       0
    3878        775.9606        917.5197          301.06       0       0
    3879        780.4366        917.5197        300.7784       0       0
    3880        784.8969        917.5197        300.3096       0       0
    3881        789.3336        917.5197        299.6544       0       0
    3882         793.739        917.5197        298.8141       0       0
    3883        798.1052        917.5197          297.79       0       0
    3884        802.4249        917.5197         296.584       0       0
    3885        806.6902        917.5197        295.1981       0       0
    3886        810.8937        917.5197        293.6348       0       0
    3887        815.0281        917.5197        291.8969       0       0
    3888        819.0861        917.5197        289.9873       0       0
    3889        823.0605        917.5197        287.9095       0       0
    3890        826.9445        917.5197        285.6671       0       0
    3891        830.7312        917.5197        283.2641       0       0
    3892        834.4139        917.5197        280.7045       0       0
    3893        837.9862        917.5197         277.993       0       0
    3894        841.4418        917.5197        275.1343       0       0
    3895        844.7747        917.5197        272.1334       0       0
    3896        847.9789        917.5197        268.9955       0       0
    3897         851.049        917.5197        265.7262       0       0
    3898        853.9795        917.5197        262.3313       0       0
    3899        856.7652        917.5197        258.8166       0       0
    3900        859.4013        917.5197        255.1883       0       0
    3901        861.8832        917.5197        251.4528       0       0
    3902        864.2064        917.5197        247.6166       0       0
    3903        866.3671        917.5197        243.6866       0       0
    3904        868.3612        917.5197        239.6695       0       0
    3905        870.1853        917.5197        235.5724       0       0
    3906        871.8363        917.5197        231.4025       0       0
    3907        873.3112        917.5197        227.1672       0       0
    3908        874.6075        917.5197        222.8738       0       0
    3909        875.7228        917.5197        218.5299       0       0
    3910        876.6552        917.5197        214.1431       0       0
    3911        877.4032        917.5197        209.7211       0       0
    3912        877.9653        917.5197        205.2716       0       0
    3913        878.3406        917.5197        200.8025       0       0
    3914        878.5284        917.5197        196.3216       0       0
    3915        878.5284        917.5197        191.8368       0       0
    3916        878.3406        917.5197        187.3559       0       0
    3917        877.9653        917.5197        182.8869       0       0
    3918        877.4033        917.5197        178.4374       0       0
    3919        876.6554        917.5197        174.0154       0       0
    3920         875.723        917.5197        169.6286       0       0
    3921        874.6076        917.5197        165.2847       0       0
    3922        873.3113        917.5197        160.9913       0       0
    3923        871.8364        917.5197        156.7559       0       0
    3924        870.1855        917.5197         152.586       0       0
    3925        868.3613        917.5197        148.4889       0       0
    3926        866.3672        917.5197        144.4718       0       0
    3927        864.2067        917.5197        140.5417       0       0
    3928        861.8835        917.5197        136.7056       0       0
    3929        859.4016        917.5197        132.9701       0       0
    3930        856.7655        917.5197        129.3418       0       0
    3931        853.9797        917.5197         125.827       0       0
    3932        851.0493        917.5197         122.432       0       0
    3933        847.9792        917.5197        119.1627       0       0
    3934        844.7749        917.5197        116.0248       0       0
    3935         841.442        917.5197        113.0239       0       0
    3936        837.9865        917.5197        110.1652       0       0
    3937        834.4141        917.5197        107.4537       0       0
    3938        830.7314        917.5197        104.8941       0       0
    3939        826.9448        917.5197         102.491       0       0
    3940        823.0608        917.5197        100.2486       0       0
    3941        819.0864        917.5197        98.17078       0       0
    3942        815.0284        917.5197        96.26124       0       0
    3943         810.894        917.5197        94.52329       0       0
    3944        806.6904        917.5197           92.96       0       0
    3945        802.4251        917.5197        91.57411       0       0
    3946        798.1055        917.5197        90.36805       0       0
    3947        793.7391        917.5197        89.34393       0       0
    3948        789.3337        917.5197        88.50356       0       0
    3949         784.897        917.5197        87.84841       0       0
    3950        780.4368        917.5197        87.37962       0       0
    3951        775.9608        917.5197        87.09801       0       0
    3952         771.477        917.5197        87.00409       0       0
    3953        766.9931        917.5197        87.09801       0       0
    3954        762.5172        917.5197        87.37962       0       0
    3955        758.0569        917.5197        87.84841       0       0
    3956        753.6202        917.5197        88.50357       0       0
    3957        749.2148        917.5197        89.34395       0       0
    3958        744.8484        917.5197        90.36806       0       0
    3959        740.5289        917.5197        91.57413       0       0
    3960        736.2635        917.5197        92.96002       0       0
    3961          732.06        917.5197        94.52331       0       0
    3962        727.9256        917.5197        96.26125       0       0
    3963        723.8676        917.5197        98.17081       0       0
    3964        719.8931        917.5197        100.2486       0       0
    3965        716.0092        917.5197         102.491       0       0
    3966        712.2225        917.5197        104.8941       0       0
    3967        708.5397        917.5197        107.4537       0       0
    3968        704.9675        917.5197        110.1652       0       0
    3969        701.5118        917.5197        113.0239       0       0
    3970         698.179        917.5197        116.0249       0       0
    3971        694.9747        917.5197        119.1628       0       0
    3972        691.9047        917.5197        122.4321       0       0
    3973        688.9742        917.5197        125.8271       0       0
    3974        686.1885        917.5197        129.3418       0       0
    3975        683.5524        917.5197        132.9701       0       0
    3976        681.0706        917.5197        136.7056       0       0
    3977        678.7473        917.5197        140.5417       0       0
    3978        676.5867        917.5197        144.4718       0       0
    3979        674.5927        917.5197        148.4889       0       0
    3980        672.7685        917.5197        152.5859       0       0
    3981        671.1176        917.5197        156.7558       0       0
    3982        669.6427        917.5197        160.9911       0       0
    3983        668.3464        917.5197        165.2845       0       0
    3984        667.2311        917.5197        169.6284       0       0
    3985        666.2987        917.5197        174.0152       0       0
    3986        665.5507        917.5197        178.4372       0       0
    3987        664.9886        917.5197        182.8867       0       0
    3988        664.6133        917.5197        187.3557       0       0
    3989        664.4255        917.5197        191.8366       0       0
    3990        664.4255        917.5197        196.3214       0       0
    3991        664.6134        917.5197        200.8023       0       0
    3992        664.9886        917.5197        205.2713       0       0
    3993        665.5508        917.5197        209.7208       0       0
    3994        666.2986        917.5197        214.1428       0       0
    3995        667.2311        917.5197        218.5296       0       0
    3996        668.3464        917.5197        222.8735       0       0
    3997        669.6426        917.5197        227.1669       0       0
    3998        671.1176        917.5197        231.4022       0       0
    3999        672.7686        917.5197        235.5721       0       0
    4000        674.5927        917.5197        239.6691       0       0
    4001        676.5868        917.5197        243.6862       0       0
    4002        678.7473        917.5197        247.6163       0       0
    4003        681.0705        917.5197        251.4524       0       0
    4004        683.5524        917.5197        255.1879       0       0
    4005        686.1885        917.5197        258.8162       0       0
    4006        688.9742        917.5197        262.3309       0       0
    4007        691.9047        917.5197        265.7259       0       0
    4008        694.9747        917.5197        268.9952       0       0
    4009         698.179        917.5197        272.1331       0       0
    4010        701.5118        917.5197         275.134       0       0
    4011        704.9674        917.5197        277.9927       0       0
    4012        708.5397        917.5197        280.7042       0       0
    4013        712.2224        917.5197        283.2638       0       0
    4014         716.009        917.5197        285.6668       0       0
    4015        719.8929        917.5197        287.9093       0       0
    4016        723.8673        917.5197        289.9871       0       0
    4017        727.9253        917.5197        291.8966       0       0
    4018        732.0597        917.5197        293.6346       0       0
    4019        736.2632        917.5197        295.1979       0       0
    4020        740.5285        917.5197        296.5837       0       0
    4021        744.8481        917.5197        297.7898       0       0
    4022        749.2144        917.5197         298.814       0       0
    4023        753.6198        917.5197        299.6543       0       0
    4024        758.0564        917.5197        300.3095       0       0
    4025        762.5167        917.5197        300.7783       0       0
    4026        766.9926        917.5197          301.06       0       0
    4027        771.4764        917.5197        301.1539       0       0
    4028         771.477          917.52         194.079       0       0
    4029         771.477          917.52         194.079       0       0
    9013         771.477          917.52         194.079       0       0
    9014         771.477          917.52         194.079       0       0
    9015         771.477         917.779         440.078       0       0
    9016         771.477         917.779         440.078       0       0
    9017         771.477         917.779         440.078       0       0
   12340        771.4768         855.756        531.9376       0       0
   12341        771.4768        858.8223        539.6435       0       0
   12342        775.3234         855.756        531.8571       0       0
   12343        775.6461        858.8223        539.5561       0       0
   12344        779.1634         855.756        531.6155       0       0
   12345        779.8081        858.8223        539.2943       0       0
   12346        782.9899         855.756        531.2133       0       0
   12347        783.9557        858.8223        538.8583       0       0
   12348        786.7961         855.756        530.6514       0       0
   12349        788.0812        858.8223        538.2492       0       0
   12350        790.5754         855.756        529.9303       0       0
   12351        792.1776        858.8223        537.4678       0       0
   12352        794.3214         855.756        529.0518       0       0
   12353        796.2377        858.8223        536.5154       0       0
   12354        798.0272         855.756        528.0171       0       0
   12355        800.2544        858.8223         535.394       0       0
   12356        801.6865         855.756        526.8281       0       0
   12357        804.2205        858.8223        534.1053       0       0
   12358        805.2925         855.756         525.487       0       0
   12359        808.1293        858.8223        532.6517       0       0
   12360        808.8395         855.756         523.996       0       0
   12361        811.9738        858.8223        531.0355       0       0
   12362        812.3209         855.756        522.3578       0       0
   12363        815.7471        858.8223        529.2599       0       0
   12364        815.7305         855.756        520.5753       0       0
   12365        819.4428        858.8223        527.3279       0       0
   12366        819.0626         855.756        518.6516       0       0
   12367        823.0544        858.8223        525.2428       0       0
   12368        822.3113         855.756          516.59       0       0
   12369        826.5756        858.8223        523.0083       0       0
   12370        825.4706         855.756        514.3942       0       0
   12371        829.9999        858.8223        520.6282       0       0
   12372        828.5353         855.756        512.0679       0       0
   12373        833.3217        858.8223        518.1069       0       0
   12374        831.4998         855.756        509.6155       0       0
   12375        836.5349        858.8223        515.4487       0       0
   12376        834.3591         855.756        507.0409       0       0
   12377        839.6342        858.8223        512.6581       0       0
   12378        837.1081         855.756        504.3489       0       0
   12379        842.6136        858.8223        509.7404       0       0
   12380        839.7418         855.756        501.5442       0       0
   12381        845.4684        858.8223        506.7004       0       0
   12382        842.2559         855.756        498.6317       0       0
   12383        848.1934        858.8223        503.5435       0       0
   12384        844.6458         855.756        495.6164       0       0
   12385        850.7836        858.8223        500.2753       0       0
   12386        846.9073         855.756        492.5038       0       0
   12387        853.2349        858.8223        496.9015       0       0
   12388        849.0364         855.756         489.299       0       0
   12389        855.5426        858.8223         493.428       0       0
   12390        851.0297         855.756         486.008       0       0
   12391         857.703        858.8223        489.8609       0       0
   12392        852.8832         855.756        482.6364       0       0
   12393        859.7122        858.8223        486.2065       0       0
   12394        854.5939         855.756        479.1901       0       0
   12395        861.5664        858.8223         482.471       0       0
   12396        856.1589         855.756        475.6753       0       0
   12397        863.2626        858.8223        478.6613       0       0
   12398        857.5753         855.756        472.0979       0       0
   12399        864.7979        858.8223        474.7839       0       0
   12400        858.8406         855.756        468.4644       0       0
   12401        866.1693        858.8223        470.8456       0       0
   12402        859.9527         855.756        464.7811       0       0
   12403        867.3747        858.8223        466.8533       0       0
   12404        860.9095         855.756        461.0545       0       0
   12405        868.4117        858.8223        462.8141       0       0
   12406        861.7095         855.756         457.291       0       0
   12407        869.2787        858.8223        458.7349       0       0
   12408        862.3511         855.756        453.4974       0       0
   12409        869.9744        858.8223        454.6231       0       0
   12410        862.8334         855.756        449.6802       0       0
   12411         870.497        858.8223        450.4857       0       0
   12412        863.1553         855.756        445.8461       0       0
   12413        870.8459        858.8223          446.33       0       0
   12414        863.3165         855.756         442.002       0       0
   12415        871.0206        858.8223        442.1634       0       0
   12416        863.3165         855.756        438.1545       0       0
   12417        871.0206        858.8223        437.9931       0       0
   12418        863.1554         855.756        434.3103       0       0
   12419        870.8459        858.8223        433.8265       0       0
   12420        862.8334         855.756        430.4763       0       0
   12421         870.497        858.8223        429.6708       0       0
   12422        862.3511         855.756        426.6591       0       0
   12423        869.9744        858.8223        425.5334       0       0
   12424        861.7097         855.756        422.8655       0       0
   12425        869.2789        858.8223        421.4216       0       0
   12426        860.9097         855.756         419.102       0       0
   12427        868.4119        858.8223        417.3424       0       0
   12428        859.9528         855.756        415.3753       0       0
   12429        867.3748        858.8223        413.3031       0       0
   12430        858.8407         855.756        411.6921       0       0
   12431        866.1694        858.8223        409.3108       0       0
   12432        857.5756         855.756        408.0586       0       0
   12433        864.7979        858.8223        405.3725       0       0
   12434        856.1592         855.756        404.4812       0       0
   12435        863.2628        858.8223        401.4951       0       0
   12436        854.5942         855.756        400.9663       0       0
   12437        861.5666        858.8223        397.6853       0       0
   12438        852.8834         855.756          397.52       0       0
   12439        859.7123        858.8223        393.9499       0       0
   12440        851.0298         855.756        394.1484       0       0
   12441        857.7033        858.8223        390.2955       0       0
   12442        849.0367         855.756        390.8573       0       0
   12443        855.5431        858.8223        386.7283       0       0
   12444        846.9076         855.756        387.6526       0       0
   12445        853.2352        858.8223        383.2548       0       0
   12446        844.6461         855.756        384.5399       0       0
   12447        850.7839        858.8223         379.881       0       0
   12448        842.2561         855.756        381.5246       0       0
   12449        848.1935        858.8223        376.6128       0       0
   12450        839.7422         855.756         378.612       0       0
   12451        845.4686        858.8223        373.4559       0       0
   12452        837.1083         855.756        375.8073       0       0
   12453         842.614        858.8223        370.4159       0       0
   12454        834.3593         855.756        373.1153       0       0
   12455        839.6343        858.8223        367.4981       0       0
   12456        831.5001         855.756        370.5408       0       0
   12457        836.5352        858.8223        364.7076       0       0
   12458        828.5355         855.756        368.0883       0       0
   12459         833.322        858.8223        362.0493       0       0
   12460        825.4708         855.756        365.7621       0       0
   12461        830.0002        858.8223         359.528       0       0
   12462        822.3113         855.756        363.5662       0       0
   12463        826.5758        858.8223        357.1479       0       0
   12464        819.0629         855.756        361.5046       0       0
   12465        823.0546        858.8223        354.9134       0       0
   12466        815.7308         855.756        359.5808       0       0
   12467        819.4431        858.8223        352.8282       0       0
   12468        812.3212         855.756        357.7983       0       0
   12469        815.7473        858.8223        350.8961       0       0
   12470        808.8398         855.756        356.1601       0       0
   12471        811.9739        858.8223        349.1205       0       0
   12472        805.2928         855.756        354.6691       0       0
   12473        808.1296        858.8223        347.5045       0       0
   12474        801.6866         855.756        353.3279       0       0
   12475        804.2208        858.8223        346.0508       0       0
   12476        798.0273         855.756         352.139       0       0
   12477        800.2547        858.8223        344.7621       0       0
   12478        794.3217         855.756        351.1043       0       0
   12479        796.2379        858.8223        343.6406       0       0
   12480        790.5757         855.756        350.2257       0       0
   12481        792.1779        858.8223        342.6883       0       0
   12482        786.7963         855.756        349.5048       0       0
   12483        788.0815        858.8223        341.9069       0       0
   12484        782.9901         855.756        348.9427       0       0
   12485        783.9559        858.8223        341.2977       0       0
   12486        779.1636         855.756        348.5405       0       0
   12487        779.8085        858.8223        340.8618       0       0
   12488        775.3237         855.756        348.2989       0       0
   12489        775.6463        858.8223           340.6       0       0
   12490         771.477         855.756        348.2183       0       0
   12491         771.477        858.8223        340.5125       0       0
   12492        767.6302         855.756        348.2989       0       0
   12493        767.3076        858.8223           340.6       0       0
   12494        763.7904         855.756        348.5405       0       0
   12495        763.1455        858.8223        340.8618       0       0
   12496        759.9638         855.756        348.9427       0       0
   12497        758.9981        858.8223        341.2977       0       0
   12498        756.1577         855.756        349.5048       0       0
   12499        754.8726        858.8223        341.9069       0       0
   12500        752.3782         855.756        350.2257       0       0
   12501        750.7761        858.8223        342.6883       0       0
   12502        748.6324         855.756        351.1043       0       0
   12503        746.7159        858.8223        343.6407       0       0
   12504        744.9266         855.756         352.139       0       0
   12505        742.6993        858.8223        344.7621       0       0
   12506        741.2673         855.756        353.3279       0       0
   12507        738.7332        858.8223        346.0508       0       0
   12508        737.6611         855.756        354.6691       0       0
   12509        734.8244        858.8223        347.5045       0       0
   12510        734.1143         855.756        356.1601       0       0
   12511        730.9799        858.8223        349.1205       0       0
   12512        730.6329         855.756        357.7983       0       0
   12513        727.2065        858.8223        350.8961       0       0
   12514        727.2231         855.756        359.5808       0       0
   12515        723.5109        858.8223        352.8282       0       0
   12516        723.8912         855.756        361.5046       0       0
   12517        719.8992        858.8223        354.9134       0       0
   12518        720.6425         855.756        363.5663       0       0
   12519        716.3782        858.8223        357.1479       0       0
   12520        717.4831         855.756         365.762       0       0
   12521        712.9538        858.8223         359.528       0       0
   12522        714.4184         855.756        368.0882       0       0
   12523        709.6321        858.8223        362.0493       0       0
   12524        711.4539         855.756        370.5408       0       0
   12525        706.4188        858.8223        364.7076       0       0
   12526        708.5946         855.756        373.1153       0       0
   12527        703.3197        858.8223        367.4981       0       0
   12528        705.8458         855.756        375.8073       0       0
   12529        700.3401        858.8223        370.4159       0       0
   12530        703.2119         855.756        378.6121       0       0
   12531        697.4854        858.8223        373.4559       0       0
   12532        700.6978         855.756        381.5246       0       0
   12533        694.7603        858.8223        376.6128       0       0
   12534        698.3079         855.756        384.5398       0       0
   12535          692.17        858.8223         379.881       0       0
   12536        696.0465         855.756        387.6526       0       0
   12537        689.7189        858.8223        383.2548       0       0
   12538        693.9172         855.756        390.8573       0       0
   12539         687.411        858.8223        386.7283       0       0
   12540         691.924         855.756        394.1483       0       0
   12541        685.2507        858.8223        390.2955       0       0
   12542        690.0705         855.756        397.5199       0       0
   12543        683.2417        858.8223        393.9498       0       0
   12544        688.3599         855.756        400.9662       0       0
   12545        681.3874        858.8223        397.6853       0       0
   12546        686.7949         855.756         404.481       0       0
   12547        679.6913        858.8223         401.495       0       0
   12548        685.3785         855.756        408.0583       0       0
   12549        678.1561        858.8223        405.3724       0       0
   12550        684.1133         855.756        411.6918       0       0
   12551        676.7847        858.8223        409.3106       0       0
   12552        683.0012         855.756        415.3751       0       0
   12553        675.5793        858.8223        413.3029       0       0
   12554        682.0444         855.756        419.1018       0       0
   12555        674.5422        858.8223        417.3422       0       0
   12556        681.2444         855.756        422.8653       0       0
   12557        673.6751        858.8223        421.4214       0       0
   12558        680.6027         855.756        426.6589       0       0
   12559        672.9796        858.8223        425.5332       0       0
   12560        680.1206         855.756         430.476       0       0
   12561         672.457        858.8223        429.6706       0       0
   12562        679.7986         855.756        434.3101       0       0
   12563        672.1081        858.8223        433.8262       0       0
   12564        679.6374         855.756        438.1542       0       0
   12565        671.9335        858.8223        437.9928       0       0
   12566        679.6375         855.756        442.0017       0       0
   12567        671.9335        858.8223        442.1631       0       0
   12568        679.7987         855.756        445.8459       0       0
   12569        672.1081        858.8223        446.3298       0       0
   12570        680.1206         855.756        449.6799       0       0
   12571         672.457        858.8223        450.4854       0       0
   12572        680.6027         855.756         453.497       0       0
   12573        672.9797        858.8223        454.6228       0       0
   12574        681.2444         855.756        457.2907       0       0
   12575        673.6751        858.8223        458.7346       0       0
   12576        682.0443         855.756        461.0541       0       0
   12577        674.5422        858.8223        462.8138       0       0
   12578        683.0012         855.756        464.7808       0       0
   12579        675.5792        858.8223         466.853       0       0
   12580        684.1132         855.756        468.4641       0       0
   12581        676.7846        858.8223        470.8453       0       0
   12582        685.3785         855.756        472.0975       0       0
   12583        678.1561        858.8223        474.7836       0       0
   12584        686.7949         855.756        475.6749       0       0
   12585        679.6913        858.8223         478.661       0       0
   12586        688.3599         855.756        479.1898       0       0
   12587        681.3874        858.8223        482.4707       0       0
   12588        690.0706         855.756        482.6361       0       0
   12589        683.2417        858.8223        486.2061       0       0
   12590        691.9241         855.756        486.0077       0       0
   12591        685.2509        858.8223        489.8605       0       0
   12592        693.9172         855.756        489.2987       0       0
   12593        687.4109        858.8223        493.4277       0       0
   12594        696.0464         855.756        492.5034       0       0
   12595        689.7188        858.8223        496.9012       0       0
   12596        698.3079         855.756        495.6161       0       0
   12597        692.1699        858.8223        500.2749       0       0
   12598        700.6978         855.756        498.6313       0       0
   12599        694.7603        858.8223        503.5432       0       0
   12600        703.2119         855.756        501.5439       0       0
   12601        697.4853        858.8223        506.7001       0       0
   12602        705.8456         855.756        504.3486       0       0
   12603        700.3401        858.8223        509.7401       0       0
   12604        708.5945         855.756        507.0406       0       0
   12605        703.3196        858.8223        512.6579       0       0
   12606        711.4537         855.756        509.6151       0       0
   12607        706.4186        858.8223        515.4484       0       0
   12608        714.4184         855.756        512.0676       0       0
   12609        709.6318        858.8223        518.1066       0       0
   12610         717.483         855.756        514.3937       0       0
   12611        712.9537        858.8223        520.6279       0       0
   12612        720.6425         855.756        516.5897       0       0
   12613         716.378        858.8223        523.0079       0       0
   12614        723.8909         855.756        518.6512       0       0
   12615         719.899        858.8223        525.2424       0       0
   12616        727.2228         855.756        520.5751       0       0
   12617        723.5107        858.8223        527.3276       0       0
   12618        730.6326         855.756        522.3577       0       0
   12619        727.2064        858.8223        529.2598       0       0
   12620         734.114         855.756        523.9957       0       0
   12621        730.9796        858.8223        531.0354       0       0
   12622        737.6608         855.756        525.4868       0       0
   12623        734.8241        858.8223        532.6514       0       0
   12624         741.267         855.756         526.828       0       0
   12625        738.7328        858.8223        534.1051       0       0
   12626        744.9261         855.756        528.0169       0       0
   12627        742.6989        858.8223        535.3937       0       0
   12628        748.6321         855.756        529.0516       0       0
   12629        746.7156        858.8223        536.5153       0       0
   12630        752.3778         855.756        529.9302       0       0
   12631        750.7757        858.8223        537.4676       0       0
   12632        756.1572         855.756        530.6511       0       0
   12633        754.8721        858.8223         538.249       0       0
   12634        759.9634         855.756        531.2133       0       0
   12635        758.9976        858.8223        538.8583       0       0
   12636        763.7899         855.756        531.6155       0       0
   12637         763.145        858.8223        539.2943       0       0
   12638        767.6299         855.756        531.8571       0       0
   12639        767.3071        858.8223        539.5561       0       0
   12640        771.4765         855.756        531.9376       0       0
   12641        771.4764        858.8223        539.6435       0       0
   12642        771.4768        862.8182        546.8882       0       0
   12643        775.9496        862.8182        546.7946       0       0
   12644        780.4144        862.8182        546.5137       0       0
   12645        784.8637        862.8182        546.0461       0       0
   12646        789.2894        862.8182        545.3926       0       0
   12647        793.6839        862.8182        544.5542       0       0
   12648        798.0394        862.8182        543.5327       0       0
   12649        802.3483        862.8182        542.3295       0       0
   12650        806.6031        862.8182        540.9471       0       0
   12651        810.7963        862.8182        539.3878       0       0
   12652        814.9204        862.8182        537.6541       0       0
   12653        818.9684        862.8182        535.7493       0       0
   12654         822.933        862.8182        533.6766       0       0
   12655        826.8074        862.8182        531.4398       0       0
   12656        830.5848        862.8182        529.0427       0       0
   12657        834.2583        862.8182        526.4894       0       0
   12658        837.8218        862.8182        523.7846       0       0
   12659        841.2689        862.8182         520.933       0       0
   12660        844.5936        862.8182        517.9395       0       0
   12661        847.7899        862.8182        514.8094       0       0
   12662        850.8524        862.8182        511.5482       0       0
   12663        853.7756        862.8182        508.1616       0       0
   12664        856.5544        862.8182        504.6555       0       0
   12665         859.184        862.8182        501.0363       0       0
   12666        861.6598        862.8182          497.31       0       0
   12667        863.9772        862.8182        493.4833       0       0
   12668        866.1326        862.8182         489.563       0       0
   12669        868.1217        862.8182        485.5558       0       0
   12670        869.9413        862.8182        481.4689       0       0
   12671        871.5883        862.8182        477.3093       0       0
   12672        873.0595        862.8182        473.0844       0       0
   12673        874.3525        862.8182        468.8016       0       0
   12674        875.4651        862.8182        464.4684       0       0
   12675        876.3953        862.8182        460.0924       0       0
   12676        877.1414        862.8182        455.6814       0       0
   12677        877.7021        862.8182        451.2429       0       0
   12678        878.0765        862.8182        446.7849       0       0
   12679        878.2639        862.8182        442.3151       0       0
   12680        878.2639        862.8182        437.8413       0       0
   12681        878.0765        862.8182        433.3716       0       0
   12682        877.7021        862.8182        428.9135       0       0
   12683        877.1414        862.8182        424.4751       0       0
   12684        876.3954        862.8182         420.064       0       0
   12685        875.4653        862.8182         415.688       0       0
   12686        874.3527        862.8182        411.3549       0       0
   12687        873.0597        862.8182         407.072       0       0
   12688        871.5884        862.8182        402.8472       0       0
   12689        869.9416        862.8182        398.6876       0       0
   12690        868.1219        862.8182        394.6006       0       0
   12691        866.1327        862.8182        390.5934       0       0
   12692        863.9775        862.8182        386.6731       0       0
   12693          861.66        862.8182        382.8464       0       0
   12694        859.1842        862.8182        379.1201       0       0
   12695        856.5547        862.8182        375.5008       0       0
   12696        853.7758        862.8182        371.9948       0       0
   12697        850.8526        862.8182        368.6081       0       0
   12698        847.7902        862.8182        365.3469       0       0
   12699        844.5938        862.8182        362.2168       0       0
   12700         841.269        862.8182        359.2233       0       0
   12701        837.8221        862.8182        356.3716       0       0
   12702        834.2585        862.8182        353.6668       0       0
   12703         830.585        862.8182        351.1135       0       0
   12704        826.8077        862.8182        348.7164       0       0
   12705        822.9333        862.8182        346.4795       0       0
   12706        818.9687        862.8182        344.4068       0       0
   12707        814.9208        862.8182        342.5021       0       0
   12708        810.7966        862.8182        340.7684       0       0
   12709        806.6034        862.8182         339.209       0       0
   12710        802.3486        862.8182        337.8265       0       0
   12711        798.0396        862.8182        336.6234       0       0
   12712        793.6841        862.8182        335.6018       0       0
   12713        789.2897        862.8182        334.7635       0       0
   12714        784.8639        862.8182          334.11       0       0
   12715        780.4146        862.8182        333.6423       0       0
   12716        775.9497        862.8182        333.3614       0       0
   12717         771.477        862.8182        333.2678       0       0
   12718        767.0042        862.8182        333.3614       0       0
   12719        762.5392        862.8182        333.6423       0       0
   12720          758.09        862.8182          334.11       0       0
   12721        753.6642        862.8182        334.7635       0       0
   12722        749.2699        862.8182        335.6018       0       0
   12723        744.9143        862.8182        336.6234       0       0
   12724        740.6053        862.8182        337.8265       0       0
   12725        736.3506        862.8182         339.209       0       0
   12726        732.1574        862.8182        340.7684       0       0
   12727        728.0332        862.8182        342.5021       0       0
   12728        723.9853        862.8182        344.4068       0       0
   12729        720.0206        862.8182        346.4795       0       0
   12730        716.1463        862.8182        348.7164       0       0
   12731        712.3689        862.8182        351.1136       0       0
   12732        708.6953        862.8182        353.6668       0       0
   12733        705.1318        862.8182        356.3716       0       0
   12734        701.6848        862.8182        359.2233       0       0
   12735        698.3602        862.8182        362.2168       0       0
   12736        695.1639        862.8182         365.347       0       0
   12737        692.1013        862.8182        368.6082       0       0
   12738        689.1781        862.8182        371.9948       0       0
   12739        686.3993        862.8182        375.5008       0       0
   12740        683.7698        862.8182        379.1201       0       0
   12741         681.294        862.8182        382.8464       0       0
   12742        678.9765        862.8182        386.6731       0       0
   12743        676.8212        862.8182        390.5934       0       0
   12744        674.8321        862.8182        394.6006       0       0
   12745        673.0125        862.8182        398.6875       0       0
   12746        671.3657        862.8182        402.8471       0       0
   12747        669.8944        862.8182        407.0719       0       0
   12748        668.6014        862.8182        411.3547       0       0
   12749        667.4888        862.8182        415.6879       0       0
   12750        666.5586        862.8182        420.0638       0       0
   12751        665.8126        862.8182        424.4749       0       0
   12752        665.2519        862.8182        428.9133       0       0
   12753        664.8775        862.8182        433.3713       0       0
   12754        664.6901        862.8182        437.8412       0       0
   12755        664.6901        862.8182        442.3148       0       0
   12756        664.8775        862.8182        446.7846       0       0
   12757        665.2519        862.8182        451.2426       0       0
   12758        665.8126        862.8182        455.6812       0       0
   12759        666.5586        862.8182        460.0921       0       0
   12760        667.4887        862.8182        464.4681       0       0
   12761        668.6014        862.8182        468.8013       0       0
   12762        669.8944        862.8182        473.0841       0       0
   12763        671.3657        862.8182        477.3089       0       0
   12764        673.0125        862.8182        481.4685       0       0
   12765        674.8321        862.8182        485.5555       0       0
   12766        676.8213        862.8182        489.5626       0       0
   12767        678.9766        862.8182        493.4829       0       0
   12768         681.294        862.8182        497.3096       0       0
   12769        683.7697        862.8182        501.0359       0       0
   12770        686.3992        862.8182        504.6552       0       0
   12771        689.1781        862.8182        508.1613       0       0
   12772        692.1013        862.8182        511.5479       0       0
   12773        695.1638        862.8182        514.8091       0       0
   12774        698.3602        862.8182        517.9391       0       0
   12775        701.6848        862.8182        520.9326       0       0
   12776        705.1318        862.8182        523.7843       0       0
   12777        708.6953        862.8182         526.489       0       0
   12778        712.3688        862.8182        529.0423       0       0
   12779        716.1461        862.8182        531.4395       0       0
   12780        720.0204        862.8182        533.6764       0       0
   12781         723.985        862.8182        535.7491       0       0
   12782         728.033        862.8182        537.6539       0       0
   12783         732.157        862.8182        539.3875       0       0
   12784        736.3502        862.8182         540.947       0       0
   12785         740.605        862.8182        542.3295       0       0
   12786        744.9139        862.8182        543.5325       0       0
   12787        749.2693        862.8182        544.5542       0       0
   12788        753.6638        862.8182        545.3924       0       0
   12789        758.0896        862.8182         546.046       0       0
   12790        762.5389        862.8182        546.5135       0       0
   12791        767.0037        862.8182        546.7946       0       0
   12792        771.4764        862.8182        546.8882       0       0
   12793        771.4768        867.6621        553.5745       0       0
   12794        776.2296        867.6621         553.475       0       0
   12795        780.9739        867.6621        553.1765       0       0
   12796        785.7017        867.6621        552.6796       0       0
   12797        790.4045        867.6621        551.9853       0       0
   12798        795.0741        867.6621        551.0945       0       0
   12799        799.7023        867.6621        550.0089       0       0
   12800        804.2809        867.6621        548.7305       0       0
   12801        808.8021        867.6621        547.2615       0       0
   12802        813.2576        867.6621        545.6044       0       0
   12803        817.6401        867.6621        543.7623       0       0
   12804        821.9413        867.6621        541.7382       0       0
   12805        826.1542        867.6621        539.5358       0       0
   12806        830.2711        867.6621        537.1591       0       0
   12807        834.2848        867.6621        534.6118       0       0
   12808        838.1884        867.6621        531.8987       0       0
   12809         841.975        867.6621        529.0247       0       0
   12810        845.6378        867.6621        525.9944       0       0
   12811        849.1706        867.6621        522.8135       0       0
   12812        852.5671        867.6621        519.4875       0       0
   12813        855.8212        867.6621        516.0222       0       0
   12814        858.9274        867.6621        512.4235       0       0
   12815        861.8802        867.6621        508.6981       0       0
   12816        864.6744        867.6621        504.8522       0       0
   12817        867.3051        867.6621        500.8926       0       0
   12818        869.7678        867.6621        496.8264       0       0
   12819         872.058        867.6621        492.6606       0       0
   12820        874.1716        867.6621        488.4026       0       0
   12821        876.1051        867.6621        484.0599       0       0
   12822        877.8551        867.6621        479.6399       0       0
   12823        879.4186        867.6621        475.1506       0       0
   12824        880.7925        867.6621        470.5996       0       0
   12825        881.9747        867.6621        465.9953       0       0
   12826        882.9631        867.6621        461.3453       0       0
   12827        883.7559        867.6621        456.6582       0       0
   12828        884.3517        867.6621        451.9418       0       0
   12829        884.7496        867.6621        447.2047       0       0
   12830        884.9486        867.6621        442.4551       0       0
   12831        884.9486        867.6621        437.7014       0       0
   12832        884.7496        867.6621        432.9518       0       0
   12833        884.3517        867.6621        428.2146       0       0
   12834        883.7559        867.6621        423.4983       0       0
   12835        882.9633        867.6621        418.8111       0       0
   12836        881.9749        867.6621        414.1612       0       0
   12837        880.7926        867.6621        409.5568       0       0
   12838        879.4187        867.6621        405.0059       0       0
   12839        877.8553        867.6621        400.5166       0       0
   12840        876.1053        867.6621        396.0966       0       0
   12841        874.1718        867.6621        391.7538       0       0
   12842         872.058        867.6621        387.4957       0       0
   12843        869.7679        867.6621        383.3299       0       0
   12844        867.3055        867.6621        379.2636       0       0
   12845        864.6747        867.6621        375.3041       0       0
   12846        861.8805        867.6621        371.4582       0       0
   12847        858.9277        867.6621        367.7328       0       0
   12848        855.8215        867.6621        364.1341       0       0
   12849        852.5673        867.6621        360.6688       0       0
   12850        849.1708        867.6621        357.3427       0       0
   12851        845.6381        867.6621        354.1618       0       0
   12852        841.9752        867.6621        351.1316       0       0
   12853        838.1887        867.6621        348.2575       0       0
   12854         834.285        867.6621        345.5444       0       0
   12855        830.2714        867.6621        342.9972       0       0
   12856        826.1544        867.6621        340.6202       0       0
   12857        821.9416        867.6621        338.4178       0       0
   12858        817.6403        867.6621        336.3938       0       0
   12859        813.2579        867.6621        334.5516       0       0
   12860        808.8022        867.6621        332.8946       0       0
   12861        804.2811        867.6621        331.4256       0       0
   12862        799.7025        867.6621        330.1471       0       0
   12863        795.0743        867.6621        329.0616       0       0
   12864        790.4048        867.6621        328.1709       0       0
   12865        785.7019        867.6621        327.4764       0       0
   12866        780.9741        867.6621        326.9795       0       0
   12867        776.2297        867.6621         326.681       0       0
   12868        771.4769        867.6621        326.5815       0       0
   12869        766.7242        867.6621         326.681       0       0
   12870        761.9798        867.6621        326.9795       0       0
   12871         757.252        867.6621        327.4764       0       0
   12872        752.5491        867.6621        328.1709       0       0
   12873        747.8796        867.6621        329.0616       0       0
   12874        743.2514        867.6621        330.1472       0       0
   12875        738.6727        867.6621        331.4256       0       0
   12876        734.1516        867.6621        332.8946       0       0
   12877        729.6961        867.6621        334.5516       0       0
   12878        725.3137        867.6621        336.3938       0       0
   12879        721.0123        867.6621        338.4178       0       0
   12880        716.7995        867.6621        340.6204       0       0
   12881        712.6826        867.6621        342.9973       0       0
   12882        708.6688        867.6621        345.5444       0       0
   12883        704.7653        867.6621        348.2575       0       0
   12884        700.9787        867.6621        351.1317       0       0
   12885        697.3159        867.6621        354.1619       0       0
   12886        693.7831        867.6621        357.3427       0       0
   12887        690.3866        867.6621        360.6689       0       0
   12888        687.1325        867.6621        364.1342       0       0
   12889        684.0263        867.6621        367.7328       0       0
   12890        681.0735        867.6621        371.4583       0       0
   12891        678.2793        867.6621        375.3041       0       0
   12892        675.6486        867.6621        379.2636       0       0
   12893        673.1859        867.6621        383.3299       0       0
   12894        670.8958        867.6621        387.4957       0       0
   12895        668.7822        867.6621        391.7537       0       0
   12896        666.8486        867.6621        396.0964       0       0
   12897        665.0986        867.6621        400.5164       0       0
   12898        663.5354        867.6621        405.0058       0       0
   12899        662.1614        867.6621        409.5566       0       0
   12900        660.9792        867.6621         414.161       0       0
   12901        659.9908        867.6621        418.8109       0       0
   12902        659.1979        867.6621        423.4981       0       0
   12903        658.6022        867.6621        428.2145       0       0
   12904        658.2044        867.6621        432.9514       0       0
   12905        658.0054        867.6621        437.7011       0       0
   12906        658.0054        867.6621        442.4549       0       0
   12907        658.2045        867.6621        447.2045       0       0
   12908        658.6022        867.6621        451.9417       0       0
   12909        659.1979        867.6621        456.6579       0       0
   12910        659.9908        867.6621        461.3451       0       0
   12911        660.9792        867.6621         465.995       0       0
   12912        662.1614        867.6621        470.5993       0       0
   12913        663.5354        867.6621        475.1503       0       0
   12914        665.0986        867.6621        479.6396       0       0
   12915        666.8487        867.6621        484.0595       0       0
   12916        668.7822        867.6621        488.4023       0       0
   12917        670.8958        867.6621        492.6603       0       0
   12918         673.186        867.6621        496.8261       0       0
   12919        675.6486        867.6621        500.8923       0       0
   12920        678.2792        867.6621        504.8519       0       0
   12921        681.0735        867.6621        508.6977       0       0
   12922        684.0263        867.6621        512.4232       0       0
   12923        687.1325        867.6621        516.0218       0       0
   12924        690.3866        867.6621        519.4872       0       0
   12925        693.7831        867.6621        522.8132       0       0
   12926        697.3157        867.6621         525.994       0       0
   12927        700.9787        867.6621        529.0242       0       0
   12928        704.7651        867.6621        531.8984       0       0
   12929        708.6687        867.6621        534.6115       0       0
   12930        712.6824        867.6621        537.1587       0       0
   12931        716.7992        867.6621        539.5355       0       0
   12932         721.012        867.6621         541.738       0       0
   12933        725.3135        867.6621        543.7621       0       0
   12934        729.6958        867.6621        545.6042       0       0
   12935        734.1513        867.6621        547.2614       0       0
   12936        738.6724        867.6621        548.7303       0       0
   12937         743.251        867.6621        550.0087       0       0
   12938        747.8792        867.6621        551.0942       0       0
   12939        752.5488        867.6621         551.985       0       0
   12940        757.2516        867.6621        552.6796       0       0
   12941        761.9793        867.6621        553.1765       0       0
   12942        766.7237        867.6621         553.475       0       0
   12943        771.4764        867.6621        553.5745       0       0
   12944        771.4768        873.2783        559.5995       0       0
   12945        776.4819        873.2783        559.4947       0       0
   12946        781.4781        873.2783        559.1803       0       0
   12947        786.4568        873.2783         558.657       0       0
   12948        791.4092        873.2783        557.9258       0       0
   12949        796.3267        873.2783        556.9877       0       0
   12950        801.2006        873.2783        555.8447       0       0
   12951        806.0223        873.2783        554.4984       0       0
   12952        810.7835        873.2783        552.9514       0       0
   12953        815.4756        873.2783        551.2064       0       0
   12954        820.0906        873.2783        549.2664       0       0
   12955        824.6202        873.2783         547.135       0       0
   12956        829.0567        873.2783        544.8156       0       0
   12957        833.3923        873.2783        542.3126       0       0
   12958         837.619        873.2783        539.6302       0       0
   12959        841.7299        873.2783        536.7731       0       0
   12960        845.7174        873.2783        533.7463       0       0
   12961        849.5746        873.2783        530.5554       0       0
   12962        853.2949        873.2783        527.2056       0       0
   12963        856.8717        873.2783         523.703       0       0
   12964        860.2987        873.2783        520.0536       0       0
   12965        863.5697        873.2783         516.264       0       0
   12966        866.6793        873.2783        512.3408       0       0
   12967        869.6218        873.2783        508.2907       0       0
   12968        872.3922        873.2783         504.121       0       0
   12969        874.9855        873.2783         499.839       0       0
   12970        877.3972        873.2783        495.4521       0       0
   12971        879.6232        873.2783        490.9679       0       0
   12972        881.6595        873.2783        486.3947       0       0
   12973        883.5023        873.2783        481.7401       0       0
   12974        885.1487        873.2783        477.0124       0       0
   12975        886.5956        873.2783        472.2199       0       0
   12976        887.8406        873.2783        467.3711       0       0
   12977        888.8813        873.2783        462.4743       0       0
   12978        889.7163        873.2783        457.5384       0       0
   12979        890.3439        873.2783        452.5717       0       0
   12980        890.7628        873.2783        447.5831       0       0
   12981        890.9723        873.2783        442.5813       0       0
   12982        890.9723        873.2783        437.5752       0       0
   12983        890.7628        873.2783        432.5735       0       0
   12984        890.3439        873.2783        427.5848       0       0
   12985        889.7163        873.2783        422.6182       0       0
   12986        888.8816        873.2783        417.6822       0       0
   12987        887.8407        873.2783        412.7854       0       0
   12988        886.5957        873.2783        407.9366       0       0
   12989        885.1488        873.2783        403.1441       0       0
   12990        883.5025        873.2783        398.4164       0       0
   12991        881.6595        873.2783        393.7618       0       0
   12992        879.6234        873.2783        389.1884       0       0
   12993        877.3975        873.2783        384.7043       0       0
   12994        874.9858        873.2783        380.3174       0       0
   12995        872.3926        873.2783        376.0354       0       0
   12996        869.6223        873.2783        371.8656       0       0
   12997        866.6796        873.2783        367.8156       0       0
   12998          863.57        873.2783        363.8923       0       0
   12999         860.299        873.2783        360.1025       0       0
   13000        856.8719        873.2783        356.4533       0       0
   13001        853.2952        873.2783        352.9506       0       0
   13002        849.5749        873.2783        349.6008       0       0
   13003        845.7176        873.2783        346.4099       0       0
   13004          841.73        873.2783        343.3831       0       0
   13005        837.6193        873.2783         340.526       0       0
   13006        833.3926        873.2783        337.8436       0       0
   13007         829.057        873.2783        335.3405       0       0
   13008        824.6207        873.2783        333.0212       0       0
   13009        820.0908        873.2783        330.8896       0       0
   13010         815.476        873.2783        328.9497       0       0
   13011        810.7836        873.2783        327.2047       0       0
   13012        806.0226        873.2783        325.6577       0       0
   13013        801.2008        873.2783        324.3115       0       0
   13014         796.327        873.2783        323.1683       0       0
   13015        791.4094        873.2783        322.2303       0       0
   13016         786.457        873.2783        321.4989       0       0
   13017        781.4783        873.2783        320.9757       0       0
   13018        776.4821        873.2783        320.6613       0       0
   13019        771.4769        873.2783        320.5564       0       0
   13020        766.4719        873.2783        320.6613       0       0
   13021        761.4756        873.2783        320.9757       0       0
   13022        756.4969        873.2783        321.4989       0       0
   13023        751.5445        873.2783        322.2303       0       0
   13024         746.627        873.2783        323.1683       0       0
   13025        741.7531        873.2783        324.3115       0       0
   13026        736.9313        873.2783        325.6578       0       0
   13027        732.1702        873.2783        327.2047       0       0
   13028        727.4781        873.2783        328.9497       0       0
   13029        722.8631        873.2783        330.8898       0       0
   13030        718.3334        873.2783        333.0212       0       0
   13031        713.8969        873.2783        335.3406       0       0
   13032        709.5615        873.2783        337.8436       0       0
   13033        705.3346        873.2783        340.5261       0       0
   13034        701.2238        873.2783        343.3831       0       0
   13035        697.2363        873.2783        346.4099       0       0
   13036         693.379        873.2783         349.601       0       0
   13037        689.6586        873.2783        352.9507       0       0
   13038        686.0818        873.2783        356.4534       0       0
   13039         682.655        873.2783        360.1027       0       0
   13040        679.3839        873.2783        363.8924       0       0
   13041        676.2744        873.2783        367.8156       0       0
   13042        673.3318        873.2783        371.8657       0       0
   13043        670.5615        873.2783        376.0354       0       0
   13044        667.9681        873.2783        380.3174       0       0
   13045        665.5564        873.2783        384.7043       0       0
   13046        663.3306        873.2783        389.1884       0       0
   13047        661.2943        873.2783        393.7617       0       0
   13048        659.4515        873.2783        398.4163       0       0
   13049        657.8052        873.2783        403.1439       0       0
   13050        656.3583        873.2783        407.9364       0       0
   13051        655.1134        873.2783        412.7852       0       0
   13052        654.0725        873.2783        417.6819       0       0
   13053        653.2376        873.2783         422.618       0       0
   13054        652.6102        873.2783        427.5846       0       0
   13055        652.1913        873.2783        432.5732       0       0
   13056        651.9817        873.2783        437.5749       0       0
   13057        651.9817        873.2783        442.5811       0       0
   13058        652.1913        873.2783        447.5829       0       0
   13059        652.6102        873.2783        452.5714       0       0
   13060        653.2377        873.2783        457.5381       0       0
   13061        654.0724        873.2783        462.4741       0       0
   13062        655.1134        873.2783        467.3708       0       0
   13063        656.3583        873.2783        472.2196       0       0
   13064        657.8052        873.2783        477.0121       0       0
   13065        659.4515        873.2783        481.7398       0       0
   13066        661.2944        873.2783        486.3943       0       0
   13067        663.3306        873.2783        490.9676       0       0
   13068        665.5566        873.2783        495.4517       0       0
   13069        667.9682        873.2783        499.8386       0       0
   13070        670.5615        873.2783        504.1206       0       0
   13071        673.3318        873.2783        508.2904       0       0
   13072        676.2744        873.2783        512.3404       0       0
   13073        679.3839        873.2783        516.2637       0       0
   13074         682.655        873.2783        520.0533       0       0
   13075        686.0818        873.2783        523.7026       0       0
   13076        689.6586        873.2783        527.2053       0       0
   13077         693.379        873.2783        530.5549       0       0
   13078        697.2362        873.2783         533.746       0       0
   13079        701.2238        873.2783        536.7728       0       0
   13080        705.3345        873.2783        539.6298       0       0
   13081        709.5613        873.2783        542.3122       0       0
   13082        713.8967        873.2783        544.8154       0       0
   13083        718.3331        873.2783        547.1346       0       0
   13084        722.8627        873.2783        549.2661       0       0
   13085        727.4778        873.2783        551.2062       0       0
   13086        732.1699        873.2783        552.9512       0       0
   13087        736.9309        873.2783        554.4981       0       0
   13088        741.7527        873.2783        555.8445       0       0
   13089        746.6265        873.2783        556.9877       0       0
   13090        751.5439        873.2783        557.9257       0       0
   13091        756.4965        873.2783         558.657       0       0
   13092        761.4751        873.2783        559.1803       0       0
   13093        766.4714        873.2783        559.4947       0       0
   13094        771.4764        873.2783        559.5995       0       0
   13095        771.4768        879.5872        564.8635       0       0
   13096        776.7023        879.5872         564.754       0       0
   13097        781.9186        879.5872        564.4258       0       0
   13098        787.1165        879.5872        563.8796       0       0
   13099        792.2871        879.5872        563.1161       0       0
   13100        797.4212        879.5872        562.1367       0       0
   13101        802.5096        879.5872        560.9432       0       0
   13102        807.5438        879.5872        559.5377       0       0
   13103        812.5147        879.5872        557.9225       0       0
   13104        817.4134        879.5872        556.1006       0       0
   13105        822.2317        879.5872        554.0753       0       0
   13106        826.9608        879.5872        551.8499       0       0
   13107        831.5926        879.5872        549.4284       0       0
   13108        836.1191        879.5872        546.8152       0       0
   13109        840.5321        879.5872        544.0146       0       0
   13110         844.824        879.5872        541.0317       0       0
   13111        848.9871        879.5872        537.8718       0       0
   13112        853.0143        879.5872        534.5402       0       0
   13113        856.8984        879.5872        531.0428       0       0
   13114        860.6327        879.5872         527.386       0       0
   13115        864.2106        879.5872        523.5759       0       0
   13116        867.6257        879.5872        519.6194       0       0
   13117        870.8722        879.5872        515.5234       0       0
   13118        873.9443        879.5872         511.295       0       0
   13119        876.8367        879.5872        506.9417       0       0
   13120        879.5443        879.5872         502.471       0       0
   13121        882.0623        879.5872        497.8908       0       0
   13122        884.3862        879.5872        493.2093       0       0
   13123        886.5121        879.5872        488.4346       0       0
   13124        888.4362        879.5872         483.575       0       0
   13125         890.155        879.5872        478.6391       0       0
   13126        891.6656        879.5872        473.6355       0       0
   13127        892.9655        879.5872        468.5731       0       0
   13128        894.0521        879.5872        463.4607       0       0
   13129        894.9238        879.5872        458.3073       0       0
   13130         895.579        879.5872        453.1219       0       0
   13131        896.0162        879.5872        447.9135       0       0
   13132        896.2351        879.5872        442.6915       0       0
   13133        896.2351        879.5872         437.465       0       0
   13134        896.0162        879.5872        432.2429       0       0
   13135         895.579        879.5872        427.0347       0       0
   13136         894.924        879.5872        421.8492       0       0
   13137        894.0524        879.5872        416.6958       0       0
   13138        892.9656        879.5872        411.5834       0       0
   13139        891.6657        879.5872         406.521       0       0
   13140        890.1552        879.5872        401.5174       0       0
   13141        888.4363        879.5872        396.5815       0       0
   13142        886.5123        879.5872         391.722       0       0
   13143        884.3864        879.5872        386.9471       0       0
   13144        882.0624        879.5872        382.2655       0       0
   13145        879.5444        879.5872        377.6854       0       0
   13146         876.837        879.5872        373.2148       0       0
   13147        873.9446        879.5872        368.8614       0       0
   13148        870.8725        879.5872         364.633       0       0
   13149         867.626        879.5872        360.5369       0       0
   13150        864.2108        879.5872        356.5803       0       0
   13151         860.633        879.5872        352.7703       0       0
   13152        856.8987        879.5872        349.1134       0       0
   13153        853.0145        879.5872        345.6161       0       0
   13154        848.9873        879.5872        342.2845       0       0
   13155        844.8242        879.5872        339.1245       0       0
   13156        840.5323        879.5872        336.1415       0       0
   13157        836.1194        879.5872         333.341       0       0
   13158        831.5929        879.5872        330.7277       0       0
   13159        826.9612        879.5872        328.3062       0       0
   13160        822.2319        879.5872        326.0808       0       0
   13161        817.4136        879.5872        324.0553       0       0
   13162        812.5148        879.5872        322.2335       0       0
   13163         807.544        879.5872        320.6184       0       0
   13164        802.5099        879.5872        319.2128       0       0
   13165        797.4214        879.5872        318.0193       0       0
   13166        792.2873        879.5872          317.04       0       0
   13167        787.1168        879.5872        316.2764       0       0
   13168        781.9187        879.5872        315.7301       0       0
   13169        776.7025        879.5872         315.402       0       0
   13170        771.4769        879.5872        315.2925       0       0
   13171        766.2514        879.5872         315.402       0       0
   13172        761.0352        879.5872        315.7301       0       0
   13173        755.8372        879.5872        316.2764       0       0
   13174        750.6666        879.5872          317.04       0       0
   13175        745.5325        879.5872        318.0194       0       0
   13176         740.444        879.5872        319.2129       0       0
   13177        735.4099        879.5872        320.6184       0       0
   13178         730.439        879.5872        322.2336       0       0
   13179        725.5402        879.5872        324.0555       0       0
   13180         720.722        879.5872        326.0809       0       0
   13181        715.9927        879.5872        328.3062       0       0
   13182        711.3609        879.5872        330.7277       0       0
   13183        706.8346        879.5872         333.341       0       0
   13184        702.4215        879.5872        336.1416       0       0
   13185        698.1297        879.5872        339.1245       0       0
   13186        693.9665        879.5872        342.2846       0       0
   13187        689.9393        879.5872        345.6162       0       0
   13188        686.0553        879.5872        349.1135       0       0
   13189        682.3209        879.5872        352.7704       0       0
   13190        678.7431        879.5872        356.5805       0       0
   13191        675.3279        879.5872         360.537       0       0
   13192        672.0814        879.5872         364.633       0       0
   13193        669.0093        879.5872        368.8614       0       0
   13194        666.1171        879.5872        373.2148       0       0
   13195        663.4094        879.5872        377.6854       0       0
   13196        660.8915        879.5872        382.2655       0       0
   13197        658.5676        879.5872        386.9471       0       0
   13198        656.4417        879.5872        391.7219       0       0
   13199        654.5176        879.5872        396.5814       0       0
   13200        652.7989        879.5872        401.5173       0       0
   13201        651.2883        879.5872        406.5208       0       0
   13202        649.9883        879.5872        411.5833       0       0
   13203        648.9017        879.5872        416.6956       0       0
   13204        648.0301        879.5872        421.8491       0       0
   13205        647.3751        879.5872        427.0344       0       0
   13206        646.9376        879.5872        432.2427       0       0
   13207        646.7188        879.5872        437.4648       0       0
   13208        646.7189        879.5872        442.6913       0       0
   13209        646.9376        879.5872        447.9133       0       0
   13210        647.3751        879.5872        453.1217       0       0
   13211        648.0302        879.5872         458.307       0       0
   13212        648.9017        879.5872        463.4605       0       0
   13213        649.9883        879.5872        468.5728       0       0
   13214        651.2881        879.5872        473.6352       0       0
   13215        652.7989        879.5872        478.6387       0       0
   13216        654.5176        879.5872        483.5746       0       0
   13217        656.4418        879.5872        488.4341       0       0
   13218        658.5677        879.5872         493.209       0       0
   13219        660.8915        879.5872        497.8905       0       0
   13220        663.4094        879.5872        502.4706       0       0
   13221        666.1171        879.5872        506.9412       0       0
   13222        669.0093        879.5872        511.2946       0       0
   13223        672.0814        879.5872         515.523       0       0
   13224        675.3279        879.5872         519.619       0       0
   13225        678.7431        879.5872        523.5756       0       0
   13226        682.3209        879.5872        527.3856       0       0
   13227        686.0552        879.5872        531.0425       0       0
   13228        689.9393        879.5872        534.5398       0       0
   13229        693.9665        879.5872        537.8715       0       0
   13230        698.1296        879.5872        541.0314       0       0
   13231        702.4215        879.5872        544.0143       0       0
   13232        706.8344        879.5872        546.8148       0       0
   13233        711.3607        879.5872        549.4282       0       0
   13234        715.9924        879.5872        551.8497       0       0
   13235        720.7217        879.5872         554.075       0       0
   13236        725.5399        879.5872        556.1005       0       0
   13237        730.4387        879.5872        557.9222       0       0
   13238        735.4095        879.5872        559.5375       0       0
   13239        740.4436        879.5872         560.943       0       0
   13240         745.532        879.5872        562.1365       0       0
   13241        750.6661        879.5872         563.116       0       0
   13242        755.8366        879.5872        563.8795       0       0
   13243        761.0346        879.5872        564.4258       0       0
   13244        766.2509        879.5872         564.754       0       0
   13245        771.4764        879.5872        564.8635       0       0
   13246        771.4768        886.4896        569.2855       0       0
   13247        776.8875        886.4896        569.1722       0       0
   13248        782.2886        886.4896        568.8324       0       0
   13249        787.6708        886.4896        568.2667       0       0
   13250        793.0246        886.4896        567.4762       0       0
   13251        798.3405        886.4896        566.4622       0       0
   13252        803.6094        886.4896        565.2264       0       0
   13253         808.822        886.4896        563.7709       0       0
   13254        813.9689        886.4896        562.0987       0       0
   13255        819.0412        886.4896        560.2122       0       0
   13256        824.0303        886.4896        558.1151       0       0
   13257        828.9269        886.4896        555.8108       0       0
   13258         833.723        886.4896        553.3035       0       0
   13259        838.4098        886.4896        550.5977       0       0
   13260        842.9792        886.4896        547.6978       0       0
   13261        847.4232        886.4896        544.6092       0       0
   13262        851.7339        886.4896        541.3372       0       0
   13263        855.9037        886.4896        537.8876       0       0
   13264        859.9255        886.4896        534.2665       0       0
   13265        863.7921        886.4896        530.4799       0       0
   13266        867.4968        886.4896         526.535       0       0
   13267         871.033        886.4896        522.4381       0       0
   13268        874.3945        886.4896         518.197       0       0
   13269        877.5756        886.4896        513.8186       0       0
   13270        880.5704        886.4896        509.3111       0       0
   13271        883.3738        886.4896        504.6819       0       0
   13272        885.9812        886.4896        499.9396       0       0
   13273        888.3873        886.4896        495.0921       0       0
   13274        890.5886        886.4896        490.1482       0       0
   13275        892.5807        886.4896        485.1164       0       0
   13276        894.3606        886.4896        480.0056       0       0
   13277        895.9247        886.4896        474.8247       0       0
   13278        897.2706        886.4896        469.5829       0       0
   13279        898.3958        886.4896        464.2893       0       0
   13280        899.2984        886.4896        458.9533       0       0
   13281        899.9768        886.4896        453.5841       0       0
   13282        900.4296        886.4896        448.1913       0       0
   13283        900.6563        886.4896        442.7842       0       0
   13284        900.6563        886.4896        437.3724       0       0
   13285        900.4296        886.4896        431.9652       0       0
   13286        899.9768        886.4896        426.5724       0       0
   13287        899.2985        886.4896        421.2032       0       0
   13288         898.396        886.4896        415.8672       0       0
   13289        897.2709        886.4896        410.5736       0       0
   13290        895.9249        886.4896        405.3318       0       0
   13291        894.3608        886.4896        400.1509       0       0
   13292         892.581        886.4896        395.0401       0       0
   13293        890.5887        886.4896        390.0084       0       0
   13294        888.3876        886.4896        385.0643       0       0
   13295        885.9813        886.4896        380.2169       0       0
   13296        883.3741        886.4896        375.4745       0       0
   13297        880.5707        886.4896        370.8453       0       0
   13298        877.5758        886.4896        366.3377       0       0
   13299        874.3948        886.4896        361.9594       0       0
   13300        871.0333        886.4896        357.7182       0       0
   13301        867.4971        886.4896        353.6214       0       0
   13302        863.7923        886.4896        349.6763       0       0
   13303        859.9258        886.4896        345.8899       0       0
   13304        855.9039        886.4896        342.2687       0       0
   13305        851.7341        886.4896        338.8189       0       0
   13306        847.4233        886.4896         335.547       0       0
   13307        842.9794        886.4896        332.4583       0       0
   13308        838.4102        886.4896        329.5585       0       0
   13309        833.7233        886.4896        326.8526       0       0
   13310        828.9272        886.4896        324.3453       0       0
   13311        824.0306        886.4896         322.041       0       0
   13312        819.0415        886.4896        319.9438       0       0
   13313        813.9691        886.4896        318.0575       0       0
   13314         808.822        886.4896        316.3851       0       0
   13315        803.6097        886.4896        314.9297       0       0
   13316        798.3408        886.4896         313.694       0       0
   13317        793.0247        886.4896        312.6799       0       0
   13318         787.671        886.4896        311.8893       0       0
   13319        782.2888        886.4896        311.3236       0       0
   13320        776.8876        886.4896        310.9838       0       0
   13321        771.4769        886.4896        310.8704       0       0
   13322        766.0663        886.4896        310.9839       0       0
   13323        760.6651        886.4896        311.3236       0       0
   13324        755.2829        886.4896        311.8893       0       0
   13325        749.9291        886.4896        312.6799       0       0
   13326         744.613        886.4896         313.694       0       0
   13327        739.3442        886.4896        314.9298       0       0
   13328        734.1318        886.4896        316.3851       0       0
   13329        728.9848        886.4896        318.0576       0       0
   13330        723.9124        886.4896        319.9439       0       0
   13331        718.9235        886.4896        322.0411       0       0
   13332        714.0266        886.4896        324.3453       0       0
   13333        709.2306        886.4896        326.8527       0       0
   13334        704.5438        886.4896        329.5585       0       0
   13335        699.9744        886.4896        332.4584       0       0
   13336        695.5305        886.4896         335.547       0       0
   13337        691.2198        886.4896         338.819       0       0
   13338        687.0499        886.4896        342.2687       0       0
   13339        683.0281        886.4896          345.89       0       0
   13340        679.1615        886.4896        349.6764       0       0
   13341        675.4568        886.4896        353.6215       0       0
   13342        671.9206        886.4896        357.7183       0       0
   13343        668.5591        886.4896        361.9595       0       0
   13344        665.3782        886.4896        366.3377       0       0
   13345        662.3833        886.4896        370.8454       0       0
   13346        659.5798        886.4896        375.4745       0       0
   13347        656.9726        886.4896        380.2169       0       0
   13348        654.5665        886.4896        385.0643       0       0
   13349        652.3652        886.4896        390.0082       0       0
   13350         650.373        886.4896          395.04       0       0
   13351        648.5932        886.4896        400.1508       0       0
   13352        647.0291        886.4896        405.3317       0       0
   13353        645.6832        886.4896        410.5735       0       0
   13354        644.5581        886.4896        415.8671       0       0
   13355        643.6555        886.4896        421.2031       0       0
   13356        642.9772        886.4896        426.5722       0       0
   13357        642.5244        886.4896         431.965       0       0
   13358        642.2977        886.4896        437.3721       0       0
   13359        642.2977        886.4896         442.784       0       0
   13360        642.5244        886.4896         448.191       0       0
   13361        642.9773        886.4896        453.5838       0       0
   13362        643.6556        886.4896         458.953       0       0
   13363         644.558        886.4896         464.289       0       0
   13364        645.6832        886.4896        469.5826       0       0
   13365        647.0291        886.4896        474.8244       0       0
   13366        648.5932        886.4896        480.0052       0       0
   13367         650.373        886.4896         485.116       0       0
   13368        652.3653        886.4896        490.1477       0       0
   13369        654.5665        886.4896        495.0918       0       0
   13370        656.9727        886.4896        499.9391       0       0
   13371        659.5798        886.4896        504.6816       0       0
   13372        662.3833        886.4896        509.3107       0       0
   13373        665.3781        886.4896        513.8182       0       0
   13374        668.5591        886.4896        518.1967       0       0
   13375        671.9206        886.4896        522.4377       0       0
   13376        675.4568        886.4896        526.5345       0       0
   13377        679.1615        886.4896        530.4796       0       0
   13378        683.0281        886.4896         534.266       0       0
   13379        687.0499        886.4896        537.8873       0       0
   13380        691.2197        886.4896        541.3369       0       0
   13381        695.5303        886.4896        544.6089       0       0
   13382        699.9742        886.4896        547.6974       0       0
   13383        704.5436        886.4896        550.5972       0       0
   13384        709.2304        886.4896        553.3033       0       0
   13385        714.0263        886.4896        555.8106       0       0
   13386        718.9232        886.4896        558.1149       0       0
   13387        723.9121        886.4896         560.212       0       0
   13388        728.9844        886.4896        562.0984       0       0
   13389        734.1313        886.4896        563.7708       0       0
   13390        739.3438        886.4896        565.2261       0       0
   13391        744.6127        886.4896        566.4619       0       0
   13392        749.9285        886.4896         567.476       0       0
   13393        755.2823        886.4896        568.2667       0       0
   13394        760.6646        886.4896        568.8324       0       0
   13395        766.0657        886.4896        569.1722       0       0
   13396        771.4763        886.4896        569.2855       0       0
   13397        771.4768        893.8782        572.7952       0       0
   13398        777.0344        893.8782        572.6787       0       0
   13399        782.5824        893.8782        572.3297       0       0
   13400        788.1107        893.8782        571.7487       0       0
   13401        793.6099        893.8782        570.9366       0       0
   13402        799.0702        893.8782         569.895       0       0
   13403        804.4822        893.8782        568.6257       0       0
   13404        809.8364        893.8782        567.1308       0       0
   13405         815.123        893.8782        565.4129       0       0
   13406        820.3333        893.8782        563.4753       0       0
   13407        825.4578        893.8782        561.3212       0       0
   13408        830.4874        893.8782        558.9544       0       0
   13409        835.4139        893.8782         556.379       0       0
   13410        840.2279        893.8782        553.5996       0       0
   13411        844.9214        893.8782        550.6211       0       0
   13412        849.4861        893.8782        547.4486       0       0
   13413        853.9138        893.8782        544.0877       0       0
   13414         858.197        893.8782        540.5444       0       0
   13415        862.3279        893.8782        536.8248       0       0
   13416        866.2997        893.8782        532.9354       0       0
   13417        870.1049        893.8782        528.8833       0       0
   13418        873.7372        893.8782        524.6752       0       0
   13419          877.19        893.8782        520.3188       0       0
   13420        880.4575        893.8782        515.8217       0       0
   13421        883.5337        893.8782        511.1916       0       0
   13422        886.4134        893.8782        506.4367       0       0
   13423        889.0914        893.8782        501.5655       0       0
   13424         891.563        893.8782        496.5864       0       0
   13425         893.824        893.8782        491.5082       0       0
   13426        895.8704        893.8782        486.3397       0       0
   13427        897.6984        893.8782        481.0901       0       0
   13428        899.3051        893.8782        475.7685       0       0
   13429        900.6875        893.8782        470.3843       0       0
   13430        901.8433        893.8782        464.9469       0       0
   13431        902.7704        893.8782        459.4659       0       0
   13432        903.4672        893.8782         453.951       0       0
   13433        903.9323        893.8782        448.4116       0       0
   13434        904.1652        893.8782        442.8577       0       0
   13435        904.1652        893.8782        437.2988       0       0
   13436        903.9323        893.8782        431.7449       0       0
   13437        903.4672        893.8782        426.2055       0       0
   13438        902.7704        893.8782        420.6906       0       0
   13439        901.8434        893.8782        415.2096       0       0
   13440        900.6877        893.8782        409.7722       0       0
   13441        899.3052        893.8782         404.388       0       0
   13442        897.6986        893.8782        399.0664       0       0
   13443        895.8705        893.8782        393.8168       0       0
   13444        893.8242        893.8782        388.6483       0       0
   13445        891.5632        893.8782          383.57       0       0
   13446        889.0914        893.8782        378.5909       0       0
   13447        886.4136        893.8782        373.7197       0       0
   13448        883.5339        893.8782        368.9648       0       0
   13449        880.4578        893.8782        364.3347       0       0
   13450        877.1903        893.8782        359.8375       0       0
   13451        873.7375        893.8782         355.481       0       0
   13452        870.1052        893.8782         351.273       0       0
   13453        866.2999        893.8782        347.2208       0       0
   13454        862.3282        893.8782        343.3315       0       0
   13455        858.1972        893.8782        339.6119       0       0
   13456         853.914        893.8782        336.0685       0       0
   13457        849.4863        893.8782        332.7076       0       0
   13458        844.9216        893.8782        329.5351       0       0
   13459        840.2282        893.8782        326.5565       0       0
   13460        835.4141        893.8782        323.7771       0       0
   13461        830.4878        893.8782        321.2017       0       0
   13462        825.4579        893.8782        318.8349       0       0
   13463        820.3334        893.8782        316.6808       0       0
   13464        815.1234        893.8782         314.743       0       0
   13465        809.8365        893.8782        313.0253       0       0
   13466        804.4825        893.8782        311.5304       0       0
   13467        799.0705        893.8782         310.261       0       0
   13468          793.61        893.8782        309.2193       0       0
   13469        788.1109        893.8782        308.4073       0       0
   13470        782.5825        893.8782        307.8263       0       0
   13471        777.0345        893.8782        307.4773       0       0
   13472        771.4769        893.8782        307.3608       0       0
   13473        765.9194        893.8782        307.4773       0       0
   13474        760.3714        893.8782        307.8263       0       0
   13475         754.843        893.8782        308.4073       0       0
   13476        749.3438        893.8782        309.2195       0       0
   13477        743.8833        893.8782         310.261       0       0
   13478        738.4714        893.8782        311.5305       0       0
   13479        733.1174        893.8782        313.0253       0       0
   13480        727.8306        893.8782        314.7432       0       0
   13481        722.6205        893.8782        316.6808       0       0
   13482        717.4958        893.8782        318.8349       0       0
   13483        712.4662        893.8782        321.2018       0       0
   13484        707.5399        893.8782        323.7772       0       0
   13485        702.7257        893.8782        326.5566       0       0
   13486        698.0322        893.8782        329.5352       0       0
   13487        693.4675        893.8782        332.7077       0       0
   13488        689.0398        893.8782        336.0686       0       0
   13489        684.7566        893.8782         339.612       0       0
   13490        680.6256        893.8782        343.3316       0       0
   13491        676.6539        893.8782        347.2209       0       0
   13492        672.8487        893.8782        351.2731       0       0
   13493        669.2165        893.8782        355.4811       0       0
   13494        665.7636        893.8782        359.8376       0       0
   13495        662.4962        893.8782        364.3347       0       0
   13496          659.42        893.8782        368.9648       0       0
   13497        656.5405        893.8782        373.7197       0       0
   13498        653.8624        893.8782        378.5909       0       0
   13499        651.3908        893.8782          383.57       0       0
   13500        649.1298        893.8782        388.6482       0       0
   13501        647.0835        893.8782        393.8167       0       0
   13502        645.2554        893.8782        399.0664       0       0
   13503        643.6488        893.8782        404.3879       0       0
   13504        642.2664        893.8782        409.7721       0       0
   13505        641.1106        893.8782        415.2094       0       0
   13506        640.1835        893.8782        420.6903       0       0
   13507        639.4868        893.8782        426.2054       0       0
   13508        639.0216        893.8782        431.7447       0       0
   13509        638.7889        893.8782        437.2986       0       0
   13510        638.7889        893.8782        442.8574       0       0
   13511        639.0217        893.8782        448.4114       0       0
   13512        639.4869        893.8782        453.9507       0       0
   13513        640.1835        893.8782        459.4657       0       0
   13514        641.1105        893.8782        464.9466       0       0
   13515        642.2664        893.8782         470.384       0       0
   13516        643.6488        893.8782        475.7682       0       0
   13517        645.2554        893.8782        481.0898       0       0
   13518        647.0835        893.8782        486.3394       0       0
   13519        649.1299        893.8782        491.5078       0       0
   13520        651.3909        893.8782        496.5861       0       0
   13521        653.8625        893.8782        501.5652       0       0
   13522        656.5405        893.8782        506.4365       0       0
   13523          659.42        893.8782        511.1912       0       0
   13524        662.4962        893.8782        515.8214       0       0
   13525        665.7636        893.8782        520.3185       0       0
   13526        669.2165        893.8782        524.6749       0       0
   13527        672.8487        893.8782         528.883       0       0
   13528        676.6539        893.8782        532.9351       0       0
   13529        680.6256        893.8782        536.8244       0       0
   13530        684.7566        893.8782         540.544       0       0
   13531        689.0398        893.8782        544.0874       0       0
   13532        693.4675        893.8782        547.4482       0       0
   13533        698.0321        893.8782        550.6207       0       0
   13534        702.7255        893.8782        553.5993       0       0
   13535        707.5396        893.8782        556.3787       0       0
   13536        712.4658        893.8782        558.9541       0       0
   13537        717.4956        893.8782         561.321       0       0
   13538        722.6201        893.8782        563.4752       0       0
   13539        727.8301        893.8782        565.4128       0       0
   13540        733.1169        893.8782        567.1305       0       0
   13541        738.4711        893.8782        568.6255       0       0
   13542        743.8829        893.8782        569.8948       0       0
   13543        749.3433        893.8782        570.9365       0       0
   13544        754.8425        893.8782        571.7486       0       0
   13545        760.3708        893.8782        572.3297       0       0
   13546        765.9187        893.8782        572.6787       0       0
   13547        771.4763        893.8782        572.7952       0       0
   13548        771.4768        901.6354        575.3475       0       0
   13549        777.1413        901.6354        575.2289       0       0
   13550        782.7958        901.6354        574.8732       0       0
   13551        788.4305        901.6354        574.2809       0       0
   13552        794.0355        901.6354        573.4532       0       0
   13553         799.601        901.6354        572.3915       0       0
   13554        805.1169        901.6354        571.0979       0       0
   13555         810.574        901.6354        569.5742       0       0
   13556        815.9625        901.6354        567.8234       0       0
   13557        821.2728        901.6354        565.8484       0       0
   13558        826.4959        901.6354         563.653       0       0
   13559        831.6224        901.6354        561.2405       0       0
   13560        836.6434        901.6354        558.6157       0       0
   13561          841.55        901.6354         555.783       0       0
   13562        846.3339        901.6354        552.7469       0       0
   13563        850.9863        901.6354        549.5135       0       0
   13564        855.4991        901.6354        546.0879       0       0
   13565        859.8648        901.6354        542.4765       0       0
   13566        864.0753        901.6354        538.6854       0       0
   13567        868.1232        901.6354        534.7213       0       0
   13568        872.0016        901.6354        530.5911       0       0
   13569        875.7039        901.6354        526.3022       0       0
   13570        879.2231        901.6354        521.8621       0       0
   13571        882.5534        901.6354        517.2783       0       0
   13572        885.6887        901.6354        512.5592       0       0
   13573        888.6237        901.6354         507.713       0       0
   13574        891.3532        901.6354         502.748       0       0
   13575        893.8724        901.6354        497.6732       0       0
   13576        896.1769        901.6354        492.4973       0       0
   13577        898.2626        901.6354        487.2294       0       0
   13578        900.1258        901.6354        481.8788       0       0
   13579        901.7635        901.6354        476.4549       0       0
   13580        903.1725        901.6354        470.9672       0       0
   13581        904.3505        901.6354        465.4252       0       0
   13582        905.2954        901.6354        459.8389       0       0
   13583        906.0055        901.6354        454.2177       0       0
   13584        906.4796        901.6354        448.5718       0       0
   13585        906.7169        901.6354        442.9111       0       0
   13586        906.7169        901.6354        437.2454       0       0
   13587        906.4796        901.6354        431.5847       0       0
   13588        906.0055        901.6354        425.9388       0       0
   13589        905.2954        901.6354        420.3177       0       0
   13590        904.3505        901.6354        414.7313       0       0
   13591        903.1725        901.6354        409.1894       0       0
   13592        901.7636        901.6354        403.7016       0       0
   13593         900.126        901.6354        398.2777       0       0
   13594        898.2628        901.6354        392.9271       0       0
   13595        896.1771        901.6354        387.6592       0       0
   13596        893.8726        901.6354        382.4833       0       0
   13597        891.3535        901.6354        377.4084       0       0
   13598        888.6239        901.6354        372.4434       0       0
   13599         885.689        901.6354        367.5971       0       0
   13600        882.5536        901.6354        362.8781       0       0
   13601        879.2234        901.6354        358.2943       0       0
   13602         875.704        901.6354        353.8541       0       0
   13603         872.002        901.6354        349.5652       0       0
   13604        868.1235        901.6354         345.435       0       0
   13605        864.0755        901.6354        341.4708       0       0
   13606         859.865        901.6354        337.6798       0       0
   13607        855.4995        901.6354        334.0683       0       0
   13608        850.9865        901.6354        330.6427       0       0
   13609         846.334        901.6354        327.4092       0       0
   13610        841.5504        901.6354        324.3734       0       0
   13611        836.6437        901.6354        321.5404       0       0
   13612        831.6227        901.6354        318.9156       0       0
   13613        826.4961        901.6354        316.5032       0       0
   13614        821.2731        901.6354        314.3076       0       0
   13615        815.9626        901.6354        312.3327       0       0
   13616        810.5742        901.6354        310.5818       0       0
   13617        805.1171        901.6354        309.0582       0       0
   13618        799.6011        901.6354        307.7644       0       0
   13619        794.0357        901.6354        306.7028       0       0
   13620        788.4307        901.6354        305.8752       0       0
   13621        782.7959        901.6354        305.2829       0       0
   13622        777.1414        901.6354        304.9271       0       0
   13623        771.4769        901.6354        304.8084       0       0
   13624        765.8124        901.6354        304.9271       0       0
   13625        760.1579        901.6354        305.2829       0       0
   13626        754.5231        901.6354        305.8752       0       0
   13627        748.9182        901.6354        306.7028       0       0
   13628        743.3527        901.6354        307.7645       0       0
   13629        737.8367        901.6354        309.0583       0       0
   13630        732.3796        901.6354        310.5818       0       0
   13631        726.9913        901.6354        312.3327       0       0
   13632        721.6808        901.6354        314.3076       0       0
   13633        716.4578        901.6354        316.5033       0       0
   13634        711.3313        901.6354        318.9156       0       0
   13635        706.3102        901.6354        321.5405       0       0
   13636        701.4036        901.6354        324.3734       0       0
   13637        696.6198        901.6354        327.4093       0       0
   13638        691.9673        901.6354        330.6428       0       0
   13639        687.4544        901.6354        334.0683       0       0
   13640        683.0889        901.6354        337.6798       0       0
   13641        678.8783        901.6354        341.4709       0       0
   13642        674.8304        901.6354        345.4352       0       0
   13643        670.9519        901.6354        349.5653       0       0
   13644        667.2499        901.6354        353.8542       0       0
   13645        663.7307        901.6354        358.2944       0       0
   13646        660.4003        901.6354        362.8781       0       0
   13647        657.2651        901.6354        367.5972       0       0
   13648          654.33        901.6354        372.4435       0       0
   13649        651.6005        901.6354        377.4084       0       0
   13650        649.0814        901.6354        382.4833       0       0
   13651        646.7769        901.6354        387.6592       0       0
   13652        644.6912        901.6354         392.927       0       0
   13653         642.828        901.6354        398.2776       0       0
   13654        641.1905        901.6354        403.7015       0       0
   13655        639.7813        901.6354        409.1892       0       0
   13656        638.6035        901.6354        414.7311       0       0
   13657        637.6586        901.6354        420.3175       0       0
   13658        636.9485        901.6354        425.9385       0       0
   13659        636.4744        901.6354        431.5845       0       0
   13660        636.2372        901.6354        437.2451       0       0
   13661        636.2372        901.6354        442.9108       0       0
   13662        636.4744        901.6354        448.5716       0       0
   13663        636.9485        901.6354        454.2176       0       0
   13664        637.6586        901.6354        459.8386       0       0
   13665        638.6035        901.6354        465.4249       0       0
   13666        639.7813        901.6354        470.9669       0       0
   13667        641.1905        901.6354        476.4546       0       0
   13668         642.828        901.6354        481.8785       0       0
   13669        644.6912        901.6354         487.229       0       0
   13670        646.7769        901.6354        492.4969       0       0
   13671        649.0814        901.6354        497.6728       0       0
   13672        651.6006        901.6354        502.7477       0       0
   13673        654.3301        901.6354        507.7126       0       0
   13674        657.2651        901.6354        512.5589       0       0
   13675        660.4005        901.6354         517.278       0       0
   13676        663.7307        901.6354        521.8617       0       0
   13677        667.2499        901.6354        526.3019       0       0
   13678        670.9519        901.6354        530.5908       0       0
   13679        674.8304        901.6354        534.7209       0       0
   13680        678.8783        901.6354        538.6851       0       0
   13681        683.0889        901.6354        542.4761       0       0
   13682        687.4544        901.6354        546.0876       0       0
   13683        691.9672        901.6354        549.5131       0       0
   13684        696.6198        901.6354        552.7466       0       0
   13685        701.4033        901.6354        555.7824       0       0
   13686        706.3101        901.6354        558.6154       0       0
   13687         711.331        901.6354        561.2404       0       0
   13688        716.4575        901.6354        563.6527       0       0
   13689        721.6805        901.6354        565.8481       0       0
   13690        726.9908        901.6354        567.8232       0       0
   13691        732.3793        901.6354         569.574       0       0
   13692        737.8363        901.6354        571.0977       0       0
   13693        743.3522        901.6354        572.3915       0       0
   13694        748.9177        901.6354        573.4531       0       0
   13695        754.5225        901.6354        574.2808       0       0
   13696        760.1573        901.6354         574.873       0       0
   13697        765.8118        901.6354        575.2289       0       0
   13698        771.4763        901.6354        575.3475       0       0
   13699        771.4768        909.6412        576.9019       0       0
   13700        777.2065        909.6412        576.7819       0       0
   13701        782.9259        909.6412        576.4221       0       0
   13702        788.6254        909.6412        575.8231       0       0
   13703        794.2948        909.6412        574.9858       0       0
   13704        799.9241        909.6412        573.9121       0       0
   13705        805.5036        909.6412        572.6034       0       0
   13706        811.0233        909.6412        571.0623       0       0
   13707        816.4736        909.6412        569.2914       0       0
   13708         821.845        909.6412        567.2938       0       0
   13709        827.1281        909.6412        565.0729       0       0
   13710        832.3136        909.6412        562.6328       0       0
   13711        837.3923        909.6412        559.9777       0       0
   13712        842.3553        909.6412        557.1124       0       0
   13713        847.1941        909.6412        554.0417       0       0
   13714           851.9        909.6412        550.7711       0       0
   13715        856.4647        909.6412        547.3062       0       0
   13716        860.8804        909.6412        543.6532       0       0
   13717        865.1392        909.6412        539.8185       0       0
   13718        869.2338        909.6412        535.8089       0       0
   13719        873.1568        909.6412        531.6313       0       0
   13720        876.9016        909.6412         527.293       0       0
   13721        880.4612        909.6412        522.8017       0       0
   13722        883.8297        909.6412        518.1654       0       0
   13723        887.0012        909.6412        513.3921       0       0
   13724        889.9698        909.6412        508.4901       0       0
   13725        892.7308        909.6412        503.4681       0       0
   13726        895.2789        909.6412         498.335       0       0
   13727        897.6099        909.6412        493.0996       0       0
   13728        899.7196        909.6412        487.7712       0       0
   13729        901.6042        909.6412        482.3591       0       0
   13730        903.2606        909.6412        476.8729       0       0
   13731        904.6859        909.6412        471.3221       0       0
   13732        905.8773        909.6412        465.7165       0       0
   13733        906.8331        909.6412        460.0659       0       0
   13734        907.5513        909.6412        454.3802       0       0
   13735        908.0309        909.6412        448.6695       0       0
   13736        908.2709        909.6412        442.9437       0       0
   13737        908.2709        909.6412        437.2128       0       0
   13738        908.0309        909.6412         431.487       0       0
   13739        907.5514        909.6412        425.7763       0       0
   13740        906.8331        909.6412        420.0906       0       0
   13741        905.8775        909.6412          414.44       0       0
   13742        904.6859        909.6412        408.8344       0       0
   13743        903.2608        909.6412        403.2836       0       0
   13744        901.6044        909.6412        397.7974       0       0
   13745        899.7196        909.6412        392.3853       0       0
   13746          897.61        909.6412        387.0569       0       0
   13747        895.2791        909.6412        381.8214       0       0
   13748        892.7309        909.6412        376.6883       0       0
   13749        889.9701        909.6412        371.6663       0       0
   13750        887.0014        909.6412        366.7643       0       0
   13751          883.83        909.6412        361.9909       0       0
   13752        880.4615        909.6412        357.3546       0       0
   13753        876.9017        909.6412        352.8633       0       0
   13754        873.1571        909.6412         348.525       0       0
   13755        869.2341        909.6412        344.3474       0       0
   13756        865.1395        909.6412        340.3378       0       0
   13757        860.8806        909.6412         336.503       0       0
   13758         856.465        909.6412        332.8501       0       0
   13759        851.9001        909.6412        329.3852       0       0
   13760        847.1942        909.6412        326.1145       0       0
   13761        842.3556        909.6412        323.0437       0       0
   13762        837.3925        909.6412        320.1783       0       0
   13763        832.3138        909.6412        317.5233       0       0
   13764        827.1284        909.6412        315.0831       0       0
   13765        821.8453        909.6412        312.8624       0       0
   13766        816.4739        909.6412        310.8647       0       0
   13767        811.0234        909.6412        309.0938       0       0
   13768        805.5037        909.6412        307.5527       0       0
   13769        799.9243        909.6412         306.244       0       0
   13770        794.2949        909.6412        305.1702       0       0
   13771        788.6255        909.6412        304.3329       0       0
   13772        782.9261        909.6412        303.7339       0       0
   13773        777.2065        909.6412         303.374       0       0
   13774        771.4769        909.6412        303.2541       0       0
   13775        765.7473        909.6412        303.3741       0       0
   13776        760.0278        909.6412        303.7339       0       0
   13777        754.3284        909.6412         304.333       0       0
   13778        748.6589        909.6412        305.1702       0       0
   13779        743.0295        909.6412        306.2441       0       0
   13780        737.4501        909.6412        307.5527       0       0
   13781        731.9304        909.6412        309.0938       0       0
   13782          726.48        909.6412        310.8648       0       0
   13783        721.1086        909.6412        312.8624       0       0
   13784        715.8255        909.6412        315.0832       0       0
   13785          710.64        909.6412        317.5233       0       0
   13786        705.5613        909.6412        320.1784       0       0
   13787        700.5983        909.6412        323.0438       0       0
   13788        695.7595        909.6412        326.1146       0       0
   13789        691.0537        909.6412        329.3853       0       0
   13790        686.4888        909.6412        332.8502       0       0
   13791        682.0732        909.6412        336.5031       0       0
   13792        677.8143        909.6412        340.3379       0       0
   13793        673.7198        909.6412        344.3475       0       0
   13794        669.7968        909.6412        348.5251       0       0
   13795        666.0522        909.6412        352.8634       0       0
   13796        662.4924        909.6412        357.3547       0       0
   13797         659.124        909.6412         361.991       0       0
   13798        655.9526        909.6412        366.7643       0       0
   13799        652.9838        909.6412        371.6663       0       0
   13800         650.223        909.6412        376.6883       0       0
   13801        647.6749        909.6412        381.8214       0       0
   13802        645.3439        909.6412        387.0568       0       0
   13803        643.2344        909.6412        392.3852       0       0
   13804        641.3496        909.6412        397.7973       0       0
   13805        639.6932        909.6412        403.2834       0       0
   13806        638.2681        909.6412        408.8343       0       0
   13807        637.0766        909.6412        414.4399       0       0
   13808        636.1207        909.6412        420.0905       0       0
   13809        635.4026        909.6412        425.7761       0       0
   13810        634.9231        909.6412        431.4868       0       0
   13811         634.683        909.6412        437.2126       0       0
   13812         634.683        909.6412        442.9435       0       0
   13813        634.9231        909.6412        448.6693       0       0
   13814        635.4026        909.6412        454.3799       0       0
   13815        636.1208        909.6412        460.0656       0       0
   13816        637.0766        909.6412        465.7162       0       0
   13817        638.2681        909.6412        471.3218       0       0
   13818        639.6932        909.6412        476.8726       0       0
   13819        641.3497        909.6412        482.3588       0       0
   13820        643.2344        909.6412        487.7708       0       0
   13821         645.344        909.6412        493.0992       0       0
   13822        647.6749        909.6412        498.3347       0       0
   13823        650.2231        909.6412        503.4678       0       0
   13824        652.9839        909.6412        508.4898       0       0
   13825        655.9526        909.6412        513.3917       0       0
   13826         659.124        909.6412        518.1651       0       0
   13827        662.4924        909.6412        522.8014       0       0
   13828        666.0522        909.6412        527.2927       0       0
   13829        669.7968        909.6412        531.6309       0       0
   13830        673.7198        909.6412        535.8085       0       0
   13831        677.8143        909.6412        539.8181       0       0
   13832        682.0732        909.6412        543.6528       0       0
   13833        686.4888        909.6412        547.3057       0       0
   13834        691.0537        909.6412        550.7706       0       0
   13835        695.7594        909.6412        554.0414       0       0
   13836        700.5981        909.6412        557.1121       0       0
   13837        705.5611        909.6412        559.9775       0       0
   13838        710.6398        909.6412        562.6326       0       0
   13839        715.8252        909.6412        565.0727       0       0
   13840        721.1083        909.6412        567.2935       0       0
   13841        726.4796        909.6412        569.2911       0       0
   13842        731.9299        909.6412         571.062       0       0
   13843        737.4497        909.6412        572.6032       0       0
   13844        743.0291        909.6412        573.9119       0       0
   13845        748.6583        909.6412        574.9858       0       0
   13846        754.3279        909.6412         575.823       0       0
   13847        760.0272        909.6412        576.4221       0       0
   13848        765.7467        909.6412        576.7819       0       0
   13849        771.4763        909.6412        576.9019       0       0
   13850        771.4768        917.7792        577.4108       0       0
   13851        777.2277        917.7792        577.2903       0       0
   13852        782.9686        917.7792        576.9291       0       0
   13853        788.6892        917.7792        576.3279       0       0
   13854        794.3796        917.7792        575.4876       0       0
   13855        800.0298        917.7792        574.4098       0       0
   13856        805.6301        917.7792        573.0962       0       0
   13857        811.1703        917.7792        571.5494       0       0
   13858        816.6409        917.7792        569.7719       0       0
   13859        822.0323        917.7792        567.7668       0       0
   13860        827.3351        917.7792        565.5378       0       0
   13861        832.5399        917.7792        563.0886       0       0
   13862        837.6373        917.7792        560.4238       0       0
   13863        842.6188        917.7792        557.5477       0       0
   13864        847.4756        917.7792        554.4655       0       0
   13865         852.199        917.7792        551.1827       0       0
   13866        856.7808        917.7792         547.705       0       0
   13867         861.213        917.7792        544.0383       0       0
   13868        865.4875        917.7792        540.1893       0       0
   13869        869.5974        917.7792        536.1649       0       0
   13870         873.535        917.7792        531.9717       0       0
   13871        877.2936        917.7792        527.6173       0       0
   13872        880.8665        917.7792        523.1094       0       0
   13873        884.2476        917.7792        518.4558       0       0
   13874        887.4307        917.7792        513.6648       0       0
   13875        890.4105        917.7792        508.7446       0       0
   13876        893.1816        917.7792        503.7039       0       0
   13877        895.7393        917.7792        498.5516       0       0
   13878        898.0789        917.7792        493.2968       0       0
   13879        900.1964        917.7792        487.9485       0       0
   13880        902.0881        917.7792        482.5164       0       0
   13881        903.7507        917.7792        477.0098       0       0
   13882        905.1812        917.7792        471.4383       0       0
   13883        906.3772        917.7792        465.8119       0       0
   13884        907.3364        917.7792        460.1403       0       0
   13885        908.0574        917.7792        454.4333       0       0
   13886        908.5388        917.7792        448.7014       0       0
   13887        908.7796        917.7792        442.9543       0       0
   13888        908.7796        917.7792        437.2022       0       0
   13889        908.5388        917.7792         431.455       0       0
   13890        908.0574        917.7792         425.723       0       0
   13891        907.3365        917.7792        420.0162       0       0
   13892        906.3773        917.7792        414.3446       0       0
   13893        905.1813        917.7792        408.7182       0       0
   13894        903.7509        917.7792        403.1468       0       0
   13895        902.0883        917.7792        397.6401       0       0
   13896        900.1967        917.7792        392.2079       0       0
   13897        898.0791        917.7792        386.8597       0       0
   13898        895.7395        917.7792        381.6048       0       0
   13899        893.1818        917.7792        376.4525       0       0
   13900        890.4107        917.7792        371.4118       0       0
   13901         887.431        917.7792        366.4916       0       0
   13902        884.2478        917.7792        361.7005       0       0
   13903        880.8668        917.7792        357.0469       0       0
   13904        877.2938        917.7792        352.5389       0       0
   13905        873.5353        917.7792        348.1846       0       0
   13906        869.5975        917.7792        343.9915       0       0
   13907        865.4878        917.7792        339.9669       0       0
   13908        861.2131        917.7792        336.1179       0       0
   13909         856.781        917.7792        332.4513       0       0
   13910        852.1993        917.7792        328.9736       0       0
   13911        847.4759        917.7792        325.6907       0       0
   13912        842.6191        917.7792        322.6085       0       0
   13913        837.6375        917.7792        319.7324       0       0
   13914          832.54        917.7792        317.0675       0       0
   13915        827.3354        917.7792        314.6183       0       0
   13916        822.0326        917.7792        312.3892       0       0
   13917        816.6412        917.7792        310.3842       0       0
   13918        811.1705        917.7792        308.6068       0       0
   13919        805.6302        917.7792        307.0598       0       0
   13920        800.0301        917.7792        305.7462       0       0
   13921        794.3798        917.7792        304.6684       0       0
   13922        788.6893        917.7792        303.8281       0       0
   13923        782.9686        917.7792        303.2268       0       0
   13924        777.2279        917.7792        302.8658       0       0
   13925        771.4769        917.7792        302.7452       0       0
   13926        765.7259        917.7792        302.8658       0       0
   13927        759.9852        917.7792        303.2268       0       0
   13928        754.2645        917.7792        303.8281       0       0
   13929         748.574        917.7792        304.6685       0       0
   13930        742.9237        917.7792        305.7463       0       0
   13931        737.3235        917.7792        307.0599       0       0
   13932        731.7833        917.7792        308.6068       0       0
   13933        726.3126        917.7792        310.3843       0       0
   13934        720.9212        917.7792        312.3893       0       0
   13935        715.6185        917.7792        314.6184       0       0
   13936        710.4138        917.7792        317.0675       0       0
   13937        705.3163        917.7792        319.7325       0       0
   13938        700.3347        917.7792        322.6086       0       0
   13939        695.4779        917.7792        325.6908       0       0
   13940        690.7545        917.7792        328.9737       0       0
   13941        686.1729        917.7792        332.4514       0       0
   13942        681.7407        917.7792         336.118       0       0
   13943        677.4659        917.7792        339.9669       0       0
   13944        673.3563        917.7792        343.9916       0       0
   13945        669.4186        917.7792        348.1848       0       0
   13946        665.6601        917.7792        352.5391       0       0
   13947        662.0872        917.7792        357.0471       0       0
   13948        658.7062        917.7792        361.7006       0       0
   13949        655.5229        917.7792        366.4917       0       0
   13950        652.5432        917.7792        371.4119       0       0
   13951         649.772        917.7792        376.4525       0       0
   13952        647.2145        917.7792        381.6048       0       0
   13953        644.8749        917.7792        386.8596       0       0
   13954        642.7574        917.7792        392.2079       0       0
   13955        640.8658        917.7792          397.64       0       0
   13956        639.2032        917.7792        403.1466       0       0
   13957        637.7726        917.7792        408.7181       0       0
   13958        636.5768        917.7792        414.3445       0       0
   13959        635.6174        917.7792        420.0161       0       0
   13960        634.8965        917.7792        425.7229       0       0
   13961        634.4152        917.7792        431.4549       0       0
   13962        634.1744        917.7792         437.202       0       0
   13963        634.1744        917.7792        442.9541       0       0
   13964        634.4152        917.7792        448.7012       0       0
   13965        634.8965        917.7792        454.4332       0       0
   13966        635.6174        917.7792          460.14       0       0
   13967        636.5768        917.7792        465.8116       0       0
   13968        637.7726        917.7792         471.438       0       0
   13969        639.2032        917.7792        477.0095       0       0
   13970        640.8658        917.7792         482.516       0       0
   13971        642.7574        917.7792        487.9482       0       0
   13972         644.875        917.7792        493.2964       0       0
   13973        647.2146        917.7792        498.5513       0       0
   13974        649.7722        917.7792        503.7036       0       0
   13975        652.5433        917.7792        508.7442       0       0
   13976        655.5229        917.7792        513.6644       0       0
   13977        658.7062        917.7792        518.4555       0       0
   13978        662.0872        917.7792        523.1091       0       0
   13979        665.6601        917.7792        527.6169       0       0
   13980        669.4186        917.7792        531.9713       0       0
   13981        673.3563        917.7792        536.1646       0       0
   13982        677.4661        917.7792         540.189       0       0
   13983        681.7407        917.7792         544.038       0       0
   13984        686.1728        917.7792        547.7045       0       0
   13985        690.7545        917.7792        551.1823       0       0
   13986        695.4779        917.7792        554.4651       0       0
   13987        700.3345        917.7792        557.5474       0       0
   13988         705.316        917.7792        560.4235       0       0
   13989        710.4136        917.7792        563.0884       0       0
   13990        715.6182        917.7792        565.5375       0       0
   13991        720.9209        917.7792        567.7666       0       0
   13992        726.3123        917.7792        569.7716       0       0
   13993        731.7829        917.7792        571.5492       0       0
   13994        737.3232        917.7792        573.0961       0       0
   13995        742.9233        917.7792        574.4095       0       0
   13996        748.5735        917.7792        575.4874       0       0
   13997         754.264        917.7792        576.3279       0       0
   13998        759.9847        917.7792        576.9291       0       0
   13999        765.7254        917.7792        577.2903       0       0
   14000        771.4763        917.7792        577.4108       0       0
*END
