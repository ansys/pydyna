*KEYWORD
$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$
$$$$  CASE DEFINITIONS
$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$
*CASE
$...>....1....>....2....>....3....>....4....>....5....>....6....>....7....>....8
$ CASEID  JOBID
       101 LOW_VELOCITY
MEMORY=20M
1
$
*CASE  
$ CASEID  JOBID
       102 ZERO_VELOCITY
MEMORY=20M
2
$
MEMORY=20M
2
$
*TITLE
Projectile Penetrating Plate
$
*TITLE CID=1
Projectile Penetrating Plate - Low Velocity Impact
$
*TITLE CID=2
Projectile Penetrating Plate - Zero Velocity (Static)
$
$  LSTC Example
$
$  Last Modified: September 8, 1997
$                 July 13, 2000
$
$   Units: gram, cm, microsec, 1e+07 N, Mbar, 1e+07 N-cm
$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$
$$$$  Control Ouput
$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$
$...>....1....>....2....>....3....>....4....>....5....>....6....>....7....>....8
$
*CONTROL_PARALLEL
$     ncpu    numrhs      accu
         1
$
*CONTROL_TERMINATION
$   endtim    endcyc     dtmin    endneg    endmas
      70.0
$
*CONTROL_CONTACT
$   slsfac    rwpnal    islchk    shlthk    penopt    thkchg     orien
       1.0
$   usrstr    usrfac     nsbcs    interm    xpenen

$
*CONTROL_ENERGY
$     hgen      rwen    slnten     rylen
         2         2         2
$
*CONTROL_OUTPUT
$    npopt    neecho    nrefup    iaccop     opifs    ipnint    ikedit
         1         3
$
$
*DATABASE_BINARY_D3PLOT
$       dt      lcdt
       5.0
$
*DATABASE_EXTENT_BINARY
$    neiph     neips    maxint    strflg    sigflg    epsflg    rltflg    engflg

$          ieverp = 1 put each plot state in separate d3plot files
$   cmpflg    ieverp    beamip
                   1
$
*DATABASE_BINARY_D3THDT
$       dt      lcdt
    999999
$
*DATABASE_GLSTAT
$       dt
      0.10
$
*DATABASE_MATSUM
$       dt
      0.10
$
*DATABASE_RCFORC
$       dt
      0.10
$
*DATABASE_SLEOUT
$       dt
      0.10
$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$
$$$$  Define Contacts
$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$
$...>....1....>....2....>....3....>....4....>....5....>....6....>....7....>....8
$
$$$  Force Transducer - contact forces on projectile: part 1
$
*CONTACT_FORCE_TRANSDUCER_PENALTY 
$     ssid      msid     sstyp     mstyp
         1                   3

 
$
$$$  Force Transducer - contact forces on plate: part 2
$
*CONTACT_FORCE_TRANSDUCER_PENALTY 
$     ssid      msid     sstyp     mstyp
         2                   3

 
$
$
$$$$$   Define contact between projectile and plate.
$
$*CONTACT_
$     ssid      msid     sstyp     mstyp    sboxid    mboxid       spr       mpr
$
$  
$       fs        fd        dc        vc       vdc    penchk        bt        dt
$
$
$      sfs       sfm       sst       mst      sfst      sfmt       fsf       vsf
$
$
$$$  card 4: only required for eroding contact
$     isym    erosop      iadj
$
$
$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$
$$$$  Initial Conditions
$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$
$$$$  Assign an initial velocity to the projectile (part 1) angled down
$$$$  towards the plate.
$
$...>....1....>....2....>....3....>....4....>....5....>....6....>....7....>....8
$
*INITIAL_VELOCITY_GENERATION
$      sid      styp     omega        vx        vy        vz
         1         1           1.246E-01 0.000E+00-3.339E-02
$
$       xc        yc        zc        nx        ny        nz     phase

$
$$$$ case 1 begins
$
*CASE_BEGIN_1
*INITIAL_VELOCITY_GENERATION
$      sid      styp     omega        vx        vy        vz
         1         1           0.623E-01 0.000E+00-1.670E-02
$
$       xc        yc        zc        nx        ny        nz     phase

*CASE_END_1
*CASE_BEGIN_2
*INITIAL_VELOCITY_GENERATION
$      sid      styp     omega        vx        vy        vz
         1         1           0.000E+00 0.000E+00 0.000E+00
$
$       xc        yc        zc        nx        ny        nz     phase

*CASE_END_2
*SET_PART
         1
         1
$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$
$$$$  Define Parts and Materials
$
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$
$...>....1....>....2....>....3....>....4....>....5....>....6....>....7....>....8
$
*PART
$      pid       sid       mid     eosid      hgid    adpopt
Projectile            
         1         1         1
Plate
         2         1         2
$
$
$$$$$$  Materials
$
$$$$  failure strain for erosion of the projectile and plate elements are
$$$$  set as:  fs = 0.8 
$
*MAT_PLASTIC_KINEMATIC
$      mid        ro         e        pr      sigy      etan      beta
         1 1.862E+01 1.170E+00      0.22 1.790E-02                 1.0
$      src       srp        fs
                           0.8
$
$
*MAT_PLASTIC_KINEMATIC
$      mid        ro         e        pr      sigy      etan      beta
         2 7.896E+00 2.100E+00     0.284 1.000E-02                 1.0
$      src       srp        fs
                           0.8
$
$
$$$$$$  Sections
$
*SECTION_SOLID
$      sid    elform
         1         0
$

*INCLUDE
elem.k
*END
