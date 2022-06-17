"""Module to create EM dyna input deck"""

import logging

from .dynabase import *


class DynaEM(DynaBase):
    """Contains methods to create keyword related to EM"""

    def __init__(self, hostname = 'localhost'):
        DynaBase.__init__(self, hostname)

    def create_em_control(self, emsol=0, numls=100, macrodt=0, ncylfem=5000,ncylbem=5000):
        """Enable the EM solver and set its options.
        Parameters
        ----------
        emsol : int
           Electromagnetism solver selector:
           EQ.-1: Turns the EM solver off after reading the EM keywords.
           EQ.1: Eddy current solver. 
           EQ.2: Induced heating solver. 
           EQ.3: Resistive heating solver. 
           EQ.11: Electrophysiology monodomain. 
           EQ.12: Electrophysiology bidomain.
           EQ.13: Electrophysiology monodomain coupled with bidomain.
        numls : int
            Number of local EM steps in a whole period for emsol = 2.
        macrodt : float
            Macro time step when EMSOL = 2.
        ncylfem : int
            Number of electromagnetism cycles between the recalculation of FEM matrices.
        ncylbem : int
            Number of electromagnetism cycles between the recalculation of BEM matrices.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateEMControl(
            EMControlRequest(
                emsol=emsol, numls=numls, macrodt=macrodt, ncylfem=ncylfem,ncylbem = ncylbem
            )
        )
        logging.info("EM Control Created...")
        return ret
    
    def create_em_timestep(self, tstype, dtconst):
        """Controls the EM time step and its evolution.
        Parameters
        ----------
        tstype : int
           Time step type:
           EQ.1: constant time step given in DTCONST 
           EQ.2: time step as a function of time given by a load curve specified in LCID 
           EQ.3: automatic time step computation, depending on the solver type. This time step is then multiplied by FACTOR
        dtconst : float
            Constant value for the time step for TSTYPE = 1.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateEMTimestep(
            EMTimestepRequest(
                tstype=tstype, dtconst=dtconst
            )
        )
        logging.info("EM Timestep Created...")
        return ret

    def create_em_control_contact(self, emct=0,cconly=0,ctype=0,dtype=0):
        """the electromagnetism contact algorithms, which detects contact between conductors.
        Parameters
        ----------
        emct : int
           EM contact activation flag:
           EQ.0: No contact detection 
           EQ.1: Contact detection
        cconly : int
            Determines on which parts of the model the EM contact should be activated
            EQ.0: Contact detection between all active parts associated with a conducting material. (Default) 
            EQ.1: Only look for EM contact between parts associated through the EM_CONTACT card. In some cases this option can reduce the calculation time.
        ctype : int
            Contact type:
            EQ.-1: Node to node contact based on constraints on the scalar potential. See Remark 1.
            EQ.0: Node to node penalty based contact on the scalar potential. 
            EQ.1: Discrete mortar penalty contact on the scalar potential.
            EQ.2: Continuous mortar penalty contact on the scalar potential and the vector potential (when active).
        dtype : int
            Detection type.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateEMControlContact(
            EMControlContactRequest(
                emct=emct, cconly=cconly,ctype=ctype,dtype=dtype
            )
        )
        logging.info("EM Control Contact Created...")
        return ret

    def create_em_contact(self, contid=0,dtype=0,psidm=0,psids=0,eps1=0.3,eps2=0.3,eps3=0.3,d0=0):
        """Optional card used for defining and specifying options on electromagnetic contacts between two sets of parts.
        Parameters
        ----------
        contid : int
            Electromagnetic contact ID
        dtype : int
            Detection type:
            EQ.0: Contact type 0 (Default). 
            EQ.1: Contact type 1.
        psidm : int
            Master part set ID.
        psids : int
            Slave part set ID.
        eps1,eps2,eps3 : float
            Contact Coefficients for contact detection conditions.
        d0 : float
            Contact condition 3 when COTYPE = 1.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateEMContact(
            EMContactRequest(
                contid=contid, dtype=dtype,psidm=psidm,psids=psids,eps1 = eps1,eps2=eps2,eps3=eps3,d0=d0
            )
        )
        logging.info("EM Contact Created...")
        return ret

    def create_circuit_rogo(self, rogid=0,setid=0,settype=0,curtyp=0):
        """Define Rogowsky coils to measure a global current vs time through a segment set or a node set.
        Parameters
        ----------
        rogid : int
            Rogowsky coil ID.
        setid : int
            Segment or node set ID.
        settype : int
            Type of set:
            EQ.1: Segment set 
            EQ.2: Node set
        curtyp : int
            Type of current measured:
            EQ.1: Volume current 
            EQ.2: Surface current (not available yet} 
            EQ.3: Magnetic field flow (B field times Area)

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateEMCircuitRogo(
            EMCircuitRogoRequest(
                rogid=rogid, setid=setid,settype=settype,curtyp=curtyp
            )
        )
        logging.info("EM Circuit Rogo Created...")
        return ret


    def create_circuit(self, circid,circtyp,lcid,sidcurr,sidvin,sidvout):
        """Define an electrical circuit.
        Parameters
        ----------
        circid : int
            Circuit ID.
        circtyp : int
            Circuit type:
            EQ.1: Imposed current vs time defined by a load curve.
            EQ.2: Imposed voltage vs time defined by a load curve.
            EQ.3: R, L, C, V0 circuit.
            EQ.11: Imposed current defined by an amplitude A, frequency F and initial time t0.
            EQ.12: Imposed voltage defined by an amplitude A, frequency F and initial time t0.
            EQ.21: Imposed current defined by a load curve over one period and a frequency F.
            EQ.22: Imposed voltage defined by a load curve over one period and a frequency F.
        lcid : int
            Load curve ID for circtyp = 1, 2, 21 or 22
        sidcurr : int
            Segment set ID for the current.
        sidvin : int 
            Segment set ID for input voltage or input current when CIRCTYP.EQ.2/3/12/22 and CIRCTYP.EQ 1/11/21 respectively.
        sidvout : int
            Segment set ID for output voltage or output current when CIRCTYP = 2/3/12/22 and CIRCTYP = 1/11/21 respectively.
        
        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateEMCircuit(
            EMCircuitRequest(
                circid=circid, circtyp=circtyp,lcid=lcid,sidcurr=sidcurr,sidvin=sidvin,sidvout=sidvout
            )
        )
        logging.info("EM Circuit Created...")
        return ret

    def create_em_mat001(self, mid,mtype,sigma):
        """Define the electromagnetic material type and properties for a material whose permeability equals the free space permeability.
        Parameters
        ----------
        mid : int
            Material ID.
        mtype : int
            Defines the electromagnetism type of the material:
            EQ.0: Air or vacuum.
            EQ.1: Insulator material: these materials have the same electromagnetism behavior as EQ.0
            EQ.2: Conductor carrying a source.
            EQ.3: Fluid conductor.
            EQ.4: Conductor not connected to any current or voltage source, where the Eddy current problem is solved.
        sigma : float
            Initial electrical conductivity of the material.
        
        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateEMMat001(
            EMMat001Request(
                mid=mid, mtype=mtype,sigma=sigma
            )
        )
        logging.info("EM Material 001 Created...")
        return ret

    def create_em_mat002(self, mid,mtype,sigma,eosid,murel):
        """Define an electromagnetic material type and properties whose permeability is different than the free space permeability.
        Parameters
        ----------
        mid : int
            Material identification.
        mtype : int
            Electromagnetism type of the material:
            EQ.0: Air or vacuum.
            EQ.1: Insulator material: these materials have the same electromagnetism behavior as EQ.0
            EQ.2: Conductor carrying a source.
            EQ.4: Conductor not connected to any current or voltage source, where the Eddy current problem is solved.
        sigma : float
            Initial electrical conductivity of the material.
        eosid : int
            ID of the EOS to be used for the electrical conductivity.
        murel : float
            Relative permeability which is the ratio of the permeability of a specific medium to the permeability of free space
        
        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateEMMat002(
            EMMat002Request(
                mid=mid, mtype=mtype,sigma=sigma,eosid=eosid,murel=murel
            )
        )
        logging.info("EM Material 002 Created...")
        return ret

    def create_em_solver_bemmat(self, matid):
        """Define the type of BEM matrices as well as the way they are assembled.
        Parameters
        ----------
        matid : int
            Defines which BEM matrix the card refers to:
            EQ.1: P matrix 
            EQ.2: Q matrix 
            EQ.3: W matrix
        
        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateEMSolverBemMat(
            EMSolverBemMatRequest(
                matid=matid
            )
        )
        logging.info("EM Solver BEMMAT Created...")
        return ret

    def create_em_solver_bem(self, reltol=1e-6,maxite=1000,stype=2,precon=2,uselast=1,ncylbem=5000):
        """Define the type of linear solver and pre-conditioner as well as tolerance for the EM_BEM solve.
        Parameters
        ----------
        reltol : int
            Relative tolerance for the iterative solvers.
        maxite : int
            Maximum number of iterations.
        stype : int 
            Solver type:
            EQ.1: direct solve - the matrices will then be considered as dense. 
            EQ.2: pre-conditioned gradient method (PCG) - this method allows for block matrices with low rank blocks, and thus reduces memory used. 
            EQ.3: GMRES method - this method allows for block matrices with low rank blocks and thus reduces memory used.
        precon : int
            Preconditioner type for PCG or GMRES iterative solves:
            EQ.0: no preconditioner 
            EQ.1: diagonal line 
            EQ.2: diagonal block 
            EQ.3: broad diagonal including all neighbor faces 
            EQ.4: LLT factorization.
        uselast : int
            This is used only for iterative solvers (PCG or GMRES).
            EQ.-1: Start from 0 as initial guess for solution of the linear system.
            EQ.1: Starts from the previous solution normalized by the right-hand-side change.
        ncylbem : int
            Number of electromagnetism cycles between the recalculation of BEM matrices.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateEMSolverBem(
            EMSolverBemRequest(
                reltol=reltol,maxite=maxite,stype=stype,precon=precon,uselast=uselast,ncylbem=ncylbem
            )
        )
        logging.info("EM Solver BEM Created...")
        return ret

    def create_em_solver_fem(self, reltol=1e-3,maxite=1000,stype=1,precon=1,uselast=1,ncylbem=5000):
        """Define some parameters for the EM FEM solver.
        Parameters
        ----------
        reltol : int
            Relative tolerance for the iterative solvers.
        maxite : int
            Maximum number of iterations.
        stype : int 
            Solver type:
            EQ.1: direct solve
            EQ.2: Conditioned Gradient Method (PCG).
        precon : int
            Preconditioner type for PCG.
            EQ.0: no preconditioner 
            EQ.1: diagonal line 
        uselast : int
            This is used only for iterative solvers (PCG).
            EQ.-1: Start from 0 as initial guess for solution of the linear system.
            EQ.1: Starts from the previous solution normalized by the right-hand-side change.
        ncylbem : int
            Number of electromagnetism cycles between the recalculation of FEM matrices.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateEMSolverFem(
            EMSolverFemRequest(
                reltol=reltol,maxite=maxite,stype=stype,precon=precon,uselast=uselast,ncylbem=ncylbem
            )
        )
        logging.info("EM Solver FEM Created...")
        return ret

    def create_em_solver_fembem_monolithic(self, mtype=0,stype=0,abstol=1e-6,reltol=1e-4,maxit=500):
        """Replaces *EM_SOLVER_FEMBEM and turns on the monolithic FEM-BEM solver.
        Parameters
        ----------
        mtype : int
            Monolithic solver type: 
            EQ.0: Direct symmetric solver.
        stype : int
            Solver type: 
            EQ.0: MINRES iterative solver 
            EQ.1: GMRES iterative solver
        abstol : float 
            Absolute tolerance.
        reltol : float
            Relative tolerance.
        maxit : int
            Maximum number of iterations.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateEMSolverFemBemMonolithic(
            EMSolverFemBemMonolithicRequest(
                mtype=mtype,stype=stype,abstol=abstol,reltol=reltol,maxit=maxit
            )
        )
        logging.info("EM Solver FEMBEM Monolithic Created...")
        return ret

    def create_em_output(self, mats=0,matf=0,sols=0,solf=0):
        """Define the level of EM related output on the screen and in the messag file.
        Parameters
        ----------
        mats : int
            Level of matrix assembly output to the screen:
            EQ.0: no output 
            EQ.1: basic assembly steps 
            EQ.2: basic assembly steps + percentage completed + final statistics 
            EQ.3: basic assembly steps + percentage completed + statistics at each percentage of completion
        matf : int
            Level of matrix assembly output to the messag file:
            EQ.0: no output 
            EQ.1: basic assembly steps 
            EQ.2: basic assembly steps + percentage completed + final statistics 
            EQ.3: basic assembly steps + percentage completed + statistics at each percentage of completion
        sols : int
            Level of solver output on the screen: 
            EQ.0: no output 
            EQ.1: global information at each FEM iteration 
            EQ.2: detailed information at each FEM iteration
        solf : int
            Level of solver output to the messag file:
            EQ.0: no output
            EQ.1: global information at each FEM iteration 
            EQ.2: detailed information at each FEM iteration
        
        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateEMOutput(
            EMOutputRequest(
                mats=mats,matf=matf,sols=sols,solf=solf
            )
        )
        logging.info("EM Output Created...")
        return ret

    def create_em_database_globalenergy(self, outlv=0):
        """ enables the output of global EM.
        Parameters
        ----------
        outlv : int
            Determines if the output file should be dumped. 
            EQ.0: No output file is generated. 
            EQ.1: The output file is generated.
        
        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateEMDatabaseGlobalEnergy(
            EMDatabaseGlobalEnergyRequest(
                outlv=outlv
            )
        )
        logging.info("EM Database Global Energy Created...")
        return ret

    def create_Permanent_magnet(self, id,partid,mtype,north,sourth,hc):
        """Defines a permanent magnet.
        Parameters
        ----------
        id : int
            ID of the magnet.
        partid : int
            Part ID.
        mtype : int
            Magnet definition type: 
            EQ.0: Magnet defined by two node sets for North and South Poles. 
            EQ.1: Magnet defined by two segments sets for North and South Poles. 
            EQ.3: Magnet defined by a global vector orientation. 
            EQ.4: Magnet defined by a global vector orientation given by two node IDs.
        north : int
            Set ID of the magnet north face for MTYPE = 0 and MTYPE = 1.
        sourth : int
            Set ID of the magnet south face for MTYPE = 0 and MTYPE = 1.
        hc : int
            Coercive force.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateEMPermanentMagnet(
            EMPermanentMagnetRequest(
                id=id,partid=partid,mtype=mtype,north=north,sourth=sourth,hc=hc
            )
        )
        logging.info("EM Permanent Magnet Created...")
        return ret

    def create_em_eos_permeability(self, eosid,eostype,lcid):
        """Define the parameters for the behavior of a material's permeability.
        Parameters
        ----------
        eosid : int
            ID of the EM_EOS.
        eostype : int
            Define the type of EOS: 
            EQ.1: Permeability defined by a B function of H curve
            EQ.2: Permeability defined by a H function of B curve
        lcid : int
            Load curve ID.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateEMEOSPermeability(
            EMEOSPermeabilityRequest(
                eosid=eosid,eostype=eostype,lcid=lcid
            )
        )
        logging.info("EM EOS Permeability Created...")
        return ret