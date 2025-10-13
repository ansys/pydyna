





:class:`AirbagParticleMppDecompositionSegmentId`
================================================


.. py:class:: airbag_particle_mpp_decomposition_segment_id.AirbagParticleMppDecompositionSegmentId(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA AIRBAG_PARTICLE_MPP_DECOMPOSITION_SEGMENT_ID keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: AirbagParticleMppDecompositionSegmentId

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sx`
            - Get or set the Scale factor for X direction use for MPP decomposition of particle domain.
          * - :py:attr:`~sy`
            - Get or set the Scale factor for Y direction use for MPP decomposition of particle domain.
          * - :py:attr:`~sz`
            - Get or set the Scale factor for Z direction use for MPP decomposition of particle domain.
          * - :py:attr:`~id`
            - Get or set the Optional Airbag ID.
          * - :py:attr:`~title`
            - Get or set the Airbag id descriptor. It is suggested that unique descriptions be used.
          * - :py:attr:`~sid1`
            - Get or set the Part or part set ID defining the complete airbag.
          * - :py:attr:`~stype1`
            - Get or set the Set type:
          * - :py:attr:`~sid2`
            - Get or set the Part or part set ID defining internal parts of the airbag.
          * - :py:attr:`~stype2`
            - Get or set the Set type:
          * - :py:attr:`~block`
            - Get or set the Blocking.  Block must be set to a two-digit number "BLOCK"="M"x10+"N",
          * - :py:attr:`~npdata`
            - Get or set the Number of parts or part sets data.
          * - :py:attr:`~fric`
            - Get or set the Friction factor F_r if -1.0 < FRIC ≤ 1.0.  Otherwise,
          * - :py:attr:`~irdp`
            - Get or set the Dynamically scaling of particle radius
          * - :py:attr:`~segsid`
            - Get or set the ID for a segment set.  The segments define the volume and should belong to the parts from SID1.
          * - :py:attr:`~np`
            - Get or set the Number of particles (Default 200,000).
          * - :py:attr:`~unit`
            - Get or set the Unit system
          * - :py:attr:`~visflg`
            - Get or set the Visible particles(only support CPM database, see remark 6)
          * - :py:attr:`~tatm`
            - Get or set the Atmospheric temperature (Default 293K).
          * - :py:attr:`~patm`
            - Get or set the Atmospheric pressure (Default 1ATM).
          * - :py:attr:`~nvent`
            - Get or set the Number of vent hole parts or part sets.
          * - :py:attr:`~tend`
            - Get or set the Time when all particles (NP) have entered bag (Default 1.0e10).
          * - :py:attr:`~tsw`
            - Get or set the Time for switch to control volume calculation (Default 1.0e10).
          * - :py:attr:`~tstop`
            - Get or set the Time at which front tracking switches from IAIR = 4 to IAIR = 2.
          * - :py:attr:`~tsmth`
            - Get or set the To avoid sudden jumps in the pressure signal during switching,
          * - :py:attr:`~occup`
            - Get or set the Particles occupy OCCUP percent of the airbag’s volume.  The default value of OCCUP is 10%.
          * - :py:attr:`~rebl`
            - Get or set the If the option is ON, all energy stored from damping will be evenly distributed as vibrational energy to all particles.
          * - :py:attr:`~sidsv`
            - Get or set the Part set ID for internal shell part.  The volume formed by this internal shell part will be excluded from the bag volume.  These internal parts must have consistent orientation to get correct excluded volume.
          * - :py:attr:`~psid1`
            - Get or set the Part set ID for external parts which have normal pointed outward.  This option is usually used with airbag integrity check while there are two CPM bags connected with bag interaction.  Therefore, one of the bag can have the correct shell orientation but the share parts for the second bag will have wrong orientation.  This option will automatically flip the parts defined in this set in the second bag during integrity checking.
          * - :py:attr:`~tsplit`
            - Get or set the Start time to activate particle splitting algorithm. See Remark 15.
          * - :py:attr:`~sffdc`
            - Get or set the Scale factor for the force decay constant.  SFFDC has a range of . The default value is 1.0.  The value given here will replaced the values from *CONTROL_CPM
          * - :py:attr:`~sfiair4`
            - Get or set the Scale factor for the ratio of initial air particles to inflator gas particles for IAIR = 4.
          * - :py:attr:`~idfric`
            - Get or set the Direction of P2F impact force:
          * - :py:attr:`~mass`
            - Get or set the Conversion factor from current unit to MKS unit.  For example, if the current unit is using kg-mm-ms, the input should be 1.0, 0.001, 0.001.
          * - :py:attr:`~time`
            - Get or set the Conversion factor from current unit to MKS unit.  For example, if the current unit is using kg-mm-ms, the input should be 1.0, 0.001, 0.001.
          * - :py:attr:`~length`
            - Get or set the Conversion factor from current unit to MKS unit.  For example, if the current unit is using kg-mm-ms, the input should be 1.0, 0.001, 0.001.
          * - :py:attr:`~iair`
            - Get or set the Initial gas inside bag considered:
          * - :py:attr:`~ngas`
            - Get or set the Number of gas components.
          * - :py:attr:`~norif`
            - Get or set the Number of orifices.
          * - :py:attr:`~nid1`
            - Get or set the NID1-NID3, Three nodes defining a moving coordinate system for the direction of flow through the gas inlet nozzles (Default fixed system).
          * - :py:attr:`~nid2`
            - Get or set the NID1-NID3, Three nodes defining a moving coordinate system for the direction of flow through the gas inlet nozzles (Default fixed system).
          * - :py:attr:`~nid3`
            - Get or set the NID1-NID3, Three nodes defining a moving coordinate system for the direction of flow through the gas inlet nozzles (Default fixed system).
          * - :py:attr:`~chm`
            - Get or set the Chamber ID used in *DEFINE_CPM_CHAMBER.
          * - :py:attr:`~cd_ext`
            - Get or set the Drag coefficient for external air. If the value is not zero, the inertial effect
          * - :py:attr:`~sidup`
            - Get or set the Part or part set ID defining the internal parts that pressure will be applied to.
          * - :py:attr:`~styup`
            - Get or set the Set defining internal parts will be applied pressure
          * - :py:attr:`~pfrac`
            - Get or set the Part or part set ID defining the internal parts that pressure will be applied to.
          * - :py:attr:`~linking`
            - Get or set the Part ID of an internal part that is coupled to the external vent definition.
          * - :py:attr:`~sidh`
            - Get or set the Part or part set ID defining part data.
          * - :py:attr:`~stypeh`
            - Get or set the Set type EQ.0: Part
          * - :py:attr:`~hconv`
            - Get or set the Heat convection coefficient used to calculate heat loss from the airbag external surface to ambient (W/K/m2).
          * - :py:attr:`~pfric`
            - Get or set the Friction factor.
          * - :py:attr:`~sdfblk`
            - Get or set the Scale down factor for blockage factor (Default=1, no scale down). The val-id factor will be (0,1]. If 0, it will set to 1.
          * - :py:attr:`~kp`
            - Get or set the Thermal conductivity of the part.
          * - :py:attr:`~inip`
            - Get or set the Place initial air particles on surface.
          * - :py:attr:`~cp`
            - Get or set the Specific heat (see Remark 16).
          * - :py:attr:`~sid3`
            - Get or set the Part or part set ID defining vent holes.
          * - :py:attr:`~stype3`
            - Get or set the Set type:
          * - :py:attr:`~c23`
            - Get or set the GE.0:    Vent hole coefficient, a parameter of Wang-Nefske leakage.  A value between 0.0 and 1.0 can be input.  See Remark 1.
          * - :py:attr:`~lctc23`
            - Get or set the Load curve defining vent hole coefficient as a function of time.  LCTC23 can be defined through *DEFINE_CURVE_FUNCTION.  If omitted, a curve equal to 1.0 used.
          * - :py:attr:`~lcpc23`
            - Get or set the Load curve defining vent hole coefficient as a function of pressure.  If omitted a curve equal to 1.0 is used..
          * - :py:attr:`~enh_v`
            - Get or set the Enhanced venting option. See Remark 8.
          * - :py:attr:`~ppop`
            - Get or set the Pressure difference between interior and ambient pressure (PATM) to open the vent holes.  Once the vents are open, they will stay open.
          * - :py:attr:`~pair`
            - Get or set the Initial pressure inside bag .
          * - :py:attr:`~tair`
            - Get or set the Initial temperature inside bag .
          * - :py:attr:`~xmair`
            - Get or set the Molar mass of gas initially inside bag.
          * - :py:attr:`~aair`
            - Get or set the Constant, linear, and quadratic heat capacity parameters.
          * - :py:attr:`~bair`
            - Get or set the Constant, linear, and quadratic heat capacity parameters.
          * - :py:attr:`~cair`
            - Get or set the Constant, linear, and quadratic heat capacity parameters.
          * - :py:attr:`~npair`
            - Get or set the Number of particle for air.
          * - :py:attr:`~nprlx`
            - Get or set the Number of cycles to reach thermal equilibrium.  See Remark 6.
          * - :py:attr:`~lcmi`
            - Get or set the Mass flow rate curve for gas component i, unless the MOLEFRACTION option is used.
          * - :py:attr:`~lcti`
            - Get or set the Temperature curve for gas component i.
          * - :py:attr:`~xmi`
            - Get or set the Molar mass of gas component i.
          * - :py:attr:`~ai`
            - Get or set the Constant, linear, and quadratic heat capacity parameters for gas component i.
          * - :py:attr:`~bi`
            - Get or set the Constant, linear, and quadratic heat capacity parameters for gas component i.
          * - :py:attr:`~ci`
            - Get or set the Constant, linear, and quadratic heat capacity parameters for gas component i.
          * - :py:attr:`~infgi`
            - Get or set the Inflator ID that this gas component belongs to (Default 1).
          * - :py:attr:`~nidi`
            - Get or set the Node ID/Shell ID defining the location of nozzle i.
          * - :py:attr:`~ani`
            - Get or set the Area of nozzle i (Default all nozzles are given the same area).
          * - :py:attr:`~vdi`
            - Get or set the GT.0:    Vector ID.  Initial direction of gas inflow at nozzle i.
          * - :py:attr:`~cai`
            - Get or set the Cone angle in degrees (defaults to30°). This option is used only when IANG is equal to 1.
          * - :py:attr:`~infoi`
            - Get or set the Inflator ID for this orifice.  (default = 1).
          * - :py:attr:`~imom`
            - Get or set the Inflator reaction forces
          * - :py:attr:`~iang`
            - Get or set the Activation for cone angle to use for friction calibration(should not use in the normal runs)
          * - :py:attr:`~chm_id`
            - Get or set the Chamber ID where the inflator node resides.  Chambers are defined using the *DEFINE_CPM_CHAMBER keyword.


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~keyword`
            - 
          * - :py:attr:`~subkeyword`
            - 






Import detail
-------------

.. code-block:: python

    from airbag_particle_mpp_decomposition_segment_id import AirbagParticleMppDecompositionSegmentId

Property detail
---------------

.. py:property:: sx
   :type: Optional[float]


   
   Get or set the Scale factor for X direction use for MPP decomposition of particle domain.
















   ..
       !! processed by numpydoc !!

.. py:property:: sy
   :type: Optional[float]


   
   Get or set the Scale factor for Y direction use for MPP decomposition of particle domain.
















   ..
       !! processed by numpydoc !!

.. py:property:: sz
   :type: Optional[float]


   
   Get or set the Scale factor for Z direction use for MPP decomposition of particle domain.
















   ..
       !! processed by numpydoc !!

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Optional Airbag ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Airbag id descriptor. It is suggested that unique descriptions be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: sid1
   :type: Optional[int]


   
   Get or set the Part or part set ID defining the complete airbag.
















   ..
       !! processed by numpydoc !!

.. py:property:: stype1
   :type: int


   
   Get or set the Set type:
   EQ.0: Part
   EQ.1: Part set.
















   ..
       !! processed by numpydoc !!

.. py:property:: sid2
   :type: int


   
   Get or set the Part or part set ID defining internal parts of the airbag.
















   ..
       !! processed by numpydoc !!

.. py:property:: stype2
   :type: int


   
   Get or set the Set type:
   EQ.0: Part
   EQ.1: Part set.
   EQ.2:   Number of parts to read (Not recommended for general use)
















   ..
       !! processed by numpydoc !!

.. py:property:: block
   :type: Optional[int]


   
   Get or set the Blocking.  Block must be set to a two-digit number "BLOCK"="M"x10+"N",
   The 10’s digit controls the treatment of particles that escape due to deleted elements (particles are always tracked and marked).
   M.EQ.0: Active particle method which causes particles to be put back into the bag.
   M.EQ.1: Particles are leaked through vents. See Remark 3.
   The 1’s digit controls the treatment of leakage.
   N.EQ.0: Always consider porosity leakage without considering blockage due to contact.
   N.EQ.1: Check if airbag node is in contact or not. If yes, 1/4 (quad) or 1/3 (tri) of the segment surface will not have porosity leakage due to contact.
   N.EQ.2: Same as 1 but no blockage for external vents
   N.EQ.3: Same as 1 but no blockage for internal vents
   N.EQ.4: Same as 1 but no blockage for all vents.
















   ..
       !! processed by numpydoc !!

.. py:property:: npdata
   :type: int


   
   Get or set the Number of parts or part sets data.
















   ..
       !! processed by numpydoc !!

.. py:property:: fric
   :type: float


   
   Get or set the Friction factor F_r if -1.0 < FRIC ≤ 1.0.  Otherwise,
   LE.-1.0:        |"FRIC" | is the curve ID which defines F_r as a function of the part pressure.
   GT.1.0: FRIC is the *DEFINE_FUNCTION ID that defines F_r.  See Remark 2
















   ..
       !! processed by numpydoc !!

.. py:property:: irdp
   :type: int


   
   Get or set the Dynamically scaling of particle radius
   EQ.0: Off
   EQ.1: On
















   ..
       !! processed by numpydoc !!

.. py:property:: segsid
   :type: Optional[int]


   
   Get or set the ID for a segment set.  The segments define the volume and should belong to the parts from SID1.
















   ..
       !! processed by numpydoc !!

.. py:property:: np
   :type: int


   
   Get or set the Number of particles (Default 200,000).
















   ..
       !! processed by numpydoc !!

.. py:property:: unit
   :type: int


   
   Get or set the Unit system
   EQ.0: kg-mm-ms-K
   EQ.1: SI-units
   EQ.2: tonne-mm-s-K.
   EQ.3:   User defined units (see Remark 11)
















   ..
       !! processed by numpydoc !!

.. py:property:: visflg
   :type: int


   
   Get or set the Visible particles(only support CPM database, see remark 6)
   EQ.0: Default to 1
   EQ.1: Output particle's coordinates, velocities, mass, radius, spin energy,
   translational energy
   EQ.2: Output reduce data set with corrdinates only
   EQ.3: Supress CPM database.
















   ..
       !! processed by numpydoc !!

.. py:property:: tatm
   :type: float


   
   Get or set the Atmospheric temperature (Default 293K).
















   ..
       !! processed by numpydoc !!

.. py:property:: patm
   :type: float


   
   Get or set the Atmospheric pressure (Default 1ATM).
















   ..
       !! processed by numpydoc !!

.. py:property:: nvent
   :type: int


   
   Get or set the Number of vent hole parts or part sets.
















   ..
       !! processed by numpydoc !!

.. py:property:: tend
   :type: float


   
   Get or set the Time when all particles (NP) have entered bag (Default 1.0e10).
















   ..
       !! processed by numpydoc !!

.. py:property:: tsw
   :type: float


   
   Get or set the Time for switch to control volume calculation (Default 1.0e10).
















   ..
       !! processed by numpydoc !!

.. py:property:: tstop
   :type: float


   
   Get or set the Time at which front tracking switches from IAIR = 4 to IAIR = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: tsmth
   :type: float


   
   Get or set the To avoid sudden jumps in the pressure signal during switching,
   the front tracking is tapered during a transition period.
   The default time of 1.0 millisecond will be applied if this value is set to zero
















   ..
       !! processed by numpydoc !!

.. py:property:: occup
   :type: float


   
   Get or set the Particles occupy OCCUP percent of the airbag’s volume.  The default value of OCCUP is 10%.
   This field can be used to balance computational cost and signal quality.  OCCUP ranges from 0.001 to 0.1..
















   ..
       !! processed by numpydoc !!

.. py:property:: rebl
   :type: int


   
   Get or set the If the option is ON, all energy stored from damping will be evenly distributed as vibrational energy to all particles.
   This improves the pressure calculation in certain applications.
   EQ.0:   Off (Default)
   EQ.1:   On.
















   ..
       !! processed by numpydoc !!

.. py:property:: sidsv
   :type: Optional[int]


   
   Get or set the Part set ID for internal shell part.  The volume formed by this internal shell part will be excluded from the bag volume.  These internal parts must have consistent orientation to get correct excluded volume.
















   ..
       !! processed by numpydoc !!

.. py:property:: psid1
   :type: Optional[int]


   
   Get or set the Part set ID for external parts which have normal pointed outward.  This option is usually used with airbag integrity check while there are two CPM bags connected with bag interaction.  Therefore, one of the bag can have the correct shell orientation but the share parts for the second bag will have wrong orientation.  This option will automatically flip the parts defined in this set in the second bag during integrity checking.
















   ..
       !! processed by numpydoc !!

.. py:property:: tsplit
   :type: Optional[float]


   
   Get or set the Start time to activate particle splitting algorithm. See Remark 15.
















   ..
       !! processed by numpydoc !!

.. py:property:: sffdc
   :type: float


   
   Get or set the Scale factor for the force decay constant.  SFFDC has a range of . The default value is 1.0.  The value given here will replaced the values from *CONTROL_CPM
















   ..
       !! processed by numpydoc !!

.. py:property:: sfiair4
   :type: float


   
   Get or set the Scale factor for the ratio of initial air particles to inflator gas particles for IAIR = 4.
   Smaller values weaken the effect of gas front tracking.
















   ..
       !! processed by numpydoc !!

.. py:property:: idfric
   :type: int


   
   Get or set the Direction of P2F impact force:
   EQ.0:   No change(default)
   EQ.1 : The force is applied in the segment normal direction
















   ..
       !! processed by numpydoc !!

.. py:property:: mass
   :type: Optional[float]


   
   Get or set the Conversion factor from current unit to MKS unit.  For example, if the current unit is using kg-mm-ms, the input should be 1.0, 0.001, 0.001.
















   ..
       !! processed by numpydoc !!

.. py:property:: time
   :type: Optional[float]


   
   Get or set the Conversion factor from current unit to MKS unit.  For example, if the current unit is using kg-mm-ms, the input should be 1.0, 0.001, 0.001.
















   ..
       !! processed by numpydoc !!

.. py:property:: length
   :type: Optional[float]


   
   Get or set the Conversion factor from current unit to MKS unit.  For example, if the current unit is using kg-mm-ms, the input should be 1.0, 0.001, 0.001.
















   ..
       !! processed by numpydoc !!

.. py:property:: iair
   :type: int


   
   Get or set the Initial gas inside bag considered:
   EQ.0:   No
   EQ.1:   Yes, using control volume method.
   EQ.-1:  Yes, using control volume method. In this case ambient air enters the bag when PATM is greater than bag pressure.
   EQ.2:   Yes, using the particle method.
   EQ.4:   Yes, using the particle method.  Initial air particles are used for the gas front tracking algorithm,
   but they do not apply forces when they collide with a segment.
   Instead, a uniform pressure is applied to the airbag based on the ratio of air and inflator particles.
   In this case NPRLX must be negative so that forces are not applied by the initial air.
















   ..
       !! processed by numpydoc !!

.. py:property:: ngas
   :type: Optional[int]


   
   Get or set the Number of gas components.
















   ..
       !! processed by numpydoc !!

.. py:property:: norif
   :type: Optional[int]


   
   Get or set the Number of orifices.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid1
   :type: int


   
   Get or set the NID1-NID3, Three nodes defining a moving coordinate system for the direction of flow through the gas inlet nozzles (Default fixed system).
















   ..
       !! processed by numpydoc !!

.. py:property:: nid2
   :type: int


   
   Get or set the NID1-NID3, Three nodes defining a moving coordinate system for the direction of flow through the gas inlet nozzles (Default fixed system).
















   ..
       !! processed by numpydoc !!

.. py:property:: nid3
   :type: int


   
   Get or set the NID1-NID3, Three nodes defining a moving coordinate system for the direction of flow through the gas inlet nozzles (Default fixed system).
















   ..
       !! processed by numpydoc !!

.. py:property:: chm
   :type: int


   
   Get or set the Chamber ID used in *DEFINE_CPM_CHAMBER.
















   ..
       !! processed by numpydoc !!

.. py:property:: cd_ext
   :type: float


   
   Get or set the Drag coefficient for external air. If the value is not zero, the inertial effect
   from external air will be considered and forces will be applied in the normal
   direction on the exterior airbag surface.
















   ..
       !! processed by numpydoc !!

.. py:property:: sidup
   :type: Optional[int]


   
   Get or set the Part or part set ID defining the internal parts that pressure will be applied to.
   This internal structure acts as a valve to control the external vent hole area.
   Pressure will be applied only after switch to UP (uniform pressure) using TSW.
















   ..
       !! processed by numpydoc !!

.. py:property:: styup
   :type: int


   
   Get or set the Set defining internal parts will be applied pressure
   Set type EQ.0: Part
   EQ.1: Part set.
















   ..
       !! processed by numpydoc !!

.. py:property:: pfrac
   :type: float


   
   Get or set the Part or part set ID defining the internal parts that pressure will be applied to.
   This internal structure acts as a valve to control the external vent hole area.
   Pressure will be applied only after switch to UP (uniform pressure) using TSW.
















   ..
       !! processed by numpydoc !!

.. py:property:: linking
   :type: Optional[int]


   
   Get or set the Part ID of an internal part that is coupled to the external vent definition.
   The minimum area of this part or the vent hole will be used for actual venting area.
















   ..
       !! processed by numpydoc !!

.. py:property:: sidh
   :type: Optional[int]


   
   Get or set the Part or part set ID defining part data.
















   ..
       !! processed by numpydoc !!

.. py:property:: stypeh
   :type: int


   
   Get or set the Set type EQ.0: Part
   EQ.1: Part set.
   EQ.2: part and HCONV is the *DEFINE_CPM_NPDATA ID
   EQ.3: part set and HCONV is the * DEFINE_CPM_NPDATA ID
















   ..
       !! processed by numpydoc !!

.. py:property:: hconv
   :type: Optional[float]


   
   Get or set the Heat convection coefficient used to calculate heat loss from the airbag external surface to ambient (W/K/m2).
   See *AIRBAG_HYBRID developments (Resp. P.O. Marklund).
   LT.0:   |HCONV | is a load curve ID defines heat convection coefficient as a function of time.
   When STYPEH is greater than 1, HCONV is an integer of *DEFINE_CPM_NPDATA ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: pfric
   :type: float


   
   Get or set the Friction factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: sdfblk
   :type: float


   
   Get or set the Scale down factor for blockage factor (Default=1, no scale down). The val-id factor will be (0,1]. If 0, it will set to 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: kp
   :type: float


   
   Get or set the Thermal conductivity of the part.
















   ..
       !! processed by numpydoc !!

.. py:property:: inip
   :type: int


   
   Get or set the Place initial air particles on surface.
   EQ.0:   yes (default)
   EQ.1:   no
   This feature exclude surfaces from initial particle placement.  This option is useful for preventing particles from being trapped between adjacent fabric layers..
















   ..
       !! processed by numpydoc !!

.. py:property:: cp
   :type: Optional[float]


   
   Get or set the Specific heat (see Remark 16).
















   ..
       !! processed by numpydoc !!

.. py:property:: sid3
   :type: Optional[int]


   
   Get or set the Part or part set ID defining vent holes.
















   ..
       !! processed by numpydoc !!

.. py:property:: stype3
   :type: int


   
   Get or set the Set type:
   EQ.0: Part
   EQ.1: Part set which each part being treated separately.
   EQ.2:   Part set and all parts are treated as one vent.  See Remark 13
















   ..
       !! processed by numpydoc !!

.. py:property:: c23
   :type: float


   
   Get or set the GE.0:    Vent hole coefficient, a parameter of Wang-Nefske leakage.  A value between 0.0 and 1.0 can be input.  See Remark 1.
   LT.0:   ID for *DEFINE_CPM_VENT.
















   ..
       !! processed by numpydoc !!

.. py:property:: lctc23
   :type: Optional[int]


   
   Get or set the Load curve defining vent hole coefficient as a function of time.  LCTC23 can be defined through *DEFINE_CURVE_FUNCTION.  If omitted, a curve equal to 1.0 used.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcpc23
   :type: Optional[int]


   
   Get or set the Load curve defining vent hole coefficient as a function of pressure.  If omitted a curve equal to 1.0 is used..
















   ..
       !! processed by numpydoc !!

.. py:property:: enh_v
   :type: int


   
   Get or set the Enhanced venting option. See Remark 8.
   EQ.0:   Off (default)
   EQ.1:   On
   EQ.2:   Two way flow for internal vent; treated as hole for external vent .
















   ..
       !! processed by numpydoc !!

.. py:property:: ppop
   :type: float


   
   Get or set the Pressure difference between interior and ambient pressure (PATM) to open the vent holes.  Once the vents are open, they will stay open.
















   ..
       !! processed by numpydoc !!

.. py:property:: pair
   :type: Optional[float]


   
   Get or set the Initial pressure inside bag .
















   ..
       !! processed by numpydoc !!

.. py:property:: tair
   :type: float


   
   Get or set the Initial temperature inside bag .
















   ..
       !! processed by numpydoc !!

.. py:property:: xmair
   :type: Optional[float]


   
   Get or set the Molar mass of gas initially inside bag.
   LT.0:   -XMAIR references the ID of a *DEFINE_CPM_GAS_PROPERTIES keyword that defines the gas thermodynamic properties.
   Note that AAIR, BAIR, and CAIR are ignored
















   ..
       !! processed by numpydoc !!

.. py:property:: aair
   :type: Optional[float]


   
   Get or set the Constant, linear, and quadratic heat capacity parameters.
















   ..
       !! processed by numpydoc !!

.. py:property:: bair
   :type: float


   
   Get or set the Constant, linear, and quadratic heat capacity parameters.
















   ..
       !! processed by numpydoc !!

.. py:property:: cair
   :type: float


   
   Get or set the Constant, linear, and quadratic heat capacity parameters.
















   ..
       !! processed by numpydoc !!

.. py:property:: npair
   :type: int


   
   Get or set the Number of particle for air.
















   ..
       !! processed by numpydoc !!

.. py:property:: nprlx
   :type: str


   
   Get or set the Number of cycles to reach thermal equilibrium.  See Remark 6.
   LT.0:   If more than 50% of the collision to fabric is from initial air particles, the contact force will not apply to the fabric segment in order to keep its original shape.
   If the number contains “.”, “e” or “E”, NPRLX will treated as an end time rather than as a cycle count.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcmi
   :type: Optional[int]


   
   Get or set the Mass flow rate curve for gas component i, unless the MOLEFRACTION option is used.
   If the MOLEFRACTION option is used, then it is the time dependent molar fraction of the total flow for gas component i.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcti
   :type: Optional[int]


   
   Get or set the Temperature curve for gas component i.
















   ..
       !! processed by numpydoc !!

.. py:property:: xmi
   :type: Optional[float]


   
   Get or set the Molar mass of gas component i.
   LT.0:   the absolute value of XMi references the ID of a *DEFINE_‌CPM_‌GAS_‌PROPERTIES keyword that defines the gas thermodynamic properties.
   Note that Ai, Bi, and Ci are ignored
















   ..
       !! processed by numpydoc !!

.. py:property:: ai
   :type: Optional[float]


   
   Get or set the Constant, linear, and quadratic heat capacity parameters for gas component i.
















   ..
       !! processed by numpydoc !!

.. py:property:: bi
   :type: float


   
   Get or set the Constant, linear, and quadratic heat capacity parameters for gas component i.
















   ..
       !! processed by numpydoc !!

.. py:property:: ci
   :type: float


   
   Get or set the Constant, linear, and quadratic heat capacity parameters for gas component i.
















   ..
       !! processed by numpydoc !!

.. py:property:: infgi
   :type: int


   
   Get or set the Inflator ID that this gas component belongs to (Default 1).
















   ..
       !! processed by numpydoc !!

.. py:property:: nidi
   :type: Optional[int]


   
   Get or set the Node ID/Shell ID defining the location of nozzle i.
















   ..
       !! processed by numpydoc !!

.. py:property:: ani
   :type: Optional[float]


   
   Get or set the Area of nozzle i (Default all nozzles are given the same area).
















   ..
       !! processed by numpydoc !!

.. py:property:: vdi
   :type: Optional[int]


   
   Get or set the GT.0:    Vector ID.  Initial direction of gas inflow at nozzle i.
   LT.0:   Values in the NIDi fields are interpreted as shell IDs.  See Remark 12.
   EQ.-1:  direction of gas inflow is using shell normal
   EQ.-2:  direction of gas inflow is in reversed shell normal.
















   ..
       !! processed by numpydoc !!

.. py:property:: cai
   :type: float


   
   Get or set the Cone angle in degrees (defaults to30°). This option is used only when IANG is equal to 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: infoi
   :type: int


   
   Get or set the Inflator ID for this orifice.  (default = 1).
















   ..
       !! processed by numpydoc !!

.. py:property:: imom
   :type: int


   
   Get or set the Inflator reaction forces
   EQ.0: Off
   EQ.1: On
















   ..
       !! processed by numpydoc !!

.. py:property:: iang
   :type: int


   
   Get or set the Activation for cone angle to use for friction calibration(should not use in the normal runs)
   EQ.0: Off(Default)
   EQ.1: On.
















   ..
       !! processed by numpydoc !!

.. py:property:: chm_id
   :type: Optional[int]


   
   Get or set the Chamber ID where the inflator node resides.  Chambers are defined using the *DEFINE_CPM_CHAMBER keyword.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'AIRBAG'


.. py:attribute:: subkeyword
   :value: 'PARTICLE_MPP_DECOMPOSITION_SEGMENT_ID'






