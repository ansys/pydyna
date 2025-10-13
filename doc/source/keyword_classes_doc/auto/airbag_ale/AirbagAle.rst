





:class:`AirbagAle`
==================


.. py:class:: airbag_ale.AirbagAle(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA AIRBAG_ALE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: AirbagAle

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the Set ID.  This set ID contains the Lagrangian elements (segments) which make up the airbag and possibly the airbag canister/compartment and/or a simple representation of the inflator.
          * - :py:attr:`~sidtyp`
            - Get or set the Set type:
          * - :py:attr:`~mwd`
            - Get or set the Mass weighted damping factor, D. Used during the CV phase
          * - :py:attr:`~spsf`
            - Get or set the Stagnation pressure scale factor, 0≤γ≤1.  SPSF is needed during the CV phase.
          * - :py:attr:`~atmost`
            - Get or set the Atmospheric ambient temperature.  See Remark 2
          * - :py:attr:`~atmosp`
            - Get or set the Atmospheric ambient pressure.  See Remark 2
          * - :py:attr:`~gc`
            - Get or set the Universal molar gas constant
          * - :py:attr:`~cc`
            - Get or set the Conversion constant. EQ.0:       Set to 1.0
          * - :py:attr:`~tnkvol`
            - Get or set the Tank volume from the inflator tank test or inflator canister volume.
          * - :py:attr:`~tnkfinp`
            - Get or set the Tank final pressure from the inflator tank test data. Only define this parameter for option 1 of TNKVOL definition above.  See Remark 10
          * - :py:attr:`~nquad`
            - Get or set the Number of (quadrature) coupling points for coupling Lagrangian parts to ALE master solid parts.
          * - :py:attr:`~ctype`
            - Get or set the Coupling type (see Remark 12):
          * - :py:attr:`~pfac`
            - Get or set the Penalty factor.  PFAC is a scale factor for scaling the estimated stiffness of the interacting (coupling) system.  It is used to compute the coupling forces to be distributed on lagrangian and ALE parts.  See Remark 13.
          * - :py:attr:`~fric`
            - Get or set the Coupling coefficient of friction.
          * - :py:attr:`~frcmin`
            - Get or set the Minimum fluid volume fraction in an ALE element to activate coupling.
          * - :py:attr:`~normtyp`
            - Get or set the Penalty coupling spring direction:
          * - :py:attr:`~ileak`
            - Get or set the Leakage control flag. Default = 2 (with energy compensation)
          * - :py:attr:`~pleak`
            - Get or set the Leakage control penalty factor (default = 0.1)
          * - :py:attr:`~ivsetid`
            - Get or set the Set ID defining the venting hole surface(s).  See Remark 4
          * - :py:attr:`~ivtype`
            - Get or set the Type of IVSET:
          * - :py:attr:`~iblock`
            - Get or set the Flag for considering blockage effects for porosity and vents (see Remark 5):
          * - :py:attr:`~vntcof`
            - Get or set the Vent Coefficient for scaling the flow.  See Remark 6
          * - :py:attr:`~nx_ida`
            - Get or set the NX is the number of ALE elements to be generated in the x-direction.
          * - :py:attr:`~ny_idg`
            - Get or set the NY is the number of ALE elements to be generated in the y-direction
          * - :py:attr:`~nz`
            - Get or set the NZ is the number of ALE elements to be generated in the z-direction
          * - :py:attr:`~movern`
            - Get or set the ALE mesh automatic motion option.
          * - :py:attr:`~zoom`
            - Get or set the ALE mesh automatic expansion option:
          * - :py:attr:`~x0`
            - Get or set the Coordinates of origin for ALE mesh generation, if NZ != 0
          * - :py:attr:`~y0`
            - Get or set the Coordinates of origin for ALE mesh generation, if NZ != 0
          * - :py:attr:`~z0`
            - Get or set the Coordinates of origin for ALE mesh generation, if NZ != 0
          * - :py:attr:`~x1`
            - Get or set the Coordinates of point 1 for ALE mesh generation, if NZ != 0
          * - :py:attr:`~y1`
            - Get or set the Coordinates of point 1 for ALE mesh generation, if NZ != 0
          * - :py:attr:`~z1`
            - Get or set the Coordinates of point 1 for ALE mesh generation, if NZ != 0
          * - :py:attr:`~x2`
            - Get or set the Coordinates of point 2 for ALE mesh generation, if NZ != 0
          * - :py:attr:`~y2`
            - Get or set the Coordinates of point 2 for ALE mesh generation, if NZ != 0
          * - :py:attr:`~z2`
            - Get or set the Coordinates of point 2 for ALE mesh generation, if NZ != 0
          * - :py:attr:`~x3`
            - Get or set the Coordinates of point 3 for ALE mesh generation, if NZ != 0
          * - :py:attr:`~y3`
            - Get or set the Coordinates of point 3 for ALE mesh generation, if NZ != 0
          * - :py:attr:`~z3`
            - Get or set the Coordinates of point 3 for ALE mesh generation, if NZ != 0
          * - :py:attr:`~swtime`
            - Get or set the Time to switch from ALE bag to control volume (AIRBAG_HYBRID). EQ:0.0 switch to control volume will take place at time equal 0.0. If this field is not defined (blnak) switch time will be set to 1.0e16.
          * - :py:attr:`~hg`
            - Get or set the Hourglass coefficient for ALE fluid mesh
          * - :py:attr:`~nair`
            - Get or set the Number of air components.  For example, this equals 2 when air contains 80% of N2 and 20% of O2.  If air is defined as a single gas, then NAIR = 1
          * - :py:attr:`~ngas`
            - Get or set the Number of inflator gas components
          * - :py:attr:`~norif`
            - Get or set the Number of point sources or orifices.  This determines the number of point source cards to be read
          * - :py:attr:`~lcvel`
            - Get or set the Load curve ID for inlet velocity (see also TNKVOL & TNKFINP of Card 2 above).
          * - :py:attr:`~lct`
            - Get or set the Load curve ID for inlet gas temperature (see *AIRBAG_HYBRID)
          * - :py:attr:`~mwair`
            - Get or set the Molecular weight of air component
          * - :py:attr:`~initm`
            - Get or set the Initial Mass Fraction of air component(s)
          * - :py:attr:`~aira`
            - Get or set the First Coefficient of molar heat capacity at constant pressure
          * - :py:attr:`~airb`
            - Get or set the Second Coefficient of molar heat capacity at constant pressure
          * - :py:attr:`~airc`
            - Get or set the Third Coefficient of molar heat capacity at constant pressure
          * - :py:attr:`~lcmf`
            - Get or set the Load curve ID for mass flow rate
          * - :py:attr:`~mwgas`
            - Get or set the Molecular weight of inflator gas components
          * - :py:attr:`~gasa`
            - Get or set the First Coefficient of molar heat capacity at constant pressure
          * - :py:attr:`~gasb`
            - Get or set the Second Coefficient of molar heat capacity at constant pressure
          * - :py:attr:`~gasc`
            - Get or set the Third Coefficient of molar heat capacity at constant pressure
          * - :py:attr:`~nodeid`
            - Get or set the Node ID defining the point source
          * - :py:attr:`~vecid`
            - Get or set the Vector Id defining the direction of flow at the point source
          * - :py:attr:`~orifare`
            - Get or set the Orifice area at the point source


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

    from airbag_ale import AirbagAle

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Set ID.  This set ID contains the Lagrangian elements (segments) which make up the airbag and possibly the airbag canister/compartment and/or a simple representation of the inflator.
















   ..
       !! processed by numpydoc !!

.. py:property:: sidtyp
   :type: int


   
   Get or set the Set type:
   EQ.0: Segment set
   EQ.1: Part set
















   ..
       !! processed by numpydoc !!

.. py:property:: mwd
   :type: float


   
   Get or set the Mass weighted damping factor, D. Used during the CV phase
















   ..
       !! processed by numpydoc !!

.. py:property:: spsf
   :type: float


   
   Get or set the Stagnation pressure scale factor, 0≤γ≤1.  SPSF is needed during the CV phase.
















   ..
       !! processed by numpydoc !!

.. py:property:: atmost
   :type: float


   
   Get or set the Atmospheric ambient temperature.  See Remark 2
















   ..
       !! processed by numpydoc !!

.. py:property:: atmosp
   :type: float


   
   Get or set the Atmospheric ambient pressure.  See Remark 2
















   ..
       !! processed by numpydoc !!

.. py:property:: gc
   :type: Optional[float]


   
   Get or set the Universal molar gas constant
















   ..
       !! processed by numpydoc !!

.. py:property:: cc
   :type: float


   
   Get or set the Conversion constant. EQ.0:       Set to 1.0
















   ..
       !! processed by numpydoc !!

.. py:property:: tnkvol
   :type: float


   
   Get or set the Tank volume from the inflator tank test or inflator canister volume.
   LCVEL = 0 and TNKFINP is defined:
   TNKVOL is the defined tank.  Inlet gas velocity is estimated by LS-DYNA method (testing).
   LCVEL = 0 and TNKFINP is not defined:
   TNKVOL is the estimated inflator canister volume inlet gas velocity is estimated automatically by the Lian-Bhalsod-Olovssonmethod.
   LCVEL ≠ 0:
   This must be left blank
















   ..
       !! processed by numpydoc !!

.. py:property:: tnkfinp
   :type: float


   
   Get or set the Tank final pressure from the inflator tank test data. Only define this parameter for option 1 of TNKVOL definition above.  See Remark 10
















   ..
       !! processed by numpydoc !!

.. py:property:: nquad
   :type: int


   
   Get or set the Number of (quadrature) coupling points for coupling Lagrangian parts to ALE master solid parts.
   If NQUAD = n, then nxn coupling points will be parametrically distributed over the surface of each Lagrangian segment.
















   ..
       !! processed by numpydoc !!

.. py:property:: ctype
   :type: int


   
   Get or set the Coupling type (see Remark 12):
   EQ.4:   Penalty coupling with coupling in the normal direction under compression only(default).
   EQ.6 : Penalty coupling in which coupling is under both tension and compression in the normal direction for the unfolded regionand under only compression in the normal direction for folded region.
















   ..
       !! processed by numpydoc !!

.. py:property:: pfac
   :type: float


   
   Get or set the Penalty factor.  PFAC is a scale factor for scaling the estimated stiffness of the interacting (coupling) system.  It is used to compute the coupling forces to be distributed on lagrangian and ALE parts.  See Remark 13.
   GT.0:   Fraction of estimated critical stiffness.
   LT.0:   -PFAC is a load curve ID.  The curve defines the relative coupling pressure (y-axis) as a function of the tolerable fluid penetration distance (x-axis)
















   ..
       !! processed by numpydoc !!

.. py:property:: fric
   :type: float


   
   Get or set the Coupling coefficient of friction.
















   ..
       !! processed by numpydoc !!

.. py:property:: frcmin
   :type: float


   
   Get or set the Minimum fluid volume fraction in an ALE element to activate coupling.
















   ..
       !! processed by numpydoc !!

.. py:property:: normtyp
   :type: int


   
   Get or set the Penalty coupling spring direction:
   EQ.0:   Normal vectors are interpolated from nodal normals
   EQ.1:   Normal vectors are interpolated from segment normals.
















   ..
       !! processed by numpydoc !!

.. py:property:: ileak
   :type: int


   
   Get or set the Leakage control flag. Default = 2 (with energy compensation)
















   ..
       !! processed by numpydoc !!

.. py:property:: pleak
   :type: float


   
   Get or set the Leakage control penalty factor (default = 0.1)
















   ..
       !! processed by numpydoc !!

.. py:property:: ivsetid
   :type: int


   
   Get or set the Set ID defining the venting hole surface(s).  See Remark 4
















   ..
       !! processed by numpydoc !!

.. py:property:: ivtype
   :type: int


   
   Get or set the Type of IVSET:
   EQ.0: Part Set
   EQ.1: Part ID
   EQ.2: Segment Set
















   ..
       !! processed by numpydoc !!

.. py:property:: iblock
   :type: int


   
   Get or set the Flag for considering blockage effects for porosity and vents (see Remark 5):
   EQ.0:   no (blockage is NOT considered, default).
   EQ.1:   yes (blockage is considered)
















   ..
       !! processed by numpydoc !!

.. py:property:: vntcof
   :type: float


   
   Get or set the Vent Coefficient for scaling the flow.  See Remark 6
















   ..
       !! processed by numpydoc !!

.. py:property:: nx_ida
   :type: Optional[int]


   
   Get or set the NX is the number of ALE elements to be generated in the x-direction.
   IDA is the Part ID of the initial air mesh
















   ..
       !! processed by numpydoc !!

.. py:property:: ny_idg
   :type: Optional[int]


   
   Get or set the NY is the number of ALE elements to be generated in the y-direction
   IDG is the Part ID of the initial air mesh
















   ..
       !! processed by numpydoc !!

.. py:property:: nz
   :type: int


   
   Get or set the NZ is the number of ALE elements to be generated in the z-direction
   Leave blank to activate
















   ..
       !! processed by numpydoc !!

.. py:property:: movern
   :type: int


   
   Get or set the ALE mesh automatic motion option.
   EQ.0:   ALE mesh is fixed in space.
   GT.0:   Node group ID.  See *ALE_REFERENCE_SYSTEM_NODE ALE mesh can be moved with PRTYP = 5,
   mesh motion follows a coordinate system defined by 3 reference nodes.  See Remark 7
















   ..
       !! processed by numpydoc !!

.. py:property:: zoom
   :type: int


   
   Get or set the ALE mesh automatic expansion option:
   EQ.0:   Do not expand ALE mesh
   EQ.1:   Expand/contract ALE mesh by keeping all airbag parts contained within the ALE mesh (equivalent to PRTYP = 9).  See Remark 8.
















   ..
       !! processed by numpydoc !!

.. py:property:: x0
   :type: Optional[float]


   
   Get or set the Coordinates of origin for ALE mesh generation, if NZ != 0
















   ..
       !! processed by numpydoc !!

.. py:property:: y0
   :type: Optional[float]


   
   Get or set the Coordinates of origin for ALE mesh generation, if NZ != 0
















   ..
       !! processed by numpydoc !!

.. py:property:: z0
   :type: Optional[float]


   
   Get or set the Coordinates of origin for ALE mesh generation, if NZ != 0
















   ..
       !! processed by numpydoc !!

.. py:property:: x1
   :type: Optional[float]


   
   Get or set the Coordinates of point 1 for ALE mesh generation, if NZ != 0
















   ..
       !! processed by numpydoc !!

.. py:property:: y1
   :type: Optional[float]


   
   Get or set the Coordinates of point 1 for ALE mesh generation, if NZ != 0
















   ..
       !! processed by numpydoc !!

.. py:property:: z1
   :type: Optional[float]


   
   Get or set the Coordinates of point 1 for ALE mesh generation, if NZ != 0
















   ..
       !! processed by numpydoc !!

.. py:property:: x2
   :type: Optional[float]


   
   Get or set the Coordinates of point 2 for ALE mesh generation, if NZ != 0
















   ..
       !! processed by numpydoc !!

.. py:property:: y2
   :type: Optional[float]


   
   Get or set the Coordinates of point 2 for ALE mesh generation, if NZ != 0
















   ..
       !! processed by numpydoc !!

.. py:property:: z2
   :type: Optional[float]


   
   Get or set the Coordinates of point 2 for ALE mesh generation, if NZ != 0
















   ..
       !! processed by numpydoc !!

.. py:property:: x3
   :type: Optional[float]


   
   Get or set the Coordinates of point 3 for ALE mesh generation, if NZ != 0
















   ..
       !! processed by numpydoc !!

.. py:property:: y3
   :type: Optional[float]


   
   Get or set the Coordinates of point 3 for ALE mesh generation, if NZ != 0
















   ..
       !! processed by numpydoc !!

.. py:property:: z3
   :type: Optional[float]


   
   Get or set the Coordinates of point 3 for ALE mesh generation, if NZ != 0
















   ..
       !! processed by numpydoc !!

.. py:property:: swtime
   :type: float


   
   Get or set the Time to switch from ALE bag to control volume (AIRBAG_HYBRID). EQ:0.0 switch to control volume will take place at time equal 0.0. If this field is not defined (blnak) switch time will be set to 1.0e16.
















   ..
       !! processed by numpydoc !!

.. py:property:: hg
   :type: float


   
   Get or set the Hourglass coefficient for ALE fluid mesh
















   ..
       !! processed by numpydoc !!

.. py:property:: nair
   :type: int


   
   Get or set the Number of air components.  For example, this equals 2 when air contains 80% of N2 and 20% of O2.  If air is defined as a single gas, then NAIR = 1
















   ..
       !! processed by numpydoc !!

.. py:property:: ngas
   :type: int


   
   Get or set the Number of inflator gas components
















   ..
       !! processed by numpydoc !!

.. py:property:: norif
   :type: int


   
   Get or set the Number of point sources or orifices.  This determines the number of point source cards to be read
















   ..
       !! processed by numpydoc !!

.. py:property:: lcvel
   :type: Optional[int]


   
   Get or set the Load curve ID for inlet velocity (see also TNKVOL & TNKFINP of Card 2 above).
   This is the same estimated velocity curve used in *SECTION_POINT_SOURCE_MIXTURE card.
















   ..
       !! processed by numpydoc !!

.. py:property:: lct
   :type: Optional[int]


   
   Get or set the Load curve ID for inlet gas temperature (see *AIRBAG_HYBRID)
















   ..
       !! processed by numpydoc !!

.. py:property:: mwair
   :type: float


   
   Get or set the Molecular weight of air component
















   ..
       !! processed by numpydoc !!

.. py:property:: initm
   :type: float


   
   Get or set the Initial Mass Fraction of air component(s)
















   ..
       !! processed by numpydoc !!

.. py:property:: aira
   :type: float


   
   Get or set the First Coefficient of molar heat capacity at constant pressure
















   ..
       !! processed by numpydoc !!

.. py:property:: airb
   :type: float


   
   Get or set the Second Coefficient of molar heat capacity at constant pressure
















   ..
       !! processed by numpydoc !!

.. py:property:: airc
   :type: float


   
   Get or set the Third Coefficient of molar heat capacity at constant pressure
















   ..
       !! processed by numpydoc !!

.. py:property:: lcmf
   :type: Optional[int]


   
   Get or set the Load curve ID for mass flow rate
















   ..
       !! processed by numpydoc !!

.. py:property:: mwgas
   :type: float


   
   Get or set the Molecular weight of inflator gas components
















   ..
       !! processed by numpydoc !!

.. py:property:: gasa
   :type: float


   
   Get or set the First Coefficient of molar heat capacity at constant pressure
















   ..
       !! processed by numpydoc !!

.. py:property:: gasb
   :type: float


   
   Get or set the Second Coefficient of molar heat capacity at constant pressure
















   ..
       !! processed by numpydoc !!

.. py:property:: gasc
   :type: float


   
   Get or set the Third Coefficient of molar heat capacity at constant pressure
















   ..
       !! processed by numpydoc !!

.. py:property:: nodeid
   :type: int


   
   Get or set the Node ID defining the point source
















   ..
       !! processed by numpydoc !!

.. py:property:: vecid
   :type: int


   
   Get or set the Vector Id defining the direction of flow at the point source
















   ..
       !! processed by numpydoc !!

.. py:property:: orifare
   :type: float


   
   Get or set the Orifice area at the point source
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'AIRBAG'


.. py:attribute:: subkeyword
   :value: 'ALE'






