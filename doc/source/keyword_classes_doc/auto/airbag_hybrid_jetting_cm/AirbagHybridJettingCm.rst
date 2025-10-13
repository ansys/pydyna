





:class:`AirbagHybridJettingCm`
==============================


.. py:class:: airbag_hybrid_jetting_cm.AirbagHybridJettingCm(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA AIRBAG_HYBRID_JETTING_CM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: AirbagHybridJettingCm

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the Set ID.
          * - :py:attr:`~sidtyp`
            - Get or set the Set type:
          * - :py:attr:`~rbid`
            - Get or set the Rigid body part ID for user defined activation subroutine:
          * - :py:attr:`~vsca`
            - Get or set the Volume scale factor, V-sca (default=1.0).
          * - :py:attr:`~psca`
            - Get or set the Pressure scale factor, P-sca (default=1.0).
          * - :py:attr:`~vini`
            - Get or set the Initial filled volume, V-ini (default=0.0).
          * - :py:attr:`~mwd`
            - Get or set the Mass weighted damping factor, D (default=0.0).
          * - :py:attr:`~spsf`
            - Get or set the Stagnation pressure scale factor, 0.0 <= gamma <= 1.0.
          * - :py:attr:`~atmost`
            - Get or set the Atmospheric temperature.
          * - :py:attr:`~atmosp`
            - Get or set the Atmospheric pressure.
          * - :py:attr:`~atmosd`
            - Get or set the Atmospheric density.
          * - :py:attr:`~gc`
            - Get or set the Universal molar gas constant.
          * - :py:attr:`~cc`
            - Get or set the Conversion constant (default=1.0).
          * - :py:attr:`~c23`
            - Get or set the Vent orifice coefficient which applies to exit hole. Set to zero if LCC23 is defined below.
          * - :py:attr:`~lcc23`
            - Get or set the Load curve number defining the vent orifice coefficient which applies to exit hole as a function of time. A nonzero value for C23 overrides LCC23.
          * - :py:attr:`~a23`
            - Get or set the Vent orifice area which applies to exit hole. Set to zero if LCA23 is defined below.
          * - :py:attr:`~lca23`
            - Get or set the Load curve number defining the vent orifice area which applies to exit hole as a function of absolute pressure. A nonzero value for A23 overrides LCA23.
          * - :py:attr:`~cp23`
            - Get or set the Orifice coefficient for leakage (fabric porosity). Set to zero if LCCP23 is defined below.
          * - :py:attr:`~lcp23`
            - Get or set the Load curve number defining the orifice coefficient for leakage (fabric porosity) as a function of time. A nonzero value for CP23 overrides LCCP23.
          * - :py:attr:`~ap23`
            - Get or set the Area for leakage (fabric porosity).
          * - :py:attr:`~lcap23`
            - Get or set the Load curve number defining the area for leakage (fabric porosity) as a function of (absolute) pressure. A nonzero value for AP23 overrides LCAP23.
          * - :py:attr:`~opt`
            - Get or set the Fabric venting option, if nonzero CP23, LCCP23, AP23, and LCAP23 are set to zero.
          * - :py:attr:`~pvent`
            - Get or set the Gauge pressure when venting begins.
          * - :py:attr:`~ngas`
            - Get or set the Number of gas inputs to be defined below (including initial air).
          * - :py:attr:`~lcefr`
            - Get or set the Optional curve for exit flow rate (mass/time) versus (gauge) pressure
          * - :py:attr:`~lcidm0`
            - Get or set the Optional curve representing inflator’s total mass inflow rate. When
          * - :py:attr:`~vntopt`
            - Get or set the Additional options for venting area definition.
          * - :py:attr:`~lcidm`
            - Get or set the Load curve ID for inflator mass flow rate (EQ.0 for gas in the bag at time 0).
          * - :py:attr:`~lcidt`
            - Get or set the Load curve ID for inflator gas temperature (EQ.0 for gas in the bag at time 0).
          * - :py:attr:`~mw`
            - Get or set the Molecular weight.
          * - :py:attr:`~initm`
            - Get or set the Initial mass fraction of gas component.
          * - :py:attr:`~a`
            - Get or set the Coefficient for molar heat capacity of inflator gas at constant pressure. (e.g., Joules/mole/oK)
          * - :py:attr:`~b`
            - Get or set the Coefficient for molar heat capacity of inflator gas at constant pressure. (e.g., Joules/mole/oK2)
          * - :py:attr:`~c`
            - Get or set the Coefficient for molar heat capacity of inflator gas at constant pressure. (e.g., Joules/mole/oK3)
          * - :py:attr:`~fmass`
            - Get or set the Fraction of additional aspirated mass.
          * - :py:attr:`~xjfp`
            - Get or set the x-coordinate of jet focal point.
          * - :py:attr:`~yjfp`
            - Get or set the y-coordinate of jet focal point.
          * - :py:attr:`~zjfp`
            - Get or set the z-coordinate of jet focal point.
          * - :py:attr:`~xjvh`
            - Get or set the x-coordinate of jet vector head to defined code centerline.
          * - :py:attr:`~yjvh`
            - Get or set the y-coordinate of jet vector head to defined code centerline.
          * - :py:attr:`~zjvh`
            - Get or set the z-coordinate of jet vector head to defined code centerline.
          * - :py:attr:`~ca`
            - Get or set the Cone angle, alpha, defined in radians./nLT.0.0: |alpha| is the load curve ID defining cone angle as a function of time.
          * - :py:attr:`~beta`
            - Get or set the Efficiency factor, beta, which scales the final value of pressure obtained from Bernoulli's equation.
          * - :py:attr:`~xsjfp`
            - Get or set the x-coordinate of secondary jet focal point, passenger side bag. If the coordinates of the secondary point are (0,0,0) then a conical jet (driver's side airbag) is assumed.
          * - :py:attr:`~ysjfp`
            - Get or set the y-coordinate of secondary jet focal point.
          * - :py:attr:`~zsjfp`
            - Get or set the z-coordinate of secondary jet focal point.
          * - :py:attr:`~psid`
            - Get or set the Optional part set ID, see *SET_PART.
          * - :py:attr:`~idum`
            - Get or set the Dummy field (variable not used).
          * - :py:attr:`~node1`
            - Get or set the Node ID located at the jet focal point.
          * - :py:attr:`~node2`
            - Get or set the Node ID for node along the axis of the jet.
          * - :py:attr:`~node3`
            - Get or set the Optional node ID located at secondary jet focal point.
          * - :py:attr:`~nreact`
            - Get or set the Node for reacting jet force.


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

    from airbag_hybrid_jetting_cm import AirbagHybridJettingCm

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: sidtyp
   :type: int


   
   Get or set the Set type:
   EQ.0: segment,
   EQ.1: part IDs.
















   ..
       !! processed by numpydoc !!

.. py:property:: rbid
   :type: int


   
   Get or set the Rigid body part ID for user defined activation subroutine:
   EQ.-RBID: sensor subroutine flags initiates the inflator. Load curves are offset by initiation time,
   EQ.0: the control volume is active from time zero,
   EQ.RBID: user sensor subroutine flags the start of the inflation. Load curves are offset by initiation time.
















   ..
       !! processed by numpydoc !!

.. py:property:: vsca
   :type: float


   
   Get or set the Volume scale factor, V-sca (default=1.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: psca
   :type: float


   
   Get or set the Pressure scale factor, P-sca (default=1.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: vini
   :type: float


   
   Get or set the Initial filled volume, V-ini (default=0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: mwd
   :type: float


   
   Get or set the Mass weighted damping factor, D (default=0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: spsf
   :type: float


   
   Get or set the Stagnation pressure scale factor, 0.0 <= gamma <= 1.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: atmost
   :type: Optional[float]


   
   Get or set the Atmospheric temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: atmosp
   :type: Optional[float]


   
   Get or set the Atmospheric pressure.
















   ..
       !! processed by numpydoc !!

.. py:property:: atmosd
   :type: Optional[float]


   
   Get or set the Atmospheric density.
















   ..
       !! processed by numpydoc !!

.. py:property:: gc
   :type: Optional[float]


   
   Get or set the Universal molar gas constant.
















   ..
       !! processed by numpydoc !!

.. py:property:: cc
   :type: float


   
   Get or set the Conversion constant (default=1.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: c23
   :type: Optional[float]


   
   Get or set the Vent orifice coefficient which applies to exit hole. Set to zero if LCC23 is defined below.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcc23
   :type: int


   
   Get or set the Load curve number defining the vent orifice coefficient which applies to exit hole as a function of time. A nonzero value for C23 overrides LCC23.
















   ..
       !! processed by numpydoc !!

.. py:property:: a23
   :type: Optional[float]


   
   Get or set the Vent orifice area which applies to exit hole. Set to zero if LCA23 is defined below.
















   ..
       !! processed by numpydoc !!

.. py:property:: lca23
   :type: int


   
   Get or set the Load curve number defining the vent orifice area which applies to exit hole as a function of absolute pressure. A nonzero value for A23 overrides LCA23.
















   ..
       !! processed by numpydoc !!

.. py:property:: cp23
   :type: Optional[float]


   
   Get or set the Orifice coefficient for leakage (fabric porosity). Set to zero if LCCP23 is defined below.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcp23
   :type: int


   
   Get or set the Load curve number defining the orifice coefficient for leakage (fabric porosity) as a function of time. A nonzero value for CP23 overrides LCCP23.
















   ..
       !! processed by numpydoc !!

.. py:property:: ap23
   :type: Optional[float]


   
   Get or set the Area for leakage (fabric porosity).
















   ..
       !! processed by numpydoc !!

.. py:property:: lcap23
   :type: int


   
   Get or set the Load curve number defining the area for leakage (fabric porosity) as a function of (absolute) pressure. A nonzero value for AP23 overrides LCAP23.
















   ..
       !! processed by numpydoc !!

.. py:property:: opt
   :type: int


   
   Get or set the Fabric venting option, if nonzero CP23, LCCP23, AP23, and LCAP23 are set to zero.
   EQ.1: Wang-Nefske formulas for venting through an orifice are used. Blockage is not considered (default).
   EQ.2: Wang-Nefske formulas for venting through an orifice are used. Blockage of venting area due to contact is considered.
   EQ.3: Leakage formulas of Graefe, Krummheuer, and Siejak [1990] are used. Blockage is not considered.
   EQ.4: Leakage formulas of Graefe, Krummheuer, and Siejak [1990] are used. Blockage of venting area due to contact is considered.
   EQ.5: Leakage formulas based on flow through a porous media are used. Blockage is not considered.
   EQ.6: Leakage formulas based on flow through a porous media are used. Blockage of venting area due to contact is considered.
   EQ.7: Simple porosity model. Blockage is not considered.
   EQ.8: Simple porosity model. Blockage of venting area due to contact is considered.
















   ..
       !! processed by numpydoc !!

.. py:property:: pvent
   :type: Optional[float]


   
   Get or set the Gauge pressure when venting begins.
















   ..
       !! processed by numpydoc !!

.. py:property:: ngas
   :type: Optional[int]


   
   Get or set the Number of gas inputs to be defined below (including initial air).
















   ..
       !! processed by numpydoc !!

.. py:property:: lcefr
   :type: int


   
   Get or set the Optional curve for exit flow rate (mass/time) versus (gauge) pressure
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidm0
   :type: int


   
   Get or set the Optional curve representing inflator’s total mass inflow rate. When
   defined, LCIDM in the following 2*NGAS cards defines the molar
   fraction of each gas component as a function of time and INITM
   defines the initial molar ratio of each gas component..
















   ..
       !! processed by numpydoc !!

.. py:property:: vntopt
   :type: Optional[int]


   
   Get or set the Additional options for venting area definition.
   For A23 ≥ 0
   EQ.1: Vent area is equal to A23.
   EQ.2: Vent area is A23 plus the eroded surface area of the airbag parts.
   EQ.10: Same as VNTOPT = 2
   For A23 < 0
   EQ.1: Vent area is the increase in surface area of part |A23|. If there is no change in surface area of part |A23| over the
   initial surface area or if the surface area reduces from the initial area, there is no venting.
   EQ.2: Vent area is the total (not change in) surface area of part
   |A23| plus the eroded surface area of all other parts comprising the airbag.
   EQ.10: Vent area is the increase in surface area of part |A23| plus
   the eroded surface area of all other parts comprising the airbag.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidm
   :type: Optional[int]


   
   Get or set the Load curve ID for inflator mass flow rate (EQ.0 for gas in the bag at time 0).
   GT.0: piece wise linear interpolation
   LT.0: cubic spline interpolation
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidt
   :type: Optional[int]


   
   Get or set the Load curve ID for inflator gas temperature (EQ.0 for gas in the bag at time 0).
   GT.0: piece wise linear interpolation
   LT.0: cubic spline interpolation
















   ..
       !! processed by numpydoc !!

.. py:property:: mw
   :type: Optional[float]


   
   Get or set the Molecular weight.
















   ..
       !! processed by numpydoc !!

.. py:property:: initm
   :type: Optional[float]


   
   Get or set the Initial mass fraction of gas component.
















   ..
       !! processed by numpydoc !!

.. py:property:: a
   :type: Optional[float]


   
   Get or set the Coefficient for molar heat capacity of inflator gas at constant pressure. (e.g., Joules/mole/oK)
















   ..
       !! processed by numpydoc !!

.. py:property:: b
   :type: Optional[float]


   
   Get or set the Coefficient for molar heat capacity of inflator gas at constant pressure. (e.g., Joules/mole/oK2)
















   ..
       !! processed by numpydoc !!

.. py:property:: c
   :type: Optional[float]


   
   Get or set the Coefficient for molar heat capacity of inflator gas at constant pressure. (e.g., Joules/mole/oK3)
















   ..
       !! processed by numpydoc !!

.. py:property:: fmass
   :type: Optional[float]


   
   Get or set the Fraction of additional aspirated mass.
















   ..
       !! processed by numpydoc !!

.. py:property:: xjfp
   :type: Optional[float]


   
   Get or set the x-coordinate of jet focal point.
















   ..
       !! processed by numpydoc !!

.. py:property:: yjfp
   :type: Optional[float]


   
   Get or set the y-coordinate of jet focal point.
















   ..
       !! processed by numpydoc !!

.. py:property:: zjfp
   :type: Optional[float]


   
   Get or set the z-coordinate of jet focal point.
















   ..
       !! processed by numpydoc !!

.. py:property:: xjvh
   :type: Optional[float]


   
   Get or set the x-coordinate of jet vector head to defined code centerline.
















   ..
       !! processed by numpydoc !!

.. py:property:: yjvh
   :type: Optional[float]


   
   Get or set the y-coordinate of jet vector head to defined code centerline.
















   ..
       !! processed by numpydoc !!

.. py:property:: zjvh
   :type: Optional[float]


   
   Get or set the z-coordinate of jet vector head to defined code centerline.
















   ..
       !! processed by numpydoc !!

.. py:property:: ca
   :type: Optional[float]


   
   Get or set the Cone angle, alpha, defined in radians./nLT.0.0: |alpha| is the load curve ID defining cone angle as a function of time.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Efficiency factor, beta, which scales the final value of pressure obtained from Bernoulli's equation.
   LT.0.0: |beta| is the load curve ID defining the efficiency factor as a function of time.
















   ..
       !! processed by numpydoc !!

.. py:property:: xsjfp
   :type: Optional[float]


   
   Get or set the x-coordinate of secondary jet focal point, passenger side bag. If the coordinates of the secondary point are (0,0,0) then a conical jet (driver's side airbag) is assumed.
















   ..
       !! processed by numpydoc !!

.. py:property:: ysjfp
   :type: Optional[float]


   
   Get or set the y-coordinate of secondary jet focal point.
















   ..
       !! processed by numpydoc !!

.. py:property:: zsjfp
   :type: Optional[float]


   
   Get or set the z-coordinate of secondary jet focal point.
















   ..
       !! processed by numpydoc !!

.. py:property:: psid
   :type: Optional[int]


   
   Get or set the Optional part set ID, see *SET_PART.
   EQ.0: all elements are included in the airbag.
















   ..
       !! processed by numpydoc !!

.. py:property:: idum
   :type: Optional[float]


   
   Get or set the Dummy field (variable not used).
















   ..
       !! processed by numpydoc !!

.. py:property:: node1
   :type: int


   
   Get or set the Node ID located at the jet focal point.
















   ..
       !! processed by numpydoc !!

.. py:property:: node2
   :type: int


   
   Get or set the Node ID for node along the axis of the jet.
















   ..
       !! processed by numpydoc !!

.. py:property:: node3
   :type: int


   
   Get or set the Optional node ID located at secondary jet focal point.
















   ..
       !! processed by numpydoc !!

.. py:property:: nreact
   :type: int


   
   Get or set the Node for reacting jet force.
   EQ.0: No jet force will be applied.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'AIRBAG'


.. py:attribute:: subkeyword
   :value: 'HYBRID_JETTING_CM'






