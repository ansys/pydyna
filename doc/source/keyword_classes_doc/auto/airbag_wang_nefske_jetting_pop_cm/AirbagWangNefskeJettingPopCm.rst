





:class:`AirbagWangNefskeJettingPopCm`
=====================================


.. py:class:: airbag_wang_nefske_jetting_pop_cm.AirbagWangNefskeJettingPopCm(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA AIRBAG_WANG_NEFSKE_JETTING_POP_CM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: AirbagWangNefskeJettingPopCm

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
          * - :py:attr:`~cv`
            - Get or set the Heat capacity at constant volume.
          * - :py:attr:`~cp`
            - Get or set the Heat capacity at constant pressure.
          * - :py:attr:`~t`
            - Get or set the Temperature of input gas (default =0.0).
          * - :py:attr:`~lct`
            - Get or set the Optional load curve number defining temperature of input gas versus time.  This overides columns T.
          * - :py:attr:`~lcmt`
            - Get or set the Load curve specifying input mass flow rate or tank pressure versus time. If the tank volume, TVOL, is nonzero the curve ID is assumed to be tank pressure versus time. If LCMT=0, then the inflator has to be modeled, see Card 4. During the dynamic relaxation phase the airbag is ignored unless the curve is flagged to act during dynamic relaxation.
          * - :py:attr:`~tvol`
            - Get or set the Tank volume which is required only for the tank pressure versus time curve, LCMT.
          * - :py:attr:`~lcdt`
            - Get or set the Load curve for time rate of change of temperature (dT/dt) versus time.
          * - :py:attr:`~iabt`
            - Get or set the Initial airbag temperature. (Optional, generally not defined).
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
          * - :py:attr:`~lccp23`
            - Get or set the Load curve number defining the orifice coefficient for leakage (fabric porosity) as a function of time. A nonzero value for CP23 overrides LCCP23.
          * - :py:attr:`~ap23`
            - Get or set the Area for leakage (fabric porosity).
          * - :py:attr:`~lcap23`
            - Get or set the Load curve number defining the area for leakage (fabric porosity) as a function of (absolute) pressure. A nonzero value for AP23 overrides LCAP23.
          * - :py:attr:`~pe`
            - Get or set the Ambient pressure.
          * - :py:attr:`~ro`
            - Get or set the Ambient density.
          * - :py:attr:`~gc`
            - Get or set the Gravitational conversion constant (mandatory - no default). If consistent units are being used for all parameters in the airbag definition then unity should be input.
          * - :py:attr:`~lcefr`
            - Get or set the Optional curve for exit flow rate versus (gauge) pressure.
          * - :py:attr:`~pover`
            - Get or set the Initial relative overpressure (gauge), P-over in control volume.
          * - :py:attr:`~ppop`
            - Get or set the Pop pressure: relative pressure (gauge) for initiating exit flow, P-pop.
          * - :py:attr:`~opt`
            - Get or set the Fabric venting option, if nonzero CP23, LCCP23, AP23, and LCAP23 are set to zero.
          * - :py:attr:`~knkdn`
            - Get or set the Optional load curve ID defining the knock down pressure scale factor versus time. This option only applies to jetting. The scale factor defined by this load curve scales the pressure applied to airbag segments which do not have a clear line-of-sight to the jet. Typically, at very early times this scale factor will be less than unity and equal to unity at later times. The full pressure is always applied to segments which can see the jets.
          * - :py:attr:`~ioc`
            - Get or set the Inflator orifice coefficient.
          * - :py:attr:`~ioa`
            - Get or set the Inflator orifice area.
          * - :py:attr:`~ivol`
            - Get or set the Inflator volume.
          * - :py:attr:`~iro`
            - Get or set the Inflator density.
          * - :py:attr:`~it`
            - Get or set the Inflator temperature.
          * - :py:attr:`~lcbf`
            - Get or set the Load curve defining burn fraction versus time.
          * - :py:attr:`~text`
            - Get or set the Ambient temperature.
          * - :py:attr:`~a`
            - Get or set the First heat capacity coefficient of inflator gas. (e.g., Joules/mole/oK)
          * - :py:attr:`~b`
            - Get or set the Second heat capacity coefficient of inflator gas. (e.g., Joules/mole/oK2)
          * - :py:attr:`~mw`
            - Get or set the Molecular weight of inflator gas. (e.g., Kg/mole)
          * - :py:attr:`~gasc`
            - Get or set the Universal gas constant of inflator gas. (e.g., 8.314 Joules/mole/oK)
          * - :py:attr:`~hconv`
            - Get or set the Convection heat transfer coefficient
          * - :py:attr:`~tdp`
            - Get or set the Time delay before initiating exit flow after pop pressure is reached (default=0.0).
          * - :py:attr:`~axp`
            - Get or set the Pop acceleration magnitude in local x-direction.
          * - :py:attr:`~ayp`
            - Get or set the Pop acceleration magnitude in local y-direction.
          * - :py:attr:`~azp`
            - Get or set the Pop acceleration magnitude in local z-direction.
          * - :py:attr:`~amagp`
            - Get or set the Pop acceleration magnitude.
          * - :py:attr:`~tdurp`
            - Get or set the Time duration pop acceleration must be exceeded to initiate exit flow. This is a cumulative time from the beginning of the calculation, i.e., it is not continuous.
          * - :py:attr:`~tda`
            - Get or set the Time delay before initiating exit flow after pop acceleration is exceeded for the prescribed time duration.
          * - :py:attr:`~rbidp`
            - Get or set the Part ID of the rigid body for checking accelerations against pop accelerations.
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
            - Get or set the Cone angle, alpha, defined in radians.
          * - :py:attr:`~beta`
            - Get or set the Efficiency factor, beta, which scales the final value of pressure obtained from Bernoulli's equation (default=1.0).
          * - :py:attr:`~xsjfp`
            - Get or set the x-coordinate of secondary jet focal point, passenger side bag. If the coordinates of the secondary point are (0,0,0) then a conical jet (driver's side airbag) is assumed.
          * - :py:attr:`~ysjfp`
            - Get or set the y-coordinate of secondary jet focal point.
          * - :py:attr:`~zsjfp`
            - Get or set the z-coordinate of secondary jet focal point.
          * - :py:attr:`~psid`
            - Get or set the Optional part set ID, see *SET_PART.
          * - :py:attr:`~angle`
            - Get or set the Not to be defined.
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

    from airbag_wang_nefske_jetting_pop_cm import AirbagWangNefskeJettingPopCm

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

.. py:property:: cv
   :type: Optional[float]


   
   Get or set the Heat capacity at constant volume.
















   ..
       !! processed by numpydoc !!

.. py:property:: cp
   :type: Optional[float]


   
   Get or set the Heat capacity at constant pressure.
















   ..
       !! processed by numpydoc !!

.. py:property:: t
   :type: float


   
   Get or set the Temperature of input gas (default =0.0).
   For temperature variations a load curve, LCT, may be defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: lct
   :type: int


   
   Get or set the Optional load curve number defining temperature of input gas versus time.  This overides columns T.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcmt
   :type: Optional[int]


   
   Get or set the Load curve specifying input mass flow rate or tank pressure versus time. If the tank volume, TVOL, is nonzero the curve ID is assumed to be tank pressure versus time. If LCMT=0, then the inflator has to be modeled, see Card 4. During the dynamic relaxation phase the airbag is ignored unless the curve is flagged to act during dynamic relaxation.
















   ..
       !! processed by numpydoc !!

.. py:property:: tvol
   :type: float


   
   Get or set the Tank volume which is required only for the tank pressure versus time curve, LCMT.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcdt
   :type: int


   
   Get or set the Load curve for time rate of change of temperature (dT/dt) versus time.
















   ..
       !! processed by numpydoc !!

.. py:property:: iabt
   :type: float


   
   Get or set the Initial airbag temperature. (Optional, generally not defined).
















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

.. py:property:: lccp23
   :type: int


   
   Get or set the Load curve number defining the orifice coefficient for leakage (fabric porosity) as a function of time. A nonzero value for CP23 overrides LCCP23.
















   ..
       !! processed by numpydoc !!

.. py:property:: ap23
   :type: float


   
   Get or set the Area for leakage (fabric porosity).
















   ..
       !! processed by numpydoc !!

.. py:property:: lcap23
   :type: int


   
   Get or set the Load curve number defining the area for leakage (fabric porosity) as a function of (absolute) pressure. A nonzero value for AP23 overrides LCAP23.
















   ..
       !! processed by numpydoc !!

.. py:property:: pe
   :type: Optional[float]


   
   Get or set the Ambient pressure.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Ambient density.
















   ..
       !! processed by numpydoc !!

.. py:property:: gc
   :type: Optional[float]


   
   Get or set the Gravitational conversion constant (mandatory - no default). If consistent units are being used for all parameters in the airbag definition then unity should be input.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcefr
   :type: int


   
   Get or set the Optional curve for exit flow rate versus (gauge) pressure.
















   ..
       !! processed by numpydoc !!

.. py:property:: pover
   :type: float


   
   Get or set the Initial relative overpressure (gauge), P-over in control volume.
















   ..
       !! processed by numpydoc !!

.. py:property:: ppop
   :type: float


   
   Get or set the Pop pressure: relative pressure (gauge) for initiating exit flow, P-pop.
















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

.. py:property:: knkdn
   :type: int


   
   Get or set the Optional load curve ID defining the knock down pressure scale factor versus time. This option only applies to jetting. The scale factor defined by this load curve scales the pressure applied to airbag segments which do not have a clear line-of-sight to the jet. Typically, at very early times this scale factor will be less than unity and equal to unity at later times. The full pressure is always applied to segments which can see the jets.
















   ..
       !! processed by numpydoc !!

.. py:property:: ioc
   :type: Optional[float]


   
   Get or set the Inflator orifice coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: ioa
   :type: Optional[float]


   
   Get or set the Inflator orifice area.
















   ..
       !! processed by numpydoc !!

.. py:property:: ivol
   :type: Optional[float]


   
   Get or set the Inflator volume.
















   ..
       !! processed by numpydoc !!

.. py:property:: iro
   :type: Optional[float]


   
   Get or set the Inflator density.
















   ..
       !! processed by numpydoc !!

.. py:property:: it
   :type: Optional[float]


   
   Get or set the Inflator temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcbf
   :type: Optional[int]


   
   Get or set the Load curve defining burn fraction versus time.
















   ..
       !! processed by numpydoc !!

.. py:property:: text
   :type: Optional[float]


   
   Get or set the Ambient temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: a
   :type: Optional[float]


   
   Get or set the First heat capacity coefficient of inflator gas. (e.g., Joules/mole/oK)
















   ..
       !! processed by numpydoc !!

.. py:property:: b
   :type: Optional[float]


   
   Get or set the Second heat capacity coefficient of inflator gas. (e.g., Joules/mole/oK2)
















   ..
       !! processed by numpydoc !!

.. py:property:: mw
   :type: Optional[float]


   
   Get or set the Molecular weight of inflator gas. (e.g., Kg/mole)
















   ..
       !! processed by numpydoc !!

.. py:property:: gasc
   :type: Optional[float]


   
   Get or set the Universal gas constant of inflator gas. (e.g., 8.314 Joules/mole/oK)
















   ..
       !! processed by numpydoc !!

.. py:property:: hconv
   :type: float


   
   Get or set the Convection heat transfer coefficient
















   ..
       !! processed by numpydoc !!

.. py:property:: tdp
   :type: float


   
   Get or set the Time delay before initiating exit flow after pop pressure is reached (default=0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: axp
   :type: float


   
   Get or set the Pop acceleration magnitude in local x-direction.
   EQ.0.0: Inactive (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: ayp
   :type: float


   
   Get or set the Pop acceleration magnitude in local y-direction.
   EQ.0.0: Inactive (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: azp
   :type: float


   
   Get or set the Pop acceleration magnitude in local z-direction.
   EQ.0.0: Inactive (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: amagp
   :type: float


   
   Get or set the Pop acceleration magnitude.
   EQ.0.0: Inactive (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: tdurp
   :type: float


   
   Get or set the Time duration pop acceleration must be exceeded to initiate exit flow. This is a cumulative time from the beginning of the calculation, i.e., it is not continuous.
















   ..
       !! processed by numpydoc !!

.. py:property:: tda
   :type: float


   
   Get or set the Time delay before initiating exit flow after pop acceleration is exceeded for the prescribed time duration.
















   ..
       !! processed by numpydoc !!

.. py:property:: rbidp
   :type: Optional[int]


   
   Get or set the Part ID of the rigid body for checking accelerations against pop accelerations.
















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


   
   Get or set the Cone angle, alpha, defined in radians.
   LT.0.0: |alpha| is the load curve ID defining cone angle as a function of time.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: float


   
   Get or set the Efficiency factor, beta, which scales the final value of pressure obtained from Bernoulli's equation (default=1.0).
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

.. py:property:: angle
   :type: Optional[float]


   
   Get or set the Not to be defined.
















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
   :value: 'WANG_NEFSKE_JETTING_POP_CM'






