





:class:`AirbagHybridId`
=======================


.. py:class:: airbag_hybrid_id.AirbagHybridId(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA AIRBAG_HYBRID_ID keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: AirbagHybridId

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Optional Airbag ID.
          * - :py:attr:`~title`
            - Get or set the Airbag id descriptor. It is suggested that unique descriptions be used.
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
          * - :py:attr:`~hconv`
            - Get or set the Heat Convection (unit: W/K*m2)
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
            - Get or set the Curve representing inflator's mass inflow rate, defined only when inflator gas inflow will be represented by this single mass curve, LCIDM0.  When defined, LCIDM in the following 2xNGAS cards will define the molar fraction of each gas component as a function of time.
          * - :py:attr:`~vntopt`
            - Get or set the Additional options for venting area definition,
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

    from airbag_hybrid_id import AirbagHybridId

Property detail
---------------

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

.. py:property:: hconv
   :type: float


   
   Get or set the Heat Convection (unit: W/K*m2)
   See *AIRBAG_HYBRID developments (Resp. P-O Marklund).
















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
   :type: Optional[int]


   
   Get or set the Optional curve for exit flow rate (mass/time) versus (gauge) pressure
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidm0
   :type: Optional[int]


   
   Get or set the Curve representing inflator's mass inflow rate, defined only when inflator gas inflow will be represented by this single mass curve, LCIDM0.  When defined, LCIDM in the following 2xNGAS cards will define the molar fraction of each gas component as a function of time.
















   ..
       !! processed by numpydoc !!

.. py:property:: vntopt
   :type: int


   
   Get or set the Additional options for venting area definition,
   EQ. 1: venting orifice area = current area of part |A23| - area of part
   |A23| at time=0. This option applies only when A23<0.
   EQ. 2: the areas of failed elements at failure times are added to the
   venting area defined by A23.
   EQ. 10: All of the above options are active.
















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



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'AIRBAG'


.. py:attribute:: subkeyword
   :value: 'HYBRID_ID'






