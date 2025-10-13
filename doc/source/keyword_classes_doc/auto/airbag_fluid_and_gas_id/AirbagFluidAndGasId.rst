





:class:`AirbagFluidAndGasId`
============================


.. py:class:: airbag_fluid_and_gas_id.AirbagFluidAndGasId(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA AIRBAG_FLUID_AND_GAS_ID keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: AirbagFluidAndGasId

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
          * - :py:attr:`~xwini`
            - Get or set the Fluid level at time t = 0 in |GDIR| direction.
          * - :py:attr:`~xwadd`
            - Get or set the Fluid level filling increment per time step.
          * - :py:attr:`~xw`
            - Get or set the Final fluid level in filling process.
          * - :py:attr:`~p`
            - Get or set the Gas pressure at time t = TEND.
          * - :py:attr:`~tend`
            - Get or set the Time when gas pressure P is reached.
          * - :py:attr:`~rho`
            - Get or set the Density of the fluid (e.g. for water, RHO is about 1.0 kg/m3).
          * - :py:attr:`~lcxw`
            - Get or set the Load curve ID for fluid level vs. time. XW, XWADD, and XWINI are with this option.
          * - :py:attr:`~lcp`
            - Get or set the Load curve ID for gas pressure vs. time. P and TEND are ignored with this option.
          * - :py:attr:`~gdir`
            - Get or set the Global direction of gravity (e.g. -3.0 for negative global z-axis).
          * - :py:attr:`~nproj`
            - Get or set the Number of projection directions (only global axis) for volume calculation.
          * - :py:attr:`~idir`
            - Get or set the First direction of projection (if |NPROJ| != 3), only global axis.
          * - :py:attr:`~iidir`
            - Get or set the Second direction of projection (if |NPROJ| = 2), only global axis.
          * - :py:attr:`~kappa`
            - Get or set the Adiabatic exponent.
          * - :py:attr:`~kbm`
            - Get or set the Bulk modulus of the fluid (e.g. for water, BKM is about 2080 N/mm2).


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

    from airbag_fluid_and_gas_id import AirbagFluidAndGasId

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

.. py:property:: xwini
   :type: Optional[float]


   
   Get or set the Fluid level at time t = 0 in |GDIR| direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: xwadd
   :type: Optional[float]


   
   Get or set the Fluid level filling increment per time step.
















   ..
       !! processed by numpydoc !!

.. py:property:: xw
   :type: Optional[float]


   
   Get or set the Final fluid level in filling process.
















   ..
       !! processed by numpydoc !!

.. py:property:: p
   :type: Optional[float]


   
   Get or set the Gas pressure at time t = TEND.
















   ..
       !! processed by numpydoc !!

.. py:property:: tend
   :type: Optional[float]


   
   Get or set the Time when gas pressure P is reached.
















   ..
       !! processed by numpydoc !!

.. py:property:: rho
   :type: Optional[float]


   
   Get or set the Density of the fluid (e.g. for water, RHO is about 1.0 kg/m3).
















   ..
       !! processed by numpydoc !!

.. py:property:: lcxw
   :type: Optional[int]


   
   Get or set the Load curve ID for fluid level vs. time. XW, XWADD, and XWINI are with this option.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcp
   :type: Optional[int]


   
   Get or set the Load curve ID for gas pressure vs. time. P and TEND are ignored with this option.
















   ..
       !! processed by numpydoc !!

.. py:property:: gdir
   :type: Optional[float]


   
   Get or set the Global direction of gravity (e.g. -3.0 for negative global z-axis).
   EQ.1.0: global x-direction,
   EQ.2.0: global y-direction,
   EQ.3.0: global z-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: nproj
   :type: int


   
   Get or set the Number of projection directions (only global axis) for volume calculation.
















   ..
       !! processed by numpydoc !!

.. py:property:: idir
   :type: Optional[int]


   
   Get or set the First direction of projection (if |NPROJ| != 3), only global axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: iidir
   :type: Optional[int]


   
   Get or set the Second direction of projection (if |NPROJ| = 2), only global axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: kappa
   :type: float


   
   Get or set the Adiabatic exponent.
















   ..
       !! processed by numpydoc !!

.. py:property:: kbm
   :type: Optional[float]


   
   Get or set the Bulk modulus of the fluid (e.g. for water, BKM is about 2080 N/mm2).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'AIRBAG'


.. py:attribute:: subkeyword
   :value: 'FLUID_AND_GAS_ID'






