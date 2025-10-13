





:class:`AirbagLinearFluidId`
============================


.. py:class:: airbag_linear_fluid_id.AirbagLinearFluidId(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA AIRBAG_LINEAR_FLUID_ID keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: AirbagLinearFluidId

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
          * - :py:attr:`~bulk`
            - Get or set the K, bulk modulus of the fluid in the control volume. Constant as a function of time. Define if LCBULK=0.  .
          * - :py:attr:`~ro`
            - Get or set the Density of the fluid.
          * - :py:attr:`~lcint`
            - Get or set the F(t) input flow curve defining mass per unit time as a function of time, see *DEFINE_CURVE.
          * - :py:attr:`~lcoutt`
            - Get or set the G(t), output flow curve defining mass per unit time as a function of time. This load curve is optional.
          * - :py:attr:`~lcoutp`
            - Get or set the H(p), output flow curve defining mass per unit time as a function of pressure. This load curve is optional.
          * - :py:attr:`~lcfit`
            - Get or set the L(t), added pressure as a function of time. This load curve is optional.
          * - :py:attr:`~lcbulk`
            - Get or set the Curve defining the bulk modulus as a function of time. This load curve is optional, but if defined, the constant, BULK, is not used.
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID defining pressure versus time, see *DEFINE_CURVE.
          * - :py:attr:`~p_limit`
            - Get or set the Limiting value on total pressure (optional).
          * - :py:attr:`~p_limlc`
            - Get or set the Curve defining the limiting pressure value as a function of time.
          * - :py:attr:`~nonull`
            - Get or set the A flag to applying pressure on null material.


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

    from airbag_linear_fluid_id import AirbagLinearFluidId

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

.. py:property:: bulk
   :type: Optional[float]


   
   Get or set the K, bulk modulus of the fluid in the control volume. Constant as a function of time. Define if LCBULK=0.  .
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Density of the fluid.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcint
   :type: Optional[int]


   
   Get or set the F(t) input flow curve defining mass per unit time as a function of time, see *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcoutt
   :type: Optional[int]


   
   Get or set the G(t), output flow curve defining mass per unit time as a function of time. This load curve is optional.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcoutp
   :type: Optional[int]


   
   Get or set the H(p), output flow curve defining mass per unit time as a function of pressure. This load curve is optional.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcfit
   :type: Optional[int]


   
   Get or set the L(t), added pressure as a function of time. This load curve is optional.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcbulk
   :type: Optional[int]


   
   Get or set the Curve defining the bulk modulus as a function of time. This load curve is optional, but if defined, the constant, BULK, is not used.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID defining pressure versus time, see *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: p_limit
   :type: Optional[float]


   
   Get or set the Limiting value on total pressure (optional).
















   ..
       !! processed by numpydoc !!

.. py:property:: p_limlc
   :type: Optional[int]


   
   Get or set the Curve defining the limiting pressure value as a function of time.
   If nonzero, P_LIMIT is ignored.
















   ..
       !! processed by numpydoc !!

.. py:property:: nonull
   :type: Optional[int]


   
   Get or set the A flag to applying pressure on null material.
   EQ.0:   apply pressure everywhere inside the airbag.
   NE.0:   do not apply pressure on null material part of the airbag.
   This feature is useful in a hydroforming simulation, where typically the part set that makes up the airbag will include a part ID of null shells, defined by *MAT_NULL.
   The null shells and a deformable sheet blank will form an airbag, which upon pressurization, will push the blank into a die cavity, forming the blank.
   This feature is available in SMP from Dev 136254.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'AIRBAG'


.. py:attribute:: subkeyword
   :value: 'LINEAR_FLUID_ID'






