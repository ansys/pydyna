





:class:`AirbagAdiabaticGasModelId`
==================================


.. py:class:: airbag_adiabatic_gas_model_id.AirbagAdiabaticGasModelId(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA AIRBAG_ADIABATIC_GAS_MODEL_ID keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: AirbagAdiabaticGasModelId

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
          * - :py:attr:`~psf`
            - Get or set the Pressure scale factor (default=1.0).
          * - :py:attr:`~lcid`
            - Get or set the Optional load curve for preload flag. See *DEFINE_CURVE.
          * - :py:attr:`~gamma`
            - Get or set the Ratio of specific heats.
          * - :py:attr:`~po`
            - Get or set the Initial pressure (gauge).
          * - :py:attr:`~pe`
            - Get or set the Ambient pressure.
          * - :py:attr:`~ro`
            - Get or set the Initial density of gas.


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

    from airbag_adiabatic_gas_model_id import AirbagAdiabaticGasModelId

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

.. py:property:: psf
   :type: float


   
   Get or set the Pressure scale factor (default=1.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Optional load curve for preload flag. See *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: gamma
   :type: Optional[float]


   
   Get or set the Ratio of specific heats.
















   ..
       !! processed by numpydoc !!

.. py:property:: po
   :type: Optional[float]


   
   Get or set the Initial pressure (gauge).
















   ..
       !! processed by numpydoc !!

.. py:property:: pe
   :type: Optional[float]


   
   Get or set the Ambient pressure.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Initial density of gas.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'AIRBAG'


.. py:attribute:: subkeyword
   :value: 'ADIABATIC_GAS_MODEL_ID'






