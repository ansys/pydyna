





:class:`ControlDynamicRelaxation`
=================================


.. py:class:: control_dynamic_relaxation.ControlDynamicRelaxation(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_DYNAMIC_RELAXATION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlDynamicRelaxation

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nrcyck`
            - Get or set the Number of iterations between convergence checks, for dynamic relaxation option (default=250).
          * - :py:attr:`~drtol`
            - Get or set the Convergence tolerance for dynamic relaxation option (default=0.001).
          * - :py:attr:`~drfctr`
            - Get or set the Dynamic relaxation factor (default=0.995).
          * - :py:attr:`~drterm`
            - Get or set the Optional termination time for dynamic relaxation (default: infinity).
          * - :py:attr:`~tssfdr`
            - Get or set the Scale factor for computed time step during dynamic relaxation:
          * - :py:attr:`~irelal`
            - Get or set the Automatic control for dynamic relaxation option.
          * - :py:attr:`~edttl`
            - Get or set the Convergence tolerance on automatic control of dynamic relaxation.
          * - :py:attr:`~idrflg`
            - Get or set the Dynamic relaxation flag for stress initialization:
          * - :py:attr:`~drpset`
            - Get or set the Part set ID for convergence checking (for IDRFLG=3 only).
          * - :py:attr:`~nc`
            - Get or set the Number of time steps for initializing geometry of IDRFLG = 2.
          * - :py:attr:`~np`
            - Get or set the Number of part sets specified for IDRFLG = 2.
          * - :py:attr:`~psid`
            - Get or set the Part set ID for IDRFLG = 2.
          * - :py:attr:`~vecid`
            - Get or set the Vector ID for defining origin and axis of rotation for IDRFLG = 2.


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

    from control_dynamic_relaxation import ControlDynamicRelaxation

Property detail
---------------

.. py:property:: nrcyck
   :type: int


   
   Get or set the Number of iterations between convergence checks, for dynamic relaxation option (default=250).
















   ..
       !! processed by numpydoc !!

.. py:property:: drtol
   :type: float


   
   Get or set the Convergence tolerance for dynamic relaxation option (default=0.001).
















   ..
       !! processed by numpydoc !!

.. py:property:: drfctr
   :type: float


   
   Get or set the Dynamic relaxation factor (default=0.995).
















   ..
       !! processed by numpydoc !!

.. py:property:: drterm
   :type: Optional[float]


   
   Get or set the Optional termination time for dynamic relaxation (default: infinity).
















   ..
       !! processed by numpydoc !!

.. py:property:: tssfdr
   :type: float


   
   Get or set the Scale factor for computed time step during dynamic relaxation:
   EQ.0.0: Value is set to SCRT defined on *CONTROL_TIMESTEP. After converging, the scale factor is reset to SCRT.
















   ..
       !! processed by numpydoc !!

.. py:property:: irelal
   :type: int


   
   Get or set the Automatic control for dynamic relaxation option.
   EQ.0: not active,
   EQ.1: active.
















   ..
       !! processed by numpydoc !!

.. py:property:: edttl
   :type: float


   
   Get or set the Convergence tolerance on automatic control of dynamic relaxation.
















   ..
       !! processed by numpydoc !!

.. py:property:: idrflg
   :type: int


   
   Get or set the Dynamic relaxation flag for stress initialization:
   EQ.-999: dynamic relaxation not activated even if specified on a load curve, see *DEFINE_CURVE,
   EQ.-3:  dynamic relaxation is activated as with IDRFLG = 1, but the convergence check is made based only on the part set specified by DRPSET.
   All parts are active during the dynamic relaxation phase
   EQ.-1: dynamic relaxation is activated and time history output is produced during dynamic relaxation,
   EQ.0: not active,
   EQ.1: dynamic relaxation is activated,
   EQ.2: initialization to a prescribed geometry.
   EQ.3 dynamic relaxation is activated as with IDRFLG=1, but with a part set ID for convergence checking
   EQ.5: initialize implicitly
   EQ.6 initialize implicity but only for the part set specified by DRPSET.
















   ..
       !! processed by numpydoc !!

.. py:property:: drpset
   :type: int


   
   Get or set the Part set ID for convergence checking (for IDRFLG=3 only).
















   ..
       !! processed by numpydoc !!

.. py:property:: nc
   :type: int


   
   Get or set the Number of time steps for initializing geometry of IDRFLG = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: np
   :type: int


   
   Get or set the Number of part sets specified for IDRFLG = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: psid
   :type: int


   
   Get or set the Part set ID for IDRFLG = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: vecid
   :type: int


   
   Get or set the Vector ID for defining origin and axis of rotation for IDRFLG = 2.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'DYNAMIC_RELAXATION'






