





:class:`ControlSolution`
========================


.. py:class:: control_solution.ControlSolution(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_SOLUTION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlSolution

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~soln`
            - Get or set the Analysis solution procedure:
          * - :py:attr:`~nlq`
            - Get or set the Define the vector length used in solution.  This value must not exceed the vector length of the system which varies based on the machine manufacturer.  The default vector length is printed at termination in the MESSAG file.
          * - :py:attr:`~isnan`
            - Get or set the Flag to check for a NaN in the force and moment arrays after the assembly of these arrays is completed.  This option can be useful for debugging purposes.  A cost overhead of approximately 2% is incurred when this option is active.
          * - :py:attr:`~lcint`
            - Get or set the Number of equally spaced points used in curve (*DEFINE_CURVE) rediscretization. A minimum number of LCINT=100 is always used, i.e., only larger input values are possible. Curve rediscretization applies only to curves used in material models.  Curves defining loads, motion, etc. are not rediscretized.
          * - :py:attr:`~lcacc`
            - Get or set the Flag to truncate curves to 6 significant figures for single precision and 13 significant figures for double precision. The truncation is done after applying the offset and scale factors specified in *DEFINE_CURVE.  Truncation is intended to prevent curve values from deviating from the input value, e.g., 0.7 being stored as 0.69999999.  This small deviation was seen to have an adverse effect in a particular analysis using *MAT_083.  In general, curve truncation is not necessary and is unlikely to have any effect on results.
          * - :py:attr:`~ncdcf`
            - Get or set the Global option to evaluate *DEFINE_CURVE_FUNCTION every NCDCF:th cycle..
          * - :py:attr:`~nocop`
            - Get or set the Avoid copying of material history variables to temporary buffers for constitutive evaluations.


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

    from control_solution import ControlSolution

Property detail
---------------

.. py:property:: soln
   :type: int


   
   Get or set the Analysis solution procedure:
   EQ.0: Structural analysis only,
   EQ.1: Thermal analysis only,
   EQ.2: Coupled structural thermal analysis.
















   ..
       !! processed by numpydoc !!

.. py:property:: nlq
   :type: Optional[int]


   
   Get or set the Define the vector length used in solution.  This value must not exceed the vector length of the system which varies based on the machine manufacturer.  The default vector length is printed at termination in the MESSAG file.
















   ..
       !! processed by numpydoc !!

.. py:property:: isnan
   :type: int


   
   Get or set the Flag to check for a NaN in the force and moment arrays after the assembly of these arrays is completed.  This option can be useful for debugging purposes.  A cost overhead of approximately 2% is incurred when this option is active.
   EQ.0: No checking,
   EQ.1: Checking is active..
















   ..
       !! processed by numpydoc !!

.. py:property:: lcint
   :type: int


   
   Get or set the Number of equally spaced points used in curve (*DEFINE_CURVE) rediscretization. A minimum number of LCINT=100 is always used, i.e., only larger input values are possible. Curve rediscretization applies only to curves used in material models.  Curves defining loads, motion, etc. are not rediscretized.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcacc
   :type: int


   
   Get or set the Flag to truncate curves to 6 significant figures for single precision and 13 significant figures for double precision. The truncation is done after applying the offset and scale factors specified in *DEFINE_CURVE.  Truncation is intended to prevent curve values from deviating from the input value, e.g., 0.7 being stored as 0.69999999.  This small deviation was seen to have an adverse effect in a particular analysis using *MAT_083.  In general, curve truncation is not necessary and is unlikely to have any effect on results.
   EQ.0:   No truncation.
   NE.0:   Truncate.
















   ..
       !! processed by numpydoc !!

.. py:property:: ncdcf
   :type: int


   
   Get or set the Global option to evaluate *DEFINE_CURVE_FUNCTION every NCDCF:th cycle..
















   ..
       !! processed by numpydoc !!

.. py:property:: nocop
   :type: int


   
   Get or set the Avoid copying of material history variables to temporary buffers for constitutive evaluations.
   EQ.0:   Not active
   EQ.1:   Active
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'SOLUTION'






