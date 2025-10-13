





:class:`InitialStressTshellSet`
===============================


.. py:class:: initial_stress_tshell_set.InitialStressTshellSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INITIAL_STRESS_TSHELL_SET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InitialStressTshellSet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the thick shell set ID, see *SET_T‌SHELL
          * - :py:attr:`~nplane`
            - Get or set the Number of in plane integration points being output
          * - :py:attr:`~nthick`
            - Get or set the Number of through thickness integration points.
          * - :py:attr:`~nhisv`
            - Get or set the Number of additional history variables
          * - :py:attr:`~large`
            - Get or set the Format size (0:off or 1:on).
          * - :py:attr:`~t`
            - Get or set the Parametric coordinate of through thickness integration point. Between -1 and 1 inclusive.
          * - :py:attr:`~sigxx`
            - Get or set the Define the xx stress component (global cartesian system).
          * - :py:attr:`~sigyy`
            - Get or set the Define the yy stress component (global cartesian system).
          * - :py:attr:`~sigzz`
            - Get or set the Define the zz stress component (global cartesian system).
          * - :py:attr:`~sigxy`
            - Get or set the Define the xy stress component (global cartesian system).
          * - :py:attr:`~sigyz`
            - Get or set the Define the yz stress component (global cartesian system).
          * - :py:attr:`~sigzx`
            - Get or set the Define the zx stress component (global cartesian system).
          * - :py:attr:`~eps`
            - Get or set the Effective plastic strain.


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

    from initial_stress_tshell_set import InitialStressTshellSet

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the thick shell set ID, see *SET_T‌SHELL
















   ..
       !! processed by numpydoc !!

.. py:property:: nplane
   :type: Optional[int]


   
   Get or set the Number of in plane integration points being output
















   ..
       !! processed by numpydoc !!

.. py:property:: nthick
   :type: Optional[int]


   
   Get or set the Number of through thickness integration points.
















   ..
       !! processed by numpydoc !!

.. py:property:: nhisv
   :type: Optional[int]


   
   Get or set the Number of additional history variables
















   ..
       !! processed by numpydoc !!

.. py:property:: large
   :type: int


   
   Get or set the Format size (0:off or 1:on).
















   ..
       !! processed by numpydoc !!

.. py:property:: t
   :type: Optional[float]


   
   Get or set the Parametric coordinate of through thickness integration point. Between -1 and 1 inclusive.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigxx
   :type: float


   
   Get or set the Define the xx stress component (global cartesian system).
















   ..
       !! processed by numpydoc !!

.. py:property:: sigyy
   :type: float


   
   Get or set the Define the yy stress component (global cartesian system).
















   ..
       !! processed by numpydoc !!

.. py:property:: sigzz
   :type: float


   
   Get or set the Define the zz stress component (global cartesian system).
















   ..
       !! processed by numpydoc !!

.. py:property:: sigxy
   :type: float


   
   Get or set the Define the xy stress component (global cartesian system).
















   ..
       !! processed by numpydoc !!

.. py:property:: sigyz
   :type: float


   
   Get or set the Define the yz stress component (global cartesian system).
















   ..
       !! processed by numpydoc !!

.. py:property:: sigzx
   :type: float


   
   Get or set the Define the zx stress component (global cartesian system).
















   ..
       !! processed by numpydoc !!

.. py:property:: eps
   :type: float


   
   Get or set the Effective plastic strain.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INITIAL'


.. py:attribute:: subkeyword
   :value: 'STRESS_TSHELL_SET'






