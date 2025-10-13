





:class:`InitialStressShellSet`
==============================


.. py:class:: initial_stress_shell_set.InitialStressShellSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INITIAL_STRESS_SHELL_SET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InitialStressShellSet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the set Shell ID.
          * - :py:attr:`~nplane`
            - Get or set the Number of in plane integration points being output.
          * - :py:attr:`~nthick`
            - Get or set the Number of through thickness integration points.
          * - :py:attr:`~nhisv`
            - Get or set the Number of additional history variables.
          * - :py:attr:`~ntensr`
            - Get or set the Number of components of tensor data taken from the element history variables.
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
          * - :py:attr:`~hisv1`
            - Get or set the Define the nth history variable. The stresses are defined in the GLOBAL cartesian system
          * - :py:attr:`~hisv2`
            - Get or set the Define the nth history variable. The stresses are defined in the GLOBAL cartesian system
          * - :py:attr:`~hisv3`
            - Get or set the Define the nth history variable. The stresses are defined in the GLOBAL cartesian system
          * - :py:attr:`~hisv4`
            - Get or set the Define the nth history variable. The stresses are defined in the GLOBAL cartesian system
          * - :py:attr:`~hisv5`
            - Get or set the Define the nth history variable. The stresses are defined in the GLOBAL cartesian system
          * - :py:attr:`~hisv6`
            - Get or set the Define the nth history variable. The stresses are defined in the GLOBAL cartesian system
          * - :py:attr:`~hisv7`
            - Get or set the Define the nth history variable. The stresses are defined in the GLOBAL cartesian system
          * - :py:attr:`~hisv8`
            - Get or set the Define the nth history variable. The stresses are defined in the GLOBAL cartesian system


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

    from initial_stress_shell_set import InitialStressShellSet

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the set Shell ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: nplane
   :type: int


   
   Get or set the Number of in plane integration points being output.
















   ..
       !! processed by numpydoc !!

.. py:property:: nthick
   :type: int


   
   Get or set the Number of through thickness integration points.
















   ..
       !! processed by numpydoc !!

.. py:property:: nhisv
   :type: int


   
   Get or set the Number of additional history variables.
















   ..
       !! processed by numpydoc !!

.. py:property:: ntensr
   :type: int


   
   Get or set the Number of components of tensor data taken from the element history variables.
















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

.. py:property:: hisv1
   :type: Optional[float]


   
   Get or set the Define the nth history variable. The stresses are defined in the GLOBAL cartesian system
















   ..
       !! processed by numpydoc !!

.. py:property:: hisv2
   :type: Optional[float]


   
   Get or set the Define the nth history variable. The stresses are defined in the GLOBAL cartesian system
















   ..
       !! processed by numpydoc !!

.. py:property:: hisv3
   :type: Optional[float]


   
   Get or set the Define the nth history variable. The stresses are defined in the GLOBAL cartesian system
















   ..
       !! processed by numpydoc !!

.. py:property:: hisv4
   :type: Optional[float]


   
   Get or set the Define the nth history variable. The stresses are defined in the GLOBAL cartesian system
















   ..
       !! processed by numpydoc !!

.. py:property:: hisv5
   :type: Optional[float]


   
   Get or set the Define the nth history variable. The stresses are defined in the GLOBAL cartesian system
















   ..
       !! processed by numpydoc !!

.. py:property:: hisv6
   :type: Optional[float]


   
   Get or set the Define the nth history variable. The stresses are defined in the GLOBAL cartesian system
















   ..
       !! processed by numpydoc !!

.. py:property:: hisv7
   :type: Optional[float]


   
   Get or set the Define the nth history variable. The stresses are defined in the GLOBAL cartesian system
















   ..
       !! processed by numpydoc !!

.. py:property:: hisv8
   :type: Optional[float]


   
   Get or set the Define the nth history variable. The stresses are defined in the GLOBAL cartesian system
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INITIAL'


.. py:attribute:: subkeyword
   :value: 'STRESS_SHELL_SET'






