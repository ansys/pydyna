





:class:`InitialStressDepth`
===========================


.. py:class:: initial_stress_depth.InitialStressDepth(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INITIAL_STRESS_DEPTH keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InitialStressDepth

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID.
          * - :py:attr:`~ro_g`
            - Get or set the Stress per unit elevation above datum (usually = density x gravity)
          * - :py:attr:`~zdatum`
            - Get or set the Z-coordinate of datum
          * - :py:attr:`~kfact`
            - Get or set the X- and Y-stress = KFACT x Z-stress
          * - :py:attr:`~lc`
            - Get or set the Optional curve of stress vs z-coordinate (ZDATUM is ignored with this option)
          * - :py:attr:`~lch`
            - Get or set the Optional curve of horizontal stress versus z-coordinate (KFACT is ignored with this option)
          * - :py:attr:`~lck0`
            - Get or set the Optional curve of K0 (ratio of horizontal_stress/vertical_stress) versus coordinate. KFACT and LCH are ignored with this option. The axis of the curve is the coordinate, the axis is K0.)


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

    from initial_stress_depth import InitialStressDepth

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro_g
   :type: Optional[float]


   
   Get or set the Stress per unit elevation above datum (usually = density x gravity)
















   ..
       !! processed by numpydoc !!

.. py:property:: zdatum
   :type: Optional[float]


   
   Get or set the Z-coordinate of datum
















   ..
       !! processed by numpydoc !!

.. py:property:: kfact
   :type: float


   
   Get or set the X- and Y-stress = KFACT x Z-stress
















   ..
       !! processed by numpydoc !!

.. py:property:: lc
   :type: Optional[int]


   
   Get or set the Optional curve of stress vs z-coordinate (ZDATUM is ignored with this option)
















   ..
       !! processed by numpydoc !!

.. py:property:: lch
   :type: Optional[int]


   
   Get or set the Optional curve of horizontal stress versus z-coordinate (KFACT is ignored with this option)
















   ..
       !! processed by numpydoc !!

.. py:property:: lck0
   :type: Optional[int]


   
   Get or set the Optional curve of K0 (ratio of horizontal_stress/vertical_stress) versus coordinate. KFACT and LCH are ignored with this option. The axis of the curve is the coordinate, the axis is K0.)
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INITIAL'


.. py:attribute:: subkeyword
   :value: 'STRESS_DEPTH'






