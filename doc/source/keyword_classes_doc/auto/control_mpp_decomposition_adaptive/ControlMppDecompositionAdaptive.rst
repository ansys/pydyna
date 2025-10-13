





:class:`ControlMppDecompositionAdaptive`
========================================


.. py:class:: control_mpp_decomposition_adaptive.ControlMppDecompositionAdaptive(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_MPP_DECOMPOSITION_ADAPTIVE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlMppDecompositionAdaptive

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~freq`
            - Get or set the Determines the number of redecompositions during the solution.
          * - :py:attr:`~defgeo`
            - Get or set the Geometry for decomposition:
          * - :py:attr:`~cweight`
            - Get or set the Element cost scale factor for an element in contact or an element that has undergone plastic strain.  If the element is under contact and has plastic strain, the weight will be doubled.  Since the element cost is measured from calculated quantities, the results will remain consistent between runs with the same input and decomposition
          * - :py:attr:`~stime`
            - Get or set the Start time for redecomposition


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

    from control_mpp_decomposition_adaptive import ControlMppDecompositionAdaptive

Property detail
---------------

.. py:property:: freq
   :type: Optional[float]


   
   Get or set the Determines the number of redecompositions during the solution.
   LT.0:   |FREQ| rounded to the nearest integer is the number of redecompositions during the solution.
   GT.0:   FREQ is the time interval between redecompositions.
















   ..
       !! processed by numpydoc !!

.. py:property:: defgeo
   :type: int


   
   Get or set the Geometry for decomposition:
   EQ.1:   Use current geometry for decomposition.When applied to a model containing SPH, deactivated SPH elements are not considered in the partition.This will give better load balancing if SPH elements are deleted during the simulation.
   EQ.2 : Use current geometry for decomposition(same as 1 if applied to a non - SPH model).When applied to a model containing SPH, all SPH elements are considered in the partition.This will give better load balancing if SPH elements are reactivated during the simulation.
















   ..
       !! processed by numpydoc !!

.. py:property:: cweight
   :type: float


   
   Get or set the Element cost scale factor for an element in contact or an element that has undergone plastic strain.  If the element is under contact and has plastic strain, the weight will be doubled.  Since the element cost is measured from calculated quantities, the results will remain consistent between runs with the same input and decomposition
















   ..
       !! processed by numpydoc !!

.. py:property:: stime
   :type: float


   
   Get or set the Start time for redecomposition
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'MPP_DECOMPOSITION_ADAPTIVE'






