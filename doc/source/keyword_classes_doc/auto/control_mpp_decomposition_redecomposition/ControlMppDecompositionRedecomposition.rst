





:class:`ControlMppDecompositionRedecomposition`
===============================================


.. py:class:: control_mpp_decomposition_redecomposition.ControlMppDecompositionRedecomposition(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_MPP_DECOMPOSITION_REDECOMPOSITION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlMppDecompositionRedecomposition

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
          * - :py:attr:`~weight`
            - Get or set the Element cost scale factor for element in contact or with plastic strain.
          * - :py:attr:`~remsph`
            - Get or set the Flag to remove deactived SPH particles:
          * - :py:attr:`~stime`
            - Get or set the Start time for redecomposition
          * - :py:attr:`~sampt_`
            - Get or set the Time interval for collecting element cost profile to use in the next REDECOMP step.GT.0: Sampling from beginning of each redecomposition for length SAMPT(t to t + SAMPT).If SAMPT ≥ FREQ, then the sampling will occur for the entire time interval, FREQ.LT.0 : Sampling from before ending of each redecomposition through to the next redecomposition(t + FREQ - SAMPT to t + FREQ


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

    from control_mpp_decomposition_redecomposition import ControlMppDecompositionRedecomposition

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

.. py:property:: weight
   :type: float


   
   Get or set the Element cost scale factor for element in contact or with plastic strain.
   If the element is under contact and has plastic strain, the weight will be doubled.
   Since the element cost is measured from calculated quantities, the results will remain consistent between runs with the same input and decomposition unlike using SAMPT option
















   ..
       !! processed by numpydoc !!

.. py:property:: remsph
   :type: int


   
   Get or set the Flag to remove deactived SPH particles:
   EQ 0. Keep deactivated particles
   EQ 1. Remove deactivated particles
















   ..
       !! processed by numpydoc !!

.. py:property:: stime
   :type: float


   
   Get or set the Start time for redecomposition
















   ..
       !! processed by numpydoc !!

.. py:property:: sampt_
   :type: Optional[float]


   
   Get or set the Time interval for collecting element cost profile to use in the next REDECOMP step.GT.0: Sampling from beginning of each redecomposition for length SAMPT(t to t + SAMPT).If SAMPT ≥ FREQ, then the sampling will occur for the entire time interval, FREQ.LT.0 : Sampling from before ending of each redecomposition through to the next redecomposition(t + FREQ - SAMPT to t + FREQ
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'MPP_DECOMPOSITION_REDECOMPOSITION'






