





:class:`FatigueMeanStressCorrection`
====================================


.. py:class:: fatigue_mean_stress_correction.FatigueMeanStressCorrection(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA FATIGUE_MEAN_STRESS_CORRECTION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: FatigueMeanStressCorrection

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~method`
            - Get or set the Mean stress correction method:
          * - :py:attr:`~mid`
            - Get or set the Material ID for which the current mean stress correction method is applied.
          * - :py:attr:`~sigma`
            - Get or set the Ultimate tensile strength to be used in the Goodman equation


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

    from fatigue_mean_stress_correction import FatigueMeanStressCorrection

Property detail
---------------

.. py:property:: method
   :type: int


   
   Get or set the Mean stress correction method:
   EQ.0: Goodman equation
   EQ.1: Soderberg equation
   EQ.2: Gerber equation
   EQ.3: Goodman tension only
   EQ.4: Gerber tension only
   EQ.11: Morrow equation
   EQ.12: Smith-Watson-Topper equation
















   ..
       !! processed by numpydoc !!

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material ID for which the current mean stress correction method is applied.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigma
   :type: Optional[float]


   
   Get or set the Ultimate tensile strength to be used in the Goodman equation
   (METHOD = 0, 3) or the Gerber equation (METHOD = 2, 4), or
   yield strength to be used in the Soderberg equation (METHOD = 1)
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'FATIGUE'


.. py:attribute:: subkeyword
   :value: 'MEAN_STRESS_CORRECTION'






