





:class:`ControlSegmentsInAleCoupling`
=====================================


.. py:class:: control_segments_in_ale_coupling.ControlSegmentsInAleCoupling(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_SEGMENTS_IN_ALE_COUPLING keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlSegmentsInAleCoupling

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~rankey`
            - Get or set the Rank of *CONSTRAINED_LAGRANGE_IN_SOLID in the input deck. (see Remark 2).
          * - :py:attr:`~segset`
            - Get or set the Set ID of *SET_SEGMENT (see Remark 2).
          * - :py:attr:`~ncychk`
            - Get or set the Number of cycles between checks to activate/deactivate coupling segments (see Remark 3).
          * - :py:attr:`~sym`
            - Get or set the Flag to deactivate coupling segments with normal boundary constraints.
          * - :py:attr:`~ninthk`
            - Get or set the Minimum number of coupling points in contact to deactivate the segment (see Remark 4).
          * - :py:attr:`~conthk`
            - Get or set the Contact thickness (see Remark 5).


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

    from control_segments_in_ale_coupling import ControlSegmentsInAleCoupling

Property detail
---------------

.. py:property:: rankey
   :type: int


   
   Get or set the Rank of *CONSTRAINED_LAGRANGE_IN_SOLID in the input deck. (see Remark 2).
















   ..
       !! processed by numpydoc !!

.. py:property:: segset
   :type: int


   
   Get or set the Set ID of *SET_SEGMENT (see Remark 2).
















   ..
       !! processed by numpydoc !!

.. py:property:: ncychk
   :type: int


   
   Get or set the Number of cycles between checks to activate/deactivate coupling segments (see Remark 3).
















   ..
       !! processed by numpydoc !!

.. py:property:: sym
   :type: int


   
   Get or set the Flag to deactivate coupling segments with normal boundary constraints.
   EQ.0:   Off
   EQ.1 : On.
















   ..
       !! processed by numpydoc !!

.. py:property:: ninthk
   :type: int


   
   Get or set the Minimum number of coupling points in contact to deactivate the segment (see Remark 4).
















   ..
       !! processed by numpydoc !!

.. py:property:: conthk
   :type: float


   
   Get or set the Contact thickness (see Remark 5).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'SEGMENTS_IN_ALE_COUPLING'






