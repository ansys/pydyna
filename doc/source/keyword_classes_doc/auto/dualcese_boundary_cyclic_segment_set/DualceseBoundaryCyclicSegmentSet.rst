





:class:`DualceseBoundaryCyclicSegmentSet`
=========================================


.. py:class:: dualcese_boundary_cyclic_segment_set.DualceseBoundaryCyclicSegmentSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DUALCESE_BOUNDARY_CYCLIC_SEGMENT_SET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DualceseBoundaryCyclicSegmentSet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ssid1`
            - Get or set the Segment set ID created with *DUALCESE_SEGMENTSET
          * - :py:attr:`~ssid2`
            - Get or set the Segment set ID created with *DUALCESE_SEGMENTSET
          * - :py:attr:`~cyctyp`
            - Get or set the Relationship between the two cyclic boundary condition surfaces:


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

    from dualcese_boundary_cyclic_segment_set import DualceseBoundaryCyclicSegmentSet

Property detail
---------------

.. py:property:: ssid1
   :type: Optional[int]


   
   Get or set the Segment set ID created with *DUALCESE_SEGMENTSET
















   ..
       !! processed by numpydoc !!

.. py:property:: ssid2
   :type: Optional[int]


   
   Get or set the Segment set ID created with *DUALCESE_SEGMENTSET
















   ..
       !! processed by numpydoc !!

.. py:property:: cyctyp
   :type: int


   
   Get or set the Relationship between the two cyclic boundary condition surfaces:
   EQ.0:   none assumed(default)
   EQ.1 : The first surface is rotated about an axis to match the second surface.
   EQ.2 : The faces of the first surface are translated in a given direction to obtain the corresponding faces on the second surface
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DUALCESE'


.. py:attribute:: subkeyword
   :value: 'BOUNDARY_CYCLIC_SEGMENT_SET'






