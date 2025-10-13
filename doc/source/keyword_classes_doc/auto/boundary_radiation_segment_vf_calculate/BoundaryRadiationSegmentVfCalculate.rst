





:class:`BoundaryRadiationSegmentVfCalculate`
============================================


.. py:class:: boundary_radiation_segment_vf_calculate.BoundaryRadiationSegmentVfCalculate(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_RADIATION_SEGMENT_VF_CALCULATE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryRadiationSegmentVfCalculate

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~n1`
            - Get or set the First node ID defining the segment.
          * - :py:attr:`~n2`
            - Get or set the Second node ID defining the segment.
          * - :py:attr:`~n3`
            - Get or set the Third node ID defining the segment.
          * - :py:attr:`~n4`
            - Get or set the Fourth node ID defining the segment.
          * - :py:attr:`~type`
            - Get or set the Radiation type:
          * - :py:attr:`~block`
            - Get or set the Flag indicating if this surface blocks the view between any other 2 surfaces.
          * - :py:attr:`~nint`
            - Get or set the Number of integration points for viewfactor calculation.
          * - :py:attr:`~selcid`
            - Get or set the Load curve ID for surface emissivity, see *DEFINE_CURVE.
          * - :py:attr:`~semult`
            - Get or set the Curve multiplier for surface emissivity, see *DEFINE_CURVE.


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

    from boundary_radiation_segment_vf_calculate import BoundaryRadiationSegmentVfCalculate

Property detail
---------------

.. py:property:: n1
   :type: Optional[int]


   
   Get or set the First node ID defining the segment.
















   ..
       !! processed by numpydoc !!

.. py:property:: n2
   :type: Optional[int]


   
   Get or set the Second node ID defining the segment.
















   ..
       !! processed by numpydoc !!

.. py:property:: n3
   :type: Optional[int]


   
   Get or set the Third node ID defining the segment.
















   ..
       !! processed by numpydoc !!

.. py:property:: n4
   :type: Optional[int]


   
   Get or set the Fourth node ID defining the segment.
















   ..
       !! processed by numpydoc !!

.. py:property:: type
   :type: int


   
   Get or set the Radiation type:
   EQ.2: Radiation within an enclosure.
















   ..
       !! processed by numpydoc !!

.. py:property:: block
   :type: int


   
   Get or set the Flag indicating if this surface blocks the view between any other 2 surfaces.
   EQ.0: no blocking (default)
   EQ.1: blocking.
















   ..
       !! processed by numpydoc !!

.. py:property:: nint
   :type: int


   
   Get or set the Number of integration points for viewfactor calculation.
   EQ.0: LS-DYNA determines the number of integration points based on the segment size and separation distance
   1 <= NINT <= 10: User specified number.
















   ..
       !! processed by numpydoc !!

.. py:property:: selcid
   :type: int


   
   Get or set the Load curve ID for surface emissivity, see *DEFINE_CURVE.
   GT.0: function versus time,
   EQ.0: use constant multiplier value, SEMULT (default),
   LT.0: function versus temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: semult
   :type: float


   
   Get or set the Curve multiplier for surface emissivity, see *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'RADIATION_SEGMENT_VF_CALCULATE'






