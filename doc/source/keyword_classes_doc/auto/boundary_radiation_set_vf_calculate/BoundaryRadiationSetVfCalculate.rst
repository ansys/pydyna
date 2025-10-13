





:class:`BoundaryRadiationSetVfCalculate`
========================================


.. py:class:: boundary_radiation_set_vf_calculate.BoundaryRadiationSetVfCalculate(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_RADIATION_SET_VF_CALCULATE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryRadiationSetVfCalculate

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ssid`
            - Get or set the Segment set ID, see also *SET_SEGMENT.
          * - :py:attr:`~type`
            - Get or set the Radiation type:
          * - :py:attr:`~rad_grp`
            - Get or set the Radiation enclosure group ID. The segment sets from all radiation enclosure definitions with the same group ID are augmented to form a single enclosure definition. If RAD_GRP is not specified or set to zero, then the segments are placed in group zero. All segments defined by the _SEGMENT option are placed in set zero.
          * - :py:attr:`~file_no`
            - Get or set the File number for view factor file. FILE_NO is added to viewfl_ to form the name of the file containing the view factors. For example, if FILE_NO is specified as 22, then the view factors are read from viewfl_22. For radiation enclosure group zero FILE_NO is ignored and view factors are read from viewfl. The same file may be used for different radiation enclosure group definitions.
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

    from boundary_radiation_set_vf_calculate import BoundaryRadiationSetVfCalculate

Property detail
---------------

.. py:property:: ssid
   :type: Optional[int]


   
   Get or set the Segment set ID, see also *SET_SEGMENT.
















   ..
       !! processed by numpydoc !!

.. py:property:: type
   :type: int


   
   Get or set the Radiation type:
   EQ.2: radiation in enclosure.
















   ..
       !! processed by numpydoc !!

.. py:property:: rad_grp
   :type: int


   
   Get or set the Radiation enclosure group ID. The segment sets from all radiation enclosure definitions with the same group ID are augmented to form a single enclosure definition. If RAD_GRP is not specified or set to zero, then the segments are placed in group zero. All segments defined by the _SEGMENT option are placed in set zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: file_no
   :type: int


   
   Get or set the File number for view factor file. FILE_NO is added to viewfl_ to form the name of the file containing the view factors. For example, if FILE_NO is specified as 22, then the view factors are read from viewfl_22. For radiation enclosure group zero FILE_NO is ignored and view factors are read from viewfl. The same file may be used for different radiation enclosure group definitions.
















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
   :value: 'RADIATION_SET_VF_CALCULATE'






