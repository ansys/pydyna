





:class:`LoadSegmentSetNonuniform`
=================================


.. py:class:: load_segment_set_nonuniform.LoadSegmentSetNonuniform(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_SEGMENT_SET_NONUNIFORM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadSegmentSetNonuniform

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the loading ID
          * - :py:attr:`~heading`
            - Get or set the A description of the loading.
          * - :py:attr:`~ssid`
            - Get or set the Segment set ID, see *SET_SEGMENT.
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID, see *DEFINE_CURVE.
          * - :py:attr:`~sf`
            - Get or set the Load curve scale factor.
          * - :py:attr:`~at`
            - Get or set the Arrival time for pressure or birth time of pressure.
          * - :py:attr:`~dt`
            - Get or set the Death time for pressure.
          * - :py:attr:`~eltype`
            - Get or set the Optional edge loading type. If left blank, pressure on the segment will be applied.
          * - :py:attr:`~cid`
            - Get or set the Coordinate system.
          * - :py:attr:`~v1`
            - Get or set the Vector direction cosines referenced to coordinate system CID to define the direction of the traction loading
          * - :py:attr:`~v2`
            - Get or set the Vector direction cosines referenced to coordinate system CID to define the direction of the traction loading
          * - :py:attr:`~v3`
            - Get or set the Vector direction cosines referenced to coordinate system CID to define the direction of the traction loading


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

    from load_segment_set_nonuniform import LoadSegmentSetNonuniform

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the loading ID
















   ..
       !! processed by numpydoc !!

.. py:property:: heading
   :type: Optional[str]


   
   Get or set the A description of the loading.
















   ..
       !! processed by numpydoc !!

.. py:property:: ssid
   :type: Optional[int]


   
   Get or set the Segment set ID, see *SET_SEGMENT.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID, see *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: sf
   :type: float


   
   Get or set the Load curve scale factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: at
   :type: float


   
   Get or set the Arrival time for pressure or birth time of pressure.
















   ..
       !! processed by numpydoc !!

.. py:property:: dt
   :type: float


   
   Get or set the Death time for pressure.
















   ..
       !! processed by numpydoc !!

.. py:property:: eltype
   :type: Optional[str]


   
   Get or set the Optional edge loading type. If left blank, pressure on the segment will be applied.
   EQ.EF1: Distributed force per unit length along edge 1, Figure 27-5.
   EQ.EF2: Distributed force per unit length along edge 2, Figure 27-5.
   EQ.EF3: Distributed force per unit length along edge 3, Figure 27-5.
   EQ.EF4: Distributed force per unit length along edge 4, Figure 27-5..
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: Optional[int]


   
   Get or set the Coordinate system.
















   ..
       !! processed by numpydoc !!

.. py:property:: v1
   :type: Optional[float]


   
   Get or set the Vector direction cosines referenced to coordinate system CID to define the direction of the traction loading
















   ..
       !! processed by numpydoc !!

.. py:property:: v2
   :type: Optional[float]


   
   Get or set the Vector direction cosines referenced to coordinate system CID to define the direction of the traction loading
















   ..
       !! processed by numpydoc !!

.. py:property:: v3
   :type: Optional[float]


   
   Get or set the Vector direction cosines referenced to coordinate system CID to define the direction of the traction loading
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'SEGMENT_SET_NONUNIFORM'






