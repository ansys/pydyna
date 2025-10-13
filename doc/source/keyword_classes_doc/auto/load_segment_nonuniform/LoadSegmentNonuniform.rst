





:class:`LoadSegmentNonuniform`
==============================


.. py:class:: load_segment_nonuniform.LoadSegmentNonuniform(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_SEGMENT_NONUNIFORM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadSegmentNonuniform

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
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID, see *DEFINE_CURVE.
          * - :py:attr:`~sf`
            - Get or set the Load curve scale factor.
          * - :py:attr:`~at`
            - Get or set the Arrival/birth time for the traction load.
          * - :py:attr:`~dt`
            - Get or set the death time for the traction load.
          * - :py:attr:`~cid`
            - Get or set the Coordinate system id.
          * - :py:attr:`~v1`
            - Get or set the Vector direction cosines  referenced to coordinate system CID to define the direction of the traction loading
          * - :py:attr:`~v2`
            - Get or set the Vector direction cosines  referenced to coordinate system CID to define the direction of the traction loading
          * - :py:attr:`~v3`
            - Get or set the Vector direction cosines  referenced to coordinate system CID to define the direction of the traction loading
          * - :py:attr:`~n1`
            - Get or set the Node ID
          * - :py:attr:`~n2`
            - Get or set the Node ID
          * - :py:attr:`~n3`
            - Get or set the Node ID.  Repeat N2 for two-dimensional geometries
          * - :py:attr:`~n4`
            - Get or set the Node ID.  Repeat N2 for two-dimensional geometries or repeat N3 for triangular segments
          * - :py:attr:`~n5`
            - Get or set the Optional mid-side node ID located between nodes 1 and 2.
          * - :py:attr:`~n6`
            - Get or set the Optional mid-side node ID located between nodes 2 and 3
          * - :py:attr:`~n7`
            - Get or set the Optional mid-side node ID located between nodes 3 and 4.
          * - :py:attr:`~n8`
            - Get or set the Optional mid-side node ID located between nodes 4 and 1.   Do not define for six node quadratic surface segments.
          * - :py:attr:`~p1`
            - Get or set the Scale factor at node ID, N1
          * - :py:attr:`~p2`
            - Get or set the Scale factor at node ID, N2
          * - :py:attr:`~p3`
            - Get or set the Scale factor at node ID, N3
          * - :py:attr:`~p4`
            - Get or set the Scale factor at node ID, N4
          * - :py:attr:`~p5`
            - Get or set the Scale factor at node ID, N5
          * - :py:attr:`~p6`
            - Get or set the Scale factor at node ID, N6
          * - :py:attr:`~p7`
            - Get or set the Scale factor at node ID, N7
          * - :py:attr:`~p8`
            - Get or set the Scale factor at node ID, N8


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

    from load_segment_nonuniform import LoadSegmentNonuniform

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


   
   Get or set the Arrival/birth time for the traction load.
















   ..
       !! processed by numpydoc !!

.. py:property:: dt
   :type: float


   
   Get or set the death time for the traction load.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: Optional[int]


   
   Get or set the Coordinate system id.
















   ..
       !! processed by numpydoc !!

.. py:property:: v1
   :type: Optional[float]


   
   Get or set the Vector direction cosines  referenced to coordinate system CID to define the direction of the traction loading
















   ..
       !! processed by numpydoc !!

.. py:property:: v2
   :type: Optional[float]


   
   Get or set the Vector direction cosines  referenced to coordinate system CID to define the direction of the traction loading
















   ..
       !! processed by numpydoc !!

.. py:property:: v3
   :type: Optional[float]


   
   Get or set the Vector direction cosines  referenced to coordinate system CID to define the direction of the traction loading
















   ..
       !! processed by numpydoc !!

.. py:property:: n1
   :type: Optional[int]


   
   Get or set the Node ID
















   ..
       !! processed by numpydoc !!

.. py:property:: n2
   :type: Optional[int]


   
   Get or set the Node ID
















   ..
       !! processed by numpydoc !!

.. py:property:: n3
   :type: Optional[int]


   
   Get or set the Node ID.  Repeat N2 for two-dimensional geometries
















   ..
       !! processed by numpydoc !!

.. py:property:: n4
   :type: Optional[int]


   
   Get or set the Node ID.  Repeat N2 for two-dimensional geometries or repeat N3 for triangular segments
















   ..
       !! processed by numpydoc !!

.. py:property:: n5
   :type: Optional[int]


   
   Get or set the Optional mid-side node ID located between nodes 1 and 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: n6
   :type: Optional[int]


   
   Get or set the Optional mid-side node ID located between nodes 2 and 3
















   ..
       !! processed by numpydoc !!

.. py:property:: n7
   :type: Optional[int]


   
   Get or set the Optional mid-side node ID located between nodes 3 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: n8
   :type: Optional[int]


   
   Get or set the Optional mid-side node ID located between nodes 4 and 1.   Do not define for six node quadratic surface segments.
















   ..
       !! processed by numpydoc !!

.. py:property:: p1
   :type: Optional[float]


   
   Get or set the Scale factor at node ID, N1
















   ..
       !! processed by numpydoc !!

.. py:property:: p2
   :type: Optional[float]


   
   Get or set the Scale factor at node ID, N2
















   ..
       !! processed by numpydoc !!

.. py:property:: p3
   :type: Optional[float]


   
   Get or set the Scale factor at node ID, N3
















   ..
       !! processed by numpydoc !!

.. py:property:: p4
   :type: Optional[float]


   
   Get or set the Scale factor at node ID, N4
















   ..
       !! processed by numpydoc !!

.. py:property:: p5
   :type: Optional[float]


   
   Get or set the Scale factor at node ID, N5
















   ..
       !! processed by numpydoc !!

.. py:property:: p6
   :type: Optional[float]


   
   Get or set the Scale factor at node ID, N6
















   ..
       !! processed by numpydoc !!

.. py:property:: p7
   :type: Optional[float]


   
   Get or set the Scale factor at node ID, N7
















   ..
       !! processed by numpydoc !!

.. py:property:: p8
   :type: Optional[float]


   
   Get or set the Scale factor at node ID, N8
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'SEGMENT_NONUNIFORM'






