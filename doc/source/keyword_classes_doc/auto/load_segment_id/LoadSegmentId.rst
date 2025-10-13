





:class:`LoadSegmentId`
======================


.. py:class:: load_segment_id.LoadSegmentId(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_SEGMENT_ID keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadSegmentId

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
            - Get or set the Arrival time for pressure or birth time of pressure.
          * - :py:attr:`~n1`
            - Get or set the Node number of first Node.
          * - :py:attr:`~n2`
            - Get or set the Node number of second Node.
          * - :py:attr:`~n3`
            - Get or set the Node number of third node. Repeat N2 for two dimensional geometries.
          * - :py:attr:`~n4`
            - Get or set the Node number of fourth node. Repeat N2 for two dimensional geometries.
          * - :py:attr:`~n5`
            - Get or set the Node number of fourth node. Repeat N2 for two dimensional geometries.
          * - :py:attr:`~n6`
            - Get or set the Optional mid-side node ID located between nodes 2 and 3
          * - :py:attr:`~n7`
            - Get or set the Optional mid-side node ID located between nodes 3 and 4
          * - :py:attr:`~n8`
            - Get or set the Optional mid-side node ID located between nodes 4 and 1. Do not define for siz node quadratic surface segments


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

    from load_segment_id import LoadSegmentId

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


   
   Get or set the Arrival time for pressure or birth time of pressure.
















   ..
       !! processed by numpydoc !!

.. py:property:: n1
   :type: Optional[int]


   
   Get or set the Node number of first Node.
















   ..
       !! processed by numpydoc !!

.. py:property:: n2
   :type: Optional[int]


   
   Get or set the Node number of second Node.
















   ..
       !! processed by numpydoc !!

.. py:property:: n3
   :type: Optional[int]


   
   Get or set the Node number of third node. Repeat N2 for two dimensional geometries.
















   ..
       !! processed by numpydoc !!

.. py:property:: n4
   :type: Optional[int]


   
   Get or set the Node number of fourth node. Repeat N2 for two dimensional geometries.
















   ..
       !! processed by numpydoc !!

.. py:property:: n5
   :type: Optional[int]


   
   Get or set the Node number of fourth node. Repeat N2 for two dimensional geometries.
















   ..
       !! processed by numpydoc !!

.. py:property:: n6
   :type: Optional[int]


   
   Get or set the Optional mid-side node ID located between nodes 2 and 3
















   ..
       !! processed by numpydoc !!

.. py:property:: n7
   :type: Optional[int]


   
   Get or set the Optional mid-side node ID located between nodes 3 and 4
















   ..
       !! processed by numpydoc !!

.. py:property:: n8
   :type: Optional[int]


   
   Get or set the Optional mid-side node ID located between nodes 4 and 1. Do not define for siz node quadratic surface segments
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'SEGMENT_ID'






