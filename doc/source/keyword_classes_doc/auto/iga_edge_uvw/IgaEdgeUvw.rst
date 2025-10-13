





:class:`IgaEdgeUvw`
===================


.. py:class:: iga_edge_uvw.IgaEdgeUvw(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA IGA_EDGE_UVW keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IgaEdgeUvw

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~eid`
            - Get or set the Parametric edge ID. A unique number must be chosen.
          * - :py:attr:`~exyzid`
            - Get or set the Physical edge IDs, see *IGA_EDGE_XYZ and Remark 1.
          * - :py:attr:`~nid`
            - Get or set the Parametric univariate NURBS ID, see *IGA_1D_NURBS_UVW, see Remark 2.
          * - :py:attr:`~sense`
            - Get or set the Sense of Orientation with respect to the physical edge.
          * - :py:attr:`~rstart`
            - Get or set the Parametric coordinate defining the start of the trimmed parametric NURBS, see Remark 3.
          * - :py:attr:`~rend`
            - Get or set the Parametric coordinate defining the end of the trimmed parametric NURBS, see Remark 3.


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

    from iga_edge_uvw import IgaEdgeUvw

Property detail
---------------

.. py:property:: eid
   :type: Optional[int]


   
   Get or set the Parametric edge ID. A unique number must be chosen.
















   ..
       !! processed by numpydoc !!

.. py:property:: exyzid
   :type: Optional[int]


   
   Get or set the Physical edge IDs, see *IGA_EDGE_XYZ and Remark 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Parametric univariate NURBS ID, see *IGA_1D_NURBS_UVW, see Remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: sense
   :type: int


   
   Get or set the Sense of Orientation with respect to the physical edge.
   EQ. 0:Same(default)
   EQ.1 : Reversed.
















   ..
       !! processed by numpydoc !!

.. py:property:: rstart
   :type: Optional[float]


   
   Get or set the Parametric coordinate defining the start of the trimmed parametric NURBS, see Remark 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: rend
   :type: Optional[float]


   
   Get or set the Parametric coordinate defining the end of the trimmed parametric NURBS, see Remark 3.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'IGA'


.. py:attribute:: subkeyword
   :value: 'EDGE_UVW'






