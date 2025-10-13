





:class:`IgaEdgeXyz`
===================


.. py:class:: iga_edge_xyz.IgaEdgeXyz(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA IGA_EDGE_XYZ keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IgaEdgeXyz

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~eid`
            - Get or set the Physical edge ID. A unique number must be chosen.
          * - :py:attr:`~nid`
            - Get or set the Physical univariate NURBS ID, see *IGA_1D_NURBS_XYZ.
          * - :py:attr:`~ori`
            - Get or set the Orientation with respect to the parametric univariate NURBS.
          * - :py:attr:`~pidstart`
            - Get or set the Parametric point ID defining the start of the trimmed physical NURBS. If
          * - :py:attr:`~pidend`
            - Get or set the Parametric point ID defining the end of the trimmed physical NURBS. If
          * - :py:attr:`~psid`
            - Get or set the Parametric point set ID, see *IGA_POINT_UVW and


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

    from iga_edge_xyz import IgaEdgeXyz

Property detail
---------------

.. py:property:: eid
   :type: Optional[int]


   
   Get or set the Physical edge ID. A unique number must be chosen.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Physical univariate NURBS ID, see *IGA_1D_NURBS_XYZ.
















   ..
       !! processed by numpydoc !!

.. py:property:: ori
   :type: int


   
   Get or set the Orientation with respect to the parametric univariate NURBS.
   EQ. 0:Same(default)
   EQ.1 : Reversed.
















   ..
       !! processed by numpydoc !!

.. py:property:: pidstart
   :type: Optional[int]


   
   Get or set the Parametric point ID defining the start of the trimmed physical NURBS. If
   PIDSTART = 0, the physical univariate NURBS is not trimmed at its start,
   see Remark 1 and Remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: pidend
   :type: Optional[int]


   
   Get or set the Parametric point ID defining the end of the trimmed physical NURBS. If
   PIDEND = 0, the physical univariate NURBS is not trimmed at its end, see
   Remark 1 and Remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: psid
   :type: Optional[int]


   
   Get or set the Parametric point set ID, see *IGA_POINT_UVW and
   * SET_IGA_POINT_UVW, see Remark 3.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'IGA'


.. py:attribute:: subkeyword
   :value: 'EDGE_XYZ'






