





:class:`IgaFaceXyz`
===================


.. py:class:: iga_face_xyz.IgaFaceXyz(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA IGA_FACE_XYZ keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IgaFaceXyz

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~fid`
            - Get or set the Physical face ID. A unique number must be chosen.
          * - :py:attr:`~nid`
            - Get or set the Physical bivariate NURBS ID, see *IGA_2D_NURBS_XYZ.
          * - :py:attr:`~ori`
            - Get or set the Orientation with respect to the physical bivariate NURBS.
          * - :py:attr:`~psid`
            - Get or set the Parametric point set ID, see *IGA_POINT_UVW, *SET_IGA_POINT_UVW.
          * - :py:attr:`~esid`
            - Get or set the Parametric edge set ID, see *IGA_EDGE_UVW, *SET_IGA_EDGE_UVW.
          * - :py:attr:`~brid1`
            - Get or set the One-dimensional boundary representation IDs, see *IGA_1D_BREP, with i= 1, nand n > 0.
          * - :py:attr:`~brid2`
            - Get or set the One-dimensional boundary representation IDs, see *IGA_1D_BREP, with i= 1, nand n > 0.
          * - :py:attr:`~brid3`
            - Get or set the One-dimensional boundary representation IDs, see *IGA_1D_BREP, with i= 1, nand n > 0.
          * - :py:attr:`~brid4`
            - Get or set the One-dimensional boundary representation IDs, see *IGA_1D_BREP, with i= 1, nand n > 0.
          * - :py:attr:`~brid5`
            - Get or set the One-dimensional boundary representation IDs, see *IGA_1D_BREP, with i= 1, nand n > 0.
          * - :py:attr:`~brid6`
            - Get or set the One-dimensional boundary representation IDs, see *IGA_1D_BREP, with i= 1, nand n > 0.
          * - :py:attr:`~brid7`
            - Get or set the One-dimensional boundary representation IDs, see *IGA_1D_BREP, with i= 1, nand n > 0.
          * - :py:attr:`~brid8`
            - Get or set the One-dimensional boundary representation IDs, see *IGA_1D_BREP, with i= 1, nand n > 0.


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

    from iga_face_xyz import IgaFaceXyz

Property detail
---------------

.. py:property:: fid
   :type: Optional[int]


   
   Get or set the Physical face ID. A unique number must be chosen.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Physical bivariate NURBS ID, see *IGA_2D_NURBS_XYZ.
















   ..
       !! processed by numpydoc !!

.. py:property:: ori
   :type: int


   
   Get or set the Orientation with respect to the physical bivariate NURBS.
   EQ.0: Same
   EQ.1 : Reversed.
















   ..
       !! processed by numpydoc !!

.. py:property:: psid
   :type: Optional[int]


   
   Get or set the Parametric point set ID, see *IGA_POINT_UVW, *SET_IGA_POINT_UVW.
















   ..
       !! processed by numpydoc !!

.. py:property:: esid
   :type: Optional[int]


   
   Get or set the Parametric edge set ID, see *IGA_EDGE_UVW, *SET_IGA_EDGE_UVW.
















   ..
       !! processed by numpydoc !!

.. py:property:: brid1
   :type: Optional[int]


   
   Get or set the One-dimensional boundary representation IDs, see *IGA_1D_BREP, with i= 1, nand n > 0.
















   ..
       !! processed by numpydoc !!

.. py:property:: brid2
   :type: Optional[int]


   
   Get or set the One-dimensional boundary representation IDs, see *IGA_1D_BREP, with i= 1, nand n > 0.
















   ..
       !! processed by numpydoc !!

.. py:property:: brid3
   :type: Optional[int]


   
   Get or set the One-dimensional boundary representation IDs, see *IGA_1D_BREP, with i= 1, nand n > 0.
















   ..
       !! processed by numpydoc !!

.. py:property:: brid4
   :type: Optional[int]


   
   Get or set the One-dimensional boundary representation IDs, see *IGA_1D_BREP, with i= 1, nand n > 0.
















   ..
       !! processed by numpydoc !!

.. py:property:: brid5
   :type: Optional[int]


   
   Get or set the One-dimensional boundary representation IDs, see *IGA_1D_BREP, with i= 1, nand n > 0.
















   ..
       !! processed by numpydoc !!

.. py:property:: brid6
   :type: Optional[int]


   
   Get or set the One-dimensional boundary representation IDs, see *IGA_1D_BREP, with i= 1, nand n > 0.
















   ..
       !! processed by numpydoc !!

.. py:property:: brid7
   :type: Optional[int]


   
   Get or set the One-dimensional boundary representation IDs, see *IGA_1D_BREP, with i= 1, nand n > 0.
















   ..
       !! processed by numpydoc !!

.. py:property:: brid8
   :type: Optional[int]


   
   Get or set the One-dimensional boundary representation IDs, see *IGA_1D_BREP, with i= 1, nand n > 0.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'IGA'


.. py:attribute:: subkeyword
   :value: 'FACE_XYZ'






