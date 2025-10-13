





:class:`IgaFaceUvw`
===================


.. py:class:: iga_face_uvw.IgaFaceUvw(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA IGA_FACE_UVW keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IgaFaceUvw

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~fid`
            - Get or set the Parametric face ID. A unique number must be chosen.
          * - :py:attr:`~fxyzid`
            - Get or set the Physical face IDs, see *IGA_FACE_XYZ.
          * - :py:attr:`~nid`
            - Get or set the Parametric bivariate NURBS ID, see *IGA_2D_NURBS_UVW.
          * - :py:attr:`~sense`
            - Get or set the Sense of Orientation with respect to the ppysical face.
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

    from iga_face_uvw import IgaFaceUvw

Property detail
---------------

.. py:property:: fid
   :type: Optional[int]


   
   Get or set the Parametric face ID. A unique number must be chosen.
















   ..
       !! processed by numpydoc !!

.. py:property:: fxyzid
   :type: Optional[int]


   
   Get or set the Physical face IDs, see *IGA_FACE_XYZ.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Parametric bivariate NURBS ID, see *IGA_2D_NURBS_UVW.
















   ..
       !! processed by numpydoc !!

.. py:property:: sense
   :type: int


   
   Get or set the Sense of Orientation with respect to the ppysical face.
   EQ.0: Same(Default)
   EQ.1 : Reversed.
















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
   :value: 'FACE_UVW'






