





:class:`CeseBoundaryCyclicPart`
===============================


.. py:class:: cese_boundary_cyclic_part.CeseBoundaryCyclicPart(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CESE_BOUNDARY_CYCLIC_PART keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: CeseBoundaryCyclicPart

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~srfprt1`
            - Get or set the Surface part numbers referenced in *MESH_SURFACE_ELEMENT cards.
          * - :py:attr:`~srfprt2`
            - Get or set the Surface part numbers referenced in *MESH_SURFACE_ELEMENT cards.
          * - :py:attr:`~cyctyp`
            - Get or set the Relationship between the two cyclic boundary condition surfaces:EQ.0: none assumed (default)
          * - :py:attr:`~axisx1`
            - Get or set the A point on the axis of rotation for CYCTYP.EQ.1.
          * - :py:attr:`~axisy1`
            - Get or set the A point on the axis of rotation for CYCTYP.EQ.1.
          * - :py:attr:`~axisz1`
            - Get or set the A point on the axis of rotation for CYCTYP.EQ.1.
          * - :py:attr:`~dirx`
            - Get or set the The direction that with AXISX1,   defines the axis of rotation for CYCTYP.EQ.1.
          * - :py:attr:`~diry`
            - Get or set the The direction that with AXISX1,   defines the axis of rotation for CYCTYP.EQ.1.
          * - :py:attr:`~dirz`
            - Get or set the The direction that with AXISX1,   defines the axis of rotation for CYCTYP.EQ.1.
          * - :py:attr:`~rotang`
            - Get or set the The angle of rotation (in degrees) that transforms the centroid of each face on the first surface to the centroid of the corresponding face on the second surface (for CYCTYP.EQ.1).
          * - :py:attr:`~transx`
            - Get or set the The translation direction that enables the identification of the segment in the second surface that matches a segment in the first surface (for CYCTYP.EQ.2).
          * - :py:attr:`~transy`
            - Get or set the The translation direction that enables the identification of the segment in the second surface that matches a segment in the first surface (for CYCTYP.EQ.2).
          * - :py:attr:`~transz`
            - Get or set the The translation direction that enables the identification of the segment in the second surface that matches a segment in the first surface (for CYCTYP.EQ.2).


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

    from cese_boundary_cyclic_part import CeseBoundaryCyclicPart

Property detail
---------------

.. py:property:: srfprt1
   :type: Optional[int]


   
   Get or set the Surface part numbers referenced in *MESH_SURFACE_ELEMENT cards.
















   ..
       !! processed by numpydoc !!

.. py:property:: srfprt2
   :type: Optional[int]


   
   Get or set the Surface part numbers referenced in *MESH_SURFACE_ELEMENT cards.
















   ..
       !! processed by numpydoc !!

.. py:property:: cyctyp
   :type: int


   
   Get or set the Relationship between the two cyclic boundary condition surfaces:EQ.0: none assumed (default)
   EQ.1: The first surface is rotated about an axis to match the second surface.
   EQ.2: The faces of the first surface are translated in a given direction to obtain the corresponding faces on the second surface.
















   ..
       !! processed by numpydoc !!

.. py:property:: axisx1
   :type: float


   
   Get or set the A point on the axis of rotation for CYCTYP.EQ.1.
















   ..
       !! processed by numpydoc !!

.. py:property:: axisy1
   :type: float


   
   Get or set the A point on the axis of rotation for CYCTYP.EQ.1.
















   ..
       !! processed by numpydoc !!

.. py:property:: axisz1
   :type: float


   
   Get or set the A point on the axis of rotation for CYCTYP.EQ.1.
















   ..
       !! processed by numpydoc !!

.. py:property:: dirx
   :type: Optional[float]


   
   Get or set the The direction that with AXISX1,   defines the axis of rotation for CYCTYP.EQ.1.
















   ..
       !! processed by numpydoc !!

.. py:property:: diry
   :type: Optional[float]


   
   Get or set the The direction that with AXISX1,   defines the axis of rotation for CYCTYP.EQ.1.
















   ..
       !! processed by numpydoc !!

.. py:property:: dirz
   :type: Optional[float]


   
   Get or set the The direction that with AXISX1,   defines the axis of rotation for CYCTYP.EQ.1.
















   ..
       !! processed by numpydoc !!

.. py:property:: rotang
   :type: Optional[float]


   
   Get or set the The angle of rotation (in degrees) that transforms the centroid of each face on the first surface to the centroid of the corresponding face on the second surface (for CYCTYP.EQ.1).
















   ..
       !! processed by numpydoc !!

.. py:property:: transx
   :type: Optional[float]


   
   Get or set the The translation direction that enables the identification of the segment in the second surface that matches a segment in the first surface (for CYCTYP.EQ.2).
















   ..
       !! processed by numpydoc !!

.. py:property:: transy
   :type: Optional[float]


   
   Get or set the The translation direction that enables the identification of the segment in the second surface that matches a segment in the first surface (for CYCTYP.EQ.2).
















   ..
       !! processed by numpydoc !!

.. py:property:: transz
   :type: Optional[float]


   
   Get or set the The translation direction that enables the identification of the segment in the second surface that matches a segment in the first surface (for CYCTYP.EQ.2).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CESE'


.. py:attribute:: subkeyword
   :value: 'BOUNDARY_CYCLIC_PART'






