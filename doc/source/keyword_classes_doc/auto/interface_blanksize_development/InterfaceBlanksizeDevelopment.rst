





:class:`InterfaceBlanksizeDevelopment`
======================================


.. py:class:: interface_blanksize_development.InterfaceBlanksizeDevelopment(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INTERFACE_BLANKSIZE_DEVELOPMENT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InterfaceBlanksizeDevelopment

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ioption`
            - Get or set the Target definition input type:
          * - :py:attr:`~iadapt`
            - Get or set the Adaptive mesh control flag. If IADAPT=1, number of elements between
          * - :py:attr:`~maxsize`
            - Get or set the The expected maximum change in initial blank size. It is used where the initial blank is not flat, and the curvature is large in the boundary region.
          * - :py:attr:`~referenc`
            - Get or set the Flag to indicate trim curve projection to a reference surface (mesh):
          * - :py:attr:`~space`
            - Get or set the Point spacing distance on the reference surface for the projected curve,Smaller value should be used for large reference surface curvature.
          * - :py:attr:`~maxgap`
            - Get or set the Point spacing distance on the reference surface for the projected curve,Smaller value should be used for large reference surface curvature.
          * - :py:attr:`~orient`
            - Get or set the Point spacing distance on the reference surface for the projected curve,Smaller value should be used for large reference surface curvature.
          * - :py:attr:`~filename1`
            - Get or set the The following file names, FILENAME1~3 are for the option DEVELOPMENT:
          * - :py:attr:`~filename2`
            - Get or set the Simulated (formed or flanged) sheet blank mesh in keyword format. This
          * - :py:attr:`~filename3`
            - Get or set the Initial sheet blank mesh in keyword format. This can be the first state mesh
          * - :py:attr:`~filename4`
            - Get or set the Reference surface (mesh) to extend the initial blank shape for trim curve projection (Figure 25-5) in keyword format. This file name needs to be defined when REFERENC is set to '1'.
          * - :py:attr:`~filename13`
            - Get or set the Reference surface onto which adjustments to the blanks trim curves in its final state are projected (ref4.k in Figure 0-7).  This surface is typically a curved extension of the formed blank and must be defined as mesh in keyword format.  This file name must be defined when ORIENT is set to 2.


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

    from interface_blanksize_development import InterfaceBlanksizeDevelopment

Property detail
---------------

.. py:property:: ioption
   :type: int


   
   Get or set the Target definition input type:
   EQ.1: (entire) blank mesh in keyword format.
   EQ.2: consecutive position coordinates of blank boundary loop
   curve in XYZ format. Blank geometry is located to the left side of the
   looped curve, as shown in Remarks below.
   EQ.2: consecutive position coordinates of blank boundary loop in
   XYZ format. Blank geometry is located to the right side of the looped   curve, as shown in Remarks below.
















   ..
       !! processed by numpydoc !!

.. py:property:: iadapt
   :type: Optional[int]


   
   Get or set the Adaptive mesh control flag. If IADAPT=1, number of elements between
   initial (FILENAME2) and simulated blank (FILENAME3) meshes can be
   different, avoiding using sheet blank from the file �adapt.msh (set IOFLAG=1 in *CONTROL_ADAPTIVE) for the initial blank mesh.
















   ..
       !! processed by numpydoc !!

.. py:property:: maxsize
   :type: float


   
   Get or set the The expected maximum change in initial blank size. It is used where the initial blank is not flat, and the curvature is large in the boundary region.
















   ..
       !! processed by numpydoc !!

.. py:property:: referenc
   :type: int


   
   Get or set the Flag to indicate trim curve projection to a reference surface (mesh):
   EQ.0: no projection.
   EQ.1: the trim curves will be projected to the reference surface.In addition, the mesh file for the reference surface is given in FILENAME4.
















   ..
       !! processed by numpydoc !!

.. py:property:: space
   :type: float


   
   Get or set the Point spacing distance on the reference surface for the projected curve,Smaller value should be used for large reference surface curvature.
















   ..
       !! processed by numpydoc !!

.. py:property:: maxgap
   :type: float


   
   Get or set the Point spacing distance on the reference surface for the projected curve,Smaller value should be used for large reference surface curvature.
















   ..
       !! processed by numpydoc !!

.. py:property:: orient
   :type: Optional[float]


   
   Get or set the Point spacing distance on the reference surface for the projected curve,Smaller value should be used for large reference surface curvature.
















   ..
       !! processed by numpydoc !!

.. py:property:: filename1
   :type: Optional[str]


   
   Get or set the The following file names, FILENAME1~3 are for the option DEVELOPMENT:
   Target input file name. If a blank mesh is used, the keyword file must contain
   *NODE and *ELEMENT_SHELL; if blank boundary is used, the file
   must consist of *DEFINE_TARGET_BOUNDARY. Once defined, the target
   never needs to be changed in an iterative optimization loop.
















   ..
       !! processed by numpydoc !!

.. py:property:: filename2
   :type: Optional[str]


   
   Get or set the Simulated (formed or flanged) sheet blank mesh in keyword format. This
   can be the final state mesh from the current simulation.
















   ..
       !! processed by numpydoc !!

.. py:property:: filename3
   :type: Optional[str]


   
   Get or set the Initial sheet blank mesh in keyword format. This can be the first state mesh
   from the current simulation. If IADAPT=1, then this mesh can just be a
   regular blank mesh (without adaptivity).
















   ..
       !! processed by numpydoc !!

.. py:property:: filename4
   :type: Optional[str]


   
   Get or set the Reference surface (mesh) to extend the initial blank shape for trim curve projection (Figure 25-5) in keyword format. This file name needs to be defined when REFERENC is set to '1'.
















   ..
       !! processed by numpydoc !!

.. py:property:: filename13
   :type: Optional[str]


   
   Get or set the Reference surface onto which adjustments to the blanks trim curves in its final state are projected (ref4.k in Figure 0-7).  This surface is typically a curved extension of the formed blank and must be defined as mesh in keyword format.  This file name must be defined when ORIENT is set to 2.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INTERFACE'


.. py:attribute:: subkeyword
   :value: 'BLANKSIZE_DEVELOPMENT'






