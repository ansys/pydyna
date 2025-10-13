





:class:`Iga3DNurbsXyz`
======================


.. py:class:: iga_3d_nurbs_xyz.Iga3DNurbsXyz(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA IGA_3D_NURBS_XYZ keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: Iga3DNurbsXyz

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nid`
            - Get or set the Physical trivariate NURBS ID. A unique number must be chosen.
          * - :py:attr:`~nr`
            - Get or set the Number of control points in the local r-direction.
          * - :py:attr:`~ns`
            - Get or set the Number of control points in the local s-direction.
          * - :py:attr:`~nt`
            - Get or set the Number of control points in the local t-direction.
          * - :py:attr:`~pr`
            - Get or set the Polynomial degree of the basis in the local r-direction.
          * - :py:attr:`~ps`
            - Get or set the Polynomial degree of the basis in the local s-direction.
          * - :py:attr:`~pt`
            - Get or set the Polynomial degree of the basis in the local t-direction.
          * - :py:attr:`~unir`
            - Get or set the Knot vector type in the local r-direction.
          * - :py:attr:`~unis`
            - Get or set the Knot vector type in the local s-direction.
          * - :py:attr:`~unit`
            - Get or set the Knot vector type in the local t-direction.
          * - :py:attr:`~r1`
            - Get or set the Knot values in the local r-direction with i = 1, NR+PR+1.
          * - :py:attr:`~r2`
            - Get or set the Knot values in the local r-direction with i = 1, NR+PR+1.
          * - :py:attr:`~r3`
            - Get or set the Knot values in the local r-direction with i = 1, NR+PR+1.
          * - :py:attr:`~r4`
            - Get or set the Knot values in the local r-direction with i = 1, NR+PR+1.
          * - :py:attr:`~rfirst`
            - Get or set the First knot value in the local r-direction.
          * - :py:attr:`~rlast`
            - Get or set the Last knot value in the local r-direction.
          * - :py:attr:`~s1`
            - Get or set the Knot values in the local s-direction with j = 1, NS+PS+1..
          * - :py:attr:`~s2`
            - Get or set the Knot values in the local s-direction with i = 1, NS+PS+1.
          * - :py:attr:`~s3`
            - Get or set the Knot values in the local s-direction with i = 1, NS+PS+1.
          * - :py:attr:`~s4`
            - Get or set the Knot values in the local s-direction with i = 1, NS+PS+1.
          * - :py:attr:`~sfirst`
            - Get or set the First knot value in the local s-direction.
          * - :py:attr:`~slast`
            - Get or set the Last knot value in the local s-direction.
          * - :py:attr:`~t1`
            - Get or set the Knot values in the local t-direction with j = 1, NT+PT+1..
          * - :py:attr:`~t2`
            - Get or set the Knot values in the local t-direction with i = 1, NT+PT+1.
          * - :py:attr:`~t3`
            - Get or set the Knot values in the local t-direction with i = 1, NT+PT+1.
          * - :py:attr:`~t4`
            - Get or set the Knot values in the local t-direction with i = 1, NT+PT+1.
          * - :py:attr:`~tfirst`
            - Get or set the First knot value in the local t-direction.
          * - :py:attr:`~tlast`
            - Get or set the Last knot value in the local t-direction.
          * - :py:attr:`~x`
            - Get or set the Non-homogeneous control point coordinates in the global x-direction with k = 1, NR*NS*NT.
          * - :py:attr:`~y`
            - Get or set the Non-homogeneous control point coordinates in the global y-direction with j = 1, NR*NS*NT.
          * - :py:attr:`~z`
            - Get or set the Non-homogeneous control point coordinates in the global z-direction with j = 1, NR*NS*NT.
          * - :py:attr:`~wgt`
            - Get or set the Control weights with j = 1, NR*NS, see Remark 3.


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

    from iga_3d_nurbs_xyz import Iga3DNurbsXyz

Property detail
---------------

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Physical trivariate NURBS ID. A unique number must be chosen.
















   ..
       !! processed by numpydoc !!

.. py:property:: nr
   :type: Optional[int]


   
   Get or set the Number of control points in the local r-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: ns
   :type: Optional[int]


   
   Get or set the Number of control points in the local s-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: nt
   :type: Optional[int]


   
   Get or set the Number of control points in the local t-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: Optional[int]


   
   Get or set the Polynomial degree of the basis in the local r-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: ps
   :type: Optional[int]


   
   Get or set the Polynomial degree of the basis in the local s-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: pt
   :type: Optional[int]


   
   Get or set the Polynomial degree of the basis in the local t-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: unir
   :type: int


   
   Get or set the Knot vector type in the local r-direction.
   EQ.0: Specify the entire knot vector in the local r - direction.
   EQ.1 : Uniform open knot vector in the local r - direction.
   EQ.2 : Uniform periodic knot vector in the local r - direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: unis
   :type: int


   
   Get or set the Knot vector type in the local s-direction.
   EQ.0: Specify the entire knot vector in the local s - direction.
   EQ.1 : Uniform open knot vector in the local s - direction.
   EQ.2 : Uniform periodic knot vector in the local s - direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: unit
   :type: int


   
   Get or set the Knot vector type in the local t-direction.
   EQ.0: Specify the entire knot vector in the local t - direction.
   EQ.1 : Uniform open knot vector in the local t - direction.
   EQ.2 : Uniform periodic knot vector in the local t - direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: r1
   :type: Optional[float]


   
   Get or set the Knot values in the local r-direction with i = 1, NR+PR+1.
















   ..
       !! processed by numpydoc !!

.. py:property:: r2
   :type: Optional[float]


   
   Get or set the Knot values in the local r-direction with i = 1, NR+PR+1.
















   ..
       !! processed by numpydoc !!

.. py:property:: r3
   :type: Optional[float]


   
   Get or set the Knot values in the local r-direction with i = 1, NR+PR+1.
















   ..
       !! processed by numpydoc !!

.. py:property:: r4
   :type: Optional[float]


   
   Get or set the Knot values in the local r-direction with i = 1, NR+PR+1.
















   ..
       !! processed by numpydoc !!

.. py:property:: rfirst
   :type: Optional[float]


   
   Get or set the First knot value in the local r-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: rlast
   :type: Optional[float]


   
   Get or set the Last knot value in the local r-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: s1
   :type: Optional[float]


   
   Get or set the Knot values in the local s-direction with j = 1, NS+PS+1..
















   ..
       !! processed by numpydoc !!

.. py:property:: s2
   :type: Optional[float]


   
   Get or set the Knot values in the local s-direction with i = 1, NS+PS+1.
















   ..
       !! processed by numpydoc !!

.. py:property:: s3
   :type: Optional[float]


   
   Get or set the Knot values in the local s-direction with i = 1, NS+PS+1.
















   ..
       !! processed by numpydoc !!

.. py:property:: s4
   :type: Optional[float]


   
   Get or set the Knot values in the local s-direction with i = 1, NS+PS+1.
















   ..
       !! processed by numpydoc !!

.. py:property:: sfirst
   :type: Optional[float]


   
   Get or set the First knot value in the local s-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: slast
   :type: Optional[float]


   
   Get or set the Last knot value in the local s-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: t1
   :type: Optional[float]


   
   Get or set the Knot values in the local t-direction with j = 1, NT+PT+1..
















   ..
       !! processed by numpydoc !!

.. py:property:: t2
   :type: Optional[float]


   
   Get or set the Knot values in the local t-direction with i = 1, NT+PT+1.
















   ..
       !! processed by numpydoc !!

.. py:property:: t3
   :type: Optional[float]


   
   Get or set the Knot values in the local t-direction with i = 1, NT+PT+1.
















   ..
       !! processed by numpydoc !!

.. py:property:: t4
   :type: Optional[float]


   
   Get or set the Knot values in the local t-direction with i = 1, NT+PT+1.
















   ..
       !! processed by numpydoc !!

.. py:property:: tfirst
   :type: Optional[float]


   
   Get or set the First knot value in the local t-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: tlast
   :type: Optional[float]


   
   Get or set the Last knot value in the local t-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: x
   :type: Optional[float]


   
   Get or set the Non-homogeneous control point coordinates in the global x-direction with k = 1, NR*NS*NT.
















   ..
       !! processed by numpydoc !!

.. py:property:: y
   :type: Optional[float]


   
   Get or set the Non-homogeneous control point coordinates in the global y-direction with j = 1, NR*NS*NT.
















   ..
       !! processed by numpydoc !!

.. py:property:: z
   :type: Optional[float]


   
   Get or set the Non-homogeneous control point coordinates in the global z-direction with j = 1, NR*NS*NT.
















   ..
       !! processed by numpydoc !!

.. py:property:: wgt
   :type: float


   
   Get or set the Control weights with j = 1, NR*NS, see Remark 3.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'IGA'


.. py:attribute:: subkeyword
   :value: '3D_NURBS_XYZ'






