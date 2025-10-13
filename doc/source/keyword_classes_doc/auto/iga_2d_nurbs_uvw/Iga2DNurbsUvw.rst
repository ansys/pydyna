





:class:`Iga2DNurbsUvw`
======================


.. py:class:: iga_2d_nurbs_uvw.Iga2DNurbsUvw(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA IGA_2D_NURBS_UVW keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: Iga2DNurbsUvw

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nid`
            - Get or set the Parametric bivariate NURBS ID. A unique number must be chosen.
          * - :py:attr:`~nr`
            - Get or set the Number of control points in the local r-direction.
          * - :py:attr:`~ns`
            - Get or set the Number of control points in the local s-direction.
          * - :py:attr:`~pr`
            - Get or set the Polynomial degree of the basis in the local r-direction.
          * - :py:attr:`~ps`
            - Get or set the Polynomial degree of the basis in the local s-direction.
          * - :py:attr:`~unir`
            - Get or set the Knot vector type in the local r-direction.
          * - :py:attr:`~unis`
            - Get or set the Knot vector type in the local s-direction.
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
          * - :py:attr:`~u`
            - Get or set the Non-homogeneous control point coordinates in the parametric u-direction with k = 1, NR*NS.
          * - :py:attr:`~v`
            - Get or set the Non-homogeneous control point coordinates in the parametric v-direction with j = 1, NR*NS.
          * - :py:attr:`~w`
            - Get or set the Non-homogeneous control point coordinates in the parametric w-direction with j = 1, NR*NS.
          * - :py:attr:`~wgt`
            - Get or set the Control weights with j = 1, NR*NS.


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

    from iga_2d_nurbs_uvw import Iga2DNurbsUvw

Property detail
---------------

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Parametric bivariate NURBS ID. A unique number must be chosen.
















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

.. py:property:: u
   :type: Optional[float]


   
   Get or set the Non-homogeneous control point coordinates in the parametric u-direction with k = 1, NR*NS.
















   ..
       !! processed by numpydoc !!

.. py:property:: v
   :type: Optional[float]


   
   Get or set the Non-homogeneous control point coordinates in the parametric v-direction with j = 1, NR*NS.
















   ..
       !! processed by numpydoc !!

.. py:property:: w
   :type: Optional[float]


   
   Get or set the Non-homogeneous control point coordinates in the parametric w-direction with j = 1, NR*NS.
















   ..
       !! processed by numpydoc !!

.. py:property:: wgt
   :type: float


   
   Get or set the Control weights with j = 1, NR*NS.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'IGA'


.. py:attribute:: subkeyword
   :value: '2D_NURBS_UVW'






