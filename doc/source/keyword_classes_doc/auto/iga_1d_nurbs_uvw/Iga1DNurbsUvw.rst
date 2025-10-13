





:class:`Iga1DNurbsUvw`
======================


.. py:class:: iga_1d_nurbs_uvw.Iga1DNurbsUvw(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA IGA_1D_NURBS_UVW keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: Iga1DNurbsUvw

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nid`
            - Get or set the Parametric univariate NURBS ID. A unique number must be chosen.
          * - :py:attr:`~nr`
            - Get or set the Number of control points in the local r-direction.
          * - :py:attr:`~pr`
            - Get or set the Polynomial degree of the basis in the local r-direction.
          * - :py:attr:`~unir`
            - Get or set the Knot vector type in the local r-direction.
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
          * - :py:attr:`~u`
            - Get or set the Non-homogeneous control point coordinates in the parametric u-direction with j = 1, NR.
          * - :py:attr:`~v`
            - Get or set the Non-homogeneous control point coordinates in the parametric v-direction with j = 1, NR.
          * - :py:attr:`~w`
            - Get or set the Non-homogeneous control point coordinates in the parametric w-direction with j = 1, NR.
          * - :py:attr:`~wgt`
            - Get or set the Control weights with j = 1, NR, see Remark 3.


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

    from iga_1d_nurbs_uvw import Iga1DNurbsUvw

Property detail
---------------

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Parametric univariate NURBS ID. A unique number must be chosen.
















   ..
       !! processed by numpydoc !!

.. py:property:: nr
   :type: Optional[int]


   
   Get or set the Number of control points in the local r-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: Optional[int]


   
   Get or set the Polynomial degree of the basis in the local r-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: unir
   :type: int


   
   Get or set the Knot vector type in the local r-direction.
   EQ.0: Specify the entire knot vector in the local r - direction.
   EQ.1 : Uniform open knot vector in the local r - direction.
   EQ.2 : Uniform period vector in the local r - direction.
















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

.. py:property:: u
   :type: Optional[float]


   
   Get or set the Non-homogeneous control point coordinates in the parametric u-direction with j = 1, NR.
















   ..
       !! processed by numpydoc !!

.. py:property:: v
   :type: Optional[float]


   
   Get or set the Non-homogeneous control point coordinates in the parametric v-direction with j = 1, NR.
















   ..
       !! processed by numpydoc !!

.. py:property:: w
   :type: Optional[float]


   
   Get or set the Non-homogeneous control point coordinates in the parametric w-direction with j = 1, NR.
















   ..
       !! processed by numpydoc !!

.. py:property:: wgt
   :type: float


   
   Get or set the Control weights with j = 1, NR, see Remark 3.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'IGA'


.. py:attribute:: subkeyword
   :value: '1D_NURBS_UVW'






