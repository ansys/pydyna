





:class:`ElementSolidNurbsPatch`
===============================


.. py:class:: element_solid_nurbs_patch.ElementSolidNurbsPatch(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ELEMENT_SOLID_NURBS_PATCH keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ElementSolidNurbsPatch

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~npeid`
            - Get or set the Nurbs-Patch Element ID.  A unique number has to be chosen.
          * - :py:attr:`~pid`
            - Get or set the Part ID, see *PART.
          * - :py:attr:`~npr`
            - Get or set the Number of control points in local r-direction.
          * - :py:attr:`~pr`
            - Get or set the Order of polynomial of univariate nurbs basis functions in local r-direction.
          * - :py:attr:`~nps`
            - Get or set the Number of control points in local s-direction.
          * - :py:attr:`~ps`
            - Get or set the Order of polynomial of univariate nurbs basis functions in local s-direction.
          * - :py:attr:`~npt`
            - Get or set the Number of control points in local t-direction.
          * - :py:attr:`~pt`
            - Get or set the Order of polynomial of univariate nurbs basis functions in local t-direction.
          * - :py:attr:`~wfl`
            - Get or set the Flag for weighting factors of the control points
          * - :py:attr:`~nisr`
            - Get or set the Number of (automatically created) Interpolation Shell elements in local r-direction per created Nurbs-element for visualization (postprocessing) and contact.
          * - :py:attr:`~niss`
            - Get or set the Number of (automatically created) Interpolation Shell elements in local s-direction per created Nurbs-element for visualization (postprocessing) and contact.
          * - :py:attr:`~nist`
            - Get or set the Number of (automatically created) Interpolation Shell elements in local t-direction per created Nurbs-element for visualization (postprocessing) and contact.
          * - :py:attr:`~imass`
            - Get or set the Option for lumping of mass matrix:
          * - :py:attr:`~idfne`
            - Get or set the Element ID of first NURBS-Element within this NURBS-Patch definition.
          * - :py:attr:`~rk1`
            - Get or set the Values of the univariate knot vector in r-direction defined
          * - :py:attr:`~rk2`
            - Get or set the Values of the univariate knot vector in r-direction defined
          * - :py:attr:`~rk3`
            - Get or set the Values of the univariate knot vector in r-direction defined
          * - :py:attr:`~rk4`
            - Get or set the Values of the univariate knot vector in r-direction defined
          * - :py:attr:`~rk5`
            - Get or set the Values of the univariate knot vector in r-direction defined
          * - :py:attr:`~rk6`
            - Get or set the Values of the univariate knot vector in r-direction defined
          * - :py:attr:`~rk7`
            - Get or set the Values of the univariate knot vector in r-direction defined
          * - :py:attr:`~rk8`
            - Get or set the Values of the univariate knot vector in r-direction defined
          * - :py:attr:`~sk1`
            - Get or set the Values of the univariate knot vector in s-direction defined
          * - :py:attr:`~sk2`
            - Get or set the Values of the univariate knot vector in s-direction defined
          * - :py:attr:`~sk3`
            - Get or set the Values of the univariate knot vector in s-direction defined
          * - :py:attr:`~sk4`
            - Get or set the Values of the univariate knot vector in s-direction defined
          * - :py:attr:`~sk5`
            - Get or set the Values of the univariate knot vector in s-direction defined
          * - :py:attr:`~sk6`
            - Get or set the Values of the univariate knot vector in s-direction defined
          * - :py:attr:`~sk7`
            - Get or set the Values of the univariate knot vector in s-direction defined
          * - :py:attr:`~sk8`
            - Get or set the Values of the univariate knot vector in s-direction defined
          * - :py:attr:`~tk1`
            - Get or set the Values of the univariate knot vector in t-direction defined
          * - :py:attr:`~tk2`
            - Get or set the Values of the univariate knot vector in t-direction defined
          * - :py:attr:`~tk3`
            - Get or set the Values of the univariate knot vector in t-direction defined
          * - :py:attr:`~tk4`
            - Get or set the Values of the univariate knot vector in t-direction defined
          * - :py:attr:`~tk5`
            - Get or set the Values of the univariate knot vector in t-direction defined
          * - :py:attr:`~tk6`
            - Get or set the Values of the univariate knot vector in t-direction defined
          * - :py:attr:`~tk7`
            - Get or set the Values of the univariate knot vector in t-direction defined
          * - :py:attr:`~tk8`
            - Get or set the Values of the univariate knot vector in t-direction defined
          * - :py:attr:`~n1`
            - Get or set the Control point i to define the control grid
          * - :py:attr:`~n2`
            - Get or set the Control point i to define the control grid
          * - :py:attr:`~n3`
            - Get or set the Control point i to define the control grid
          * - :py:attr:`~n4`
            - Get or set the Control point i to define the control grid
          * - :py:attr:`~n5`
            - Get or set the Control point i to define the control grid
          * - :py:attr:`~n6`
            - Get or set the Control point i to define the control grid
          * - :py:attr:`~n7`
            - Get or set the Control point i to define the control grid
          * - :py:attr:`~n8`
            - Get or set the Control point i to define the control grid
          * - :py:attr:`~w1`
            - Get or set the Weighting factor of control point i defined
          * - :py:attr:`~w2`
            - Get or set the Weighting factor of control point i defined
          * - :py:attr:`~w3`
            - Get or set the Weighting factor of control point i defined
          * - :py:attr:`~w4`
            - Get or set the Weighting factor of control point i defined
          * - :py:attr:`~w5`
            - Get or set the Weighting factor of control point i defined
          * - :py:attr:`~w6`
            - Get or set the Weighting factor of control point i defined
          * - :py:attr:`~w7`
            - Get or set the Weighting factor of control point i defined
          * - :py:attr:`~w8`
            - Get or set the Weighting factor of control point i defined


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

    from element_solid_nurbs_patch import ElementSolidNurbsPatch

Property detail
---------------

.. py:property:: npeid
   :type: Optional[int]


   
   Get or set the Nurbs-Patch Element ID.  A unique number has to be chosen.
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID, see *PART.
















   ..
       !! processed by numpydoc !!

.. py:property:: npr
   :type: Optional[int]


   
   Get or set the Number of control points in local r-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: Optional[int]


   
   Get or set the Order of polynomial of univariate nurbs basis functions in local r-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: nps
   :type: Optional[int]


   
   Get or set the Number of control points in local s-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: ps
   :type: Optional[int]


   
   Get or set the Order of polynomial of univariate nurbs basis functions in local s-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: npt
   :type: Optional[int]


   
   Get or set the Number of control points in local t-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: pt
   :type: Optional[int]


   
   Get or set the Order of polynomial of univariate nurbs basis functions in local t-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: wfl
   :type: int


   
   Get or set the Flag for weighting factors of the control points
   EQ.0: all weights at the control points are set to 1.0 (B-spline basis) and no optional cards e are allowed.
   NE.0: the weights at the control points are defined in optional cards E which must be defined after cards D.
















   ..
       !! processed by numpydoc !!

.. py:property:: nisr
   :type: Optional[int]


   
   Get or set the Number of (automatically created) Interpolation Shell elements in local r-direction per created Nurbs-element for visualization (postprocessing) and contact.
















   ..
       !! processed by numpydoc !!

.. py:property:: niss
   :type: Optional[int]


   
   Get or set the Number of (automatically created) Interpolation Shell elements in local s-direction per created Nurbs-element for visualization (postprocessing) and contact.
















   ..
       !! processed by numpydoc !!

.. py:property:: nist
   :type: Optional[int]


   
   Get or set the Number of (automatically created) Interpolation Shell elements in local t-direction per created Nurbs-element for visualization (postprocessing) and contact.
















   ..
       !! processed by numpydoc !!

.. py:property:: imass
   :type: int


   
   Get or set the Option for lumping of mass matrix:
   EQ.0: row sum.
   EQ.1: diagonal weighting.
















   ..
       !! processed by numpydoc !!

.. py:property:: idfne
   :type: int


   
   Get or set the Element ID of first NURBS-Element within this NURBS-Patch definition.
















   ..
       !! processed by numpydoc !!

.. py:property:: rk1
   :type: Optional[float]


   
   Get or set the Values of the univariate knot vector in r-direction defined
















   ..
       !! processed by numpydoc !!

.. py:property:: rk2
   :type: Optional[float]


   
   Get or set the Values of the univariate knot vector in r-direction defined
















   ..
       !! processed by numpydoc !!

.. py:property:: rk3
   :type: Optional[float]


   
   Get or set the Values of the univariate knot vector in r-direction defined
















   ..
       !! processed by numpydoc !!

.. py:property:: rk4
   :type: Optional[float]


   
   Get or set the Values of the univariate knot vector in r-direction defined
















   ..
       !! processed by numpydoc !!

.. py:property:: rk5
   :type: Optional[float]


   
   Get or set the Values of the univariate knot vector in r-direction defined
















   ..
       !! processed by numpydoc !!

.. py:property:: rk6
   :type: Optional[float]


   
   Get or set the Values of the univariate knot vector in r-direction defined
















   ..
       !! processed by numpydoc !!

.. py:property:: rk7
   :type: Optional[float]


   
   Get or set the Values of the univariate knot vector in r-direction defined
















   ..
       !! processed by numpydoc !!

.. py:property:: rk8
   :type: Optional[float]


   
   Get or set the Values of the univariate knot vector in r-direction defined
















   ..
       !! processed by numpydoc !!

.. py:property:: sk1
   :type: Optional[float]


   
   Get or set the Values of the univariate knot vector in s-direction defined
















   ..
       !! processed by numpydoc !!

.. py:property:: sk2
   :type: Optional[float]


   
   Get or set the Values of the univariate knot vector in s-direction defined
















   ..
       !! processed by numpydoc !!

.. py:property:: sk3
   :type: Optional[float]


   
   Get or set the Values of the univariate knot vector in s-direction defined
















   ..
       !! processed by numpydoc !!

.. py:property:: sk4
   :type: Optional[float]


   
   Get or set the Values of the univariate knot vector in s-direction defined
















   ..
       !! processed by numpydoc !!

.. py:property:: sk5
   :type: Optional[float]


   
   Get or set the Values of the univariate knot vector in s-direction defined
















   ..
       !! processed by numpydoc !!

.. py:property:: sk6
   :type: Optional[float]


   
   Get or set the Values of the univariate knot vector in s-direction defined
















   ..
       !! processed by numpydoc !!

.. py:property:: sk7
   :type: Optional[float]


   
   Get or set the Values of the univariate knot vector in s-direction defined
















   ..
       !! processed by numpydoc !!

.. py:property:: sk8
   :type: Optional[float]


   
   Get or set the Values of the univariate knot vector in s-direction defined
















   ..
       !! processed by numpydoc !!

.. py:property:: tk1
   :type: Optional[float]


   
   Get or set the Values of the univariate knot vector in t-direction defined
















   ..
       !! processed by numpydoc !!

.. py:property:: tk2
   :type: Optional[float]


   
   Get or set the Values of the univariate knot vector in t-direction defined
















   ..
       !! processed by numpydoc !!

.. py:property:: tk3
   :type: Optional[float]


   
   Get or set the Values of the univariate knot vector in t-direction defined
















   ..
       !! processed by numpydoc !!

.. py:property:: tk4
   :type: Optional[float]


   
   Get or set the Values of the univariate knot vector in t-direction defined
















   ..
       !! processed by numpydoc !!

.. py:property:: tk5
   :type: Optional[float]


   
   Get or set the Values of the univariate knot vector in t-direction defined
















   ..
       !! processed by numpydoc !!

.. py:property:: tk6
   :type: Optional[float]


   
   Get or set the Values of the univariate knot vector in t-direction defined
















   ..
       !! processed by numpydoc !!

.. py:property:: tk7
   :type: Optional[float]


   
   Get or set the Values of the univariate knot vector in t-direction defined
















   ..
       !! processed by numpydoc !!

.. py:property:: tk8
   :type: Optional[float]


   
   Get or set the Values of the univariate knot vector in t-direction defined
















   ..
       !! processed by numpydoc !!

.. py:property:: n1
   :type: Optional[int]


   
   Get or set the Control point i to define the control grid
















   ..
       !! processed by numpydoc !!

.. py:property:: n2
   :type: Optional[int]


   
   Get or set the Control point i to define the control grid
















   ..
       !! processed by numpydoc !!

.. py:property:: n3
   :type: Optional[int]


   
   Get or set the Control point i to define the control grid
















   ..
       !! processed by numpydoc !!

.. py:property:: n4
   :type: Optional[int]


   
   Get or set the Control point i to define the control grid
















   ..
       !! processed by numpydoc !!

.. py:property:: n5
   :type: Optional[int]


   
   Get or set the Control point i to define the control grid
















   ..
       !! processed by numpydoc !!

.. py:property:: n6
   :type: Optional[int]


   
   Get or set the Control point i to define the control grid
















   ..
       !! processed by numpydoc !!

.. py:property:: n7
   :type: Optional[int]


   
   Get or set the Control point i to define the control grid
















   ..
       !! processed by numpydoc !!

.. py:property:: n8
   :type: Optional[int]


   
   Get or set the Control point i to define the control grid
















   ..
       !! processed by numpydoc !!

.. py:property:: w1
   :type: Optional[float]


   
   Get or set the Weighting factor of control point i defined
















   ..
       !! processed by numpydoc !!

.. py:property:: w2
   :type: Optional[float]


   
   Get or set the Weighting factor of control point i defined
















   ..
       !! processed by numpydoc !!

.. py:property:: w3
   :type: Optional[float]


   
   Get or set the Weighting factor of control point i defined
















   ..
       !! processed by numpydoc !!

.. py:property:: w4
   :type: Optional[float]


   
   Get or set the Weighting factor of control point i defined
















   ..
       !! processed by numpydoc !!

.. py:property:: w5
   :type: Optional[float]


   
   Get or set the Weighting factor of control point i defined
















   ..
       !! processed by numpydoc !!

.. py:property:: w6
   :type: Optional[float]


   
   Get or set the Weighting factor of control point i defined
















   ..
       !! processed by numpydoc !!

.. py:property:: w7
   :type: Optional[float]


   
   Get or set the Weighting factor of control point i defined
















   ..
       !! processed by numpydoc !!

.. py:property:: w8
   :type: Optional[float]


   
   Get or set the Weighting factor of control point i defined
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ELEMENT'


.. py:attribute:: subkeyword
   :value: 'SOLID_NURBS_PATCH'






