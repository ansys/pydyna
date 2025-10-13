





:class:`ElementNurbsPatch2DTrimmed`
===================================


.. py:class:: element_nurbs_patch_2d_trimmed.ElementNurbsPatch2DTrimmed(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ELEMENT_NURBS_PATCH_2D_TRIMMED keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ElementNurbsPatch2DTrimmed

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~npid`
            - Get or set the Nurbs-Patch Element ID.  A unique number has to be chosen.
          * - :py:attr:`~pid`
            - Get or set the Part ID, see *PART.
          * - :py:attr:`~npr`
            - Get or set the Number of control points in r-direction
          * - :py:attr:`~pr`
            - Get or set the Polynomial order of univariate shape functions in r-direction
          * - :py:attr:`~nps`
            - Get or set the Number of control points in s-direction
          * - :py:attr:`~ps`
            - Get or set the Polynomial order of univariate shape functions in s-direction
          * - :py:attr:`~wfl`
            - Get or set the Flag for weighting factors. If WFL=0, all weights will be set to 1.0 otherwise the weights of the control points have to be defined in the optional cards D.
          * - :py:attr:`~elform`
            - Get or set the Shell formulation to be used:
          * - :py:attr:`~int_`
            - Get or set the In-plane numerical integration rule:
          * - :py:attr:`~nisr`
            - Get or set the Number of Interpolation Shells in r-direction per created NURBS-Element for visualization (postprocessing) and contact
          * - :py:attr:`~niss`
            - Get or set the Number of Interpolation Shells in s-direction per created NURBS-Element for visualization (postprocessing) and contact
          * - :py:attr:`~imass`
            - Get or set the Mass matric option:
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
          * - :py:attr:`~title`
            - Get or set the Trimming Loop Title.
          * - :py:attr:`~c1`
            - Get or set the Trimming curve ID.
          * - :py:attr:`~c2`
            - Get or set the Trimming curve ID.
          * - :py:attr:`~c3`
            - Get or set the Trimming curve ID.
          * - :py:attr:`~c4`
            - Get or set the Trimming curve ID.
          * - :py:attr:`~c5`
            - Get or set the Trimming curve ID.
          * - :py:attr:`~c6`
            - Get or set the Trimming curve ID.
          * - :py:attr:`~c7`
            - Get or set the Trimming curve ID.
          * - :py:attr:`~c8`
            - Get or set the Trimming curve ID.


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

    from element_nurbs_patch_2d_trimmed import ElementNurbsPatch2DTrimmed

Property detail
---------------

.. py:property:: npid
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


   
   Get or set the Number of control points in r-direction
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: Optional[int]


   
   Get or set the Polynomial order of univariate shape functions in r-direction
















   ..
       !! processed by numpydoc !!

.. py:property:: nps
   :type: Optional[int]


   
   Get or set the Number of control points in s-direction
















   ..
       !! processed by numpydoc !!

.. py:property:: ps
   :type: Optional[int]


   
   Get or set the Polynomial order of univariate shape functions in s-direction
















   ..
       !! processed by numpydoc !!

.. py:property:: wfl
   :type: Optional[int]


   
   Get or set the Flag for weighting factors. If WFL=0, all weights will be set to 1.0 otherwise the weights of the control points have to be defined in the optional cards D.
















   ..
       !! processed by numpydoc !!

.. py:property:: elform
   :type: int


   
   Get or set the Shell formulation to be used:
   EQ.0: Hughes-Liu with rotational DOFs
   EQ.1: Hughes-Liu without rotational DOFs
   EQ.2: Kirchhoff-Theory without rotational DOFs
   EQ.3: Kirchhoff-Theory with rotational DOFs
   EQ.4/-4: combination of FORM=0 and FORM=1
















   ..
       !! processed by numpydoc !!

.. py:property:: int_
   :type: int


   
   Get or set the In-plane numerical integration rule:
   EQ.0: reduced Gauss integration (NIP=PR*PS)
   EQ.1: full Gauss integration (NIP=(PR+1)*(PS+1))
















   ..
       !! processed by numpydoc !!

.. py:property:: nisr
   :type: Optional[int]


   
   Get or set the Number of Interpolation Shells in r-direction per created NURBS-Element for visualization (postprocessing) and contact
















   ..
       !! processed by numpydoc !!

.. py:property:: niss
   :type: Optional[int]


   
   Get or set the Number of Interpolation Shells in s-direction per created NURBS-Element for visualization (postprocessing) and contact
















   ..
       !! processed by numpydoc !!

.. py:property:: imass
   :type: int


   
   Get or set the Mass matric option:
   EQ.0: row sum
   EQ.1: diagonal weighting
















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

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Trimming Loop Title.
















   ..
       !! processed by numpydoc !!

.. py:property:: c1
   :type: Optional[int]


   
   Get or set the Trimming curve ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: c2
   :type: Optional[int]


   
   Get or set the Trimming curve ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: c3
   :type: Optional[int]


   
   Get or set the Trimming curve ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: c4
   :type: Optional[int]


   
   Get or set the Trimming curve ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: c5
   :type: Optional[int]


   
   Get or set the Trimming curve ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: c6
   :type: Optional[int]


   
   Get or set the Trimming curve ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: c7
   :type: Optional[int]


   
   Get or set the Trimming curve ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: c8
   :type: Optional[int]


   
   Get or set the Trimming curve ID.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ELEMENT'


.. py:attribute:: subkeyword
   :value: 'NURBS_PATCH_2D_TRIMMED'






