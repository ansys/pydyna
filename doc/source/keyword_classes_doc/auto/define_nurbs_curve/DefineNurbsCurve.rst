





:class:`DefineNurbsCurve`
=========================


.. py:class:: define_nurbs_curve.DefineNurbsCurve(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_NURBS_CURVE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineNurbsCurve

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Curve ID.
          * - :py:attr:`~n`
            - Get or set the Number of control points.
          * - :py:attr:`~p`
            - Get or set the Polynomial degree.
          * - :py:attr:`~type`
            - Get or set the Coordinate type.
          * - :py:attr:`~wfl`
            - Get or set the Flag for user defined control weights.
          * - :py:attr:`~k1`
            - Get or set the Values of the univariate knot vector.
          * - :py:attr:`~k2`
            - Get or set the Values of the univariate knot vector.
          * - :py:attr:`~k3`
            - Get or set the Values of the univariate knot vector.
          * - :py:attr:`~k4`
            - Get or set the Values of the univariate knot vector.
          * - :py:attr:`~k5`
            - Get or set the Values of the univariate knot vector.
          * - :py:attr:`~k6`
            - Get or set the Values of the univariate knot vector.
          * - :py:attr:`~k7`
            - Get or set the Values of the univariate knot vector.
          * - :py:attr:`~k8`
            - Get or set the Values of the univariate knot vector.
          * - :py:attr:`~x`
            - Get or set the Spatial coordinates in the global X direction.
          * - :py:attr:`~y`
            - Get or set the Spatial coordinates in the global Y direction.
          * - :py:attr:`~z`
            - Get or set the Spatial coordinates in the global Z direction.
          * - :py:attr:`~w`
            - Get or set the Control weights.
          * - :py:attr:`~title`
            - Get or set the Additional title line


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~keyword`
            - 
          * - :py:attr:`~subkeyword`
            - 
          * - :py:attr:`~option_specs`
            - Get the card format type.






Import detail
-------------

.. code-block:: python

    from define_nurbs_curve import DefineNurbsCurve

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Curve ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: n
   :type: Optional[int]


   
   Get or set the Number of control points.
















   ..
       !! processed by numpydoc !!

.. py:property:: p
   :type: Optional[int]


   
   Get or set the Polynomial degree.
















   ..
       !! processed by numpydoc !!

.. py:property:: type
   :type: int


   
   Get or set the Coordinate type.
   EQ.0:Spatial.
   EQ.1:Parametric.
















   ..
       !! processed by numpydoc !!

.. py:property:: wfl
   :type: int


   
   Get or set the Flag for user defined control weights.
   EQ.0: Control weights are assumed to be uniform and positive.
   EQ.1: Control weights are defined on the forth entry of cards B.
















   ..
       !! processed by numpydoc !!

.. py:property:: k1
   :type: Optional[float]


   
   Get or set the Values of the univariate knot vector.
















   ..
       !! processed by numpydoc !!

.. py:property:: k2
   :type: Optional[float]


   
   Get or set the Values of the univariate knot vector.
















   ..
       !! processed by numpydoc !!

.. py:property:: k3
   :type: Optional[float]


   
   Get or set the Values of the univariate knot vector.
















   ..
       !! processed by numpydoc !!

.. py:property:: k4
   :type: Optional[float]


   
   Get or set the Values of the univariate knot vector.
















   ..
       !! processed by numpydoc !!

.. py:property:: k5
   :type: Optional[float]


   
   Get or set the Values of the univariate knot vector.
















   ..
       !! processed by numpydoc !!

.. py:property:: k6
   :type: Optional[float]


   
   Get or set the Values of the univariate knot vector.
















   ..
       !! processed by numpydoc !!

.. py:property:: k7
   :type: Optional[float]


   
   Get or set the Values of the univariate knot vector.
















   ..
       !! processed by numpydoc !!

.. py:property:: k8
   :type: Optional[float]


   
   Get or set the Values of the univariate knot vector.
















   ..
       !! processed by numpydoc !!

.. py:property:: x
   :type: Optional[float]


   
   Get or set the Spatial coordinates in the global X direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: y
   :type: Optional[float]


   
   Get or set the Spatial coordinates in the global Y direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: z
   :type: Optional[float]


   
   Get or set the Spatial coordinates in the global Z direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: w
   :type: Optional[float]


   
   Get or set the Control weights.
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Additional title line
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DEFINE'


.. py:attribute:: subkeyword
   :value: 'NURBS_CURVE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





