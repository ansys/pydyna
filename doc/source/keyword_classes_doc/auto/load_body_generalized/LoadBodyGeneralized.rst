





:class:`LoadBodyGeneralized`
============================


.. py:class:: load_body_generalized.LoadBodyGeneralized(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_BODY_GENERALIZED keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadBodyGeneralized

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~n1`
            - Get or set the Beginning node ID for body force load.
          * - :py:attr:`~n2`
            - Get or set the Ending node ID for body force load.
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID, see *DEFINE_CURVE.
          * - :py:attr:`~drlcid`
            - Get or set the Load curve ID for dynamic relaxation phase. Only if dynamic relaxation is defined.
          * - :py:attr:`~xc`
            - Get or set the X-center of rotation. Define only for angular velocity.
          * - :py:attr:`~yc`
            - Get or set the Y-center of rotation. Define only for angular velocity.
          * - :py:attr:`~zc`
            - Get or set the Z-center of rotation. Define only for angular velocity.
          * - :py:attr:`~ax`
            - Get or set the Scale factor for acceleration in x-direction.
          * - :py:attr:`~ay`
            - Get or set the Scale factor for acceleration in y-direction.
          * - :py:attr:`~az`
            - Get or set the Scale factor for acceleration in z-direction.
          * - :py:attr:`~omx`
            - Get or set the Scale factor for x-angular velocity.
          * - :py:attr:`~omy`
            - Get or set the Scale factor for y-angular velocity.
          * - :py:attr:`~omz`
            - Get or set the Scale factor for z-angular velocity.
          * - :py:attr:`~cid`
            - Get or set the Coordinate system ID to define acceleration in the local coordinate system.  The coordinate (XC, YC, ZC) is defined with respect to the local coordinate system if CID is nonzero.  The accelerations, LCID and their scale factors are with respect to CID.
          * - :py:attr:`~angtyp`
            - Get or set the Type of body loads due to angular motion:


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

    from load_body_generalized import LoadBodyGeneralized

Property detail
---------------

.. py:property:: n1
   :type: Optional[int]


   
   Get or set the Beginning node ID for body force load.
















   ..
       !! processed by numpydoc !!

.. py:property:: n2
   :type: Optional[int]


   
   Get or set the Ending node ID for body force load.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID, see *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: drlcid
   :type: int


   
   Get or set the Load curve ID for dynamic relaxation phase. Only if dynamic relaxation is defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: xc
   :type: float


   
   Get or set the X-center of rotation. Define only for angular velocity.
















   ..
       !! processed by numpydoc !!

.. py:property:: yc
   :type: float


   
   Get or set the Y-center of rotation. Define only for angular velocity.
















   ..
       !! processed by numpydoc !!

.. py:property:: zc
   :type: float


   
   Get or set the Z-center of rotation. Define only for angular velocity.
















   ..
       !! processed by numpydoc !!

.. py:property:: ax
   :type: float


   
   Get or set the Scale factor for acceleration in x-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: ay
   :type: float


   
   Get or set the Scale factor for acceleration in y-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: az
   :type: float


   
   Get or set the Scale factor for acceleration in z-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: omx
   :type: float


   
   Get or set the Scale factor for x-angular velocity.
















   ..
       !! processed by numpydoc !!

.. py:property:: omy
   :type: float


   
   Get or set the Scale factor for y-angular velocity.
















   ..
       !! processed by numpydoc !!

.. py:property:: omz
   :type: float


   
   Get or set the Scale factor for z-angular velocity.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: int


   
   Get or set the Coordinate system ID to define acceleration in the local coordinate system.  The coordinate (XC, YC, ZC) is defined with respect to the local coordinate system if CID is nonzero.  The accelerations, LCID and their scale factors are with respect to CID.
















   ..
       !! processed by numpydoc !!

.. py:property:: angtyp
   :type: str


   
   Get or set the Type of body loads due to angular motion:
   EQ.CENT:        body load from centrifugal acceleration,ρ[ω×(ω×r)].
   EQ.CORI : body load from Coriolis - type acceleration,2ρ(ω×v).
   EQ.ROTA : body load from rotational acceleration,ρ(α×r),        where ω is the angular velocity, α is the angular acceleration, r is the position vector relative to center of rotationand v is the velocity vector
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'BODY_GENERALIZED'






