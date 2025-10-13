





:class:`AleInjection`
=====================


.. py:class:: ale_injection.AleInjection(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ALE_INJECTION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: AleInjection

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mmgset`
            - Get or set the Multi-Material Set ID (see *SET_MULTI-MATERIAL_GROUP_LIST).
          * - :py:attr:`~segset`
            - Get or set the Segment set ID (see *SET_SEGMENT). A local coordinate system
          * - :py:attr:`~global_`
            - Get or set the Three digit flag to control how to select the elements, how to
          * - :py:attr:`~lce`
            - Get or set the Curve ID for the internal energy (see Remark 6):
          * - :py:attr:`~lcrvl`
            - Get or set the Curve ID for the relative volume (see Remark 6):
          * - :py:attr:`~lcvt`
            - Get or set the Curve ID for the translational velocity:
          * - :py:attr:`~vect`
            - Get or set the Vector to orient the translation. See *DEFINE_VECTOR.
          * - :py:attr:`~lcvr`
            - Get or set the Curve ID for the rotational velocity:
          * - :py:attr:`~vecr`
            - Get or set the Vector to orient the rotational axis (see *DEFINE_VECTOR).
          * - :py:attr:`~boxv`
            - Get or set the Box (see *DEFINE_BOX) defining the region where the velocities are applied (see Remark 7).
          * - :py:attr:`~xg`
            - Get or set the Position of the rotation center (see Remark 8).
          * - :py:attr:`~yg`
            - Get or set the Position of the rotation center (see Remark 8).
          * - :py:attr:`~zg`
            - Get or set the Position of the rotation center (see Remark 8).
          * - :py:attr:`~surfct`
            - Get or set the Flag to define the surface, inside which the nodes and elements are selected:
          * - :py:attr:`~ndiv`
            - Get or set the Number of divisions of an element cut by the surface SURFCT to
          * - :py:attr:`~xl`
            - Get or set the Length of the geometry SURFCT in the local x-direction.
          * - :py:attr:`~yl`
            - Get or set the Length of the geometry SURFCT in the local y-direction.
          * - :py:attr:`~zd`
            - Get or set the Length for the geometry SURFCT in the local 𝑧-direction for z < 0,
          * - :py:attr:`~zu`
            - Get or set the Length for the geometry SURFCT in the local 𝑧-direction for z > 0.
          * - :py:attr:`~xc`
            - Get or set the x-coordinate in the segment of the local coordinate center (see Remark 14).
          * - :py:attr:`~yc`
            - Get or set the y-coordinate in the segment of the local coordinate center (see Remark 14).


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

    from ale_injection import AleInjection

Property detail
---------------

.. py:property:: mmgset
   :type: Optional[int]


   
   Get or set the Multi-Material Set ID (see *SET_MULTI-MATERIAL_GROUP_LIST).
















   ..
       !! processed by numpydoc !!

.. py:property:: segset
   :type: Optional[int]


   
   Get or set the Segment set ID (see *SET_SEGMENT). A local coordinate system
   is created for each segment. See Remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: global_
   :type: int


   
   Get or set the Three digit flag to control how to select the elements, how to
   prescribe the velocities and how to define the geometrical
   parameters of Cards 2 and 3 (including BOXV):
   EQ._ _ 0: Geometrical parameters are local to the segments of SEGSET
   EQ._ _ 1: Geometrical parameters are natural to SEGSET
   segments (see Remark 3 and Figure 4-1)
   EQ._ 0 _: Velocities are applied in local coordinate systems
   attached to each segment of SEGSET
   EQ._ 1 _: Velocities are applied in the global coordinate system
   EQ.0 _ _: Select the elements and nodes in the local volume
   around each segment of SEGSET
   EQ.1 _ _: Select the elements in the global volume formed by
   all the segments of SEGSET
   EQ.2 _ _: Select the elements and nodes in the global volume
   formed by all the segments of SEGSET. Velocities are
   applied in the global coordinate system.
















   ..
       !! processed by numpydoc !!

.. py:property:: lce
   :type: int


   
   Get or set the Curve ID for the internal energy (see Remark 6):
   GT.0: Load curve ID; see *DEFINE_CURVE. See Remark 2.
   LT.0: -LCE is the function ID for the internal energy which
   depends on 26 arguments: time, number of cycles, and
   nodal coordinates of the 8 nodes for the ALE element.
   See *DEFINE_FUNCTION. See Remark 5.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcrvl
   :type: int


   
   Get or set the Curve ID for the relative volume (see Remark 6):
   GT.0: Load curve ID; see *DEFINE_CURVE. See Remark 2.
   LT.0: -LCRVL is the function ID for the relative volume which
   depends on 26 arguments: time, number of cycles, and
   nodal coordinates of the 8 nodes for the ALE element.
   See *DEFINE_FUNCTION. See Remark 5.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcvt
   :type: int


   
   Get or set the Curve ID for the translational velocity:
   GT.0: Load curve ID; see *DEFINE_CURVE.
   LT.0: -LCVT is the function ID for the translational velocity
   which depends on 5 arguments: time, number of cycles,
   and nodal coordinates. See *DEFINE_FUNCTION. See Remark 5..
















   ..
       !! processed by numpydoc !!

.. py:property:: vect
   :type: int


   
   Get or set the Vector to orient the translation. See *DEFINE_VECTOR.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcvr
   :type: int


   
   Get or set the Curve ID for the rotational velocity:
   GT.0: Load curve ID; see *DEFINE_CURVE.
   LT.0: -LCVR is the function ID for the rotational velocity which
   depends on 5 arguments: time, number of cycles, and
   nodal coordinates. See *DEFINE_FUNCTION. See Remark     5.
















   ..
       !! processed by numpydoc !!

.. py:property:: vecr
   :type: int


   
   Get or set the Vector to orient the rotational axis (see *DEFINE_VECTOR).
















   ..
       !! processed by numpydoc !!

.. py:property:: boxv
   :type: int


   
   Get or set the Box (see *DEFINE_BOX) defining the region where the velocities are applied (see Remark 7).
















   ..
       !! processed by numpydoc !!

.. py:property:: xg
   :type: float


   
   Get or set the Position of the rotation center (see Remark 8).
















   ..
       !! processed by numpydoc !!

.. py:property:: yg
   :type: float


   
   Get or set the Position of the rotation center (see Remark 8).
















   ..
       !! processed by numpydoc !!

.. py:property:: zg
   :type: float


   
   Get or set the Position of the rotation center (see Remark 8).
















   ..
       !! processed by numpydoc !!

.. py:property:: surfct
   :type: int


   
   Get or set the Flag to define the surface, inside which the nodes and elements are selected:
   LT.0: -SURFCT is the Function ID (see *DEFINE_FUNCTION)
   for the rotational velocity with 17 arguments: time, number
   of cycles, ALE element center coordinates, segment nodal coordinates.
   EQ.0: Ellipsoid;
   EQ.1: Ellipse-based cylinder;
   EQ.2: Truncated ellipse-based cone;
   EQ.3: Drop geometry meaning a cone for -ZD < z < 0 and
   half an ellipsoid for 0< z < ZU (see Remark 11 and Figure 4-6);
   EQ.4: Box with side lengths -XL < x < XL, -YL < y < YL,
   and -ZD < z < ZU (see Figure 4-7)
   EQ.5: Segment based cylinder (see Remark 12 and Figure 4-8).
















   ..
       !! processed by numpydoc !!

.. py:property:: ndiv
   :type: int


   
   Get or set the Number of divisions of an element cut by the surface SURFCT to
   compute the volume fractions (see Remark 13 and Figure 4-2).
















   ..
       !! processed by numpydoc !!

.. py:property:: xl
   :type: float


   
   Get or set the Length of the geometry SURFCT in the local x-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: yl
   :type: float


   
   Get or set the Length of the geometry SURFCT in the local y-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: zd
   :type: float


   
   Get or set the Length for the geometry SURFCT in the local 𝑧-direction for z < 0,
   except for SURFCT = 2 where z > 0. ZD can be input as a negative or positive value.
















   ..
       !! processed by numpydoc !!

.. py:property:: zu
   :type: float


   
   Get or set the Length for the geometry SURFCT in the local 𝑧-direction for z > 0.
















   ..
       !! processed by numpydoc !!

.. py:property:: xc
   :type: float


   
   Get or set the x-coordinate in the segment of the local coordinate center (see Remark 14).
















   ..
       !! processed by numpydoc !!

.. py:property:: yc
   :type: float


   
   Get or set the y-coordinate in the segment of the local coordinate center (see Remark 14).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ALE'


.. py:attribute:: subkeyword
   :value: 'INJECTION'






