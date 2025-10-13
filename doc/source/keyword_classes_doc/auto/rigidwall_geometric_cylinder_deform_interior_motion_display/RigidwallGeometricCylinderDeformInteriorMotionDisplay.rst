





:class:`RigidwallGeometricCylinderDeformInteriorMotionDisplay`
==============================================================


.. py:class:: rigidwall_geometric_cylinder_deform_interior_motion_display.RigidwallGeometricCylinderDeformInteriorMotionDisplay(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA RIGIDWALL_GEOMETRIC_CYLINDER_DEFORM_INTERIOR_MOTION_DISPLAY keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: RigidwallGeometricCylinderDeformInteriorMotionDisplay

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Optional Rigidwall ID.
          * - :py:attr:`~title`
            - Get or set the Ridigwall id descriptor. It is suggested that unique descriptions be used.
          * - :py:attr:`~nsid`
            - Get or set the Node set ID containing tracked nodes, see *SET_NODE_OPTION.
          * - :py:attr:`~nsidex`
            - Get or set the Node set ID containing nodes that exempted as tracked nodes, see *SET_NODE_OPTION.
          * - :py:attr:`~boxid`
            - Get or set the If defined, only nodes in box are included as tracked nodes for the rigid wall.
          * - :py:attr:`~birth`
            - Get or set the Birth time of rigid wall.  The time values of the load curves that control the motion of the wall are offset by the birth time.
          * - :py:attr:`~death`
            - Get or set the Death time of rigid wall.  At this time the wall is deleted from the calculation
          * - :py:attr:`~xt`
            - Get or set the x-coordinate of tail of any outward drawn normal vector, n, originating on wall (tail) and terminating in space (head).
          * - :py:attr:`~yt`
            - Get or set the y-coordinate of tail of normal vector n.
          * - :py:attr:`~zt`
            - Get or set the z-coordinate of tail of normal vector n.
          * - :py:attr:`~xh`
            - Get or set the x-coordinate of head of normal vector n.
          * - :py:attr:`~yh`
            - Get or set the y-coordinate of head of normal vector n.
          * - :py:attr:`~zh`
            - Get or set the z-coordinate of head of normal vector n.
          * - :py:attr:`~fric`
            - Get or set the Coulomb friction coefficient, except as noted below:
          * - :py:attr:`~radcyl`
            - Get or set the Radius of cylinder.
          * - :py:attr:`~lencyl`
            - Get or set the Length of cylinder. Only if a value larger than zero is specified is a finite length is assumed.
          * - :py:attr:`~nsegs`
            - Get or set the Number of subsections
          * - :py:attr:`~vl`
            - Get or set the Distance from the Cylinder base
          * - :py:attr:`~height`
            - Get or set the Section height
          * - :py:attr:`~xp`
            - Get or set the Coordinates of a point in the local xz-plane, see Remark 1.
          * - :py:attr:`~yp`
            - Get or set the Coordinates of a point in the local xz-plane, see Remark 1.
          * - :py:attr:`~zp`
            - Get or set the Coordinates of a point in the local xz-plane, see Remark 1.
          * - :py:attr:`~nl`
            - Get or set the Number of auto-generated elements in the longitudinal direction. If DISPLAY option is not used, NL will be ignored. See Remark 2.
          * - :py:attr:`~narc`
            - Get or set the Number of auto-generated elements in the circumferential direction. If DISPLAY option is not used, NARC will be ignored.
          * - :py:attr:`~nr`
            - Get or set the Number of auto-generated elements in the radius direction. If DISPLAY option is not used, NR will be ignored.
          * - :py:attr:`~lcidr`
            - Get or set the Curve ID to describe the change of the radius over time.
          * - :py:attr:`~lcida`
            - Get or set the Curve ID to describe the change of the rotation in radians about the local x-axis over time.
          * - :py:attr:`~lcidb`
            - Get or set the Curve ID to describe the change of the bending curvature over time. Bending occurs
          * - :py:attr:`~lcidg`
            - Get or set the Curve ID to describe the change of the rotation in radians about the local z-axis over time.
          * - :py:attr:`~lcid`
            - Get or set the Stonewall motion curve number, see *DEFINE_CURVE.
          * - :py:attr:`~opt`
            - Get or set the Type of motion:
          * - :py:attr:`~vx`
            - Get or set the x-direction cosine of velocity/displacement vector.
          * - :py:attr:`~vy`
            - Get or set the y-direction cosine of velocity/displacement vector.
          * - :py:attr:`~vz`
            - Get or set the z-direction cosine of velocity/displacement vector.
          * - :py:attr:`~pid`
            - Get or set the Unique part ID for moving geometric rigid wall.  If zero, a part ID will be set that is larger than the maximum of all user defined part IDs.
          * - :py:attr:`~ro`
            - Get or set the Density of rigid wall.
          * - :py:attr:`~e`
            - Get or set the Youngs modulus.
          * - :py:attr:`~pr`
            - Get or set the Poissons ratio.


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

    from rigidwall_geometric_cylinder_deform_interior_motion_display import RigidwallGeometricCylinderDeformInteriorMotionDisplay

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Optional Rigidwall ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Ridigwall id descriptor. It is suggested that unique descriptions be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsid
   :type: Optional[int]


   
   Get or set the Node set ID containing tracked nodes, see *SET_NODE_OPTION.
   EQ.0: all nodes are tracked with respects to the rigid wall.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsidex
   :type: int


   
   Get or set the Node set ID containing nodes that exempted as tracked nodes, see *SET_NODE_OPTION.
















   ..
       !! processed by numpydoc !!

.. py:property:: boxid
   :type: int


   
   Get or set the If defined, only nodes in box are included as tracked nodes for the rigid wall.
















   ..
       !! processed by numpydoc !!

.. py:property:: birth
   :type: float


   
   Get or set the Birth time of rigid wall.  The time values of the load curves that control the motion of the wall are offset by the birth time.
















   ..
       !! processed by numpydoc !!

.. py:property:: death
   :type: float


   
   Get or set the Death time of rigid wall.  At this time the wall is deleted from the calculation
















   ..
       !! processed by numpydoc !!

.. py:property:: xt
   :type: float


   
   Get or set the x-coordinate of tail of any outward drawn normal vector, n, originating on wall (tail) and terminating in space (head).
















   ..
       !! processed by numpydoc !!

.. py:property:: yt
   :type: float


   
   Get or set the y-coordinate of tail of normal vector n.
















   ..
       !! processed by numpydoc !!

.. py:property:: zt
   :type: float


   
   Get or set the z-coordinate of tail of normal vector n.
















   ..
       !! processed by numpydoc !!

.. py:property:: xh
   :type: float


   
   Get or set the x-coordinate of head of normal vector n.
















   ..
       !! processed by numpydoc !!

.. py:property:: yh
   :type: float


   
   Get or set the y-coordinate of head of normal vector n.
















   ..
       !! processed by numpydoc !!

.. py:property:: zh
   :type: float


   
   Get or set the z-coordinate of head of normal vector n.
















   ..
       !! processed by numpydoc !!

.. py:property:: fric
   :type: float


   
   Get or set the Coulomb friction coefficient, except as noted below:
   EQ.0.0: Frictionless sliding when in contact,
   EQ.1.0: No sliding when in contact
















   ..
       !! processed by numpydoc !!

.. py:property:: radcyl
   :type: Optional[float]


   
   Get or set the Radius of cylinder.
















   ..
       !! processed by numpydoc !!

.. py:property:: lencyl
   :type: Optional[float]


   
   Get or set the Length of cylinder. Only if a value larger than zero is specified is a finite length is assumed.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsegs
   :type: Optional[int]


   
   Get or set the Number of subsections
















   ..
       !! processed by numpydoc !!

.. py:property:: vl
   :type: Optional[float]


   
   Get or set the Distance from the Cylinder base
















   ..
       !! processed by numpydoc !!

.. py:property:: height
   :type: Optional[float]


   
   Get or set the Section height
















   ..
       !! processed by numpydoc !!

.. py:property:: xp
   :type: Optional[float]


   
   Get or set the Coordinates of a point in the local xz-plane, see Remark 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: yp
   :type: Optional[float]


   
   Get or set the Coordinates of a point in the local xz-plane, see Remark 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: zp
   :type: Optional[float]


   
   Get or set the Coordinates of a point in the local xz-plane, see Remark 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: nl
   :type: Optional[int]


   
   Get or set the Number of auto-generated elements in the longitudinal direction. If DISPLAY option is not used, NL will be ignored. See Remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: narc
   :type: Optional[int]


   
   Get or set the Number of auto-generated elements in the circumferential direction. If DISPLAY option is not used, NARC will be ignored.
















   ..
       !! processed by numpydoc !!

.. py:property:: nr
   :type: Optional[int]


   
   Get or set the Number of auto-generated elements in the radius direction. If DISPLAY option is not used, NR will be ignored.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidr
   :type: Optional[int]


   
   Get or set the Curve ID to describe the change of the radius over time.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcida
   :type: Optional[int]


   
   Get or set the Curve ID to describe the change of the rotation in radians about the local x-axis over time.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidb
   :type: Optional[int]


   
   Get or set the Curve ID to describe the change of the bending curvature over time. Bending occurs
   in the local xz-plane with the center of the bending lying on the negative side of the local x-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidg
   :type: Optional[int]


   
   Get or set the Curve ID to describe the change of the rotation in radians about the local z-axis over time.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Stonewall motion curve number, see *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: opt
   :type: int


   
   Get or set the Type of motion:
   EQ.0: velocity specified,
   EQ.1: displacement specified.
















   ..
       !! processed by numpydoc !!

.. py:property:: vx
   :type: Optional[float]


   
   Get or set the x-direction cosine of velocity/displacement vector.
















   ..
       !! processed by numpydoc !!

.. py:property:: vy
   :type: Optional[float]


   
   Get or set the y-direction cosine of velocity/displacement vector.
















   ..
       !! processed by numpydoc !!

.. py:property:: vz
   :type: Optional[float]


   
   Get or set the z-direction cosine of velocity/displacement vector.
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Unique part ID for moving geometric rigid wall.  If zero, a part ID will be set that is larger than the maximum of all user defined part IDs.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: float


   
   Get or set the Density of rigid wall.
















   ..
       !! processed by numpydoc !!

.. py:property:: e
   :type: float


   
   Get or set the Youngs modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: float


   
   Get or set the Poissons ratio.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'RIGIDWALL'


.. py:attribute:: subkeyword
   :value: 'GEOMETRIC_CYLINDER_DEFORM_INTERIOR_MOTION_DISPLAY'






