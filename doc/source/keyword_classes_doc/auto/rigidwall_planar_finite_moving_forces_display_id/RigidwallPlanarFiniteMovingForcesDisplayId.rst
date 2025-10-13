





:class:`RigidwallPlanarFiniteMovingForcesDisplayId`
===================================================


.. py:class:: rigidwall_planar_finite_moving_forces_display_id.RigidwallPlanarFiniteMovingForcesDisplayId(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA RIGIDWALL_PLANAR_FINITE_MOVING_FORCES_DISPLAY_ID keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: RigidwallPlanarFiniteMovingForcesDisplayId

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
            - Get or set the All nodes in box are included as tracked nodes for the interacting with ther rigid wall, see *DEFINE_BOX. If options NSID or NSIDEX are active then only the subset of nodes activated by these options are checked to see if they are within the box.
          * - :py:attr:`~offset`
            - Get or set the All nodes within a normal offset distance, OFFSET, to the rigid wall are included as tracked nodes for the rigid wall. If options NSID, NSIDEX, or BOXID are active then only the subset of nodes activated by these options are checked to see if they are within the offset distance.
          * - :py:attr:`~birth`
            - Get or set the Birth time of rigid wall.  The time values of the load curves that control the motion of the wall are offset by the birth time.
          * - :py:attr:`~death`
            - Get or set the Death time of rigid wall.  At this time the wall is deleted from the calculation
          * - :py:attr:`~rwksf`
            - Get or set the Stiffness scaling factor. If RWKSF is also specified in *CONTROL_ CONTACT, the stiffness is scaled by the product of the two values.
          * - :py:attr:`~xt`
            - Get or set the x-coordinate of tail of any outward drawn normal vector, n, originating on wall (tail) and terminating in space (head).
          * - :py:attr:`~yt`
            - Get or set the y-coordinate of tail of normal vector n.
          * - :py:attr:`~zt`
            - Get or set the z-coordinate of tail of normal vector n
          * - :py:attr:`~xh`
            - Get or set the x-coordinate of head of normal vector n
          * - :py:attr:`~yh`
            - Get or set the y-coordinate of head of normal vector n.
          * - :py:attr:`~zh`
            - Get or set the z-coordinate of head of normal vector n.
          * - :py:attr:`~fric`
            - Get or set the Interface friction:
          * - :py:attr:`~wvel`
            - Get or set the Critical normal velocity at which nodes weld to wall (FRIC = 2.0 or 3.0).
          * - :py:attr:`~xhev`
            - Get or set the x-coordinate of head of edge vector l.
          * - :py:attr:`~yhev`
            - Get or set the y-coordinate of head of edge vector l.
          * - :py:attr:`~zhev`
            - Get or set the z-coordinate of head of edge vector l.
          * - :py:attr:`~lenl`
            - Get or set the Length of l edge.
          * - :py:attr:`~lenm`
            - Get or set the Length of m edge.
          * - :py:attr:`~mass`
            - Get or set the Total mass of stonewall.
          * - :py:attr:`~v0`
            - Get or set the Initial velocity of stonewall in direction of defining vector, n.
          * - :py:attr:`~soft`
            - Get or set the Number of cycles to zero relative velocity to reduce force spike
          * - :py:attr:`~ssid`
            - Get or set the Segment set ID for defining areas for force output, see *SET_SEGMENT.
          * - :py:attr:`~n1`
            - Get or set the Optional nodal point for visualization in LS-DYNA database.
          * - :py:attr:`~n2`
            - Get or set the Optional nodal point for visualization in LS-DYNA database.
          * - :py:attr:`~n3`
            - Get or set the Optional nodal point for visualization in LS-DYNA database.
          * - :py:attr:`~n4`
            - Get or set the Optional nodal point for visualization in LS-DYNA database.


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

    from rigidwall_planar_finite_moving_forces_display_id import RigidwallPlanarFiniteMovingForcesDisplayId

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
   EQ.0: All nodes are tracked for interacting with the rigid wall.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsidex
   :type: int


   
   Get or set the Node set ID containing nodes that exempted as tracked nodes, see *SET_NODE_OPTION.
















   ..
       !! processed by numpydoc !!

.. py:property:: boxid
   :type: int


   
   Get or set the All nodes in box are included as tracked nodes for the interacting with ther rigid wall, see *DEFINE_BOX. If options NSID or NSIDEX are active then only the subset of nodes activated by these options are checked to see if they are within the box.
















   ..
       !! processed by numpydoc !!

.. py:property:: offset
   :type: float


   
   Get or set the All nodes within a normal offset distance, OFFSET, to the rigid wall are included as tracked nodes for the rigid wall. If options NSID, NSIDEX, or BOXID are active then only the subset of nodes activated by these options are checked to see if they are within the offset distance.
















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

.. py:property:: rwksf
   :type: float


   
   Get or set the Stiffness scaling factor. If RWKSF is also specified in *CONTROL_ CONTACT, the stiffness is scaled by the product of the two values.
















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


   
   Get or set the z-coordinate of tail of normal vector n
















   ..
       !! processed by numpydoc !!

.. py:property:: xh
   :type: float


   
   Get or set the x-coordinate of head of normal vector n
















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


   
   Get or set the Interface friction:
   EQ.0.0: frictionless sliding after contact,
   EQ.1.0: no sliding after contact, 0.0 < FRIC < 1.0: Coulomb friction coefficient,
   EQ.2.0: node is welded after contact with frictionless sliding. Welding occurs if and only if the normal value of the impact velocity exceeds the critical value specified by WVEL,
   EQ.3.0: node is welded after contact with no sliding. Welding occurs if and only if the normal value of the impact velocity exceeds the critical value specified by WVEL.
















   ..
       !! processed by numpydoc !!

.. py:property:: wvel
   :type: float


   
   Get or set the Critical normal velocity at which nodes weld to wall (FRIC = 2.0 or 3.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: xhev
   :type: float


   
   Get or set the x-coordinate of head of edge vector l.
















   ..
       !! processed by numpydoc !!

.. py:property:: yhev
   :type: float


   
   Get or set the y-coordinate of head of edge vector l.
















   ..
       !! processed by numpydoc !!

.. py:property:: zhev
   :type: float


   
   Get or set the z-coordinate of head of edge vector l.
















   ..
       !! processed by numpydoc !!

.. py:property:: lenl
   :type: float


   
   Get or set the Length of l edge.
   EQ.0.0: defines an infinite size plane.
















   ..
       !! processed by numpydoc !!

.. py:property:: lenm
   :type: float


   
   Get or set the Length of m edge.
   EQ.0.0: defines an infinite size plane.
















   ..
       !! processed by numpydoc !!

.. py:property:: mass
   :type: Optional[float]


   
   Get or set the Total mass of stonewall.
















   ..
       !! processed by numpydoc !!

.. py:property:: v0
   :type: float


   
   Get or set the Initial velocity of stonewall in direction of defining vector, n.
















   ..
       !! processed by numpydoc !!

.. py:property:: soft
   :type: int


   
   Get or set the Number of cycles to zero relative velocity to reduce force spike
















   ..
       !! processed by numpydoc !!

.. py:property:: ssid
   :type: int


   
   Get or set the Segment set ID for defining areas for force output, see *SET_SEGMENT.
















   ..
       !! processed by numpydoc !!

.. py:property:: n1
   :type: int


   
   Get or set the Optional nodal point for visualization in LS-DYNA database.
















   ..
       !! processed by numpydoc !!

.. py:property:: n2
   :type: int


   
   Get or set the Optional nodal point for visualization in LS-DYNA database.
















   ..
       !! processed by numpydoc !!

.. py:property:: n3
   :type: int


   
   Get or set the Optional nodal point for visualization in LS-DYNA database.
















   ..
       !! processed by numpydoc !!

.. py:property:: n4
   :type: int


   
   Get or set the Optional nodal point for visualization in LS-DYNA database.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'RIGIDWALL'


.. py:attribute:: subkeyword
   :value: 'PLANAR_FINITE_MOVING_FORCES_DISPLAY_ID'






