





:class:`RigidwallGeometricPrismDisplay`
=======================================


.. py:class:: rigidwall_geometric_prism_display.RigidwallGeometricPrismDisplay(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA RIGIDWALL_GEOMETRIC_PRISM_DISPLAY keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: RigidwallGeometricPrismDisplay

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
          * - :py:attr:`~lenp`
            - Get or set the Length of prism in the direction negative to n.
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

    from rigidwall_geometric_prism_display import RigidwallGeometricPrismDisplay

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

.. py:property:: lenp
   :type: float


   
   Get or set the Length of prism in the direction negative to n.
















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
   :value: 'GEOMETRIC_PRISM_DISPLAY'






