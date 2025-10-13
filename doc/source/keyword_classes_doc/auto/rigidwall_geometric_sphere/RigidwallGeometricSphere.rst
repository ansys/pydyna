





:class:`RigidwallGeometricSphere`
=================================


.. py:class:: rigidwall_geometric_sphere.RigidwallGeometricSphere(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA RIGIDWALL_GEOMETRIC_SPHERE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: RigidwallGeometricSphere

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
          * - :py:attr:`~radsph`
            - Get or set the Radius of sphere.


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

    from rigidwall_geometric_sphere import RigidwallGeometricSphere

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

.. py:property:: radsph
   :type: float


   
   Get or set the Radius of sphere.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'RIGIDWALL'


.. py:attribute:: subkeyword
   :value: 'GEOMETRIC_SPHERE'






