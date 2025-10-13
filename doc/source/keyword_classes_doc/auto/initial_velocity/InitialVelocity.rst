





:class:`InitialVelocity`
========================


.. py:class:: initial_velocity.InitialVelocity(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INITIAL_VELOCITY keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InitialVelocity

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nsid`
            - Get or set the Nodal set ID, see *SET_NODE, containing nodes for initial velocity:
          * - :py:attr:`~nsidex`
            - Get or set the Nodal set ID, see *SET_NODE, containing nodes that are exempted from the imposed velocities and may have other initial velocities.
          * - :py:attr:`~boxid`
            - Get or set the All nodes in the box which belong to NSID are initialized. Nodes outside the box are not initalized. Exempted nodes are initialized to velocities defined by VXE, VYE, and VZE below regardless of their location relative to the box.
          * - :py:attr:`~irigid`
            - Get or set the Option to overwrite rigid body velocities defined on *PART_INERTIA and *CONSTRAINED_NODAL_RIGID_BODY_INERTIA cards.
          * - :py:attr:`~icid`
            - Get or set the Local coordinate system ID. The initial velocity is specified in the local coordinate system if ICID is greater than zero.
          * - :py:attr:`~vx`
            - Get or set the Initial velocity in x-direction.
          * - :py:attr:`~vy`
            - Get or set the Initial velocity in y-direction.
          * - :py:attr:`~vz`
            - Get or set the Initial velocity in z-direction.
          * - :py:attr:`~vxr`
            - Get or set the Initial rotational velocity about the x-axis.
          * - :py:attr:`~vyr`
            - Get or set the Initial rotational velocity about the y-axis.
          * - :py:attr:`~vzr`
            - Get or set the Initial rotational velocity about the z-axis.
          * - :py:attr:`~vxe`
            - Get or set the Initial velocity in x-direction of exempted nodes.
          * - :py:attr:`~vye`
            - Get or set the Initial velocity in y-direction of exempted nodes.
          * - :py:attr:`~vze`
            - Get or set the Initial velocity in z-direction of exempted nodes.
          * - :py:attr:`~vxre`
            - Get or set the Initial rotational velocity in x-direction of exempted nodes.
          * - :py:attr:`~vyre`
            - Get or set the Initial rotational velocity in y-direction of exempted nodes.
          * - :py:attr:`~vzre`
            - Get or set the Initial rotational velocity in z-direction of exempted nodes.


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

    from initial_velocity import InitialVelocity

Property detail
---------------

.. py:property:: nsid
   :type: Optional[int]


   
   Get or set the Nodal set ID, see *SET_NODE, containing nodes for initial velocity:
   EQ.0: all nodes are included.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsidex
   :type: int


   
   Get or set the Nodal set ID, see *SET_NODE, containing nodes that are exempted from the imposed velocities and may have other initial velocities.
















   ..
       !! processed by numpydoc !!

.. py:property:: boxid
   :type: int


   
   Get or set the All nodes in the box which belong to NSID are initialized. Nodes outside the box are not initalized. Exempted nodes are initialized to velocities defined by VXE, VYE, and VZE below regardless of their location relative to the box.
   Note VXE,VYE and VZE will only be shown once a value is input for NSIDEX.
















   ..
       !! processed by numpydoc !!

.. py:property:: irigid
   :type: int


   
   Get or set the Option to overwrite rigid body velocities defined on *PART_INERTIA and *CONSTRAINED_NODAL_RIGID_BODY_INERTIA cards.
   GE.1: part set ID, containing ID of parts to overwrite. Centre of gravity of part must lie within box BOXID. If BOXID is not defined then all parts defined in the set are overwritten.
   EQ.-1: Overwrite velocities for all *PART_INERTIA's and *CONSTRAINED_NODAL_RIGID_BODY_INERTIA 's with a centre of gravity within box BOXID. If BOXID is not defined then all are overwritten.
   EQ.-2: Overwrite velocities for all *PART_INERTIA's and *CONSTRAINED_NODAL_RIGID_BODY_INERTIA's.
















   ..
       !! processed by numpydoc !!

.. py:property:: icid
   :type: int


   
   Get or set the Local coordinate system ID. The initial velocity is specified in the local coordinate system if ICID is greater than zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: vx
   :type: float


   
   Get or set the Initial velocity in x-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: vy
   :type: float


   
   Get or set the Initial velocity in y-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: vz
   :type: float


   
   Get or set the Initial velocity in z-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: vxr
   :type: float


   
   Get or set the Initial rotational velocity about the x-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: vyr
   :type: float


   
   Get or set the Initial rotational velocity about the y-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: vzr
   :type: float


   
   Get or set the Initial rotational velocity about the z-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: vxe
   :type: float


   
   Get or set the Initial velocity in x-direction of exempted nodes.
















   ..
       !! processed by numpydoc !!

.. py:property:: vye
   :type: float


   
   Get or set the Initial velocity in y-direction of exempted nodes.
















   ..
       !! processed by numpydoc !!

.. py:property:: vze
   :type: float


   
   Get or set the Initial velocity in z-direction of exempted nodes.
















   ..
       !! processed by numpydoc !!

.. py:property:: vxre
   :type: float


   
   Get or set the Initial rotational velocity in x-direction of exempted nodes.
















   ..
       !! processed by numpydoc !!

.. py:property:: vyre
   :type: float


   
   Get or set the Initial rotational velocity in y-direction of exempted nodes.
















   ..
       !! processed by numpydoc !!

.. py:property:: vzre
   :type: float


   
   Get or set the Initial rotational velocity in z-direction of exempted nodes.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INITIAL'


.. py:attribute:: subkeyword
   :value: 'VELOCITY'






