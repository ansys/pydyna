





:class:`ConstrainedNodalRigidBodySpcInertia`
============================================


.. py:class:: constrained_nodal_rigid_body_spc_inertia.ConstrainedNodalRigidBodySpcInertia(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_NODAL_RIGID_BODY_SPC_INERTIA keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedNodalRigidBodySpcInertia

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID of the nodal rigid body.
          * - :py:attr:`~cid`
            - Get or set the Coordinate system ID for output of data in local system, see *DEFINE_COORDINATE_OPTION. Only necessary if no local system is defined below.
          * - :py:attr:`~nsid`
            - Get or set the Node set ID, see *SET_NODE. This nodal set defines the rigid body.If NSID=0, then NSID=PID, i.e., the node set ID and the part ID are assumed to be identical.
          * - :py:attr:`~pnode`
            - Get or set the An optional, possibly massless, nodal point located at the mass center of the nodal rigid body. The initial nodal coordinates will be reset if necessary to ensure that they lie at the mass center. In the output files, the coordinates, accelerations, velocites, and displacements of this node will coorespond to the mass center of the nodal rigid body. If CID is defined, the velocities and accelerations of PNODE will be output in the local system in the D3PLOT and D3THDT files unless PNODE is specified as a negative number in which case the global system is used.
          * - :py:attr:`~iprt`
            - Get or set the Print flag.  For nodal rigid bodies the following values apply:
          * - :py:attr:`~drflag`
            - Get or set the Displacement release flag for all nodes except the first node in the definition.
          * - :py:attr:`~rrflag`
            - Get or set the Rotation release flag for all nodes except the first node in the definition.
          * - :py:attr:`~cmo`
            - Get or set the Center of mass constraint option, CMO:
          * - :py:attr:`~con1`
            - Get or set the First constraint parameter:
          * - :py:attr:`~con2`
            - Get or set the If CMO=+1.0, then specify global rotational constraint:
          * - :py:attr:`~xc`
            - Get or set the x-coordinate of center of mass. If nodal point, NODEID, is defined XC, YC, and ZC are ignored and the coordinates of the nodal point, NODEID, are taken as the center of mass.
          * - :py:attr:`~yc`
            - Get or set the y-coordinate of center of mass.
          * - :py:attr:`~zc`
            - Get or set the z-coordinate of center of mass.
          * - :py:attr:`~tm`
            - Get or set the Translational mass.
          * - :py:attr:`~ircs`
            - Get or set the Flag for inertia tensor reference coordinate system:
          * - :py:attr:`~nodeid`
            - Get or set the Optional nodal point defining the CG of the rigid body. If this node is not a member of the set NSID above, its motion will not be updated to correspond with the nodal rigid body after the calculation begins. PNODE and NODEID can be identical if and only if PNODE physically lies at the mass center at time zero.
          * - :py:attr:`~ixx`
            - Get or set the XX component of inertia tensor.
          * - :py:attr:`~ixy`
            - Get or set the XY component of inertia tesor (set to zero if IRCS=1).
          * - :py:attr:`~ixz`
            - Get or set the XZ component of inertia tesor (set to zero if IRCS=1).
          * - :py:attr:`~iyy`
            - Get or set the YY component of inertia tensor.
          * - :py:attr:`~iyz`
            - Get or set the YZ component of inertia tesor (set to zero if IRCS=1).
          * - :py:attr:`~izz`
            - Get or set the ZZ component of inertia tensor.
          * - :py:attr:`~vtx`
            - Get or set the x-rigid body initial translational velocity in global coordinate system.
          * - :py:attr:`~vty`
            - Get or set the y-rigid body initial translational velocity in global coordinate system.
          * - :py:attr:`~vtz`
            - Get or set the z-rigid body initial translational velocity in global coordinate system.
          * - :py:attr:`~vrx`
            - Get or set the x-rigid body initial rotational velocity in global coordinate system.
          * - :py:attr:`~vry`
            - Get or set the y-rigid body initial rotational velocity in global coordinate system.
          * - :py:attr:`~vrz`
            - Get or set the z-rigid body initial rotational velocity in global coordinate system.
          * - :py:attr:`~xl`
            - Get or set the x-coordinate of local x-axis. Origin lies at (0,0,0)
          * - :py:attr:`~yl`
            - Get or set the y-coordinate of local x-axis.
          * - :py:attr:`~zl`
            - Get or set the z-coordinate of local x-axis.
          * - :py:attr:`~xlip`
            - Get or set the x-coordinate of local in-plane vector
          * - :py:attr:`~ylip`
            - Get or set the y-coordinate of local in-plane vector
          * - :py:attr:`~zlip`
            - Get or set the z-coordinate of local in-plane vector
          * - :py:attr:`~cid2`
            - Get or set the Local coordinate system ID, see *DEFINE_COORDINATE, with this option leave fields 1-6 blank.
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

    from constrained_nodal_rigid_body_spc_inertia import ConstrainedNodalRigidBodySpcInertia

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID of the nodal rigid body.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: Optional[int]


   
   Get or set the Coordinate system ID for output of data in local system, see *DEFINE_COORDINATE_OPTION. Only necessary if no local system is defined below.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsid
   :type: Optional[int]


   
   Get or set the Node set ID, see *SET_NODE. This nodal set defines the rigid body.If NSID=0, then NSID=PID, i.e., the node set ID and the part ID are assumed to be identical.
















   ..
       !! processed by numpydoc !!

.. py:property:: pnode
   :type: int


   
   Get or set the An optional, possibly massless, nodal point located at the mass center of the nodal rigid body. The initial nodal coordinates will be reset if necessary to ensure that they lie at the mass center. In the output files, the coordinates, accelerations, velocites, and displacements of this node will coorespond to the mass center of the nodal rigid body. If CID is defined, the velocities and accelerations of PNODE will be output in the local system in the D3PLOT and D3THDT files unless PNODE is specified as a negative number in which case the global system is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: iprt
   :type: int


   
   Get or set the Print flag.  For nodal rigid bodies the following values apply:
   EQ.1:   Write data into rbdout.
   EQ.2 : Do not write data into rbdout.
   Except for in the case of two - noded rigid bodies, IPRT(if 0 or unset) defaults to the value of IPRTF in* CONTROL_OUTPUT.For two - noded rigid bodies, printing is suppressed(IPRT = 2) unless IPRT is set to 1.  This is to avoid excessively large rbdout files when the model contains many two - noded welds.
















   ..
       !! processed by numpydoc !!

.. py:property:: drflag
   :type: int


   
   Get or set the Displacement release flag for all nodes except the first node in the definition.
   EQ.-7: release x, y, and z displacement in global system,
   EQ.-6: release z and x displacement in global system,
   EQ.-5: release y and z displacement in global system,
   EQ.-4: release x and y displacement in global system,
   EQ.-3: release z displacement in global system,
   EQ.-2: release y displacement in global system,
   EQ.-1: release x displacement in global system,
   EQ. 0: off for rigid body behavior,
   EQ. 1: release x displacement in rigid body local system,
   EQ. 2: release y displacement in rigid body local system,
   EQ. 3: release z displacement in rigid body local system,
   EQ. 4: release x and y displacement in rigid body local system,
   EQ. 5: release y and z displacement in rigid body local system,
   EQ. 6: release z and x displacement in rigid body local system,
   EQ. 7: release x, y, and z displacement in rigid body local system
















   ..
       !! processed by numpydoc !!

.. py:property:: rrflag
   :type: int


   
   Get or set the Rotation release flag for all nodes except the first node in the definition.
   EQ.-7: release x, y, and z rotations in global system,
   EQ.-6: release z and x rotations in global system,
   EQ.-5: release y and z rotations in global system,
   EQ.-4: release x and y rotations in global system,
   EQ.-3: release z rotation in global system,
   EQ.-2: release y rotation in global system,
   EQ.-1: release x rotation in global system,
   EQ. 0: off for rigid body behavior,
   EQ. 1: release x rotation in rigid body local system,
   EQ. 2: release y rotation in rigid body local system,
   EQ. 3: release z rotation in rigid body local system,
   EQ. 4: release x and y rotations in rigid body local system,
   EQ. 5: release y and z rotations in rigid body local system,
   EQ. 6: release z and x rotations in rigid body local system,
   EQ. 7: release x, y, and z rotations in rigid body local system,
















   ..
       !! processed by numpydoc !!

.. py:property:: cmo
   :type: float


   
   Get or set the Center of mass constraint option, CMO:
   EQ.+1.0: constraints applied in global directions,
   EQ. 0.0: no constraints,
   EQ. -1.0: constraints applied in local directions (SPC constraint).
















   ..
       !! processed by numpydoc !!

.. py:property:: con1
   :type: float


   
   Get or set the First constraint parameter:
   If CMO=+1.0, then specify global translational constraint:
   EQ.0: no constraints,
   EQ.1: constrained x displacement,
   EQ.2: constrained y displacement,
   EQ.3: constrained z displacement,
   EQ.4: constrained x and y displacements,
   EQ.5: constrained y and z displacements,
   EQ.6: constrained z and x displacements,
   EQ.7: constrained x, y, and z displacements.
   If CM0=-1.0, then specify local coordinate system ID. See *DEFINE_ COORDINATE_OPTION: This coordinate system is fixed in time.
















   ..
       !! processed by numpydoc !!

.. py:property:: con2
   :type: float


   
   Get or set the If CMO=+1.0, then specify global rotational constraint:
   EQ.0: no constraints,
   EQ.1: constrained x rotation,
   EQ.2: constrained y rotation,
   EQ.3: constrained z rotation,
   EQ.4: constrained x and y rotations,
   EQ.5: constrained y and z rotations,
   EQ.6: constrained z and x rotations,
   EQ.7: constrained x, y, and z rotations.
   If CM0=-1.0, then specify local (SPC) constraint:
   EQ.000000 no constraint,
   EQ.100000 constrained x translation,
   EQ.010000 constrained y translation,
   EQ.001000 constrained z translation,
   EQ.000100 constrained x rotation,
   EQ.000010 constrained y rotation,
   EQ.000001 constrained z rotation.
   Any combination of local constraints can be achieved by adding the number 1 into the corresponding column.
















   ..
       !! processed by numpydoc !!

.. py:property:: xc
   :type: float


   
   Get or set the x-coordinate of center of mass. If nodal point, NODEID, is defined XC, YC, and ZC are ignored and the coordinates of the nodal point, NODEID, are taken as the center of mass.
















   ..
       !! processed by numpydoc !!

.. py:property:: yc
   :type: float


   
   Get or set the y-coordinate of center of mass.
















   ..
       !! processed by numpydoc !!

.. py:property:: zc
   :type: float


   
   Get or set the z-coordinate of center of mass.
















   ..
       !! processed by numpydoc !!

.. py:property:: tm
   :type: float


   
   Get or set the Translational mass.
















   ..
       !! processed by numpydoc !!

.. py:property:: ircs
   :type: int


   
   Get or set the Flag for inertia tensor reference coordinate system:
   EQ.0: global inertia tensor,
   EQ.1: principal moments of inertias with orientation vectors as given below.
















   ..
       !! processed by numpydoc !!

.. py:property:: nodeid
   :type: int


   
   Get or set the Optional nodal point defining the CG of the rigid body. If this node is not a member of the set NSID above, its motion will not be updated to correspond with the nodal rigid body after the calculation begins. PNODE and NODEID can be identical if and only if PNODE physically lies at the mass center at time zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: ixx
   :type: Optional[float]


   
   Get or set the XX component of inertia tensor.
















   ..
       !! processed by numpydoc !!

.. py:property:: ixy
   :type: float


   
   Get or set the XY component of inertia tesor (set to zero if IRCS=1).
















   ..
       !! processed by numpydoc !!

.. py:property:: ixz
   :type: float


   
   Get or set the XZ component of inertia tesor (set to zero if IRCS=1).
















   ..
       !! processed by numpydoc !!

.. py:property:: iyy
   :type: Optional[float]


   
   Get or set the YY component of inertia tensor.
















   ..
       !! processed by numpydoc !!

.. py:property:: iyz
   :type: float


   
   Get or set the YZ component of inertia tesor (set to zero if IRCS=1).
















   ..
       !! processed by numpydoc !!

.. py:property:: izz
   :type: float


   
   Get or set the ZZ component of inertia tensor.
















   ..
       !! processed by numpydoc !!

.. py:property:: vtx
   :type: float


   
   Get or set the x-rigid body initial translational velocity in global coordinate system.
















   ..
       !! processed by numpydoc !!

.. py:property:: vty
   :type: float


   
   Get or set the y-rigid body initial translational velocity in global coordinate system.
















   ..
       !! processed by numpydoc !!

.. py:property:: vtz
   :type: float


   
   Get or set the z-rigid body initial translational velocity in global coordinate system.
















   ..
       !! processed by numpydoc !!

.. py:property:: vrx
   :type: float


   
   Get or set the x-rigid body initial rotational velocity in global coordinate system.
















   ..
       !! processed by numpydoc !!

.. py:property:: vry
   :type: float


   
   Get or set the y-rigid body initial rotational velocity in global coordinate system.
















   ..
       !! processed by numpydoc !!

.. py:property:: vrz
   :type: float


   
   Get or set the z-rigid body initial rotational velocity in global coordinate system.
















   ..
       !! processed by numpydoc !!

.. py:property:: xl
   :type: Optional[float]


   
   Get or set the x-coordinate of local x-axis. Origin lies at (0,0,0)
















   ..
       !! processed by numpydoc !!

.. py:property:: yl
   :type: Optional[float]


   
   Get or set the y-coordinate of local x-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: zl
   :type: Optional[float]


   
   Get or set the z-coordinate of local x-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: xlip
   :type: Optional[float]


   
   Get or set the x-coordinate of local in-plane vector
















   ..
       !! processed by numpydoc !!

.. py:property:: ylip
   :type: Optional[float]


   
   Get or set the y-coordinate of local in-plane vector
















   ..
       !! processed by numpydoc !!

.. py:property:: zlip
   :type: Optional[float]


   
   Get or set the z-coordinate of local in-plane vector
















   ..
       !! processed by numpydoc !!

.. py:property:: cid2
   :type: Optional[int]


   
   Get or set the Local coordinate system ID, see *DEFINE_COORDINATE, with this option leave fields 1-6 blank.
















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
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'NODAL_RIGID_BODY_SPC_INERTIA'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





