





:class:`ConstrainedNodalRigidBodyOverride`
==========================================


.. py:class:: constrained_nodal_rigid_body_override.ConstrainedNodalRigidBodyOverride(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_NODAL_RIGID_BODY_OVERRIDE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedNodalRigidBodyOverride

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
          * - :py:attr:`~icnt`
            - Get or set the Flag for contact synchronization:
          * - :py:attr:`~ibag`
            - Get or set the Flag for control volume airbag synchronization:
          * - :py:attr:`~ipsm`
            - Get or set the Flag for prescribed-motion synchronization:
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

    from constrained_nodal_rigid_body_override import ConstrainedNodalRigidBodyOverride

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

.. py:property:: icnt
   :type: int


   
   Get or set the Flag for contact synchronization:
   EQ.0:   No synchronization,
   EQ.1 : Since there exists no contact when both slave and master sides belong to the same rigid body,
   setting ICNT = 1 will turn off / on all contact definitions of which the slave and master sides belong to
   the same nodal rigid body PID when PID is turned on / off by * SENSOR_CONTROL.
















   ..
       !! processed by numpydoc !!

.. py:property:: ibag
   :type: int


   
   Get or set the Flag for control volume airbag synchronization:
   EQ.0:   No synchronization,
   EQ.1 : Since airbag pressure will not change when all segments constituting the airbag belong to
   the same rigid body, setting IBAG = 1 will skip calculation of control volume airbags of
   which all the segments belong to the same nodal rigid body PID when PID is on.The airbag calculation will be resumed,
   with time offset to related airbag time - dependent curves, when PID is turned off by* SENSOR_CONTROL.
















   ..
       !! processed by numpydoc !!

.. py:property:: ipsm
   :type: int


   
   Get or set the Flag for prescribed-motion synchronization:
   EQ.0:   No synchronization,
   EQ.1 : Prescribed boundary conditions,* BOUNDARY_PRESCRIBED_MOTION, for PID will be turned off
   automatically when PID is turned off by* SENSOR_CONTROL.Prescribed boundary condition not for PIDand of
   which or all related nodes belong to PID will be turned off when PID is active to avoid boundary
   condition conflict.Those boundary conditions will be turned on, with time
   offset to related time - dependent curves, when PID is turned off by* SENSOR_CONTROL.
   EQ.2 : Same as IPSM = 1, however, without time offset when those boundary conditions not for PID are turned on..
















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
   :value: 'NODAL_RIGID_BODY_OVERRIDE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





