





:class:`ConstrainedNodalRigidbody`
==================================


.. py:class:: constrained_nodal_rigidbody.ConstrainedNodalRigidbody(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_NODAL_RIGIDBODY keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedNodalRigidbody

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

    from constrained_nodal_rigidbody import ConstrainedNodalRigidbody

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



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'NODAL_RIGIDBODY'






