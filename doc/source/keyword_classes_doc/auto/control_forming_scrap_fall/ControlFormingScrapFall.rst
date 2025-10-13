





:class:`ControlFormingScrapFall`
================================


.. py:class:: control_forming_scrap_fall.ControlFormingScrapFall(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_FORMING_SCRAP_FALL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlFormingScrapFall

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID of a scrap piece.  This part ID becomes a dummy ID if all trimmed scrap pieces are defined by NEWID. See definition for NEWID and Figure 0-3.
          * - :py:attr:`~vectid`
            - Get or set the Vector ID for a trim steel movement, as defined by *DEFINE_VECTOR. If left undefined (blank), global z-direction is assumed.
          * - :py:attr:`~ndset`
            - Get or set the A node set consists of all nodes along the cutting edge of the trim steel.  Note that prior to Revision 90339 the nodes in the set must be defined in consecutive order.  See Remarks (LS-PrePost) below on how to define a node set along a path in LS-PrePost.  This node set, together with VECTID, is projected to the sheet metal to form a trim curve.  To trim a scrap out of a parent piece involving a neighboring trim steel, which also serves as a scrap cutter, the node set needs to be defined for the scrap cutter portion only for the scrap, see Figure 0-3.
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID governing the trim steel kinematics, as defined by *DEFINE_CURVE.
          * - :py:attr:`~depth`
            - Get or set the A small penetrating distance between the cutting edge of the trim steel and the scrap piece, as shown in Figure 0-2.  Nodes along the scrap edge are released from automatically added constraints at the simulation start and are free to move after this distance is reached.
          * - :py:attr:`~dist`
            - Get or set the A distance tolerance measured in the plane normal to the trim steel moving direction, between nodes along the cutting edge of the trim steel defined by NDSET and nodes along an edge of the scrap, as shown in Figure 0-1.  This tolerance is used to determine if the constraints need to be added at the simulation start to the nodes along the trim edge of the scrap piece.
          * - :py:attr:`~idrgd`
            - Get or set the Part ID of a parent piece, which is the remaining sheet metal after the scrap is successfully trimmed out of a large sheet metal.  Note the usual *PART needs to be defined somewhere in the input deck, along with *MAT_20 and totally fixed translational and rotational DOFs.  See Figure 0-3.
          * - :py:attr:`~ifseed`
            - Get or set the A flag to indicate the location of the scrap piece.
          * - :py:attr:`~nobead`
            - Get or set the A node set to be excluded from initially imposed constraints after trimming.  This node set typically consists of nodes in the scrap draw bead region where due to modeling problems the beads on the scrap initially interfere with the beads on the rigid tooling; it causes scrap to get stuck later in the simulation if left as is.  See Figure 0-4.
          * - :py:attr:`~seedx`
            - Get or set the x, y, z coordinates of the seed node on the scrap side; define only when IFSEED is set to “-1”.  See Figure 0-3.
          * - :py:attr:`~seedy`
            - Get or set the x, y, z coordinates of the seed node on the scrap side; define only when IFSEED is set to “-1”.  See Figure 0-3.
          * - :py:attr:`~seedz`
            - Get or set the x, y, z coordinates of the seed node on the scrap side; define only when IFSEED is set to “-1”.  See Figure 0-3.
          * - :py:attr:`~effset`
            - Get or set the Scrap edge offset amount away from the trim steel edge, towards the scrap seed node side.  This is useful to remove initial interference between the trimmed scrap (because of poorly modeled trim steel) and coarsely modeled lower trim post.  See Figure 0-3..
          * - :py:attr:`~gap`
            - Get or set the Scrap piece offset amount from the part set defined by IPSET (e.g. top surfaces of the scrap cutters), in the direction of the element normals of the IPSET.  This parameter makes it easier to remove initial interference between the scrap and other die components.  See Figure 0-5.
          * - :py:attr:`~ipset`
            - Get or set the A part set ID from which the scrap will be offset to remove the initial interference, works together only with GAP.  The part set ID should only include portions of tool parts that are directly underneath the scrap (top surface portion of the tools).  The normals of the IPSET must point toward the scrap.  The parts that should belong to IPSET are typically of those elements on the top surface of the scrap cutter, see Figure 0-5.
          * - :py:attr:`~extend`
            - Get or set the An amount to extend a trim steel’s edge based on the NDSET defined, so it can form a continuous trim line together with a neighboring trim steel, whose edge may also be extended, to trim out the scrap piece.  See Figure 0-3..
          * - :py:attr:`~newid`
            - Get or set the New part ID of a scrap piece for the scrap area defined by the seed location.  If this is not defined (left blank) or input as “0”, the scrap piece will retain original PID as its part ID.  See Figure 0-3.  This is useful in case where one original scrap is trimmed into multiple smaller pieces, and contacts between these smaller pieces need to be defined..


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

    from control_forming_scrap_fall import ControlFormingScrapFall

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID of a scrap piece.  This part ID becomes a dummy ID if all trimmed scrap pieces are defined by NEWID. See definition for NEWID and Figure 0-3.
















   ..
       !! processed by numpydoc !!

.. py:property:: vectid
   :type: Optional[int]


   
   Get or set the Vector ID for a trim steel movement, as defined by *DEFINE_VECTOR. If left undefined (blank), global z-direction is assumed.
















   ..
       !! processed by numpydoc !!

.. py:property:: ndset
   :type: Optional[int]


   
   Get or set the A node set consists of all nodes along the cutting edge of the trim steel.  Note that prior to Revision 90339 the nodes in the set must be defined in consecutive order.  See Remarks (LS-PrePost) below on how to define a node set along a path in LS-PrePost.  This node set, together with VECTID, is projected to the sheet metal to form a trim curve.  To trim a scrap out of a parent piece involving a neighboring trim steel, which also serves as a scrap cutter, the node set needs to be defined for the scrap cutter portion only for the scrap, see Figure 0-3.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID governing the trim steel kinematics, as defined by *DEFINE_CURVE.
   GT.0:   velocity-controlled kinematics
   LT.0:   displacement-controlled kinematics
   An example input deck is provided below.
















   ..
       !! processed by numpydoc !!

.. py:property:: depth
   :type: Optional[float]


   
   Get or set the A small penetrating distance between the cutting edge of the trim steel and the scrap piece, as shown in Figure 0-2.  Nodes along the scrap edge are released from automatically added constraints at the simulation start and are free to move after this distance is reached.
















   ..
       !! processed by numpydoc !!

.. py:property:: dist
   :type: Optional[float]


   
   Get or set the A distance tolerance measured in the plane normal to the trim steel moving direction, between nodes along the cutting edge of the trim steel defined by NDSET and nodes along an edge of the scrap, as shown in Figure 0-1.  This tolerance is used to determine if the constraints need to be added at the simulation start to the nodes along the trim edge of the scrap piece.
















   ..
       !! processed by numpydoc !!

.. py:property:: idrgd
   :type: Optional[int]


   
   Get or set the Part ID of a parent piece, which is the remaining sheet metal after the scrap is successfully trimmed out of a large sheet metal.  Note the usual *PART needs to be defined somewhere in the input deck, along with *MAT_20 and totally fixed translational and rotational DOFs.  See Figure 0-3.
















   ..
       !! processed by numpydoc !!

.. py:property:: ifseed
   :type: Optional[int]


   
   Get or set the A flag to indicate the location of the scrap piece.
   EQ.0:   automatically determined.  The trim steel defined will be responsible to trim as well as to push (have contact with) the scrap piece.
   EQ.1:   automatically determined, however, the trim steel in definition will only be used to trim out the scrap, not to push (have contact with) the scrap piece.
   EQ.-1:  user specified by defining SEEDX, SEEDY, and SEEDZ.
















   ..
       !! processed by numpydoc !!

.. py:property:: nobead
   :type: Optional[int]


   
   Get or set the A node set to be excluded from initially imposed constraints after trimming.  This node set typically consists of nodes in the scrap draw bead region where due to modeling problems the beads on the scrap initially interfere with the beads on the rigid tooling; it causes scrap to get stuck later in the simulation if left as is.  See Figure 0-4.
















   ..
       !! processed by numpydoc !!

.. py:property:: seedx
   :type: Optional[float]


   
   Get or set the x, y, z coordinates of the seed node on the scrap side; define only when IFSEED is set to “-1”.  See Figure 0-3.
















   ..
       !! processed by numpydoc !!

.. py:property:: seedy
   :type: Optional[float]


   
   Get or set the x, y, z coordinates of the seed node on the scrap side; define only when IFSEED is set to “-1”.  See Figure 0-3.
















   ..
       !! processed by numpydoc !!

.. py:property:: seedz
   :type: Optional[float]


   
   Get or set the x, y, z coordinates of the seed node on the scrap side; define only when IFSEED is set to “-1”.  See Figure 0-3.
















   ..
       !! processed by numpydoc !!

.. py:property:: effset
   :type: Optional[float]


   
   Get or set the Scrap edge offset amount away from the trim steel edge, towards the scrap seed node side.  This is useful to remove initial interference between the trimmed scrap (because of poorly modeled trim steel) and coarsely modeled lower trim post.  See Figure 0-3..
















   ..
       !! processed by numpydoc !!

.. py:property:: gap
   :type: Optional[float]


   
   Get or set the Scrap piece offset amount from the part set defined by IPSET (e.g. top surfaces of the scrap cutters), in the direction of the element normals of the IPSET.  This parameter makes it easier to remove initial interference between the scrap and other die components.  See Figure 0-5.
















   ..
       !! processed by numpydoc !!

.. py:property:: ipset
   :type: Optional[int]


   
   Get or set the A part set ID from which the scrap will be offset to remove the initial interference, works together only with GAP.  The part set ID should only include portions of tool parts that are directly underneath the scrap (top surface portion of the tools).  The normals of the IPSET must point toward the scrap.  The parts that should belong to IPSET are typically of those elements on the top surface of the scrap cutter, see Figure 0-5.
















   ..
       !! processed by numpydoc !!

.. py:property:: extend
   :type: Optional[int]


   
   Get or set the An amount to extend a trim steel’s edge based on the NDSET defined, so it can form a continuous trim line together with a neighboring trim steel, whose edge may also be extended, to trim out the scrap piece.  See Figure 0-3..
















   ..
       !! processed by numpydoc !!

.. py:property:: newid
   :type: Optional[int]


   
   Get or set the New part ID of a scrap piece for the scrap area defined by the seed location.  If this is not defined (left blank) or input as “0”, the scrap piece will retain original PID as its part ID.  See Figure 0-3.  This is useful in case where one original scrap is trimmed into multiple smaller pieces, and contacts between these smaller pieces need to be defined..
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'FORMING_SCRAP_FALL'






