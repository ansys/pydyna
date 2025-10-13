





:class:`InitialVolumeFractionGeometry`
======================================


.. py:class:: initial_volume_fraction_geometry.InitialVolumeFractionGeometry(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INITIAL_VOLUME_FRACTION_GEOMETRY keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InitialVolumeFractionGeometry

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~fmsid`
            - Get or set the Background ALE (fluid) mesh SID to be initialized or filled with various AMMGs.  This set ID refers to one or more ALE parts
          * - :py:attr:`~fmidtyp`
            - Get or set the ALE mesh set ID type:
          * - :py:attr:`~bammg`
            - Get or set the The background fluid group ID or ALE Multi-Material group ID (AMMGID) that initially fills the entire ALE mesh region defined by FMSID.For S-ALE, AMMG name (AMMGNM) could be also used in place of AMMGID
          * - :py:attr:`~ntrace`
            - Get or set the Number of sampling points for volume filling detection.  Typically NTRACE ranges from 3 to maybe 10 (or more).  The higher it is, the finer the ALE element is divided so that small gaps between 2 Lagrangian shells may be filled in.
          * - :py:attr:`~conttyp`
            - Get or set the A "container" defines a Lagrangian surface boundary of a spatial region, inside (or outside) of which, an AMMG would fill up.  CONTTYP defines the container geometry type of this surface boundary (or shell structure).
          * - :py:attr:`~fillopt`
            - Get or set the A flag to indicate which side of the container surface the AMMG is supposed to fill.  CNTTYP = 1, 2, and 3, the “head” side of a container surface/segment is defined as the side pointed to by the heads of the normal vectors of the segments (“tail” side refers to opposite direction to “head”).  See Remark 5. Note that for CNTTYP = 1 and 2, the fluid interface can be offset from the container walls with XOFFST. XOFFST does not apply to the other container geometries.
          * - :py:attr:`~fammg`
            - Get or set the This defines the fluid group ID or ALE Multi-Material group ID (AMMGID) which will fill up the interior (or exterior) of the space defined by the “container”. The order of AMMGIDs is determined by the order in which they are listed under *ALE_MULTI-MATERIAL_GROUP card.  For example, the first data card under the *ALE_MULTI-MATERIAL_GROUP keyword defines the multi-material group with ID (AMMGID) 1, the second data card defined AMMGID = 2, and so on. In case of S-ALE, AMMG name (AMMGNM) could be also used in place of AMMGID. See Remark 8.
          * - :py:attr:`~vx`
            - Get or set the A uniform initial X-velocity applied to the filled material group.
          * - :py:attr:`~vy`
            - Get or set the A uniform initial Y-velocity applied to the filled material group.
          * - :py:attr:`~vz`
            - Get or set the A uniform initial Z-velocity applied to the filled material group.
          * - :py:attr:`~sid`
            - Get or set the A Set ID pointing to a part ID (PID) or part set ID (PSID) of the Lagrangian shell element structure defining the "container" geometry to be filled (see *PART or *SET_PART)
          * - :py:attr:`~stype`
            - Get or set the Set ID type:
          * - :py:attr:`~normdir`
            - Get or set the Obsolete
          * - :py:attr:`~xoffset`
            - Get or set the |XOFFST| is the absolute length for offsetting the fluid interface from the nominal fluid interface LS-DYNA would otherwise define by default.  The sign of XOFFST determines which direction the interface is offset. It is based on the normal vectors of the segments associated with the container.
          * - :py:attr:`~sgsid_`
            - Get or set the Segment Set ID defining the "container", see *SET_SEGMENT
          * - :py:attr:`~x0`
            - Get or set the X-coordinate of a spatial point  on  the plane
          * - :py:attr:`~y0`
            - Get or set the Y-coordinate of a spatial point  on  the plane
          * - :py:attr:`~z0`
            - Get or set the Z-coordinate of a spatial point  on  the plane
          * - :py:attr:`~xcos`
            - Get or set the X-direction cosines of the plane normal vector
          * - :py:attr:`~ycos`
            - Get or set the Y-direction cosines of the plane normal vector
          * - :py:attr:`~zcos`
            - Get or set the Z-direction cosines of the plane normal vector
          * - :py:attr:`~x1`
            - Get or set the X-coordinate of the center of the upper base of the cone
          * - :py:attr:`~y1`
            - Get or set the Y-coordinate of the center of the upper base of the cone
          * - :py:attr:`~z1`
            - Get or set the Z-coordinate of the center of the upper base of the cone
          * - :py:attr:`~r1`
            - Get or set the Radius of the lower base
          * - :py:attr:`~r2`
            - Get or set the Radius of the upper base
          * - :py:attr:`~lcsid`
            - Get or set the Local coordinate system ID, if defined, the box is aligned with the
          * - :py:attr:`~r0`
            - Get or set the Radius of the sphere


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

    from initial_volume_fraction_geometry import InitialVolumeFractionGeometry

Property detail
---------------

.. py:property:: fmsid
   :type: Optional[int]


   
   Get or set the Background ALE (fluid) mesh SID to be initialized or filled with various AMMGs.  This set ID refers to one or more ALE parts
















   ..
       !! processed by numpydoc !!

.. py:property:: fmidtyp
   :type: int


   
   Get or set the ALE mesh set ID type:
   EQ.0:  FMSID is an ALE part set ID (PSID).
   EQ.1:  FMSID is an ALE part ID (PID).
















   ..
       !! processed by numpydoc !!

.. py:property:: bammg
   :type: int


   
   Get or set the The background fluid group ID or ALE Multi-Material group ID (AMMGID) that initially fills the entire ALE mesh region defined by FMSID.For S-ALE, AMMG name (AMMGNM) could be also used in place of AMMGID
















   ..
       !! processed by numpydoc !!

.. py:property:: ntrace
   :type: int


   
   Get or set the Number of sampling points for volume filling detection.  Typically NTRACE ranges from 3 to maybe 10 (or more).  The higher it is, the finer the ALE element is divided so that small gaps between 2 Lagrangian shells may be filled in.
















   ..
       !! processed by numpydoc !!

.. py:property:: conttyp
   :type: int


   
   Get or set the A "container" defines a Lagrangian surface boundary of a spatial region, inside (or outside) of which, an AMMG would fill up.  CONTTYP defines the container geometry type of this surface boundary (or shell structure).
   EQ.1: The container geometry is defined by a part ID (PID) or a part set ID (PSID), where the parts should be defined by shell elements (see *PART or *SET_PART).
   EQ.2: The container geometry is defined by a segment set (SGSID).
   EQ.3: The container geometry is defined by a plane: a point and a normal vector.
   EQ.4: The container geometry is defined by a conical surface: 2 end points and 2 corresponding radii.
   EQ.5: The container geometry is defined by a cuboid or rectangular box: 2 opposing end points, minimum to maximum coordinates.
   EQ.6: The container geometry is defined by a sphere: 1 center point, and a radius
   EQ.7:   The container geometry is defined by a user-defined function implemented in *DEFINE_FUNCTION.
   The arguments of the function should be the coordinates of a point (x,y,z).
   The function should return 1.0 if the point is inside the geometry
















   ..
       !! processed by numpydoc !!

.. py:property:: fillopt
   :type: int


   
   Get or set the A flag to indicate which side of the container surface the AMMG is supposed to fill.  CNTTYP = 1, 2, and 3, the “head” side of a container surface/segment is defined as the side pointed to by the heads of the normal vectors of the segments (“tail” side refers to opposite direction to “head”).  See Remark 5. Note that for CNTTYP = 1 and 2, the fluid interface can be offset from the container walls with XOFFST. XOFFST does not apply to the other container geometries.
   EQ.0:   The “head” side of the geometry defined above will be filled with fluid(default).For CNTTYP = 4, 5, 6,and 7, the inside of the container is filled.
   EQ.1 : The “tail” side of the geometry defined above will be filled with fluid.For CNTTYP = 4, 5, 6,and 7, the outside of the container is filled.
















   ..
       !! processed by numpydoc !!

.. py:property:: fammg
   :type: Optional[int]


   
   Get or set the This defines the fluid group ID or ALE Multi-Material group ID (AMMGID) which will fill up the interior (or exterior) of the space defined by the “container”. The order of AMMGIDs is determined by the order in which they are listed under *ALE_MULTI-MATERIAL_GROUP card.  For example, the first data card under the *ALE_MULTI-MATERIAL_GROUP keyword defines the multi-material group with ID (AMMGID) 1, the second data card defined AMMGID = 2, and so on. In case of S-ALE, AMMG name (AMMGNM) could be also used in place of AMMGID. See Remark 8.
   LT.0: | FAMMG | is a * SET_MULTI - MATERIAL_GROUP_LIST ID listing pairs of group IDs.For each pair, the 2nd group replaces the first one in the “container”.
















   ..
       !! processed by numpydoc !!

.. py:property:: vx
   :type: Optional[float]


   
   Get or set the A uniform initial X-velocity applied to the filled material group.
















   ..
       !! processed by numpydoc !!

.. py:property:: vy
   :type: Optional[float]


   
   Get or set the A uniform initial Y-velocity applied to the filled material group.
















   ..
       !! processed by numpydoc !!

.. py:property:: vz
   :type: Optional[float]


   
   Get or set the A uniform initial Z-velocity applied to the filled material group.
















   ..
       !! processed by numpydoc !!

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the A Set ID pointing to a part ID (PID) or part set ID (PSID) of the Lagrangian shell element structure defining the "container" geometry to be filled (see *PART or *SET_PART)
















   ..
       !! processed by numpydoc !!

.. py:property:: stype
   :type: int


   
   Get or set the Set ID type:
   EQ.0:  Container SID is a Lagrangian part set ID (PSID).
   EQ.1:  Container SID is a Lagrangian part ID (PID).
















   ..
       !! processed by numpydoc !!

.. py:property:: normdir
   :type: Optional[int]


   
   Get or set the Obsolete
















   ..
       !! processed by numpydoc !!

.. py:property:: xoffset
   :type: float


   
   Get or set the |XOFFST| is the absolute length for offsetting the fluid interface from the nominal fluid interface LS-DYNA would otherwise define by default.  The sign of XOFFST determines which direction the interface is offset. It is based on the normal vectors of the segments associated with the container.
   XOFFST.GT.0:    Interface is offset along the positive direction of the segments of the container.
   XOFFST.LT.0 : Interface is offset in the negative direction of the normal vectors of the segments of the container.
   This is applicable to cases in which high pressure fluid is contained within a container.The offset allows LS - DYNA time to prevent leakage.In general, this may be set to roughly 5 - 10 % of the ALE element width.It may be important only for when ILEAK is turned ON to give the code time to catch the leakage(see * CONSTRAINED_LAGRANGE_IN_SOLID).If ILEAK is not ON, this may not be necessary
















   ..
       !! processed by numpydoc !!

.. py:property:: sgsid_
   :type: Optional[int]


   
   Get or set the Segment Set ID defining the "container", see *SET_SEGMENT
















   ..
       !! processed by numpydoc !!

.. py:property:: x0
   :type: Optional[float]


   
   Get or set the X-coordinate of a spatial point  on  the plane
















   ..
       !! processed by numpydoc !!

.. py:property:: y0
   :type: Optional[float]


   
   Get or set the Y-coordinate of a spatial point  on  the plane
















   ..
       !! processed by numpydoc !!

.. py:property:: z0
   :type: Optional[float]


   
   Get or set the Z-coordinate of a spatial point  on  the plane
















   ..
       !! processed by numpydoc !!

.. py:property:: xcos
   :type: Optional[float]


   
   Get or set the X-direction cosines of the plane normal vector
















   ..
       !! processed by numpydoc !!

.. py:property:: ycos
   :type: Optional[float]


   
   Get or set the Y-direction cosines of the plane normal vector
















   ..
       !! processed by numpydoc !!

.. py:property:: zcos
   :type: Optional[float]


   
   Get or set the Z-direction cosines of the plane normal vector
















   ..
       !! processed by numpydoc !!

.. py:property:: x1
   :type: Optional[float]


   
   Get or set the X-coordinate of the center of the upper base of the cone
















   ..
       !! processed by numpydoc !!

.. py:property:: y1
   :type: Optional[float]


   
   Get or set the Y-coordinate of the center of the upper base of the cone
















   ..
       !! processed by numpydoc !!

.. py:property:: z1
   :type: Optional[float]


   
   Get or set the Z-coordinate of the center of the upper base of the cone
















   ..
       !! processed by numpydoc !!

.. py:property:: r1
   :type: Optional[float]


   
   Get or set the Radius of the lower base
















   ..
       !! processed by numpydoc !!

.. py:property:: r2
   :type: Optional[float]


   
   Get or set the Radius of the upper base
















   ..
       !! processed by numpydoc !!

.. py:property:: lcsid
   :type: Optional[int]


   
   Get or set the Local coordinate system ID, if defined, the box is aligned with the
   local coordinate system instead of global coordinate system.
   Please see *DEFINE_COORDINATE_ for details
















   ..
       !! processed by numpydoc !!

.. py:property:: r0
   :type: Optional[float]


   
   Get or set the Radius of the sphere
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INITIAL'


.. py:attribute:: subkeyword
   :value: 'VOLUME_FRACTION_GEOMETRY'






