





:class:`SetSegmentGeneral`
==========================


.. py:class:: set_segment_general.SetSegmentGeneral(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SET_SEGMENT_GENERAL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SetSegmentGeneral

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the Segment set ID. All segment sets should have a unique set ID.
          * - :py:attr:`~da1`
            - Get or set the First segment attribute default value is 0.0.
          * - :py:attr:`~da2`
            - Get or set the Second segment attribute default value is 0.0.
          * - :py:attr:`~da3`
            - Get or set the Third segment attribute default value is 0.0.
          * - :py:attr:`~da4`
            - Get or set the Fourth segment attribute default value is 0.0.
          * - :py:attr:`~solver`
            - Get or set the EQ.MECH: mechanics.
          * - :py:attr:`~its`
            - Get or set the Define coupling type across different scales in two-scale co-simulation. See *INCLUDE_COSIM in Manual Volume IV: Multiscale Solvers.
          * - :py:attr:`~option`
            - Get or set the ALL
          * - :py:attr:`~e1`
            - Get or set the OPTION.EQ.DBOX: Segment inside box E1 will be excluded from the current set,
          * - :py:attr:`~e2`
            - Get or set the OPTION.EQ.DBOX: Segment inside box E2 will be excluded from the current set,
          * - :py:attr:`~e3`
            - Get or set the OPTION.EQ.DBOX: Segment inside box E3 will be excluded from the current set,
          * - :py:attr:`~e4`
            - Get or set the OPTION.EQ.DBOX: Segment inside box E4 will be excluded from the current set,
          * - :py:attr:`~e5`
            - Get or set the OPTION.EQ.DBOX: Segment inside box E5 will be excluded from the current set,
          * - :py:attr:`~e6`
            - Get or set the OPTION.EQ.DBOX: Segment inside box E6 will be excluded from the current set,
          * - :py:attr:`~e7`
            - Get or set the OPTION.EQ.DBOX: Segment inside box E7 will be excluded from the current set,
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

    from set_segment_general import SetSegmentGeneral

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Segment set ID. All segment sets should have a unique set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: da1
   :type: float


   
   Get or set the First segment attribute default value is 0.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: da2
   :type: float


   
   Get or set the Second segment attribute default value is 0.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: da3
   :type: float


   
   Get or set the Third segment attribute default value is 0.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: da4
   :type: float


   
   Get or set the Fourth segment attribute default value is 0.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: solver
   :type: str


   
   Get or set the EQ.MECH: mechanics.
   EQ.CESE: CE/SE compressible fluid flow solver.
   EQ.ICFD: Incompressible fluid flow solver.
















   ..
       !! processed by numpydoc !!

.. py:property:: its
   :type: Optional[int]


   
   Get or set the Define coupling type across different scales in two-scale co-simulation. See *INCLUDE_COSIM in Manual Volume IV: Multiscale Solvers.
   EQ.1:   Tie - contact coupling.
   EQ.2 : Solid - in - shell immersed coupling
















   ..
       !! processed by numpydoc !!

.. py:property:: option
   :type: str


   
   Get or set the ALL
   All exterior segments will be included in the set.
   BOX
   Generate segments inside boxes having IDs E1, E2, and E3 with attributes having values E4, E5, E6, and E7.  For shell elements one segment per shell is generated. For solid elements only those segments wrapping the solid part and pointing outward from the part will be generated.
   BOX_SHELL
   Generate segments inside boxes having IDs E1, E2, and E3 with attributes having values E4, E5, E6, and E7.  The segments are only generated for shell elements.  One segment per shell is generated.
   BOX_SLDIO
   Generate segments inside boxes having IDs E1, E2, and E3 with attributes having values E4, E5, E6, and E7.  Both exterior segments and inter-element segments are generated.
   BOX_SOLID
   Generate segments inside boxes having IDs E1, E2, and E3 with attributes having values E4, E5, E6, and E7.  The segments are only generated for exterior solid elements
   PART
   Generate segments of parts E1, E2, and E3 with attributes E4, E5, E6, and E7.  For shell elements one segment per shell is generated.  For solid elements only those segments wrapping the solid part and pointing outward from the part will be generated.  PART could refer to beam parts when defining 2D segments for traction application.
   PART_IO
   Generate segments from parts E1, E2, E3 with attributes E4, E5, E6, and E7.  Same as the PART option above except that inter-element segments inside parts will be generated as well.  This option is sometimes useful for single surface contact of solid elements to prevent negative volumes.
   PSLDFi  Generate segments from the i’th face of solid parts E1, E2, E3 with attributes E4, E5, E6, and E7.  See table below for face definition.
   SEG
   Create segment with node IDs E1, E2, E3, and E4.
   VOL     Generate segments inside contact volume IDs E1, E2, and E3 with attributes having values E4, E5, E6, and E7.  See BOX option for other details.
   VOL_SHELL
   Generate segments for shells inside contact volume IDs E1, E2, and E3 with attributes having values E4, E5, E6, and E7
   VOL_SLDIO
   Generate segments for solid elements inside contact volume IDs E1, E2, and E3 with attributes E4, E5, E6, and E7.  See BOX_SLDIO for other details.
   VOL_SOLID
   Generate segments for solid elements inside contact volume IDs E1, E2, and E3 with attributes E4, E5, E6, and E7.  See BOX_SOLID for other details.
   SET_SHELL
   Generate segments for shell elements in SET_SHELL_LIST with IDs E1, E2, and E3 with attributes E4, E5, E6, and E7.
   SET_SOLID
   Generate segments for solid elements in SET_SOLID_LIST with IDs E1, E2, and E3 with attributes E4, E5, E6, and E7.
   SET_SLDIO
   Generate segments for solid elements in SET_SOLID_LIST with IDs E1, E2, and E3 with attributes E4, E5, E6, and E7.  Both exterior & interior segments are generated.
   SET_SLDFi
   Generate segments from the ith face of solid elements in SET_SOLID_LIST with IDs E1, E2, and E3 with attributes E4, E5, E6, and E7.  See table below for face definition.
   SET_TSHELL
   Generate segments for thick shell elements in SET_TSHELL_LIST with IDs of E1, E2, and E3 with attributes E4, E5, E6, and E7.  Only exterior segments are generated.
   SET_TSHIO
   Generate segments for thick shell elements in SET_TSHELL_LIST with IDs of E1, E2, and E3 with attributes E5, E5, E6, and E7.  Both exterior & interior segments are generated.
   SHELL Generate segments for shell elements with IDs of E1, E2, and E3 with attributes E4, E5, E6, and E7.
   DBOX    Segments inside boxes with IDs E1, ? E7 will be excluded.
   DBOX_SHELL
   Shell related segments inside boxes of IDs E1, ? E7 will be excluded.
   DBOX_SOLID
   Solid related segments inside boxes of IDs E1, ? E7 will be excluded.
   DPART   Segments of parts with IDs E1, ? E7 will be excluded.
   DSEG    Segment with node IDs  E1, E2, E3, and E4 will be deleted.
   DVOL    Segments inside contact volumes having IDs E1, ? E7 will be excluded.
   DVOL_SHELL
   Shell related segments inside contact volumes having IDs E1, ? E7 will be excluded.
   DVOL_SOLID
   Solid related segments inside contact volumes having IDs E1, ? E7 will be excluded.
   SALECPT Segments inside a box in Structured ALE mesh. E1 here is the S-ALE mesh ID (MSHID).  E2, E3, E4, E5, E6, E7 correspond to XMIN, XMAX, YMIN, YMAX, ZMIN, ZMAX.  They are the minimum and the maximum nodal indices along each direction in S-ALE mesh.  This option is only to be used for Structured ALE mesh and should not be used in a mixed manner with other “_GENERAL?options.
   Please refer to *ALE_STRUCTURED_MESH_CONTROL_POINTS and *ALE_STRUCTURED_MESH_CONTROL for more details.
   SALEFAC Segments on the face of Structured ALE mesh. E1 here is the S-ALE mesh ID (MSHID).  E2, E3, E4, E5, E6, E7 correspond to -X, +X, -Y, +Y, -Z, +Z faces.  Assigning 1 to these 6 values would include all the surface segments at these faces in the segment set.  This option is only to be used for Structured ALE mesh and should not be used in a mixed manner with other “_GENERAL?options.
   Please refer to *ALE_STRUCTURED_MESH_CONTROL_POINTS and *ALE_STRUCTURED_MESH_CONTROL for more details
















   ..
       !! processed by numpydoc !!

.. py:property:: e1
   :type: Optional[int]


   
   Get or set the OPTION.EQ.DBOX: Segment inside box E1 will be excluded from the current set,
   OPTION.EQ.DBOX_SHELL: Shell related segments inside box E1 will be excluded from the current set,
   OPTION.EQ.DBOX_SOLID: Solid related segments inside box E1 previously added will be excluded from the current set,OPTION.EQ.DPART: Segments of part E1 previously added will be excluded from the current set,
   OPTION.EQ.DSEG: Segments with node ID's E1, E2, E3, and E4 previously added will be deleted. The numbering sequence is irrelevant,
   OPTION.EQ.SEG: Create segment with node ID's E1, E2, E3, and E4.
















   ..
       !! processed by numpydoc !!

.. py:property:: e2
   :type: Optional[int]


   
   Get or set the OPTION.EQ.DBOX: Segment inside box E2 will be excluded from the current set,
   OPTION.EQ.DBOX_SHELL: Shell related segments inside box E2 will be excluded from the current set,
   OPTION.EQ.DBOX_SOLID: Solid related segments inside box E2 previously added will be excluded from the current set,OPTION.EQ.DPART: Segments of part E2 previously added will be excluded from the current set,
   OPTION.EQ.DSEG: Segments with node ID's E1, E2, E3, and E4 previously added will be deleted. The numbering sequence is irrelevant,
   OPTION.EQ.SEG: Create segment with node ID's E1, E2, E3, and E4.
















   ..
       !! processed by numpydoc !!

.. py:property:: e3
   :type: Optional[int]


   
   Get or set the OPTION.EQ.DBOX: Segment inside box E3 will be excluded from the current set,
   OPTION.EQ.DBOX_SHELL: Shell related segments inside box E3 will be excluded from the current set,
   OPTION.EQ.DBOX_SOLID: Solid related segments inside box E3 previously added will be excluded from the current set,OPTION.EQ.DPART: Segments of part E3 previously added will be excluded from the current set,
   OPTION.EQ.DSEG: Segments with node ID's E1, E2, E3, and E4 previously added will be deleted. The numbering sequence is irrelevant,
   OPTION.EQ.SEG: Create segment with node ID's E1, E2, E3, and E4.
















   ..
       !! processed by numpydoc !!

.. py:property:: e4
   :type: Optional[int]


   
   Get or set the OPTION.EQ.DBOX: Segment inside box E4 will be excluded from the current set,
   OPTION.EQ.DBOX_SHELL: Shell related segments inside box E4 will be excluded from the current set,
   OPTION.EQ.DBOX_SOLID: Solid related segments inside box E4 previously added will be excluded from the current set,OPTION.EQ.DPART: Segments of part E4 previously added will be excluded from the current set,
   OPTION.EQ.DSEG: Segments with node ID's E1, E2, E3, and E4 previously added will be deleted. The numbering sequence is irrelevant,
   OPTION.EQ.SEG: Create segment with node ID's E1, E2, E3, and E4.
















   ..
       !! processed by numpydoc !!

.. py:property:: e5
   :type: Optional[int]


   
   Get or set the OPTION.EQ.DBOX: Segment inside box E5 will be excluded from the current set,
   OPTION.EQ.DBOX_SHELL: Shell related segments inside box E5 will be excluded from the current set,
   OPTION.EQ.DBOX_SOLID: Solid related segments inside box E5 previously added will be excluded from the current set,OPTION.EQ.DPART: Segments of part E5 previously added will be excluded from the current set,
   OPTION.EQ.DSEG: Not used,
   OPTION.EQ.SEG: Not used.
















   ..
       !! processed by numpydoc !!

.. py:property:: e6
   :type: Optional[int]


   
   Get or set the OPTION.EQ.DBOX: Segment inside box E6 will be excluded from the current set,
   OPTION.EQ.DBOX_SHELL: Shell related segments inside box E6 will be excluded from the current set,
   OPTION.EQ.DBOX_SOLID: Solid related segments inside box E6 previously added will be excluded from the current set,OPTION.EQ.DPART: Segments of part E6 previously added will be excluded from the current set,
   OPTION.EQ.DSEG: Not used,
   OPTION.EQ.SEG: Not used.
















   ..
       !! processed by numpydoc !!

.. py:property:: e7
   :type: Optional[int]


   
   Get or set the OPTION.EQ.DBOX: Segment inside box E7 will be excluded from the current set,
   OPTION.EQ.DBOX_SHELL: Shell related segments inside box E7 will be excluded from the current set,
   OPTION.EQ.DBOX_SOLID: Solid related segments inside box E7 previously added will be excluded from the current set,OPTION.EQ.DPART: Segments of part E7 previously added will be excluded from the current set,
   OPTION.EQ.DSEG: Not used,
   OPTION.EQ.SEG: Not used.
















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
   :value: 'SET'


.. py:attribute:: subkeyword
   :value: 'SEGMENT_GENERAL'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





