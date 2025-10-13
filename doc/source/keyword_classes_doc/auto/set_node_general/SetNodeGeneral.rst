





:class:`SetNodeGeneral`
=======================


.. py:class:: set_node_general.SetNodeGeneral(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SET_NODE_GENERAL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SetNodeGeneral

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the Node set ID. All node sets should have a unique set ID.
          * - :py:attr:`~da1`
            - Get or set the First nodal attribute default value is 0.0.
          * - :py:attr:`~da2`
            - Get or set the Second nodal attribute default value is 0.0.
          * - :py:attr:`~da3`
            - Get or set the Third nodal attribute default value is 0.0.
          * - :py:attr:`~da4`
            - Get or set the Fourth nodal attribute default value is 0.0.
          * - :py:attr:`~solver`
            - Get or set the EQ.MECH: mechanics.
          * - :py:attr:`~its`
            - Get or set the Specify coupling type across different scales in two-scale co-simulation. This flag should only be included for node sets that provide coupling information in the input file referred to by *INCLUDE_COSIM;
          * - :py:attr:`~option`
            - Get or set the OPTION.EQ.ALL: All nodes will be included in the set,
          * - :py:attr:`~e1`
            - Get or set the OPTION.EQ.ALL: E1 not used,
          * - :py:attr:`~e2`
            - Get or set the OPTION.EQ.ALL: E2 not used,
          * - :py:attr:`~e3`
            - Get or set the OPTION.EQ.ALL: E3 not used,
          * - :py:attr:`~e4`
            - Get or set the OPTION.EQ.ALL: E4 not used,
          * - :py:attr:`~e5`
            - Get or set the OPTION.EQ.ALL: E5 not used,
          * - :py:attr:`~e6`
            - Get or set the OPTION.EQ.ALL: E6 not used,
          * - :py:attr:`~e7`
            - Get or set the OPTION.EQ.ALL: E7 not used,
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

    from set_node_general import SetNodeGeneral

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Node set ID. All node sets should have a unique set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: da1
   :type: float


   
   Get or set the First nodal attribute default value is 0.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: da2
   :type: float


   
   Get or set the Second nodal attribute default value is 0.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: da3
   :type: float


   
   Get or set the Third nodal attribute default value is 0.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: da4
   :type: float


   
   Get or set the Fourth nodal attribute default value is 0.0.
















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
   :type: str


   
   Get or set the Specify coupling type across different scales in two-scale co-simulation. This flag should only be included for node sets that provide coupling information in the input file referred to by *INCLUDE_COSIM;
   EQ.1:   Tied contact coupling
   EQ.2 : Solid - in - shell immersed coupling
















   ..
       !! processed by numpydoc !!

.. py:property:: option
   :type: str


   
   Get or set the OPTION.EQ.ALL: All nodes will be included in the set,
   OPTION.EQ.NODE: Nodes E1...E7 will be included in the current set,
   OPTION.EQ.DNODE: Nodes E1...E7 previously added will be excluded from the current set,
   OPTION.EQ.PART: Nodes from parts E1...E7 will be included in the current set,
   OPTION.EQ.DPART: Nores from parts E1...E7 previously added will be excluded from the current set,
   OPTION.EQ.BOX: Nodes inside boxes E1...E7 will be included in the current set,
   OPTION.EQ.DBOX: Nodes inside boxes E1...E7 previously added will be excluded from the current set.
   OPTION.EQ.SALECPT:      Nodes inside a box in Structured ALE mesh. E1 here is the S-ALE mesh
   ID (MSHID). E2, E3, E4, E5, E6, E7 correspond to XMIN, XMAX,
   YMIN, YMAX, ZMIN, ZMAX. They are the minimum and the
   maximum nodal indices along each direction in S-ALE mesh. This
   option is only to be used for Structured ALE mesh and should not be
   used in a mixed manner with other  _GENERAL  options.
   Please refer to *ALE_STRUCTURED_MESH_CONTROL_POINTS
   and *ALE_STRUCTURED_MESH_CONTROL for more details.
   OPTION.EQ.SALEFAC: Nodes on the face of Structured ALE mesh. E1 here is the S-ALE mesh
   ID (MSHID). E2, E3, E4, E5, E6, E7 correspond to -X, +X, -Y, +Y, -Z,
   +Z faces. Assigning 1 to these 6 values would include all the surface
   segments at these faces in the segment set. This option is only to be
   used for Structured ALE mesh and should not be used in a mixed
   manner with other  _GENERAL  options.
   Please refer to *ALE_STRUCTURED_MESH_CONTROL_POINTS
   and *ALE_STRUCTURED_MESH_CONTROL for more details.
















   ..
       !! processed by numpydoc !!

.. py:property:: e1
   :type: Optional[int]


   
   Get or set the OPTION.EQ.ALL: E1 not used,
   OPTION.EQ.ELEM: Node E1 will be included in the current set,
   OPTION.EQ.DELEM: Node E1 will be excluded from the current set,
   OPTION.EQ.PART: Nodes from part E1 will be included in the current set,
   OPTION.EQ.DPART: Nodes from part E1 will be excluded from the current set,
   OPTION.EQ.BOX: Nodes inside box E1 will be included in the current set,
   OPTION.EQ.DBOX: Nodes inside box E1 will be excluded from the current set.
















   ..
       !! processed by numpydoc !!

.. py:property:: e2
   :type: Optional[int]


   
   Get or set the OPTION.EQ.ALL: E2 not used,
   OPTION.EQ.ELEM: Node E2 will be included in the current set,
   OPTION.EQ.DELEM: Node E2 will be excluded from the current set,
   OPTION.EQ.PART: Nodes from part E2 will be included in the current set,
   OPTION.EQ.DPART: Nodes from part E2 will be excluded from the current set,
   OPTION.EQ.BOX: Nodes inside box E2 will be included in the current set,
   OPTION.EQ.DBOX: Nodes inside box E2 will be excluded from the current set.
















   ..
       !! processed by numpydoc !!

.. py:property:: e3
   :type: Optional[int]


   
   Get or set the OPTION.EQ.ALL: E3 not used,
   OPTION.EQ.ELEM: Node E3 will be included in the current set,
   OPTION.EQ.DELEM: Node E3 will be excluded from the current set,
   OPTION.EQ.PART: Nodes from part E3 will be included in the current set,
   OPTION.EQ.DPART: Nodes from part E3 will be excluded from the current set,
   OPTION.EQ.BOX: Nodes inside box E3 will be included in the current set,
   OPTION.EQ.DBOX: Nodes inside box E3 will be excluded from the current set.
















   ..
       !! processed by numpydoc !!

.. py:property:: e4
   :type: Optional[int]


   
   Get or set the OPTION.EQ.ALL: E4 not used,
   OPTION.EQ.ELEM: Node E4 will be included in the current set,
   OPTION.EQ.DELEM: Node E4 will be excluded from the current set,
   OPTION.EQ.PART: Nodes from part E4 will be included in the current set,
   OPTION.EQ.DPART: Nodes from part E4 will be excluded from the current set,
   OPTION.EQ.BOX: Nodes inside box E4 will be included in the current set,
   OPTION.EQ.DBOX: Nodes inside box E4 will be excluded from the current set.
















   ..
       !! processed by numpydoc !!

.. py:property:: e5
   :type: Optional[int]


   
   Get or set the OPTION.EQ.ALL: E5 not used,
   OPTION.EQ.ELEM: Node E5 will be included in the current set,
   OPTION.EQ.DELEM: Node E5 will be excluded from the current set,
   OPTION.EQ.PART: Nodes from part E5 will be included in the current set,
   OPTION.EQ.DPART: Nodes from part E5 will be excluded from the current set,
   OPTION.EQ.BOX: Nodes inside box E5 will be included in the current set,
   OPTION.EQ.DBOX: Nodes inside box E5 will be excluded from the current set.
















   ..
       !! processed by numpydoc !!

.. py:property:: e6
   :type: Optional[int]


   
   Get or set the OPTION.EQ.ALL: E6 not used,
   OPTION.EQ.ELEM: Node E6 will be included in the current set,
   OPTION.EQ.DELEM: Node E6 will be excluded from the current set,
   OPTION.EQ.PART: Nodes from part E6 will be included in the current set,
   OPTION.EQ.DPART: Nodes from part E6 will be excluded from the current set,
   OPTION.EQ.BOX: Nodes inside box E6 will be included in the current set,
   OPTION.EQ.DBOX: Nodes inside box E6 will be excluded from the current set.
















   ..
       !! processed by numpydoc !!

.. py:property:: e7
   :type: Optional[int]


   
   Get or set the OPTION.EQ.ALL: E7 not used,
   OPTION.EQ.ELEM: Node E7 will be included in the current set,
   OPTION.EQ.DELEM: Node E7 will be excluded from the current set,
   OPTION.EQ.PART: Nodes from part E7 will be included in the current set,
   OPTION.EQ.DPART: Nodes from part E7 will be excluded from the current set,
   OPTION.EQ.BOX: Nodes inside box E7 will be included in the current set,
   OPTION.EQ.DBOX: Nodes inside box E7 will be excluded from the current set.
















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
   :value: 'NODE_GENERAL'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





