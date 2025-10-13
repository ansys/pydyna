





:class:`RveAnalysisFem`
=======================


.. py:class:: rve_analysis_fem.RveAnalysisFem(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA RVE_ANALYSIS_FEM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: RveAnalysisFem

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~filename`
            - Get or set the Name of an input file that contains the mesh information (nodal coordinates, element connectivity) of the RVE model.
          * - :py:attr:`~inpt`
            - Get or set the Type of input:
          * - :py:attr:`~oupt`
            - Get or set the =1: RVE homogenization results will be output to a database file "rveout". Please refer to the keyword *DATABASE_RVE
          * - :py:attr:`~lcid`
            - Get or set the ID of the loading curve. To perform RVE analysis, a loading curve defined by the keyword *DEFINE_CURVE
          * - :py:attr:`~idof`
            - Get or set the Dimension of the RVE.
          * - :py:attr:`~bc`
            - Get or set the Type of the RVE boundary condition:
          * - :py:attr:`~imatch`
            - Get or set the Type of the given RVE mesh:
          * - :py:attr:`~h11`
            - Get or set the Component 11 of the prescribed macroscopic displacement gradient.
          * - :py:attr:`~h22`
            - Get or set the Component 22 of the prescribed macroscopic displacement gradient.
          * - :py:attr:`~h33`
            - Get or set the Component 33 of the prescribed macroscopic displacement gradient.
          * - :py:attr:`~h12`
            - Get or set the Component 12 of the prescribed macroscopic displacement gradient.
          * - :py:attr:`~h23`
            - Get or set the Component 23 of the prescribed macroscopic displacement gradient.
          * - :py:attr:`~h13`
            - Get or set the Component 13 of the prescribed macroscopic displacement gradient.


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

    from rve_analysis_fem import RveAnalysisFem

Property detail
---------------

.. py:property:: filename
   :type: Optional[str]


   
   Get or set the Name of an input file that contains the mesh information (nodal coordinates, element connectivity) of the RVE model.
   Note that this keyword variable should be of the format "XXX.k", where file name extension ".k" is included.
   The finite element mesh given in this file is used for the spatial discretization of the material microstructures,
   and it does not involve any special 'control nodes' or 'control elements'
















   ..
       !! processed by numpydoc !!

.. py:property:: inpt
   :type: int


   
   Get or set the Type of input:
   EQ.0: RVE boundary conditions are fully defined by two factors: (1) the parameter "BC" of this input card,
   and (2) the mesh information in the file [MESHFILE]. When running an RVE simulation, LS-DYNA automatically
   creates a file named "rve_[MESHFILE].k",
   which contains all the necessary information (e.g., control nodes, displacement constraints, etc.) for boundary condition enforcement.
   EQ.1: Users provide a file named  rve_[MESHFILE].k  to define the boundary condition keywords
   (e.g. *CONSTRAINED_MULTIPLE_GLOBAL, *BOUNDARY_SPC_NODE, *BOUNDARY_MOTION_NODE, etc.) and control nodes for
   enforcing RVE boundary conditions. Note that, it is usually non-trivial to manually define all the keywords for RVE boundary conditions.
   If the file "rve_[MESHFILE].k" is not given when running RVE simulations, then the option INPT=1 will be ignored, and
   LS-DYNA will create "rve_[MESHFILE].k" based on the parameter "BC" of this input card and the mesh information in the file [MESHFILE].
















   ..
       !! processed by numpydoc !!

.. py:property:: oupt
   :type: int


   
   Get or set the =1: RVE homogenization results will be output to a database file "rveout". Please refer to the keyword *DATABASE_RVE
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the ID of the loading curve. To perform RVE analysis, a loading curve defined by the keyword *DEFINE_CURVE
   is required to specify the loading history. There are two columns in the loading curve, where the first column
   is adopted as a scaling factor for the user-defined macroscopic deformation measure (H11, H22,   H13, which are defined in CARD3 of this *RVE_ANALYSIS_FEM keyword),
   and the second column provides the corresponding scaling factor for the loading time (1.0 in the second column denotes the end of the loading).
















   ..
       !! processed by numpydoc !!

.. py:property:: idof
   :type: Optional[int]


   
   Get or set the Dimension of the RVE.
   EQ.2: 2D geometry.
   EQ.3: 3D geometry.
















   ..
       !! processed by numpydoc !!

.. py:property:: bc
   :type: int


   
   Get or set the Type of the RVE boundary condition:
   EQ. 0: Periodic Displacement Boundary Condition (PDBC).
   EQ. 1: Linear Displacement Boundary Condition (LDBC).
















   ..
       !! processed by numpydoc !!

.. py:property:: imatch
   :type: int


   
   Get or set the Type of the given RVE mesh:
   EQ. 0: The mesh is non-matching for PDBC.
   EQ. 1: The mesh is PDBC-matching. This variable is effective only when the user chooses to impose PDBC by setting BC=0.
   When the mesh is PDBC-matching, the nodal distributions on the RVEs opposite sides match well with each other.
   For instance, let us consider two opposite surfaces (surface A, and surface B) that are both perpendicular to the X-axis,
   for any FEM node on surface A, if we draw a straight line that is parallel to the X-axis, then the intersection
   point of this line with surface B must also be an FEM node. For such PDBC-matching meshes, an efficient
   direct nearest neighbor search algorithm can be used for the PDBC imposition, so a PDBC-matching mesh is preferred if
   users would like to impose the periodic displacement boundary condition for RVE analysis. However, it is not always straightforward
   to create PDBC-matching meshes for RVE models if very complex material micro-structures exist. In this case,
   IMATCH=0 can be chosen to impose PDBC on a non-matching mesh by employing a projection-based constraint imposition method.
















   ..
       !! processed by numpydoc !!

.. py:property:: h11
   :type: Optional[float]


   
   Get or set the Component 11 of the prescribed macroscopic displacement gradient.
















   ..
       !! processed by numpydoc !!

.. py:property:: h22
   :type: Optional[float]


   
   Get or set the Component 22 of the prescribed macroscopic displacement gradient.
















   ..
       !! processed by numpydoc !!

.. py:property:: h33
   :type: Optional[float]


   
   Get or set the Component 33 of the prescribed macroscopic displacement gradient.
















   ..
       !! processed by numpydoc !!

.. py:property:: h12
   :type: Optional[float]


   
   Get or set the Component 12 of the prescribed macroscopic displacement gradient.
















   ..
       !! processed by numpydoc !!

.. py:property:: h23
   :type: Optional[float]


   
   Get or set the Component 23 of the prescribed macroscopic displacement gradient.
















   ..
       !! processed by numpydoc !!

.. py:property:: h13
   :type: Optional[float]


   
   Get or set the Component 13 of the prescribed macroscopic displacement gradient.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'RVE'


.. py:attribute:: subkeyword
   :value: 'ANALYSIS_FEM'






