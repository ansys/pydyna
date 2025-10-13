





:class:`PartRepositionPrintAttachmentNodes`
===========================================


.. py:class:: part_reposition_print_attachment_nodes.PartRepositionPrintAttachmentNodes(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA PART_REPOSITION_PRINT_ATTACHMENT_NODES keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: PartRepositionPrintAttachmentNodes

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~title`
            - Get or set the Heading for the part.
          * - :py:attr:`~pid`
            - Get or set the Part ID.
          * - :py:attr:`~secid`
            - Get or set the Section ID defined in *SECTION section.
          * - :py:attr:`~mid`
            - Get or set the Material ID defined in *MAT section.
          * - :py:attr:`~eosid`
            - Get or set the Equation of state ID defined in the *EOS section. Nonzero only for solid elements using an equation of state to compute pressure.
          * - :py:attr:`~hgid`
            - Get or set the Hourglass/bulk viscosity ID defined in *HOURGLASS section.
          * - :py:attr:`~grav`
            - Get or set the Part initialization for gravity loading. This option initializes hydrostatic pressure in the part due to gravity acting on an overburden material. This option applies to brick elements only and must be used with the *LOAD_DENSITY_DEPTH option:
          * - :py:attr:`~adpopt`
            - Get or set the Indicate if this part is adapted or not. See also *CONTROL_ADAPTIVITY.
          * - :py:attr:`~tmid`
            - Get or set the Thermal material property identication defined in the *MAT_THERMAL section. Thermal properties must be specified for all solid, shell, and thick shell parts if a thermal or coupled thermal structual/analysis is being performed. Beams and discrete elements are not considered in thermal analyses.
          * - :py:attr:`~cmsn`
            - Get or set the CAL3D segment number/MADYMO system number. See the numbering in the corresponding program.
          * - :py:attr:`~mdep`
            - Get or set the MADYMO ellipse/plane number:
          * - :py:attr:`~movopt`
            - Get or set the Flag to deactivate moving for merged rigid bodies, see *CONSTRAINED_RIGID_BODIES. This option allows a merged rigid body to be fixed in space while the nodes and elements of the generated CAL3D/MADYMO parts are repositioned:
          * - :py:attr:`~prbf`
            - Get or set the Print flag for RBDOUT and MATSUM files
          * - :py:attr:`~ansid`
            - Get or set the Attachment node set ID. This option should be used very cautiously and applies only to rigid bodies. The attachment point nodes are updated each cycle whereas other nodes in the rigid body are updated only in the output databases. All loads seen by the rigid body must be applied through this nodal subset or directly to the center of gravity of the rigid body. If the rigid body is in contact this set must include all interacting nodes.


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

    from part_reposition_print_attachment_nodes import PartRepositionPrintAttachmentNodes

Property detail
---------------

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Heading for the part.
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: secid
   :type: Optional[int]


   
   Get or set the Section ID defined in *SECTION section.
















   ..
       !! processed by numpydoc !!

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material ID defined in *MAT section.
















   ..
       !! processed by numpydoc !!

.. py:property:: eosid
   :type: int


   
   Get or set the Equation of state ID defined in the *EOS section. Nonzero only for solid elements using an equation of state to compute pressure.
















   ..
       !! processed by numpydoc !!

.. py:property:: hgid
   :type: int


   
   Get or set the Hourglass/bulk viscosity ID defined in *HOURGLASS section.
   EQ.0: default values are used.
















   ..
       !! processed by numpydoc !!

.. py:property:: grav
   :type: int


   
   Get or set the Part initialization for gravity loading. This option initializes hydrostatic pressure in the part due to gravity acting on an overburden material. This option applies to brick elements only and must be used with the *LOAD_DENSITY_DEPTH option:
   EQ.0: all parts initialized,
   EQ.1: only current material initialized.
















   ..
       !! processed by numpydoc !!

.. py:property:: adpopt
   :type: Optional[int]


   
   Get or set the Indicate if this part is adapted or not. See also *CONTROL_ADAPTIVITY.
   LT.0: R-adaptive remeshing for 2-D solids, |ADPOPT| gives the load curve ID that defines the element size as a function of time.
   EQ.0:Adaptive remeshing is inactive for this part ID.
   EQ.1:   h - adaptive for 3D shells and for shell / solid / shell sandwich composites.
   EQ.2 : r - adaptive remeshing for 2D solids, 3D tetrahedrons and 3D EFG.
   EQ.3 : Axisymmetric r - adaptive remeshing for 3D solid(see Remark 6).
   EQ.9 : Passive h - adaptive for 3D shells.The elements in this part will not be split unless their neighboring elements in other parts need to be split more than one level.
















   ..
       !! processed by numpydoc !!

.. py:property:: tmid
   :type: int


   
   Get or set the Thermal material property identication defined in the *MAT_THERMAL section. Thermal properties must be specified for all solid, shell, and thick shell parts if a thermal or coupled thermal structual/analysis is being performed. Beams and discrete elements are not considered in thermal analyses.
   EQ.0: defaults to MID.
















   ..
       !! processed by numpydoc !!

.. py:property:: cmsn
   :type: Optional[int]


   
   Get or set the CAL3D segment number/MADYMO system number. See the numbering in the corresponding program.
















   ..
       !! processed by numpydoc !!

.. py:property:: mdep
   :type: int


   
   Get or set the MADYMO ellipse/plane number:
   GT.0: ellipse number,
   EQ.0: default,
   LT.0: absolute value is plane number.
















   ..
       !! processed by numpydoc !!

.. py:property:: movopt
   :type: int


   
   Get or set the Flag to deactivate moving for merged rigid bodies, see *CONSTRAINED_RIGID_BODIES. This option allows a merged rigid body to be fixed in space while the nodes and elements of the generated CAL3D/MADYMO parts are repositioned:
   EQ.0: merged rigid body is repositioned (default),
   EQ.1: merged rigid body is not repositioned.
















   ..
       !! processed by numpydoc !!

.. py:property:: prbf
   :type: int


   
   Get or set the Print flag for RBDOUT and MATSUM files
   EQ.0: default is taken from the keyword *CONTROL_OUTPUT
   EQ.1: write data into RDBOUT file only
   EQ.2: write data into MATSUM file only
   EQ.3: do not write data into RBDOUT AND MATSUM files
















   ..
       !! processed by numpydoc !!

.. py:property:: ansid
   :type: int


   
   Get or set the Attachment node set ID. This option should be used very cautiously and applies only to rigid bodies. The attachment point nodes are updated each cycle whereas other nodes in the rigid body are updated only in the output databases. All loads seen by the rigid body must be applied through this nodal subset or directly to the center of gravity of the rigid body. If the rigid body is in contact this set must include all interacting nodes.
   EQ.0: All nodal updates are skipped for this rigid body. The null option can be used if the rigid body is fixed in space or if the rigid body does not interact with other parts, e.g., the rigid body is only used for some visual purpose (default).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'PART'


.. py:attribute:: subkeyword
   :value: 'REPOSITION_PRINT_ATTACHMENT_NODES'






