





:class:`InterfaceSpringbackLsdynaThickness`
===========================================


.. py:class:: interface_springback_lsdyna_thickness.InterfaceSpringbackLsdynaThickness(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INTERFACE_SPRINGBACK_LSDYNA_THICKNESS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InterfaceSpringbackLsdynaThickness

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~psid`
            - Get or set the Part set ID for springback, see * SET_PART.
          * - :py:attr:`~nshv`
            - Get or set the Number of additional shell history variables to be initialized. The shell stresses and plastic strains are written to the interface file. If NSHV is nonzero, the shell formulations and constitutive models should not change between runs.
          * - :py:attr:`~ftype`
            - Get or set the EQ:0 ascii output.
          * - :py:attr:`~ftensr`
            - Get or set the Flag for dumping tensor data from the element history variables into the dynain file.
          * - :py:attr:`~nthhsv`
            - Get or set the Number of thermal history variables
          * - :py:attr:`~rflag`
            - Get or set the Flag to carry over reference quantities, for hyperelastic materials and such.
          * - :py:attr:`~intstrn`
            - Get or set the Output of strains at all integration points of shell element is requested, see also *INITIAL_STRAIN_SHELL
          * - :py:attr:`~optc`
            - Get or set the &
          * - :py:attr:`~sldo`
            - Get or set the Output of solid element data as
          * - :py:attr:`~ncyc`
            - Get or set the Number of process cycles this simulation corresponds to in the simulation of wear processes
          * - :py:attr:`~fsplit`
            - Get or set the Flag for splitting of the dynain file (only for ASCII format).
          * - :py:attr:`~ndflag`
            - Get or set the Flag to dump nodes into dynain file.
          * - :py:attr:`~cflag`
            - Get or set the Output contact state.
          * - :py:attr:`~hflag`
            - Get or set the Output hourglass state, only valid for FTYPE=3:
          * - :py:attr:`~nid`
            - Get or set the Node ID, see *NODE.
          * - :py:attr:`~tc`
            - Get or set the Translational constraint:
          * - :py:attr:`~rc`
            - Get or set the Rotational constraint:


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

    from interface_springback_lsdyna_thickness import InterfaceSpringbackLsdynaThickness

Property detail
---------------

.. py:property:: psid
   :type: Optional[int]


   
   Get or set the Part set ID for springback, see * SET_PART.
















   ..
       !! processed by numpydoc !!

.. py:property:: nshv
   :type: Optional[int]


   
   Get or set the Number of additional shell history variables to be initialized. The shell stresses and plastic strains are written to the interface file. If NSHV is nonzero, the shell formulations and constitutive models should not change between runs.
















   ..
       !! processed by numpydoc !!

.. py:property:: ftype
   :type: int


   
   Get or set the EQ:0 ascii output.
   EQ:1 binary output.
   EQ:2 ascii and binary output.
   EQ.3: LSDA format
   EQ.10: ASCII large format (see *INITIAL_STRESS_SHELL)
   EQ.11: binary large format
   EQ.12: both ASCII and binary large format
















   ..
       !! processed by numpydoc !!

.. py:property:: ftensr
   :type: int


   
   Get or set the Flag for dumping tensor data from the element history variables into the dynain file.
   EQ.0: Dont dump tensor data from element history variables
   EQ.1: Dump any tensor data from element history variables into
   the dynain file in GLOBAL coordinate system. Currently, only Material 190 supports this option
















   ..
       !! processed by numpydoc !!

.. py:property:: nthhsv
   :type: Optional[int]


   
   Get or set the Number of thermal history variables
















   ..
       !! processed by numpydoc !!

.. py:property:: rflag
   :type: Optional[int]


   
   Get or set the Flag to carry over reference quantities, for hyperelastic materials and such.
   EQ.0:   default, do not output.
   EQ.1:   output reference coordinates and nodal masses.
















   ..
       !! processed by numpydoc !!

.. py:property:: intstrn
   :type: Optional[int]


   
   Get or set the Output of strains at all integration points of shell element is requested, see also *INITIAL_STRAIN_SHELL
















   ..
       !! processed by numpydoc !!

.. py:property:: optc
   :type: str


   
   Get or set the &
















   ..
       !! processed by numpydoc !!

.. py:property:: sldo
   :type: int


   
   Get or set the Output of solid element data as
   EQ.0:   *ELEMENT_SOLID, or
   EQ.1:   *ELEMENT_SOLID_ORTHO(only for anisotropic material).
















   ..
       !! processed by numpydoc !!

.. py:property:: ncyc
   :type: Optional[int]


   
   Get or set the Number of process cycles this simulation corresponds to in the simulation of wear processes
















   ..
       !! processed by numpydoc !!

.. py:property:: fsplit
   :type: int


   
   Get or set the Flag for splitting of the dynain file (only for ASCII format).
   EQ.0:   dynain file written in one piece.
   EQ.1:   Output is divided into two files, dynain_geo including the geometry data and dynain_ini including initial stresses and strains.
















   ..
       !! processed by numpydoc !!

.. py:property:: ndflag
   :type: int


   
   Get or set the Flag to dump nodes into dynain file.
   EQ.0: default, dump only sph and element nodes
   EQ.1: dump all nodes.
















   ..
       !! processed by numpydoc !!

.. py:property:: cflag
   :type: int


   
   Get or set the Output contact state.
   EQ.0: default, do not output
   EQ.1: output contact state, currently only Mortar segment pair information and selected tied contacts with restrictions.
















   ..
       !! processed by numpydoc !!

.. py:property:: hflag
   :type: Optional[int]


   
   Get or set the Output hourglass state, only valid for FTYPE=3:
   EQ.0:   default, do not output.
   EQ.1:   output hourglass stresses for carrying over to next simulation.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Node ID, see *NODE.
















   ..
       !! processed by numpydoc !!

.. py:property:: tc
   :type: int


   
   Get or set the Translational constraint:
   EQ.0: no constraints,
   EQ.1: constrained x displacement,
   EQ.2: constrained y displacement,
   EQ.3: constrained z displacement,
   EQ.4: constrained x and y displacements,
   EQ.5: constrained y and z displacements,
   EQ.6: constrained z and x displacements,
   EQ.7: constrained x, y, and z displacements.
















   ..
       !! processed by numpydoc !!

.. py:property:: rc
   :type: int


   
   Get or set the Rotational constraint:
   EQ.0: no constraints,
   EQ.1: constrained x rotation,
   EQ.2: constrained y rotation,
   EQ.3: constrained z rotation,
   EQ.4: constrained x and y rotations,
   EQ.5: constrained y and z rotations,
   EQ.6: constrained z and x rotations,
   EQ.7: constrained x, y, and z rotations.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INTERFACE'


.. py:attribute:: subkeyword
   :value: 'SPRINGBACK_LSDYNA_THICKNESS'






