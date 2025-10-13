





:class:`DatabaseExtentBinary`
=============================


.. py:class:: database_extent_binary.DatabaseExtentBinary(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DATABASE_EXTENT_BINARY keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DatabaseExtentBinary

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~neiph`
            - Get or set the Number of additional integration point history variables written to the binary database for solid elements. The integration point data is written in the same order that it is stored in memory-each material modal has its own history variables that are stored. For user defined materials it is important to store the history data that is needed for plotting before the data which is not of interest.
          * - :py:attr:`~neips`
            - Get or set the Number of additional integration point history variables written to the binary database for both shell and thick shell elements for each integration point, see NEIPH above.
          * - :py:attr:`~maxint`
            - Get or set the Number of shell integration points written to the LS-DYNA database, see also *INTEGRATION_SHELL. If the default value of 3 is used then results are output for the outermost (top) and innermost (bottom) integration points together with results for the neutral axis. If MAXINT is set to 3 and the element has 1 integration point then all three results will be the same. If a value other than 3 is used then the results for the first MAXINT integration points in the element will be output. NOTE: If the element has an even number of integration points and MAXINT is not set to 3 then you will not get mid-surface results. (See Remarks in user's manual).If MAXINT is set to a negative
          * - :py:attr:`~strflg`
            - Get or set the Flag for output of strain tensors.  STRFLG is interpreted digit-wise STRFLG = [NML], STRFLG = 100*N + 10*M + L
          * - :py:attr:`~sigflg`
            - Get or set the Flag for including stress tensor in the shell LS-DYNA database:
          * - :py:attr:`~epsflg`
            - Get or set the Flag for including the effective plastic strains in the shell LS-DYNA database:
          * - :py:attr:`~rltflg`
            - Get or set the Flag for including stress resultants in the shell LS-DYNA database:
          * - :py:attr:`~engflg`
            - Get or set the Flag for including internal energy and thickness in the LS-DYNA database:
          * - :py:attr:`~cmpflg`
            - Get or set the Orthotropic and anisotropic material stress output in local coordinate system for shells and thick shells.
          * - :py:attr:`~ieverp`
            - Get or set the Every plot state for D3PLOT database is written to a separate file. This option will limit the database to 100 states:
          * - :py:attr:`~beamip`
            - Get or set the Number of beam integration points for output. This option does not apply to beams that use a resultant formulation.
          * - :py:attr:`~dcomp`
            - Get or set the Data compression to eliminate rigid body data:
          * - :py:attr:`~shge`
            - Get or set the Output shell hourglass energy:
          * - :py:attr:`~stssz`
            - Get or set the Output shell element time step, mass or added mass:
          * - :py:attr:`~n3thdt`
            - Get or set the Material energy write option for D3THDT database
          * - :py:attr:`~ialemat`
            - Get or set the Output solid part id list containing ale materials
          * - :py:attr:`~nintsld`
            - Get or set the Number of solid element integration points written to the LS-DYNA database. The default value is 1. For solids with multiple integration points NINTSLD may be set to 8. Currently, no other values for INITSLD are allowed. For solids with multiple integration points, an average value is output if NINTSLD is set to 1.
          * - :py:attr:`~pkp_sen`
            - Get or set the Flag to output the peak pressure and surface energy computed by each contact interface into the interface force database.   To obtain the surface energy, FRCENG, must be sent to 1 on the control contact card.  When PKP_SEN=1, it is possible to identify the energies generated on the upper and lower shell surfaces, which is important in metal forming applications.  This data is mapped after each H-adaptive remeshing.
          * - :py:attr:`~sclp`
            - Get or set the A scaling parameter used in the computation of the peak pressure.  This parameter is generally set to unity (the default), but it must be greater than 0..
          * - :py:attr:`~hydro`
            - Get or set the Either 3 or 5 additional history variables useful to shock physics are
          * - :py:attr:`~msscl`
            - Get or set the Output nodal information related to mass scaling into the D3PLOT database.  This option can be activated if and only if DT2MS < 0.0, see control card *CONTROL_TIMESTEP.  This option is available starting with the second release of Version 971.
          * - :py:attr:`~therm`
            - Get or set the Output of thermal data to d3plot. The use of this option (THERM>0) may make the database incompatible with other 3rd party software.
          * - :py:attr:`~intout`
            - Get or set the Output stress/strain at all integration points for detailed element output in the file ELOUTDET.  DT and BINARY of *DATABASE_ELOUT apply to ELOUTDET.  See remarks 4-10 below.
          * - :py:attr:`~nodout`
            - Get or set the Output extrapolated stress/strain at connectivity nodes points for detailed element output in the file ELOUTDET.  DT and BINARY of *DATABASE_ELOUT apply to ELOUTDET.
          * - :py:attr:`~dtdt`
            - Get or set the Output of node point dtemperature/dtime data to d3plot
          * - :py:attr:`~resplt`
            - Get or set the Output of translational and rotational residual forces to d3plot and d3iter
          * - :py:attr:`~neipb`
            - Get or set the Number of additional integration point history variables written to the binary database for beam elements.
          * - :py:attr:`~quadsld`
            - Get or set the Output option for quadratic higher order solid elements EQ.1: output full connectivity,
          * - :py:attr:`~cubsld`
            - Get or set the Output option for cubic higher order solid elements EQ.1: output full connectivity,
          * - :py:attr:`~deleres`
            - Get or set the Output flag for results of deleted elements:


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

    from database_extent_binary import DatabaseExtentBinary

Property detail
---------------

.. py:property:: neiph
   :type: int


   
   Get or set the Number of additional integration point history variables written to the binary database for solid elements. The integration point data is written in the same order that it is stored in memory-each material modal has its own history variables that are stored. For user defined materials it is important to store the history data that is needed for plotting before the data which is not of interest.
















   ..
       !! processed by numpydoc !!

.. py:property:: neips
   :type: int


   
   Get or set the Number of additional integration point history variables written to the binary database for both shell and thick shell elements for each integration point, see NEIPH above.
















   ..
       !! processed by numpydoc !!

.. py:property:: maxint
   :type: int


   
   Get or set the Number of shell integration points written to the LS-DYNA database, see also *INTEGRATION_SHELL. If the default value of 3 is used then results are output for the outermost (top) and innermost (bottom) integration points together with results for the neutral axis. If MAXINT is set to 3 and the element has 1 integration point then all three results will be the same. If a value other than 3 is used then the results for the first MAXINT integration points in the element will be output. NOTE: If the element has an even number of integration points and MAXINT is not set to 3 then you will not get mid-surface results. (See Remarks in user's manual).If MAXINT is set to a negative
   number, MAXINT integration points are output for each in plane
   integration point location and no averaging is used. This can greatly
   increase the size of the binary databases d3plot, d3thdt, and d3part
















   ..
       !! processed by numpydoc !!

.. py:property:: strflg
   :type: int


   
   Get or set the Flag for output of strain tensors.  STRFLG is interpreted digit-wise STRFLG = [NML], STRFLG = 100*N + 10*M + L
   L.EQ.1: Write strain tensor data to d3plot, elout, and dynain.  For shell and thick shell elements two tensors are written, one at the innermost and one at the outermost integration point.  For solid elements a single strain tensor is written
   M.EQ.1: Write plastic strain data to d3plot.
   N.EQ.1: Write thermal strain data to d3plot.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigflg
   :type: int


   
   Get or set the Flag for including stress tensor in the shell LS-DYNA database:
   EQ.1: include (default),
   EQ.2: exclude.
















   ..
       !! processed by numpydoc !!

.. py:property:: epsflg
   :type: int


   
   Get or set the Flag for including the effective plastic strains in the shell LS-DYNA database:
   EQ.1: include (default),
   EQ.2: exclude.
















   ..
       !! processed by numpydoc !!

.. py:property:: rltflg
   :type: int


   
   Get or set the Flag for including stress resultants in the shell LS-DYNA database:
   EQ.1: include (default),
   EQ.2: exclude.
















   ..
       !! processed by numpydoc !!

.. py:property:: engflg
   :type: int


   
   Get or set the Flag for including internal energy and thickness in the LS-DYNA database:
   EQ.1: include (default),
   EQ.2: exclude.
















   ..
       !! processed by numpydoc !!

.. py:property:: cmpflg
   :type: int


   
   Get or set the Orthotropic and anisotropic material stress output in local coordinate system for shells and thick shells.
   EQ.0: global,
   EQ.1: local.
















   ..
       !! processed by numpydoc !!

.. py:property:: ieverp
   :type: int


   
   Get or set the Every plot state for D3PLOT database is written to a separate file. This option will limit the database to 100 states:
   EQ.0: more than one state can be on each plotfile,
   EQ.1: one state only on each plotfile.
















   ..
       !! processed by numpydoc !!

.. py:property:: beamip
   :type: int


   
   Get or set the Number of beam integration points for output. This option does not apply to beams that use a resultant formulation.
















   ..
       !! processed by numpydoc !!

.. py:property:: dcomp
   :type: int


   
   Get or set the Data compression to eliminate rigid body data:
   EQ.1: off (default), no data compression,
   EQ.2: on.
   EQ.3: off, no rigid body data compression, but nodal velocities and accelerations are eliminated from the database.
   EQ.4: on, rigid body data compression active and nodal velocities and accelerations are eliminaated from the database.
   EQ.5: on, rigid body data compression active and rigid nodal data are eliminated from the database. Only 6 dof rigid body motion is written.
   EQ.6: on, rigid body data compression active, rigid nodal data, and nodal velocities and accelerations are eliminated from the database.  Only 6 dof rigid body motion is written.
















   ..
       !! processed by numpydoc !!

.. py:property:: shge
   :type: int


   
   Get or set the Output shell hourglass energy:
   EQ.1: off (default), no hourglass energy written,
   EQ.2: on.
















   ..
       !! processed by numpydoc !!

.. py:property:: stssz
   :type: int


   
   Get or set the Output shell element time step, mass or added mass:
   EQ.1: off (default),
   EQ.2: out time step size,
   EQ.3: output mass, added mass, or time step size.
   (See Remark 3 in user's manual).
















   ..
       !! processed by numpydoc !!

.. py:property:: n3thdt
   :type: int


   
   Get or set the Material energy write option for D3THDT database
   EQ.1: off, energy is NOT written to D3THDT database,
   EQ.2: on (default), energy is written to D3THDT database.
















   ..
       !! processed by numpydoc !!

.. py:property:: ialemat
   :type: int


   
   Get or set the Output solid part id list containing ale materials
   EQ.1: on (default)
   EQ.0: off
















   ..
       !! processed by numpydoc !!

.. py:property:: nintsld
   :type: int


   
   Get or set the Number of solid element integration points written to the LS-DYNA database. The default value is 1. For solids with multiple integration points NINTSLD may be set to 8. Currently, no other values for INITSLD are allowed. For solids with multiple integration points, an average value is output if NINTSLD is set to 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: pkp_sen
   :type: int


   
   Get or set the Flag to output the peak pressure and surface energy computed by each contact interface into the interface force database.   To obtain the surface energy, FRCENG, must be sent to 1 on the control contact card.  When PKP_SEN=1, it is possible to identify the energies generated on the upper and lower shell surfaces, which is important in metal forming applications.  This data is mapped after each H-adaptive remeshing.
   EQ.0: No data is written
   EQ.1: Output the peak pressures and surface energy by contact interface.
















   ..
       !! processed by numpydoc !!

.. py:property:: sclp
   :type: float


   
   Get or set the A scaling parameter used in the computation of the peak pressure.  This parameter is generally set to unity (the default), but it must be greater than 0..
















   ..
       !! processed by numpydoc !!

.. py:property:: hydro
   :type: int


   
   Get or set the Either 3 or 5 additional history variables useful to shock physics are
   output as the last history variables. For HYDRO = 1, the internal energy
   per reference volume, the reference volume, and the value of the bulk
   viscosity are added to the database, and for HYDRO = 2, the relative
   volume and current density are also added
















   ..
       !! processed by numpydoc !!

.. py:property:: msscl
   :type: int


   
   Get or set the Output nodal information related to mass scaling into the D3PLOT database.  This option can be activated if and only if DT2MS < 0.0, see control card *CONTROL_TIMESTEP.  This option is available starting with the second release of Version 971.
   EQ.0: No data is written
   EQ.1: Output incremental nodal mass
   EQ.2: Output percentage increase in nodal mass.
















   ..
       !! processed by numpydoc !!

.. py:property:: therm
   :type: int


   
   Get or set the Output of thermal data to d3plot. The use of this option (THERM>0) may make the database incompatible with other 3rd party software.
   EQ.0: (default) output temperature
   EQ.1: output temperature
   EQ.2: output temperature and flux
   EQ.3: output temperature, flux, and shell bottom and top surface temperature
















   ..
       !! processed by numpydoc !!

.. py:property:: intout
   :type: str


   
   Get or set the Output stress/strain at all integration points for detailed element output in the file ELOUTDET.  DT and BINARY of *DATABASE_ELOUT apply to ELOUTDET.  See remarks 4-10 below.
   EQ.STRESS: when stress output is required
   EQ.STRAIN when strain output is required
   EQ.ALL when both stress and strain output are required
















   ..
       !! processed by numpydoc !!

.. py:property:: nodout
   :type: str


   
   Get or set the Output extrapolated stress/strain at connectivity nodes points for detailed element output in the file ELOUTDET.  DT and BINARY of *DATABASE_ELOUT apply to ELOUTDET.
   EQ.STRESS when stress output is required
   EQ.STRAIN when strain output is required
   EQ.ALL when both stress and strain output are required
   EQ.STRESS_GL when nodal averaged stress output is required
   EQ.STRAIN_GL when nodal averaged strain output is required
   EQ.ALL_GL for nodal averaged stress and strain output
















   ..
       !! processed by numpydoc !!

.. py:property:: dtdt
   :type: int


   
   Get or set the Output of node point dtemperature/dtime data to d3plot
   EQ.0: (default) no output
   EQ.1: output dT/dt
















   ..
       !! processed by numpydoc !!

.. py:property:: resplt
   :type: int


   
   Get or set the Output of translational and rotational residual forces to d3plot and d3iter
   EQ.0: No output
   EQ.1: Output residual
















   ..
       !! processed by numpydoc !!

.. py:property:: neipb
   :type: Optional[int]


   
   Get or set the Number of additional integration point history variables written to the binary database for beam elements.
















   ..
       !! processed by numpydoc !!

.. py:property:: quadsld
   :type: int


   
   Get or set the Output option for quadratic higher order solid elements EQ.1: output full connectivity,
   EQ.2: full connectivity and data at all integration points.
















   ..
       !! processed by numpydoc !!

.. py:property:: cubsld
   :type: int


   
   Get or set the Output option for cubic higher order solid elements EQ.1: output full connectivity,
   EQ.2: full connectivity and data at all integration points.
















   ..
       !! processed by numpydoc !!

.. py:property:: deleres
   :type: int


   
   Get or set the Output flag for results of deleted elements:
   EQ.0:   no results output(all zero)
   EQ.1 : last available results, e.g., stressesand history variables, are written to d3plotand d3part.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DATABASE'


.. py:attribute:: subkeyword
   :value: 'EXTENT_BINARY'






