





:class:`ControlOutput`
======================


.. py:class:: control_output.ControlOutput(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_OUTPUT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlOutput

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~npopt`
            - Get or set the Print suppression during input phase flag for the printed output file:
          * - :py:attr:`~neecho`
            - Get or set the Print suppression during input phase flag for echo file:
          * - :py:attr:`~nrefup`
            - Get or set the Flag to update reference node coordinates for beam elements.
          * - :py:attr:`~iaccop`
            - Get or set the Averaged accelerations from velocities in file NODOUT and the time history database file d3thdt:
          * - :py:attr:`~opifs`
            - Get or set the Output interval for interface file (Dt).
          * - :py:attr:`~ipnint`
            - Get or set the Print initial time step sizes for all elements on the first cycle:
          * - :py:attr:`~ikedit`
            - Get or set the Problem status report interval steps to the D3HSP (printed output)
          * - :py:attr:`~iflush`
            - Get or set the Number of time steps interval for flushing I/O buffers (default =5000).
          * - :py:attr:`~iprtf`
            - Get or set the Default print flag for RBDOUT and MATSUM files.
          * - :py:attr:`~ierode`
            - Get or set the Output eroded internal and kinetic energy into the matsum file.  Also, (1) under the heading of part ID 0 in matsum, output the kinetic energy from nonstructural mass, lumped mass elements, and lumped inertia elements, and (2) under the heading of part ID -1in matsum, output the kinetic energy associated with distributed mass from *ELEMENT_MASS_PART..
          * - :py:attr:`~tet10s8`
            - Get or set the Output ten connectivity nodes for the 10-node solid tetrahedral and the eight connectivity nodes for the 8-node shell into “d3plot” database.  The current default is set to 2 since this change in the database may make the data unreadable for many popular post-processors and older versions of LS-PrePost.  The default will change to 1 later.
          * - :py:attr:`~msgmax`
            - Get or set the Maximum number of each error/warning message
          * - :py:attr:`~ipcurv`
            - Get or set the Flag to output digitized curve data to d3msg and d3hsp files.
          * - :py:attr:`~gmdt`
            - Get or set the Output interval for recorded motions from *INTERFACE_SSI_AUX
          * - :py:attr:`~ip1dblt`
            - Get or set the Output information of 1D (bar-type) seatbelt created for 2D (shell-type) seatbelt to sbtout.
          * - :py:attr:`~eocs`
            - Get or set the elout Coordinate System: controls the coordinate system to be used when writing out shell data to the elout file.  EOCS has no affect on eloutdet.:
          * - :py:attr:`~tolev`
            - Get or set the Timing Output Levels: controls the # of levels output in the timing summary at termination. The default is 2.
          * - :py:attr:`~newleg`
            - Get or set the New Legends: controls the format of the LEGEND section of various ascii output files.
          * - :py:attr:`~frfreq`
            - Get or set the Output frequency for failed elemetn report, in cycles. The default is to report the summary every cycle on which an element fails. If > 1, the summary will be reported every FRFREQ cycles whether an element fails that cycle or not, provided some element has failed since the last summary report. Individual element failure is still reported as it occurs.
          * - :py:attr:`~minfo`
            - Get or set the Output penetration information for mortar contact after each implicit step, not applicable in explicit analysis.
          * - :py:attr:`~solsig`
            - Get or set the Flag to extrapolate stresses and other history variables for multi-integration point solids from integration points to nodes.   These extrapolated nodal values replace the integration point values normally stored in d3plot.  When a nonzero SOLSIG is invoked, NINTSLD in *DATABASE_EXTENT_BINARY should be set to 8 as any other value of NINTSLD will result in only one value being reported for each element.  Supported solid formulations are: -1, -2, 2, 3, 4, 18, 16, 17, and 23.
          * - :py:attr:`~msgflg`
            - Get or set the Option for printing detail message rto d3msg
          * - :py:attr:`~cdetol`
            - Get or set the Tolerance for output of *DEFINE_CURVE discretization warnings.  After each curve is discretized, the resulting curve is evaluated at each of the original definition points, and the values compared.  A warning will be issued for any curve where this comparison results in an error of more than CDETOL/100*M, where the curve specific value M is computed as the median of the absolute values of the non-zero curve values.
          * - :py:attr:`~phschng`
            - Get or set the Message to messag file when materials 216, 217, and 218 change phase..
          * - :py:attr:`~demden`
            - Get or set the Output DEM density data to d3plot database..
          * - :py:attr:`~icrfile`
            - Get or set the Flag to output node sets and element sets used in computing secforc data; see *DATABASE_CROSS_SECTION_OPTION and *DATABASE_SECFORC.  These sets are written in keyword format (*SET_...) and thus can be displayed using LS-PrePost.  The assigned set IDs are the same as the ID of the cross-section.
          * - :py:attr:`~spc2bnd`
            - Get or set the converts all constraints on MAT_RIGID (see CMO, CON1, CON2)
          * - :py:attr:`~penout`
            - Get or set the Flag to output contact penetration to sleout (binout format only) and d3plot for Mortar contact.
          * - :py:attr:`~shlsig`
            - Get or set the extrapolation/or not of stresses from integration points to the "corners" of thin shells.
          * - :py:attr:`~hisnout`
            - Get or set the Flag to invoke output of extra history variable names. Usually, the extra history variables of material models are given as just numbers. The corresponding meaning of these variables can be determined, for example, using this website: www.dynasupport.com/howtos/material/history-variables.  As an alternative, this new option allows the output of those names to some files, listed for each part separately. The number of supported material models is continuously increasing.
          * - :py:attr:`~engout`
            - Get or set the Flag to output contact sliding energy densities to d3plot for Mortar contact.
          * - :py:attr:`~insf`
            - Get or set the Flag to invoke output of *SET_NODE data:
          * - :py:attr:`~isolsf`
            - Get or set the Flag to invoke output of *SET_SOLID data:
          * - :py:attr:`~ibsf`
            - Get or set the Flag to invoke output of *SET_BEAM data:
          * - :py:attr:`~issf`
            - Get or set the Flag to invoke output of *SET_SHELL data:
          * - :py:attr:`~mlkbag`
            - Get or set the Flag to invoke output of accumulated airbag mass leakage:


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

    from control_output import ControlOutput

Property detail
---------------

.. py:property:: npopt
   :type: int


   
   Get or set the Print suppression during input phase flag for the printed output file:
   EQ.0: no suppression,
   EQ.1: nodal coordinates, element connectivities, rigid wall definitions and initial velocities are not printed.
















   ..
       !! processed by numpydoc !!

.. py:property:: neecho
   :type: int


   
   Get or set the Print suppression during input phase flag for echo file:
   EQ.0: all data printed,
   EQ.1: nodal printing is suppressed,
   EQ.2: element printing is suppressed,
   EQ.3: both node and element printing is suppressed.
















   ..
       !! processed by numpydoc !!

.. py:property:: nrefup
   :type: int


   
   Get or set the Flag to update reference node coordinates for beam elements.
   EQ.0: no update(default),
   EQ.1: update.
















   ..
       !! processed by numpydoc !!

.. py:property:: iaccop
   :type: int


   
   Get or set the Averaged accelerations from velocities in file NODOUT and the time history database file d3thdt:
   EQ.0: no average (default),
   EQ.1: averaged between output intervals.
   EQ.2: Built-in, user-defined filtering. With this option the keyword parameter DT2MS on *CONTROL_TIMESTEP must be defined. All data points between output intervals are stored and used to obtain the filtered output values. The user defined filter must be provided and linked. The procedure for handling is not yet defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: opifs
   :type: float


   
   Get or set the Output interval for interface file (Dt).
















   ..
       !! processed by numpydoc !!

.. py:property:: ipnint
   :type: int


   
   Get or set the Print initial time step sizes for all elements on the first cycle:
   EQ.0: 100 elements with the smallest time step sizes are printed.
   EQ.1: the governing time step sizes for each element are printed.
















   ..
       !! processed by numpydoc !!

.. py:property:: ikedit
   :type: int


   
   Get or set the Problem status report interval steps to the D3HSP (printed output)
















   ..
       !! processed by numpydoc !!

.. py:property:: iflush
   :type: int


   
   Get or set the Number of time steps interval for flushing I/O buffers (default =5000).
















   ..
       !! processed by numpydoc !!

.. py:property:: iprtf
   :type: int


   
   Get or set the Default print flag for RBDOUT and MATSUM files.
   EQ.0: write part data into both MATSUM and RBDOUT
   EQ.1: write data into RBDOUT file only
   EQ.2: write data into MATSUM file only
   EQ.3: do not write data into RBDOUT and MATSUM
















   ..
       !! processed by numpydoc !!

.. py:property:: ierode
   :type: int


   
   Get or set the Output eroded internal and kinetic energy into the matsum file.  Also, (1) under the heading of part ID 0 in matsum, output the kinetic energy from nonstructural mass, lumped mass elements, and lumped inertia elements, and (2) under the heading of part ID -1in matsum, output the kinetic energy associated with distributed mass from *ELEMENT_MASS_PART..
   EQ.0: do not output extra data.
   EQ.1: output the eroded internal and kinetic energy
















   ..
       !! processed by numpydoc !!

.. py:property:: tet10s8
   :type: int


   
   Get or set the Output ten connectivity nodes for the 10-node solid tetrahedral and the eight connectivity nodes for the 8-node shell into “d3plot” database.  The current default is set to 2 since this change in the database may make the data unreadable for many popular post-processors and older versions of LS-PrePost.  The default will change to 1 later.
   EQ.1:   write the full node connectivity into the “d3plot” database
   EQ.2:   write only the corner nodes of the elements into the “d3plot” database
















   ..
       !! processed by numpydoc !!

.. py:property:: msgmax
   :type: int


   
   Get or set the Maximum number of each error/warning message
















   ..
       !! processed by numpydoc !!

.. py:property:: ipcurv
   :type: int


   
   Get or set the Flag to output digitized curve data to d3msg and d3hsp files.
   EQ.0: off
   EQ.1: on
















   ..
       !! processed by numpydoc !!

.. py:property:: gmdt
   :type: float


   
   Get or set the Output interval for recorded motions from *INTERFACE_SSI_AUX
















   ..
       !! processed by numpydoc !!

.. py:property:: ip1dblt
   :type: int


   
   Get or set the Output information of 1D (bar-type) seatbelt created for 2D (shell-type) seatbelt to sbtout.
   EQ.0: the analysis results of internally created 1D seatbelts are extracted and processed to yield the 2D belt information. The 2D belt information is stored in sbtout,
   EQ.1: the analysis results of internally created 1D retractors and sliprings are stored in sbtout. Belt load can be yielded by *DATABASE_CROSS_SECTION.This might lead to different results from that of IP1DBLT=0 in MPP, if the model it not robust.
   EQ.2:   Same as IP1DBLT = 1, but the model is decomposed in the same way of IP1DBLT = 0 in MPPand, therefore, guarantee result consistency.
















   ..
       !! processed by numpydoc !!

.. py:property:: eocs
   :type: int


   
   Get or set the elout Coordinate System: controls the coordinate system to be used when writing out shell data to the elout file.  EOCS has no affect on eloutdet.:
   EQ.0:   default (local element coordinate system, or if an orthotropic material model and CMPFLG=1, then material coordinate system)
   EQ.1: local element coordinate system
   EQ.2: global coordinate system
















   ..
       !! processed by numpydoc !!

.. py:property:: tolev
   :type: int


   
   Get or set the Timing Output Levels: controls the # of levels output in the timing summary at termination. The default is 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: newleg
   :type: int


   
   Get or set the New Legends: controls the format of the LEGEND section of various ascii output files.
   EQ.0: use the normal format
   EQ.1: use the optional format with extra fields.
















   ..
       !! processed by numpydoc !!

.. py:property:: frfreq
   :type: int


   
   Get or set the Output frequency for failed elemetn report, in cycles. The default is to report the summary every cycle on which an element fails. If > 1, the summary will be reported every FRFREQ cycles whether an element fails that cycle or not, provided some element has failed since the last summary report. Individual element failure is still reported as it occurs.
















   ..
       !! processed by numpydoc !!

.. py:property:: minfo
   :type: int


   
   Get or set the Output penetration information for mortar contact after each implicit step, not applicable in explicit analysis.
   EQ.0: No information
   EQ.1: Penetrations reported for each contact interface
















   ..
       !! processed by numpydoc !!

.. py:property:: solsig
   :type: int


   
   Get or set the Flag to extrapolate stresses and other history variables for multi-integration point solids from integration points to nodes.   These extrapolated nodal values replace the integration point values normally stored in d3plot.  When a nonzero SOLSIG is invoked, NINTSLD in *DATABASE_EXTENT_BINARY should be set to 8 as any other value of NINTSLD will result in only one value being reported for each element.  Supported solid formulations are: -1, -2, 2, 3, 4, 18, 16, 17, and 23.
   EQ.0:   No extrapolation.
   EQ.1:   Extrapolate the stress for linear materials only.
   EQ.2:   Extrapolate the stress if plastic strain is zero.
   EQ.3:   Extrapolate the stress always.
   EQ.4:   Extrapolate all history variables.
















   ..
       !! processed by numpydoc !!

.. py:property:: msgflg
   :type: int


   
   Get or set the Option for printing detail message rto d3msg
   EQ.0:   No detail message
   EQ.1:   Print detail message to d3msg at the termination time
















   ..
       !! processed by numpydoc !!

.. py:property:: cdetol
   :type: float


   
   Get or set the Tolerance for output of *DEFINE_CURVE discretization warnings.  After each curve is discretized, the resulting curve is evaluated at each of the original definition points, and the values compared.  A warning will be issued for any curve where this comparison results in an error of more than CDETOL/100*M, where the curve specific value M is computed as the median of the absolute values of the non-zero curve values.
















   ..
       !! processed by numpydoc !!

.. py:property:: phschng
   :type: int


   
   Get or set the Message to messag file when materials 216, 217, and 218 change phase..
   EQ.0: (default) no message.
   EQ.1: The time and element ID are written..
















   ..
       !! processed by numpydoc !!

.. py:property:: demden
   :type: int


   
   Get or set the Output DEM density data to d3plot database..
   EQ.0: (default) no output.
   EQ.1: output data.
















   ..
       !! processed by numpydoc !!

.. py:property:: icrfile
   :type: int


   
   Get or set the Flag to output node sets and element sets used in computing secforc data; see *DATABASE_CROSS_SECTION_OPTION and *DATABASE_SECFORC.  These sets are written in keyword format (*SET_...) and thus can be displayed using LS-PrePost.  The assigned set IDs are the same as the ID of the cross-section.
   EQ.0:   Do not write sets (default).
   EQ.1:   Write a separate file for each cross-section called cross_section_# where # is the cross-section ID.
   EQ.2:   Write sets for all cross-sections to a file called cross_sections
















   ..
       !! processed by numpydoc !!

.. py:property:: spc2bnd
   :type: Optional[int]


   
   Get or set the converts all constraints on MAT_RIGID (see CMO, CON1, CON2)
   to corresponding BOUNDARY_PRESCRIBED_MOTION_RIGID with a zero curve,
   which allows the reaction force associated with this constraint to be monitored in bndout
















   ..
       !! processed by numpydoc !!

.. py:property:: penout
   :type: int


   
   Get or set the Flag to output contact penetration to sleout (binout format only) and d3plot for Mortar contact.
   In sleout the maximum absolute and/or relative penetration per interface is output, in magnutide only.
   In d3plot a nodal vector field is output for absolute and/or relative penetration, respectively,
   each giving the maximum penetration (magnitude and direction) for all nodes in any sliding interface.
   See also NPEN on *DATABASE_EXTENT_INTFOR.
   EQ.0:   Do not output.
   GE.1 : Output absolute penetration.
   GE.2 : Output relative penetration.
















   ..
       !! processed by numpydoc !!

.. py:property:: shlsig
   :type: int


   
   Get or set the extrapolation/or not of stresses from integration points to the "corners" of thin shells.
















   ..
       !! processed by numpydoc !!

.. py:property:: hisnout
   :type: int


   
   Get or set the Flag to invoke output of extra history variable names. Usually, the extra history variables of material models are given as just numbers. The corresponding meaning of these variables can be determined, for example, using this website: www.dynasupport.com/howtos/material/history-variables.  As an alternative, this new option allows the output of those names to some files, listed for each part separately. The number of supported material models is continuously increasing.
   EQ.0:   No output(default)
   EQ.1 : Information written to d3hsp
   EQ.2 : Information written to d3hsp and XML file hisnames.xml
















   ..
       !! processed by numpydoc !!

.. py:property:: engout
   :type: int


   
   Get or set the Flag to output contact sliding energy densities to d3plot for Mortar contact.
   If set to 1, a nodal scalar field is output giving the minimum sliding energy density for each node in any sliding interface.
   See also NENG on *DATABASE_EXTENT_INTFOR
















   ..
       !! processed by numpydoc !!

.. py:property:: insf
   :type: int


   
   Get or set the Flag to invoke output of *SET_NODE data:
   EQ.0:   no output (default)
   EQ.1:   information written to file. See Remark 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: isolsf
   :type: int


   
   Get or set the Flag to invoke output of *SET_SOLID data:
   EQ.0:   no output (default).
   EQ.1:   information written to file. See Remark 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: ibsf
   :type: int


   
   Get or set the Flag to invoke output of *SET_BEAM data:
   EQ.0:   no output (default)
   EQ.1:   information written to file. See Remark 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: issf
   :type: int


   
   Get or set the Flag to invoke output of *SET_SHELL data:
   EQ.0:   no output (default)
   EQ.1:   information written to file. See Remark 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: mlkbag
   :type: int


   
   Get or set the Flag to invoke output of accumulated airbag mass leakage:
   EQ.0: airbag mass leakage rate is output(default)
   EQ.1 : accumulated airbag mass leakage is output..
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'OUTPUT'






