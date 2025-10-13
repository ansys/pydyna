





:class:`DatabaseBinaryD3Plot`
=============================


.. py:class:: database_binary_d3plot.DatabaseBinaryD3Plot(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DATABASE_BINARY_D3PLOT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DatabaseBinaryD3Plot

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~dt`
            - Get or set the This field defines the time interval between output states, DT, for all options except D3DUMP, RUNRSF, and D3DRLF.
          * - :py:attr:`~lcdt`
            - Get or set the Optional load curve ID specifying the output time interval as a function of time. This variable is only available for options D3PLOT, D3PART, D3THDT, INTFOR and BLSTFOR.
          * - :py:attr:`~beam`
            - Get or set the Discrete element option flag (*DATABASE_‌BINARY_‌D3PLOT only):
          * - :py:attr:`~npltc`
            - Get or set the DT=ENDTIM/NPLTC.  Applies to D3PLOT, D3PART, DEMFOR, and INTFOR options only.  This overrides the DT specified in the first field. ENDTIM is specified in *CONTROL_TERMINATION
          * - :py:attr:`~psetid`
            - Get or set the Part set ID for D3PART and D3PLOT options only.  See *SET_‌PART.  Parts in PSETID will excluded in the d3plot database.  Only parts in PSETID are included in the d3part database.
          * - :py:attr:`~ioopt`
            - Get or set the Flag to govern behavior of plot frequency load curve:
          * - :py:attr:`~rate`
            - Get or set the Time interval T between filter sampling.  See Remark 7
          * - :py:attr:`~cutoff`
            - Get or set the Frequency cut-off  in Hz.  See Remark 7
          * - :py:attr:`~window`
            - Get or set the The width of the window W in units of time for storing the single, forward filtering required for the TYPE = 2 filter option.
          * - :py:attr:`~type`
            - Get or set the Flag for filtering options.  See Remark 7.
          * - :py:attr:`~pset`
            - Get or set the Part set ID for filtering.  If no set is specified, all parts are included.  For each element integration point in the d3plot file,


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

    from database_binary_d3plot import DatabaseBinaryD3Plot

Property detail
---------------

.. py:property:: dt
   :type: Optional[float]


   
   Get or set the This field defines the time interval between output states, DT, for all options except D3DUMP, RUNRSF, and D3DRLF.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcdt
   :type: Optional[int]


   
   Get or set the Optional load curve ID specifying the output time interval as a function of time. This variable is only available for options D3PLOT, D3PART, D3THDT, INTFOR and BLSTFOR.
















   ..
       !! processed by numpydoc !!

.. py:property:: beam
   :type: int


   
   Get or set the Discrete element option flag (*DATABASE_‌BINARY_‌D3PLOT only):
   EQ.0:   Discrete spring and damper elements are added to the d3plot database where they are displayed as beam elements.The discrete elements’ global x, global y, global zand resultant forces(moments) and change in length(rotation) are written to the database where LS - PrePost(incorrectly) labels them as though they were beam quantities, such as axial force, S - shear resultant, T - shear resultant, etc.
   EQ.1 : No discrete spring, damperand seatbelt elements are added to the d3plot database.This option is useful when translating old LS - DYNA input decks to KEYWORD input.In older input decks there is no requirement that beam and spring elements have unique IDs,and beam elements may be created for the springand dampers with identical IDs to existing beam elements causing a fatal error.However, this option comes with some limitationsand, therefore, should be used with caution.
   Contact interfaces which are based on part IDs of seatbelt elements will not be properly generated if this option is used.
   DEFORMABLE_TO_RIGID will not work if PID refers to discrete, damper, or seatbelt elements.
   EQ.2 : Discrete spring and damper elements are added to the d3plot database where they are displayed as beam elements(similar to option 0).In this option the element resultant force is written to its first database position allowing beam axial forces and spring resultant forces to be plotted at the same time.This can be useful during some post - processing applications.
   This flag, set in* DATABASE_BINARY_D3PLOT, also affects the display of discrete elements in several other databases, such as d3drlfand d3part.
















   ..
       !! processed by numpydoc !!

.. py:property:: npltc
   :type: Optional[int]


   
   Get or set the DT=ENDTIM/NPLTC.  Applies to D3PLOT, D3PART, DEMFOR, and INTFOR options only.  This overrides the DT specified in the first field. ENDTIM is specified in *CONTROL_TERMINATION
















   ..
       !! processed by numpydoc !!

.. py:property:: psetid
   :type: Optional[int]


   
   Get or set the Part set ID for D3PART and D3PLOT options only.  See *SET_‌PART.  Parts in PSETID will excluded in the d3plot database.  Only parts in PSETID are included in the d3part database.
















   ..
       !! processed by numpydoc !!

.. py:property:: ioopt
   :type: int


   
   Get or set the Flag to govern behavior of plot frequency load curve:
   EQ.1: At the time each plot is generated, the load curve value is added to the current time to determine the next plot time.(this is the default behavior).
   EQ 2: At the time each plot is generated, the next plot time T is computed so that T = the current time plus the load curve value at time T.
   EQ 3: A plot is generated for each ordinate point in the load curve definition. The actual value of the load curve is ignored.
















   ..
       !! processed by numpydoc !!

.. py:property:: rate
   :type: Optional[float]


   
   Get or set the Time interval T between filter sampling.  See Remark 7
















   ..
       !! processed by numpydoc !!

.. py:property:: cutoff
   :type: Optional[float]


   
   Get or set the Frequency cut-off  in Hz.  See Remark 7
















   ..
       !! processed by numpydoc !!

.. py:property:: window
   :type: Optional[float]


   
   Get or set the The width of the window W in units of time for storing the single, forward filtering required for the TYPE = 2 filter option.
   Increasing the width of the window will increase the memory required for the analysis.
   A window that is too narrow will reduce the amplitude of the filtered result significantly, and values below 15 are not recommended for that reason.
   In general, the results for the TYPE = 2 option are sensitive to the width of the window and experimentation is required.  See Remark 7.
















   ..
       !! processed by numpydoc !!

.. py:property:: type
   :type: int


   
   Get or set the Flag for filtering options.  See Remark 7.
   EQ.0:   No filtering (default).
   EQ.1:   Single pass, forward Butterworth filtering.
   EQ.2:   Two pass filtering over the specified time window. Backward Butterworth filtering is applied to the forward Butterworth results that have been stored.
   This option improves the phase accuracy significantly at the expense of memory
















   ..
       !! processed by numpydoc !!

.. py:property:: pset
   :type: int


   
   Get or set the Part set ID for filtering.  If no set is specified, all parts are included.  For each element integration point in the d3plot file,
   24 words of memory are required in LS-DYNA for the single pass filtering, and more for the two pass filtering.
   Specifying PSET is recommended to minimize the memory requirements.  See Remark 7.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DATABASE'


.. py:attribute:: subkeyword
   :value: 'BINARY_D3PLOT'






