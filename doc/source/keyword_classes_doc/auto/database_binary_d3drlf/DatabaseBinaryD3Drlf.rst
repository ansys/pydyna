





:class:`DatabaseBinaryD3Drlf`
=============================


.. py:class:: database_binary_d3drlf.DatabaseBinaryD3Drlf(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DATABASE_BINARY_D3DRLF keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DatabaseBinaryD3Drlf

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~cycl`
            - Get or set the For D3DUMP and RUNRSF options this field is the number of time steps between output states.  For the D3DLF option, the value, n, inputted in this field causes an output state to be written every nth convergence check during the explicit dynamic relaxation phase
          * - :py:attr:`~lcdt`
            - Get or set the Optional load curve ID specifying time interval between dumps.  This variable is only available for options D3DUMP, D3PART, D3PLOT,D3THDT, INTFOR and BLSTFOR.
          * - :py:attr:`~beam`
            - Get or set the Discrete element option flag (*DATABASE_‌BINARY_‌D3PLOT only):
          * - :py:attr:`~npltc`
            - Get or set the DT=ENDTIM/NPLTC.  Applies to D3PLOT, D3PART, DEMFOR, and INTFOR options only.  This overrides the DT specified in the first field. ENDTIM is specified in *CONTROL_TERMINATION
          * - :py:attr:`~psetid`
            - Get or set the Part set ID for D3PART and D3PLOT options only.  See *SET_‌PART.  Parts in PSETID will excluded in the d3plot database.  Only parts in PSETID are included in the d3part database.


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

    from database_binary_d3drlf import DatabaseBinaryD3Drlf

Property detail
---------------

.. py:property:: cycl
   :type: Optional[float]


   
   Get or set the For D3DUMP and RUNRSF options this field is the number of time steps between output states.  For the D3DLF option, the value, n, inputted in this field causes an output state to be written every nth convergence check during the explicit dynamic relaxation phase
















   ..
       !! processed by numpydoc !!

.. py:property:: lcdt
   :type: Optional[int]


   
   Get or set the Optional load curve ID specifying time interval between dumps.  This variable is only available for options D3DUMP, D3PART, D3PLOT,D3THDT, INTFOR and BLSTFOR.
















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



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DATABASE'


.. py:attribute:: subkeyword
   :value: 'BINARY_D3DRLF'






