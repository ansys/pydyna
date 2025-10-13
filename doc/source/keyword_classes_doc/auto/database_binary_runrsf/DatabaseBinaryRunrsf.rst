





:class:`DatabaseBinaryRunrsf`
=============================


.. py:class:: database_binary_runrsf.DatabaseBinaryRunrsf(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DATABASE_BINARY_RUNRSF keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DatabaseBinaryRunrsf

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~cycl`
            - Get or set the Output interval in time steps (a time step is a cycle). For the D3DRFL
          * - :py:attr:`~nr`
            - Get or set the Number of Running Restart Files, RUNRSF, written in a cyclical fashion.  The default number is one, i.e. the same file is overwritten each time.
          * - :py:attr:`~beam`
            - Get or set the Option flag:
          * - :py:attr:`~npltc`
            - Get or set the DT=ENDTIME/NPLTC. This overrides the DT specified in the first field.
          * - :py:attr:`~psetid`
            - Get or set the Set part ID, see also *SET_PART_OPTION.


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

    from database_binary_runrsf import DatabaseBinaryRunrsf

Property detail
---------------

.. py:property:: cycl
   :type: Optional[float]


   
   Get or set the Output interval in time steps (a time step is a cycle). For the D3DRFL
   file a positive number 'n' will cause plot dumps to be written at every
   n'th convergence check interval specified on the *CONTROL_DYNAMIC_RELAXATION card.
















   ..
       !! processed by numpydoc !!

.. py:property:: nr
   :type: Optional[int]


   
   Get or set the Number of Running Restart Files, RUNRSF, written in a cyclical fashion.  The default number is one, i.e. the same file is overwritten each time.
















   ..
       !! processed by numpydoc !!

.. py:property:: beam
   :type: int


   
   Get or set the Option flag:
   EQ.0: Discrete spring and damper elements are added to the D3PART database where they are display as beam elements. The element global X, global Y, global Z and resultant forces are written to the database (default),
   EQ.1 No discrete spring and damper elements are added to the D3PART database. This option is useful when translating old LS-DYNA input decks to KEYWORD input. In older input decks there is no requirement that beam and spring elements have unique ID's, and beam elements may be created for the spring and dampers with identical ID's to existing beam elements causing a fatal error, EQ.2. Discrete spring and damper elements are added to the or D3PART database where they are displayed as beam elements (similar to option 0). In this option the element resultant force is written to its first database position allowing beam axial forces and spring resultant forces to be plotted at the same time. This can be useful during some post-processing applications.
















   ..
       !! processed by numpydoc !!

.. py:property:: npltc
   :type: Optional[int]


   
   Get or set the DT=ENDTIME/NPLTC. This overrides the DT specified in the first field.
















   ..
       !! processed by numpydoc !!

.. py:property:: psetid
   :type: Optional[int]


   
   Get or set the Set part ID, see also *SET_PART_OPTION.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DATABASE'


.. py:attribute:: subkeyword
   :value: 'BINARY_RUNRSF'






