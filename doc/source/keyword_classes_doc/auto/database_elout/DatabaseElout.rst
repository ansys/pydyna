





:class:`DatabaseElout`
======================


.. py:class:: database_elout.DatabaseElout(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DATABASE_ELOUT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DatabaseElout

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~dt`
            - Get or set the Time interval between outputs. If DT is zero, no output is printed, This field will be used for all selected ASCII_options that have no unique DT value specified
          * - :py:attr:`~binary`
            - Get or set the Flag for binary file
          * - :py:attr:`~lcur`
            - Get or set the Optional load curveid specifying time interval between dumps.
          * - :py:attr:`~ioopt`
            - Get or set the Flag to govern behavior of the plot frequency load curve defined by LCUR:
          * - :py:attr:`~option1`
            - Get or set the OPTION1 applies to either the NODOUT or ELOUT files. For the NODOUT file OPTION1 is a real variable that defines the time interval between outputs for the high frequency file, NODOUTHF. If OPTION1 is zero, no output is printed. Nodal points that are to be output at a higher frequency are flagged using HFO in the DATABASE_HISTORY_NODE_LOCAL input. For the ELOUT file OPTION1 is a integer variable that gives the number of additional history variables written into the ELOUT file for each integration point in the solid elements.
          * - :py:attr:`~option2`
            - Get or set the OPTION2 applies to either the NODOUTHF or ELOUT files. For the NODOUTHF OPTION2 defines the binary file flag for the high frequency NODOUTHF file. See BINARY above. For the ELOUT file OPTION2 is a integer variable that gives the number of additional history variables written into the ELOUT file for each integration point in the shell elements.
          * - :py:attr:`~option3`
            - Get or set the OPTION3 applies to the ELOUT file only. For the ELOUT file OPTION3 is a integer variable that gives the number of additional history variables written into the ELOUT file for each integration point in the thick shell elements.
          * - :py:attr:`~option4`
            - Get or set the OPTION4 applies to the ELOUT file only. For the ELOUT file OPTION4 is a integer variable that gives the number of additional history variables written into the ELOUT file for each integration point in the beam elements.


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

    from database_elout import DatabaseElout

Property detail
---------------

.. py:property:: dt
   :type: float


   
   Get or set the Time interval between outputs. If DT is zero, no output is printed, This field will be used for all selected ASCII_options that have no unique DT value specified
















   ..
       !! processed by numpydoc !!

.. py:property:: binary
   :type: int


   
   Get or set the Flag for binary file
   EQ.1: ASCII file is written. This is the default on serial and shared memory computers.
   EQ.2:Data written to a binary database, which contains data that would otherwise be output to the ASCII file. The ASCII file in this case is not created. This is the default on distributed memory computers.
   EQ.3: ASCII file is written and the data is also written to the binary database.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcur
   :type: int


   
   Get or set the Optional load curveid specifying time interval between dumps.
















   ..
       !! processed by numpydoc !!

.. py:property:: ioopt
   :type: int


   
   Get or set the Flag to govern behavior of the plot frequency load curve defined by LCUR:
   EQ.1: At the time each plot is generated, the load curve value is added to the current time to determine the next plot time. (default)
   EQ.2: At the time each plot is generated, the next plot time T is computed so that T = the current time plus the load curve value at time T.
   EQ.3: A plot is generated for each abscissa point in the load curve definition. The actual value of the load curve is ignored.
















   ..
       !! processed by numpydoc !!

.. py:property:: option1
   :type: int


   
   Get or set the OPTION1 applies to either the NODOUT or ELOUT files. For the NODOUT file OPTION1 is a real variable that defines the time interval between outputs for the high frequency file, NODOUTHF. If OPTION1 is zero, no output is printed. Nodal points that are to be output at a higher frequency are flagged using HFO in the DATABASE_HISTORY_NODE_LOCAL input. For the ELOUT file OPTION1 is a integer variable that gives the number of additional history variables written into the ELOUT file for each integration point in the solid elements.
















   ..
       !! processed by numpydoc !!

.. py:property:: option2
   :type: int


   
   Get or set the OPTION2 applies to either the NODOUTHF or ELOUT files. For the NODOUTHF OPTION2 defines the binary file flag for the high frequency NODOUTHF file. See BINARY above. For the ELOUT file OPTION2 is a integer variable that gives the number of additional history variables written into the ELOUT file for each integration point in the shell elements.
















   ..
       !! processed by numpydoc !!

.. py:property:: option3
   :type: int


   
   Get or set the OPTION3 applies to the ELOUT file only. For the ELOUT file OPTION3 is a integer variable that gives the number of additional history variables written into the ELOUT file for each integration point in the thick shell elements.
















   ..
       !! processed by numpydoc !!

.. py:property:: option4
   :type: int


   
   Get or set the OPTION4 applies to the ELOUT file only. For the ELOUT file OPTION4 is a integer variable that gives the number of additional history variables written into the ELOUT file for each integration point in the beam elements.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DATABASE'


.. py:attribute:: subkeyword
   :value: 'ELOUT'






