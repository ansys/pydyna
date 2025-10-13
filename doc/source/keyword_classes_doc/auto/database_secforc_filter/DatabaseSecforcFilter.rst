





:class:`DatabaseSecforcFilter`
==============================


.. py:class:: database_secforc_filter.DatabaseSecforcFilter(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DATABASE_SECFORC_FILTER keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DatabaseSecforcFilter

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
          * - :py:attr:`~rate`
            - Get or set the Time interval T between filter sampling
          * - :py:attr:`~cutoff`
            - Get or set the Frequency cut-off C in Hz.
          * - :py:attr:`~window`
            - Get or set the The width of the window W in units of time for storing the single,
          * - :py:attr:`~type`
            - Get or set the Flag for filtering options.


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

    from database_secforc_filter import DatabaseSecforcFilter

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

.. py:property:: rate
   :type: float


   
   Get or set the Time interval T between filter sampling
















   ..
       !! processed by numpydoc !!

.. py:property:: cutoff
   :type: Optional[float]


   
   Get or set the Frequency cut-off C in Hz.
















   ..
       !! processed by numpydoc !!

.. py:property:: window
   :type: Optional[float]


   
   Get or set the The width of the window W in units of time for storing the single,
   forward filtering required for the TYPE = 2 filter option.
   Increasing the width of the window will increase the memory
   required for the analysis. A window that is too narrow will
   reduce the amplitude of the filtered result significantly, and
   values below 15 are not recommended for that reason. In general,
   the results for the TYPE = 2 option are sensitive to the width of
   the window and experimentation is required
















   ..
       !! processed by numpydoc !!

.. py:property:: type
   :type: int


   
   Get or set the Flag for filtering options.
   EQ.0: No filtering (default).
   EQ.1: Single pass, forward Butterworth filtering.
   EQ.2: Two pass filtering over the specified time window.
   Backward Butterworth filtering is applied to the forward
   Butterworth results that have been stored. This option
   improves the phase accuracy significantly at the expense
   of memory.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DATABASE'


.. py:attribute:: subkeyword
   :value: 'SECFORC_FILTER'






