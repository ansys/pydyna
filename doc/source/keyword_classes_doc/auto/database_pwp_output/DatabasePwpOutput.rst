





:class:`DatabasePwpOutput`
==========================


.. py:class:: database_pwp_output.DatabasePwpOutput(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DATABASE_PWP_OUTPUT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DatabasePwpOutput

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ivel`
            - Get or set the Meaning of "velocity" in d3plot and d3thdt output files
          * - :py:attr:`~iaccx`
            - Get or set the Meaning of "X-Acceleration" in d3plot and d3thdt output files
          * - :py:attr:`~iaccy`
            - Get or set the Meaning of "Y-Acceleration" in d3plot and d3thdt output files
          * - :py:attr:`~iaccz`
            - Get or set the Meaning of "Z-Acceleration" in d3plot and d3thdt output files
          * - :py:attr:`~ncyout`
            - Get or set the Number of cycles between outputs of calculation status to d3hsp, log, and tdc_control_output.csv files (time-dependent and steady-state analysis types)


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

    from database_pwp_output import DatabasePwpOutput

Property detail
---------------

.. py:property:: ivel
   :type: int


   
   Get or set the Meaning of "velocity" in d3plot and d3thdt output files
   0:  Nodal velocity vector
   1:  Seepage velocity vector
















   ..
       !! processed by numpydoc !!

.. py:property:: iaccx
   :type: int


   
   Get or set the Meaning of "X-Acceleration" in d3plot and d3thdt output files
   0:  Not written
   1:  Total pwp head
   2:  Excess pwp head (this is also written as temperature)
   3: Target rate of volume change
   4: Actual rate of volume change
   7:  Hydraulic pwp head
   8:  Error in rate of volume change (calculated from seepage minus actual)
   9:  Volume at node
   10:  Rate of volume change calculated from seepage
   14:  Void volume (generated at suction limit)
   17: NFIXCON (e.g. +4/-4 for nodes on suction limit)
















   ..
       !! processed by numpydoc !!

.. py:property:: iaccy
   :type: int


   
   Get or set the Meaning of "Y-Acceleration" in d3plot and d3thdt output files
   0:  Not written
   1:  Total pwp head
   2:  Excess pwp head (this is also written as temperature)
   3: Target rate of volume change
   4: Actual rate of volume change
   7:  Hydraulic pwp head
   8:  Error in rate of volume change (calculated from seepage minus actual)
   9:  Volume at node
   10:  Rate of volume change calculated from seepage
   14:  Void volume (generated at suction limit)
   17: NFIXCON (e.g. +4/-4 for nodes on suction limit)
















   ..
       !! processed by numpydoc !!

.. py:property:: iaccz
   :type: int


   
   Get or set the Meaning of "Z-Acceleration" in d3plot and d3thdt output files
   0:  Not written
   1:  Total pwp head
   2:  Excess pwp head (this is also written as temperature)
   3: Target rate of volume change
   4: Actual rate of volume change
   7:  Hydraulic pwp head
   8:  Error in rate of volume change (calculated from seepage minus actual)
   9:  Volume at node
   10:  Rate of volume change calculated from seepage
   14:  Void volume (generated at suction limit)
   17: NFIXCON (e.g. +4/-4 for nodes on suction limit)
















   ..
       !! processed by numpydoc !!

.. py:property:: ncyout
   :type: int


   
   Get or set the Number of cycles between outputs of calculation status to d3hsp, log, and tdc_control_output.csv files (time-dependent and steady-state analysis types)
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DATABASE'


.. py:attribute:: subkeyword
   :value: 'PWP_OUTPUT'






