





:class:`IcfdDatabaseHtc`
========================


.. py:class:: icfd_database_htc.IcfdDatabaseHtc(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_DATABASE_HTC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdDatabaseHtc

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~out`
            - Get or set the Determines if the solver should calculate the heat transfer coefficient and how to output it:
          * - :py:attr:`~htc`
            - Get or set the Determines how the bulk temperature is defined.
          * - :py:attr:`~tb`
            - Get or set the Value of the bulk temperature if HTC = 1.
          * - :py:attr:`~outdt`
            - Get or set the Output frequency of the HTC in the various ASCII files. If left to 0.,the solver will output the HTC at every timestep.


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

    from icfd_database_htc import IcfdDatabaseHtc

Property detail
---------------

.. py:property:: out
   :type: int


   
   Get or set the Determines if the solver should calculate the heat transfer coefficient and how to output it:
   EQ.0:No HTC calculation
   EQ.1:HTC calculated and output in LSPP as a surface variable.
   EQ.2:The solver will also look for FSI boundaries and output the HTC value at the solid nodes in an ASCII file called icfdhtci.dat.
   EQ.3:The solver will also look for FSI boundaries that are part of SEGMENT_SETS and output the HTC for those segments in an ASCII file called icfd_convseg.****.key in a format that can directly read by LS-DYNA for a subsequent pure structural thermal analysis.
















   ..
       !! processed by numpydoc !!

.. py:property:: htc
   :type: int


   
   Get or set the Determines how the bulk temperature is defined.
   EQ.0: Automatically calculated by the solver based on the average temperature flowing through the pipe section
   EQ.1: User imposed value
















   ..
       !! processed by numpydoc !!

.. py:property:: tb
   :type: float


   
   Get or set the Value of the bulk temperature if HTC = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: outdt
   :type: float


   
   Get or set the Output frequency of the HTC in the various ASCII files. If left to 0.,the solver will output the HTC at every timestep.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'DATABASE_HTC'






