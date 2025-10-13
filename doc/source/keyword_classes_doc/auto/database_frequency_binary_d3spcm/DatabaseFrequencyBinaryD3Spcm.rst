





:class:`DatabaseFrequencyBinaryD3Spcm`
======================================


.. py:class:: database_frequency_binary_d3spcm.DatabaseFrequencyBinaryD3Spcm(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DATABASE_FREQUENCY_BINARY_D3SPCM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DatabaseFrequencyBinaryD3Spcm

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~binary`
            - Get or set the Flag for writing the binary plot file.
          * - :py:attr:`~istate`
            - Get or set the State number in a binary plot file with name FILENAME. The structural analysis results at this state will be combined with the results from the current run.
          * - :py:attr:`~filename`
            - Get or set the Path and file name of precomputed structural response binary plot file (see Remark 4).


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

    from database_frequency_binary_d3spcm import DatabaseFrequencyBinaryD3Spcm

Property detail
---------------

.. py:property:: binary
   :type: int


   
   Get or set the Flag for writing the binary plot file.
   EQ.0: Off
   EQ.1: write the binary plot file
   EQ.2: include the individual mode response in the binary plot file D3SPCM.
   EQ.3:   Write the binary plot file which combines response spectrum analysis results and other structural analysis results provided by Card 2c (OPTION1‌ = D3SPCM).
















   ..
       !! processed by numpydoc !!

.. py:property:: istate
   :type: Optional[int]


   
   Get or set the State number in a binary plot file with name FILENAME. The structural analysis results at this state will be combined with the results from the current run.
















   ..
       !! processed by numpydoc !!

.. py:property:: filename
   :type: Optional[str]


   
   Get or set the Path and file name of precomputed structural response binary plot file (see Remark 4).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DATABASE'


.. py:attribute:: subkeyword
   :value: 'FREQUENCY_BINARY_D3SPCM'






