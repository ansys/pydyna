





:class:`DatabaseFrequencyBinaryD3Erp`
=====================================


.. py:class:: database_frequency_binary_d3erp.DatabaseFrequencyBinaryD3Erp(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DATABASE_FREQUENCY_BINARY_D3ERP keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DatabaseFrequencyBinaryD3Erp

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~binary`
            - Get or set the Flag for writing the binary plot file.  See Remark 1.


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

    from database_frequency_binary_d3erp import DatabaseFrequencyBinaryD3Erp

Property detail
---------------

.. py:property:: binary
   :type: Optional[int]


   
   Get or set the Flag for writing the binary plot file.  See Remark 1.
   EQ.0:   Off
   EQ.1 : Write the binary plot file.
   EQ.2 : Write the complex variable binary plot file D3SSD(OPTION1 = D3SSD) or include the individual mode response in the binary plot file D3SPCM(OPTION1‌ = D3SPCM).
   EQ.3 : Write the binary plot file which combines response spectrum analysis results and other structural analysis results provided by the file specified with Card  2c(OPTION1‌ = D3SPCM).
   EQ.90 : Write only real part of frequency response(D3SSD only).
   EQ.91 : Write only imaginary part of frequency response(D3SSD only).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DATABASE'


.. py:attribute:: subkeyword
   :value: 'FREQUENCY_BINARY_D3ERP'






