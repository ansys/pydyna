





:class:`DatabaseFrequencyAsciiNodforSsd`
========================================


.. py:class:: database_frequency_ascii_nodfor_ssd.DatabaseFrequencyAsciiNodforSsd(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DATABASE_FREQUENCY_ASCII_NODFOR_SSD keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DatabaseFrequencyAsciiNodforSsd

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~fmin`
            - Get or set the Minimum frequency for output (cycles/time).
          * - :py:attr:`~fmax`
            - Get or set the Maximum frequency for output (cycles/time).
          * - :py:attr:`~nfreq`
            - Get or set the Number of frequencies for output.
          * - :py:attr:`~fspace`
            - Get or set the Frequency spacing option for output:
          * - :py:attr:`~lcfreq`
            - Get or set the Load Curve ID defining the frequencies for output.


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

    from database_frequency_ascii_nodfor_ssd import DatabaseFrequencyAsciiNodforSsd

Property detail
---------------

.. py:property:: fmin
   :type: float


   
   Get or set the Minimum frequency for output (cycles/time).
















   ..
       !! processed by numpydoc !!

.. py:property:: fmax
   :type: float


   
   Get or set the Maximum frequency for output (cycles/time).
















   ..
       !! processed by numpydoc !!

.. py:property:: nfreq
   :type: int


   
   Get or set the Number of frequencies for output.
















   ..
       !! processed by numpydoc !!

.. py:property:: fspace
   :type: int


   
   Get or set the Frequency spacing option for output:
   EQ.0: linear,
   EQ.1: logarithmic,
   EQ.2: biased.
   EQ.3:   Eigenfrequencies only
















   ..
       !! processed by numpydoc !!

.. py:property:: lcfreq
   :type: int


   
   Get or set the Load Curve ID defining the frequencies for output.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DATABASE'


.. py:attribute:: subkeyword
   :value: 'FREQUENCY_ASCII_NODFOR_SSD'






