





:class:`DatabasePowerSpectralDensityFrequency`
==============================================


.. py:class:: database_power_spectral_density_frequency.DatabasePowerSpectralDensityFrequency(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DATABASE_POWER_SPECTRAL_DENSITY_FREQUENCY keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DatabasePowerSpectralDensityFrequency

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~fbeg`
            - Get or set the Beginning frequency for PSD database output.
          * - :py:attr:`~fend`
            - Get or set the Ending frequency for PSD database output.
          * - :py:attr:`~fintval`
            - Get or set the Interval of frequencies for PSD database output.
          * - :py:attr:`~ftrunk`
            - Get or set the If FBEG and FEND are not given, the ending frequency for PSD database output is FTRUNK*the highest resonance frequency. Output for higher frequencies is truncated.


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

    from database_power_spectral_density_frequency import DatabasePowerSpectralDensityFrequency

Property detail
---------------

.. py:property:: fbeg
   :type: float


   
   Get or set the Beginning frequency for PSD database output.
















   ..
       !! processed by numpydoc !!

.. py:property:: fend
   :type: float


   
   Get or set the Ending frequency for PSD database output.
















   ..
       !! processed by numpydoc !!

.. py:property:: fintval
   :type: float


   
   Get or set the Interval of frequencies for PSD database output.
















   ..
       !! processed by numpydoc !!

.. py:property:: ftrunk
   :type: float


   
   Get or set the If FBEG and FEND are not given, the ending frequency for PSD database output is FTRUNK*the highest resonance frequency. Output for higher frequencies is truncated.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DATABASE'


.. py:attribute:: subkeyword
   :value: 'POWER_SPECTRAL_DENSITY_FREQUENCY'






