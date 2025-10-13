





:class:`LoadThermalBinout`
==========================


.. py:class:: load_thermal_binout.LoadThermalBinout(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_THERMAL_BINOUT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadThermalBinout

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~deftemp`
            - Get or set the Default temperature that is applied to nodes no temperature information is provided in the binout file(s) for.
          * - :py:attr:`~filename`
            - Get or set the Name of the file that contains the temperature information.
          * - :py:attr:`~startt`
            - Get or set the Start time Tstart for the temperature mapping.  Until this point in time the nodal temperature for the first step provided in the file is used.
          * - :py:attr:`~tsf`
            - Get or set the Time scale factor that represents the speed-up factor of the mechanical analysis to the previous thermal analysis.


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

    from load_thermal_binout import LoadThermalBinout

Property detail
---------------

.. py:property:: deftemp
   :type: float


   
   Get or set the Default temperature that is applied to nodes no temperature information is provided in the binout file(s) for.
















   ..
       !! processed by numpydoc !!

.. py:property:: filename
   :type: Optional[str]


   
   Get or set the Name of the file that contains the temperature information.
















   ..
       !! processed by numpydoc !!

.. py:property:: startt
   :type: float


   
   Get or set the Start time Tstart for the temperature mapping.  Until this point in time the nodal temperature for the first step provided in the file is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: tsf
   :type: float


   
   Get or set the Time scale factor that represents the speed-up factor of the mechanical analysis to the previous thermal analysis.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'THERMAL_BINOUT'






