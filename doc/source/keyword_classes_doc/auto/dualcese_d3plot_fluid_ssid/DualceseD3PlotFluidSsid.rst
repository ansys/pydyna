





:class:`DualceseD3PlotFluidSsid`
================================


.. py:class:: dualcese_d3plot_fluid_ssid.DualceseD3PlotFluidSsid(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DUALCESE_D3PLOT_FLUID_SSID keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DualceseD3PlotFluidSsid

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ssid`
            - Get or set the Segment set ID created with *DUALCESE_SEGMENTSET
          * - :py:attr:`~flow_var`
            - Get or set the Name of a flow variable to output to the d3plot file. The currently supported variables are:


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

    from dualcese_d3plot_fluid_ssid import DualceseD3PlotFluidSsid

Property detail
---------------

.. py:property:: ssid
   :type: Optional[int]


   
   Get or set the Segment set ID created with *DUALCESE_SEGMENTSET
















   ..
       !! processed by numpydoc !!

.. py:property:: flow_var
   :type: Optional[str]


   
   Get or set the Name of a flow variable to output to the d3plot file. The currently supported variables are:
   DENSITY
   VELOCITY
   MOMENTUM
   VORTICITY
   TOTAL_ENERGY
   INTERNAL_ENERGY
   PRESSURE
   TEMPERATURE
   ENTROPY
   ENTHALPY
   SCHLIEREN_NUMBER
   VOID_FRACTION
   VOLUME_FRACTION
   REACTANT_MASS_FRACTION
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DUALCESE'


.. py:attribute:: subkeyword
   :value: 'D3PLOT_FLUID_SSID'






