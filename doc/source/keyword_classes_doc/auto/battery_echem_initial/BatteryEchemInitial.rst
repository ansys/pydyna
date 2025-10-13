





:class:`BatteryEchemInitial`
============================


.. py:class:: battery_echem_initial.BatteryEchemInitial(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BATTERY_ECHEM_INITIAL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BatteryEchemInitial

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~echemid`
            - Get or set the Identifier of the electrochemistry control card to use
          * - :py:attr:`~mid`
            - Get or set the Identifier of the battery material to use. Currently not used
          * - :py:attr:`~lic`
            - Get or set the Initial concentration of Lithium ions
          * - :py:attr:`~lisic`
            - Get or set the Initial concentration of Lithium ions in the solid particles
          * - :py:attr:`~phi2ic`
            - Get or set the Initial condition of the electrolyte potential
          * - :py:attr:`~phi1ic`
            - Get or set the Initial condition of the electrode potential
          * - :py:attr:`~curic`
            - Get or set the Initial operating current
          * - :py:attr:`~fluxic`
            - Get or set the Initial pore-wall flux


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

    from battery_echem_initial import BatteryEchemInitial

Property detail
---------------

.. py:property:: echemid
   :type: Optional[int]


   
   Get or set the Identifier of the electrochemistry control card to use
















   ..
       !! processed by numpydoc !!

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Identifier of the battery material to use. Currently not used
















   ..
       !! processed by numpydoc !!

.. py:property:: lic
   :type: Optional[float]


   
   Get or set the Initial concentration of Lithium ions
















   ..
       !! processed by numpydoc !!

.. py:property:: lisic
   :type: Optional[float]


   
   Get or set the Initial concentration of Lithium ions in the solid particles
















   ..
       !! processed by numpydoc !!

.. py:property:: phi2ic
   :type: Optional[float]


   
   Get or set the Initial condition of the electrolyte potential
















   ..
       !! processed by numpydoc !!

.. py:property:: phi1ic
   :type: Optional[float]


   
   Get or set the Initial condition of the electrode potential
















   ..
       !! processed by numpydoc !!

.. py:property:: curic
   :type: Optional[float]


   
   Get or set the Initial operating current
















   ..
       !! processed by numpydoc !!

.. py:property:: fluxic
   :type: Optional[float]


   
   Get or set the Initial pore-wall flux
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BATTERY'


.. py:attribute:: subkeyword
   :value: 'ECHEM_INITIAL'






