





:class:`ControlPoreAir`
=======================


.. py:class:: control_pore_air.ControlPoreAir(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_PORE_AIR keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlPoreAir

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pa_rho`
            - Get or set the Density of atmospheric air, = 1.184 kg/m3 at 25 C
          * - :py:attr:`~air_p`
            - Get or set the Pressure of atmospheric air, = 101.325 kPa at 25 C
          * - :py:attr:`~eterm`
            - Get or set the Event termination time, default to ENDTIME of *CONTROL_TERMINATION.
          * - :py:attr:`~anamsg`
            - Get or set the Flag to turn off the printing of pore air analysis status message,


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

    from control_pore_air import ControlPoreAir

Property detail
---------------

.. py:property:: pa_rho
   :type: Optional[float]


   
   Get or set the Density of atmospheric air, = 1.184 kg/m3 at 25 C
















   ..
       !! processed by numpydoc !!

.. py:property:: air_p
   :type: Optional[float]


   
   Get or set the Pressure of atmospheric air, = 101.325 kPa at 25 C
















   ..
       !! processed by numpydoc !!

.. py:property:: eterm
   :type: Optional[float]


   
   Get or set the Event termination time, default to ENDTIME of *CONTROL_TERMINATION.
















   ..
       !! processed by numpydoc !!

.. py:property:: anamsg
   :type: int


   
   Get or set the Flag to turn off the printing of pore air analysis status message,
   including the analysis time, the node with the highest pressure change.
   EQ. 0 Status messages are printed, the default value.
   EQ. 1 Status messages are not printed
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'PORE_AIR'






