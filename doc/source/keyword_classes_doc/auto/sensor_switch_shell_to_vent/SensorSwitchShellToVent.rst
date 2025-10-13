





:class:`SensorSwitchShellToVent`
================================


.. py:class:: sensor_switch_shell_to_vent.SensorSwitchShellToVent(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SENSOR_SWITCH_SHELL_TO_VENT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SensorSwitchShellToVent

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Part set ID/Part ID.
          * - :py:attr:`~type`
            - Get or set the EQ.0: Part
          * - :py:attr:`~c23`
            - Get or set the Vent Coefficient (Default = 0.7)
          * - :py:attr:`~amax`
            - Get or set the Maximum allowable area for failed vent surface area(VA).  If the area is bigger than AMAX, C23 will be scaled down by a factor of AMAX/VA.Otherwise C23 will be used
          * - :py:attr:`~ssid`
            - Get or set the ID of *SET_SHELL_LIST.
          * - :py:attr:`~ftime`
            - Get or set the Time to convert shell list to vent. (Default is from t = 0.)
          * - :py:attr:`~c23v`
            - Get or set the Vent Coefficient (Default = C23)
          * - :py:attr:`~title`
            - Get or set the Additional title line


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~keyword`
            - 
          * - :py:attr:`~subkeyword`
            - 
          * - :py:attr:`~option_specs`
            - Get the card format type.






Import detail
-------------

.. code-block:: python

    from sensor_switch_shell_to_vent import SensorSwitchShellToVent

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Part set ID/Part ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: type
   :type: int


   
   Get or set the EQ.0: Part
   EQ.1: Part set
















   ..
       !! processed by numpydoc !!

.. py:property:: c23
   :type: float


   
   Get or set the Vent Coefficient (Default = 0.7)
   LT.0: User defined load curve ID. The vent coefficient will be
   determined by this pressure-vent_coeff curve.
















   ..
       !! processed by numpydoc !!

.. py:property:: amax
   :type: Optional[float]


   
   Get or set the Maximum allowable area for failed vent surface area(VA).  If the area is bigger than AMAX, C23 will be scaled down by a factor of AMAX/VA.Otherwise C23 will be used
















   ..
       !! processed by numpydoc !!

.. py:property:: ssid
   :type: Optional[int]


   
   Get or set the ID of *SET_SHELL_LIST.
















   ..
       !! processed by numpydoc !!

.. py:property:: ftime
   :type: float


   
   Get or set the Time to convert shell list to vent. (Default is from t = 0.)
















   ..
       !! processed by numpydoc !!

.. py:property:: c23v
   :type: float


   
   Get or set the Vent Coefficient (Default = C23)
   LT.0: User defined load curve ID. The vent coefficient will be
   determined by this pressure-vent_coeff curve.
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Additional title line
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'SENSOR'


.. py:attribute:: subkeyword
   :value: 'SWITCH_SHELL_TO_VENT'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





