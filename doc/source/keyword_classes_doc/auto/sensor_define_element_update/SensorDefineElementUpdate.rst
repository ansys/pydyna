





:class:`SensorDefineElementUpdate`
==================================


.. py:class:: sensor_define_element_update.SensorDefineElementUpdate(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SENSOR_DEFINE_ELEMENT_UPDATE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SensorDefineElementUpdate

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sensid`
            - Get or set the Sensor ID.
          * - :py:attr:`~etype`
            - Get or set the Element type:
          * - :py:attr:`~elemid`
            - Get or set the Element ID or element set ID when the SET keyword option is active.
          * - :py:attr:`~comp`
            - Get or set the Element type:
          * - :py:attr:`~ctype`
            - Get or set the Component type:
          * - :py:attr:`~layer`
            - Get or set the Layer of integration point in shell element
          * - :py:attr:`~sf`
            - Get or set the Optional parameters, scale factor and power, for users to adjust the resultant sensor value
          * - :py:attr:`~pwr`
            - Get or set the Optional parameters, scale factor and power, for users to adjust the resultant sensor value
          * - :py:attr:`~birth`
            - Get or set the Sensor IBirth time of this sensor.
          * - :py:attr:`~death`
            - Get or set the Death time of this sensor.
          * - :py:attr:`~dtupd`
            - Get or set the Time interval between updates. If negative, -DTUPD is the curve defining update interval as a function of time.
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

    from sensor_define_element_update import SensorDefineElementUpdate

Property detail
---------------

.. py:property:: sensid
   :type: Optional[int]


   
   Get or set the Sensor ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: etype
   :type: str


   
   Get or set the Element type:
   EQ.BEAM :       beam element.
   EQ.SHELL:       shell element
   EQ.SOLID:       solid element
   EQ.DISC-ELE:    discrete element.
   EQ.SEATBELT: seatbelt element
   EQ.TSHELL: thick shell element
















   ..
       !! processed by numpydoc !!

.. py:property:: elemid
   :type: Optional[int]


   
   Get or set the Element ID or element set ID when the SET keyword option is active.
   In the case of the SET keyword option with SETOPT not defined, determining the status of a related* SENSOR_SWITCH depends on the sign of ELEMID.If SETOPT is defined, then ELEMID must be greater than 0.
















   ..
       !! processed by numpydoc !!

.. py:property:: comp
   :type: str


   
   Get or set the Element type:
   EQ.XX:          x-normal component for shells and solids
   EQ.YY:          y-normal component for shells and solids
   EQ.ZZ:          z-normal component for shells and solids
   EQ.XY:          xy-shear component for shells and solids
   EQ.YZ:          yz-shear component for shells and solids
   EQ.ZX:          zx-shear component for shells and solids
   EQ:AXIAL:       axial
   EQ:SHEARS:      local s-direction
   EQ:SHEART:      local t-direction
   EQ:               :     leave blank for discrete elements
















   ..
       !! processed by numpydoc !!

.. py:property:: ctype
   :type: str


   
   Get or set the Component type:
   EQ.STRAIN:      strain component for shells and solids
   EQ.STRESS:      stress component for shells and solids
   EQ.FORCE:       force resultants for beams
   EQ.MOMENT:      moment resultants for beams
   EQ.FORCE:       discrete element force
   EQ.DLEN:        change in length for discrete element
   EQ.FAIL:        failure of element, sensor value = 1 when element fails, = 0 otherwise.
















   ..
       !! processed by numpydoc !!

.. py:property:: layer
   :type: str


   
   Get or set the Layer of integration point in shell element
   EQ.BOT: component at lower surface
   EQ.TOP: component at upper surface
















   ..
       !! processed by numpydoc !!

.. py:property:: sf
   :type: Optional[float]


   
   Get or set the Optional parameters, scale factor and power, for users to adjust the resultant sensor value
















   ..
       !! processed by numpydoc !!

.. py:property:: pwr
   :type: Optional[float]


   
   Get or set the Optional parameters, scale factor and power, for users to adjust the resultant sensor value
















   ..
       !! processed by numpydoc !!

.. py:property:: birth
   :type: Optional[float]


   
   Get or set the Sensor IBirth time of this sensor.
















   ..
       !! processed by numpydoc !!

.. py:property:: death
   :type: Optional[float]


   
   Get or set the Death time of this sensor.
















   ..
       !! processed by numpydoc !!

.. py:property:: dtupd
   :type: Optional[float]


   
   Get or set the Time interval between updates. If negative, -DTUPD is the curve defining update interval as a function of time.
















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
   :value: 'DEFINE_ELEMENT_UPDATE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





