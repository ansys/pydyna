





:class:`SensorDefineElementSetUpdate`
=====================================


.. py:class:: sensor_define_element_set_update.SensorDefineElementSetUpdate(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SENSOR_DEFINE_ELEMENT_SET_UPDATE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SensorDefineElementSetUpdate

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
          * - :py:attr:`~setopt`
            - Get or set the Option to process set of data when SET option is specified.If you set SETOPT, then ELEMID must be greater than 0. When SETOPT is defined, a single value will be reported.  The single reported value could be:
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

    from sensor_define_element_set_update import SensorDefineElementSetUpdate

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
   EQ.BEAM :       beam element set.
   EQ.SHELL:       shell element set
   EQ.SOLID:       solid element set
   EQ.DISC-ELE:    discrete element set.
















   ..
       !! processed by numpydoc !!

.. py:property:: elemid
   :type: Optional[int]


   
   Get or set the Element ID or element set ID when the SET keyword option is active.
   In the case of the SET keyword option with SETOPT not defined, determining the status of a related* SENSOR_SWITCH depends on the sign of ELEMID
















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

.. py:property:: setopt
   :type: str


   
   Get or set the Option to process set of data when SET option is specified.If you set SETOPT, then ELEMID must be greater than 0. When SETOPT is defined, a single value will be reported.  The single reported value could be:
   EQ.AVG: the average value of the dataset
   EQ.MAX: the maximum value of the dataset
   EQ.MIN: the minimum value of the dataset
   EQ.SUM: the sum of the dataset.
















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
   :value: 'DEFINE_ELEMENT_SET_UPDATE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





