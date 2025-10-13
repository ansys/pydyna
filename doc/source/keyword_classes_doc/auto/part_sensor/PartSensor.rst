





:class:`PartSensor`
===================


.. py:class:: part_sensor.PartSensor(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA PART_SENSOR keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: PartSensor

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID, which is controlled by sensor.
          * - :py:attr:`~sida`
            - Get or set the Sensor ID to activate or deactivate part.
          * - :py:attr:`~active`
            - Get or set the Flag. If zero, the part is active from time zero until a signal is received by the part to deactivate. If one, the part is inactive from time zero and becomes active when a signal is received by the part to activate. The history variables for inactive parts are initialized at time zero.


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

    from part_sensor import PartSensor

Property detail
---------------

.. py:property:: pid
   :type: int


   
   Get or set the Part ID, which is controlled by sensor.
















   ..
       !! processed by numpydoc !!

.. py:property:: sida
   :type: int


   
   Get or set the Sensor ID to activate or deactivate part.
















   ..
       !! processed by numpydoc !!

.. py:property:: active
   :type: int


   
   Get or set the Flag. If zero, the part is active from time zero until a signal is received by the part to deactivate. If one, the part is inactive from time zero and becomes active when a signal is received by the part to activate. The history variables for inactive parts are initialized at time zero.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'PART'


.. py:attribute:: subkeyword
   :value: 'SENSOR'






