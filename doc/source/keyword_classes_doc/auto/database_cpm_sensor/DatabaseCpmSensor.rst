





:class:`DatabaseCpmSensor`
==========================


.. py:class:: database_cpm_sensor.DatabaseCpmSensor(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DATABASE_CPM_SENSOR keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DatabaseCpmSensor

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~dt`
            - Get or set the Output interval
          * - :py:attr:`~binary`
            - Get or set the Flag for the binary file
          * - :py:attr:`~segsid`
            - Get or set the Segment set ID
          * - :py:attr:`~offset`
            - Get or set the Offset distance between the center of the sensor and  the segment center. If it is positive, it is on the side pointed to by the segment normal vector.
          * - :py:attr:`~r_lx`
            - Get or set the Radius(sphere)/length in local X direction(rectangular) of the sensor.
          * - :py:attr:`~len_ly`
            - Get or set the Length(cylinder)/length in local Y direction(rectangular) of the sensor.
          * - :py:attr:`~lz`
            - Get or set the Length in local Z direction(rectangular) of the sensor.


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

    from database_cpm_sensor import DatabaseCpmSensor

Property detail
---------------

.. py:property:: dt
   :type: Optional[float]


   
   Get or set the Output interval
















   ..
       !! processed by numpydoc !!

.. py:property:: binary
   :type: int


   
   Get or set the Flag for the binary file
   EQ.1:  ASCII file is written,
   EQ.2:  Data written to the binary file binout,
   EQ.3:  ASCII file  is written and the data written to the binary file binout
















   ..
       !! processed by numpydoc !!

.. py:property:: segsid
   :type: Optional[int]


   
   Get or set the Segment set ID
















   ..
       !! processed by numpydoc !!

.. py:property:: offset
   :type: Optional[float]


   
   Get or set the Offset distance between the center of the sensor and  the segment center. If it is positive, it is on the side pointed to by the segment normal vector.
















   ..
       !! processed by numpydoc !!

.. py:property:: r_lx
   :type: Optional[float]


   
   Get or set the Radius(sphere)/length in local X direction(rectangular) of the sensor.
















   ..
       !! processed by numpydoc !!

.. py:property:: len_ly
   :type: Optional[float]


   
   Get or set the Length(cylinder)/length in local Y direction(rectangular) of the sensor.
















   ..
       !! processed by numpydoc !!

.. py:property:: lz
   :type: Optional[float]


   
   Get or set the Length in local Z direction(rectangular) of the sensor.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DATABASE'


.. py:attribute:: subkeyword
   :value: 'CPM_SENSOR'






