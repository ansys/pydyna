





:class:`DatabasePblastSensor`
=============================


.. py:class:: database_pblast_sensor.DatabasePblastSensor(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DATABASE_PBLAST_SENSOR keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DatabasePblastSensor

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~dt`
            - Get or set the Output interval.
          * - :py:attr:`~binary`
            - Get or set the Flag for the binary file:
          * - :py:attr:`~id`
            - Get or set the Set ID.
          * - :py:attr:`~itype`
            - Get or set the EQ.0: *SET_SHELL ID
          * - :py:attr:`~offset`
            - Get or set the Offset distance, d, between sensor and the segment center. Where d>0 is along shell normal and d<0 is against shell normal.
          * - :py:attr:`~radius`
            - Get or set the Radius of sphere of the sensor.  Please see *DATABASE_CPM_SENSOR for sphere sensor.


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

    from database_pblast_sensor import DatabasePblastSensor

Property detail
---------------

.. py:property:: dt
   :type: float


   
   Get or set the Output interval.
















   ..
       !! processed by numpydoc !!

.. py:property:: binary
   :type: int


   
   Get or set the Flag for the binary file:
   EQ.3:   Data is written to the binary file binout.
















   ..
       !! processed by numpydoc !!

.. py:property:: id
   :type: int


   
   Get or set the Set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: itype
   :type: int


   
   Get or set the EQ.0: *SET_SHELL ID
   EQ.1: Shell ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: offset
   :type: float


   
   Get or set the Offset distance, d, between sensor and the segment center. Where d>0 is along shell normal and d<0 is against shell normal.
















   ..
       !! processed by numpydoc !!

.. py:property:: radius
   :type: float


   
   Get or set the Radius of sphere of the sensor.  Please see *DATABASE_CPM_SENSOR for sphere sensor.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DATABASE'


.. py:attribute:: subkeyword
   :value: 'PBLAST_SENSOR'






