





:class:`DatabaseFsiSensor`
==========================


.. py:class:: database_fsi_sensor.DatabaseFsiSensor(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DATABASE_FSI_SENSOR keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DatabaseFsiSensor

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~dtout`
            - Get or set the Output interval
          * - :py:attr:`~binary`
            - Get or set the Flag for binary output.  See remarks under "Output Files and Post-Processing" in Appendix O, "LS-DYNA MPP User Guide."
          * - :py:attr:`~dbfsi_id`
            - Get or set the Pressure-Sensor ID
          * - :py:attr:`~nid`
            - Get or set the Node ID,If NID=0 or blank, the sensor will be placed in the center of a four-sided segment defined by SEGMID.
          * - :py:attr:`~segmid`
            - Get or set the SegmentID (Lagrangian shell element ID) associated with the above node ID.
          * - :py:attr:`~offset`
            - Get or set the Offset distance between the pressure sensor and the Lagrangian segment surface.  If it is positive, it is on the side pointed to by the segment normal vector.
          * - :py:attr:`~nd1`
            - Get or set the Nodes defining the solid face for the solid element in the three-dimensional model
          * - :py:attr:`~nd2`
            - Get or set the Nodes defining the solid face for the solid element in the three-dimensional model
          * - :py:attr:`~nd3`
            - Get or set the Nodes defining the solid face for the solid element in the three-dimensional model


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

    from database_fsi_sensor import DatabaseFsiSensor

Property detail
---------------

.. py:property:: dtout
   :type: Optional[float]


   
   Get or set the Output interval
















   ..
       !! processed by numpydoc !!

.. py:property:: binary
   :type: int


   
   Get or set the Flag for binary output.  See remarks under "Output Files and Post-Processing" in Appendix O, "LS-DYNA MPP User Guide."
   EQ.1:   ASCII file is written:  This is the default for shared memory parallel (SMP) LS-DYNA executables.
   EQ.2:   Data written to a binary database binout, which contains data that would otherwise be output to the ASCII file.
   The ASCII file in this case is not created.  This is the default for MPP LS-DYNA executables.
   EQ.3:   ASCII file is written, and the data is also written to the binary database (NOTE: MPP LS-DYNA executables will only produce the binary database).
















   ..
       !! processed by numpydoc !!

.. py:property:: dbfsi_id
   :type: Optional[int]


   
   Get or set the Pressure-Sensor ID
















   ..
       !! processed by numpydoc !!

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Node ID,If NID=0 or blank, the sensor will be placed in the center of a four-sided segment defined by SEGMID.
















   ..
       !! processed by numpydoc !!

.. py:property:: segmid
   :type: Optional[int]


   
   Get or set the SegmentID (Lagrangian shell element ID) associated with the above node ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: offset
   :type: Optional[float]


   
   Get or set the Offset distance between the pressure sensor and the Lagrangian segment surface.  If it is positive, it is on the side pointed to by the segment normal vector.
















   ..
       !! processed by numpydoc !!

.. py:property:: nd1
   :type: Optional[int]


   
   Get or set the Nodes defining the solid face for the solid element in the three-dimensional model
   or shell side for the shell element in the two-dimensional model, from which the sensor is located.
   In three dimensions, if the solid face has 4 nodes, only the diagonal opposites ND1 and ND2 are required.
   If the solid face is triangular, a third node ND3 should be provided. In two dimensions, only ND1 and ND2 are required to define the shell side
















   ..
       !! processed by numpydoc !!

.. py:property:: nd2
   :type: Optional[int]


   
   Get or set the Nodes defining the solid face for the solid element in the three-dimensional model
   or shell side for the shell element in the two-dimensional model, from which the sensor is located.
   In three dimensions, if the solid face has 4 nodes, only the diagonal opposites ND1 and ND2 are required.
   If the solid face is triangular, a third node ND3 should be provided. In two dimensions, only ND1 and ND2 are required to define the shell side
















   ..
       !! processed by numpydoc !!

.. py:property:: nd3
   :type: Optional[int]


   
   Get or set the Nodes defining the solid face for the solid element in the three-dimensional model
   or shell side for the shell element in the two-dimensional model, from which the sensor is located.
   In three dimensions, if the solid face has 4 nodes, only the diagonal opposites ND1 and ND2 are required.
   If the solid face is triangular, a third node ND3 should be provided. In two dimensions, only ND1 and ND2 are required to define the shell side
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DATABASE'


.. py:attribute:: subkeyword
   :value: 'FSI_SENSOR'






