





:class:`SensorDefineForce`
==========================


.. py:class:: sensor_define_force.SensorDefineForce(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SENSOR_DEFINE_FORCE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SensorDefineForce

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sensid`
            - Get or set the Sensor ID.
          * - :py:attr:`~ftype`
            - Get or set the Force type.
          * - :py:attr:`~typeid`
            - Get or set the ID defined in the associated KEYWORD command
          * - :py:attr:`~vid`
            - Get or set the Vector along which the forces is measured.
          * - :py:attr:`~crd`
            - Get or set the Coordinate system, defined by *DEFINE_COORDINATE_NODES, to which VECT is attached.
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

    from sensor_define_force import SensorDefineForce

Property detail
---------------

.. py:property:: sensid
   :type: Optional[int]


   
   Get or set the Sensor ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: ftype
   :type: str


   
   Get or set the Force type.
















   ..
       !! processed by numpydoc !!

.. py:property:: typeid
   :type: Optional[int]


   
   Get or set the ID defined in the associated KEYWORD command
















   ..
       !! processed by numpydoc !!

.. py:property:: vid
   :type: Optional[str]


   
   Get or set the Vector along which the forces is measured.
   EQ.X:x-direction in coordinate system CRD.
   EQ.Y:y-direction in coordinate system CRD.
   EQ.Z:z-direction in coordinate system CRD.
   EQ.XL:  x-direction in the local coordinate system, in JOINTSTIF only.
   EQ.YL:  y - direction in the local coordinate system, in JOINTSTIF only.
   EQ.ZL : z - direction in the local coordinate system, in JOINTSTIF only.
   EQ.M: Force magnitude.
   EQ.XMOMENT:     x-direction moment for JOINT.
   EQ.YMOMENT:     y-direction moment for JOINT.
   EQ.ZMOMENT:     z-direction moment for JOINT.
   EQ.XLMOMENT:    x-direction moment for the local coordinate system, in JOINTSTIF only.
   EQ.YLMOMENT:    y - direction moment for the local coordinate system, in JOINTSTIF only.
   EQ.ZLMOMENT : z - direction moment for the local coordinate system, in JOINTSTIF only.
   EQ.MMOMENT: Moment magnitude for JOINT, JOINTSTIF, PRESC-MOT or SPC.
   EQ.n:           Vector ID n in coordinate system CRD.
















   ..
       !! processed by numpydoc !!

.. py:property:: crd
   :type: Optional[int]


   
   Get or set the Coordinate system, defined by *DEFINE_COORDINATE_NODES, to which VECT is attached.
















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
   :value: 'DEFINE_FORCE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





