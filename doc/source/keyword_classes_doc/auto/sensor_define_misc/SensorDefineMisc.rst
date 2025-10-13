





:class:`SensorDefineMisc`
=========================


.. py:class:: sensor_define_misc.SensorDefineMisc(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SENSOR_DEFINE_MISC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SensorDefineMisc

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sensid`
            - Get or set the Sensor ID.
          * - :py:attr:`~mtype`
            - Get or set the Entity to be traced:
          * - :py:attr:`~i0`
            - Get or set the See MTYPE
          * - :py:attr:`~i1`
            - Get or set the See MTYPE.
          * - :py:attr:`~i2`
            - Get or set the See MTYPE.
          * - :py:attr:`~i3`
            - Get or set the See MTYPE
          * - :py:attr:`~i4`
            - Get or set the See MTYPE.
          * - :py:attr:`~i5`
            - Get or set the See MTYPE.
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

    from sensor_define_misc import SensorDefineMisc

Property detail
---------------

.. py:property:: sensid
   :type: Optional[int]


   
   Get or set the Sensor ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: mtype
   :type: str


   
   Get or set the Entity to be traced:
   EQ.ANGLE:  Angular accelerometer sensor tracing the angle between two lines.
   The fields I1 and I2 are node numbers defining the 1st line, while I3
   and I4 are node numbers defining the 2nd line.
   EQ.BNDOUT:      Boundary condition energy as reported in file bndout?  I1 is the ID as defined in *BOUNDARY_RESCRIBED_MOTION
   EQ.CURVE:       The value of a time-dependent curve defined by *DEFINE_CURVE_FUNCTION or *DEFINE_CURVE.  I1 is the curve ID.
   EQ.CVBAG: Information reported in ABSTAT for control volume airbag I1, including
   I0.EQ.TEMP: airbag temperature
   I0.EQ.VOL: airbag volume
   EQ.ICVOL:       Information reported in ICVOUT for incompressible control volume I1, see *DEFINE_CONTROL_VOLUME, including
   I0.EQ.PRES:     Temperature of control volume
   I0.EQ.VOL : Volume of control volume
   EQ.MATSUM:      Information reported in MATSUM for part set I1, including.I0.eq.KINETIC: kinetic energy;I0.eq.INTERNAL: internal energy;I0.eq.ERODEKE: eroded kinetic energy;I0.eq.ERODEIE: eroded internal energy
   EQ.NFAILE: Number of failed elements of type I0 in set I1 will be traced. I0, element type, can be  SOLID
   for solid elements,  SHELL  for thin shell elements,  TSHELL  for thick shell elements,
   BEAM  for beam elements or  DISC  for discrete elements. I1 is the related element set
   number. If undefined, the failure of all elements of type I0 will be traced
   EQ.RETRACTOR: The seatbelt retractor payout rate is traced. I1 is the retractor ID.
   EQ.RIGIDBODY: Accelerometer sensor tracing the kinematics of
   a rigid body with id I1. The I2 field specifies which kinematical component is to be traced.
   It may be set to  TX ,  TY , or  TZ  for X, Y, and Z translations and to  RX ,  RY , or  RZ
   for the X, Y, and Z components of the rotation. The I3 field specifies the kinematics type:  D
   for displacement,  V  for velocity and  A  for acceleration. Output is calculated with respect
   to the global coordinate system when the I4 field is set to  0 , its default value; the local
   rigid-body coordinate system is used when I4 is set to  1 .
   EQ.TIME:  The current analysis time is traced.
















   ..
       !! processed by numpydoc !!

.. py:property:: i0
   :type: Optional[str]


   
   Get or set the See MTYPE
















   ..
       !! processed by numpydoc !!

.. py:property:: i1
   :type: Optional[str]


   
   Get or set the See MTYPE.
















   ..
       !! processed by numpydoc !!

.. py:property:: i2
   :type: Optional[str]


   
   Get or set the See MTYPE.
















   ..
       !! processed by numpydoc !!

.. py:property:: i3
   :type: Optional[str]


   
   Get or set the See MTYPE
















   ..
       !! processed by numpydoc !!

.. py:property:: i4
   :type: Optional[str]


   
   Get or set the See MTYPE.
















   ..
       !! processed by numpydoc !!

.. py:property:: i5
   :type: Optional[str]


   
   Get or set the See MTYPE.
















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
   :value: 'DEFINE_MISC'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





