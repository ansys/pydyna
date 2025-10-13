





:class:`SensorControl`
======================


.. py:class:: sensor_control.SensorControl(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SENSOR_CONTROL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SensorControl

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~cntlid`
            - Get or set the Control ID
          * - :py:attr:`~type`
            - Get or set the Entity to be controlled:
          * - :py:attr:`~typeid`
            - Get or set the ID of entity to be controlled if TYPE ≠ FUNCTION, LOADTHM, or POREAIR.  If TYPE = FUNCTION, see Remark 5.  For TYPE = LOADTHM, TYPEID is the node set for which the temperature boundary condition specified by either *LOAD_THERMAL_VARIABLE or *LOAD_THERMAL_VARIABLE_NODE will be controlled.  For TYPE = POREAIR, TYPEID is the ID of the part containing material with pore air.
          * - :py:attr:`~timeoff`
            - Get or set the Flag for offset of time in curve:
          * - :py:attr:`~nrep`
            - Get or set the Number of repeat of cycle of switches, SWITn, defined on the 2nd card.
          * - :py:attr:`~estyp`
            - Get or set the Type of element set to be controlled. With initial status set to  ON ,
          * - :py:attr:`~initstt`
            - Get or set the Initial status:
          * - :py:attr:`~swit1`
            - Get or set the ID of switch which will change the initial status after its condition is met.
          * - :py:attr:`~swit2`
            - Get or set the ID of nth switch which will change the status set by switch n-1 after its condition is met.
          * - :py:attr:`~swit3`
            - Get or set the ID of nth switch which will change the status set by switch n-1 after its condition is met.
          * - :py:attr:`~swit4`
            - Get or set the ID of nth switch which will change the status set by switch n-1 after its condition is met.
          * - :py:attr:`~swit5`
            - Get or set the ID of nth switch which will change the status set by switch n-1 after its condition is met.
          * - :py:attr:`~swit6`
            - Get or set the ID of nth switch which will change the status set by switch n-1 after its condition is met.
          * - :py:attr:`~swit7`
            - Get or set the ID of nth switch which will change the status set by switch n-1 after its condition is met.
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

    from sensor_control import SensorControl

Property detail
---------------

.. py:property:: cntlid
   :type: Optional[int]


   
   Get or set the Control ID
















   ..
       !! processed by numpydoc !!

.. py:property:: type
   :type: str


   
   Get or set the Entity to be controlled:
   EQ.AIRBAG:      *AIRBAG
   EQ.BAGVENTPOP:  to close/open the airbag venting holes (see remark 1)
   EQ.BELTPRET:    to fire belt pretensioner (see remark 2)
   EQ.BELTRETRA:   to lock belt retractor (see remark 2)
   EQ.BELTSLIP:    for one-way slip ring element (see remark 3)
   EQ.CONTACT:     *CONTACT
   EQ.CONTACT2D:   *CONTACT_2D
   EQ.CONSTRL: * CONSTRAINED_LOCAL
   EQ.CNRB: *CONSTRAINED_NODAL_RIGID_BODY
   EQ. CPM  *AIRBAG_PARTICLE
   EQ.DEF2RIG:     *DEFORMABLE_TO_RIGID_AUTOMATIC (see remark 4)
   EQ.ELESET: Element set, see ESTYP below.
   EQ.EM: EM solver
   EQ.FUNCTION:    *DEFINE_CURVE_FUNCTION (see remarks 5 & 6)
   EQ.JOINT:       *CONSTRAINED_JOINT
   EQ.JOINTSTIF:   *CONSTRAINED_JOINT_STIFFNESS
   EQ.M PRESSURE:  *LOAD_MOVING_PRESSURE
   EQ.POREAIR: *MAT_ADD_PORE_AIR
   EQ.PRESC-MOT:   *BOUNDARY_PRESCRIBED_MOTION
   EQ. PRESC-ORI : *BOUNDARY_PRESCRIBED_ORIENTATION_RIGID
   EQ.PRESSURE:    *LOAD_SEGMENT_SET
   EQ.PZBC: *BOUNDARY PZEPOT
   EQ.RWALL:       *RIGID_WALL
   EQ.SPC: *BOUNDARY_SPC
   EQ.BPWPN:       *BOUNDARY_PWP_NODE/SET_ID
















   ..
       !! processed by numpydoc !!

.. py:property:: typeid
   :type: Optional[int]


   
   Get or set the ID of entity to be controlled if TYPE ≠ FUNCTION, LOADTHM, or POREAIR.  If TYPE = FUNCTION, see Remark 5.  For TYPE = LOADTHM, TYPEID is the node set for which the temperature boundary condition specified by either *LOAD_THERMAL_VARIABLE or *LOAD_THERMAL_VARIABLE_NODE will be controlled.  For TYPE = POREAIR, TYPEID is the ID of the part containing material with pore air.
















   ..
       !! processed by numpydoc !!

.. py:property:: timeoff
   :type: int


   
   Get or set the Flag for offset of time in curve:
   EQ.0: No offset is applied.
   EQ.1: Offset the abscissa of the time-dependent curve by the time value at which the sensor is triggered.
   The curves affected when TIMEOFF = 1 are those specified in
   *LOAD_SEGMENT and *BOUNDARY_PRESCRIBED_MOTION when TYPE is PRESSURE and PRESC-MOT, respectively
















   ..
       !! processed by numpydoc !!

.. py:property:: nrep
   :type: int


   
   Get or set the Number of repeat of cycle of switches, SWITn, defined on the 2nd card.
   For example, a definition of SWITn like "601, 602, 601, 602, 601, 602" can be replaced by setting NREP to 2 and SWITn to "601, 602". Setting NREP = -1 repeats the cycle for infinite number of times. Default is 0.
















   ..
       !! processed by numpydoc !!

.. py:property:: estyp
   :type: str


   
   Get or set the Type of element set to be controlled. With initial status set to  ON ,
   all the elements included in set TYPEID can be eroded when the
   controller status is changed to  OFF . When TYPEID is not defined,
   all elements of type ESTYP in the whole system will be eroded.
   EQ. BEAM : Beam element set.
   EQ. DISC : Discrete element set
   EQ. SHELL : Thin shell element set
   EQ. SOLID : Solid element set
   EQ. TSHELL : Thick shell element set
















   ..
       !! processed by numpydoc !!

.. py:property:: initstt
   :type: str


   
   Get or set the Initial status:
   EQ.On:
   EQ.Off.
















   ..
       !! processed by numpydoc !!

.. py:property:: swit1
   :type: Optional[int]


   
   Get or set the ID of switch which will change the initial status after its condition is met.
















   ..
       !! processed by numpydoc !!

.. py:property:: swit2
   :type: Optional[int]


   
   Get or set the ID of nth switch which will change the status set by switch n-1 after its condition is met.
















   ..
       !! processed by numpydoc !!

.. py:property:: swit3
   :type: Optional[int]


   
   Get or set the ID of nth switch which will change the status set by switch n-1 after its condition is met.
















   ..
       !! processed by numpydoc !!

.. py:property:: swit4
   :type: Optional[int]


   
   Get or set the ID of nth switch which will change the status set by switch n-1 after its condition is met.
















   ..
       !! processed by numpydoc !!

.. py:property:: swit5
   :type: Optional[int]


   
   Get or set the ID of nth switch which will change the status set by switch n-1 after its condition is met.
















   ..
       !! processed by numpydoc !!

.. py:property:: swit6
   :type: Optional[int]


   
   Get or set the ID of nth switch which will change the status set by switch n-1 after its condition is met.
















   ..
       !! processed by numpydoc !!

.. py:property:: swit7
   :type: Optional[int]


   
   Get or set the ID of nth switch which will change the status set by switch n-1 after its condition is met.
















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
   :value: 'CONTROL'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





