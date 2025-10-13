





:class:`BoundaryThermalWeldTrajectory`
======================================


.. py:class:: boundary_thermal_weld_trajectory.BoundaryThermalWeldTrajectory(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_THERMAL_WELD_TRAJECTORY keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryThermalWeldTrajectory

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID or Part Set ID of solids or shells to which weld source is applied.
          * - :py:attr:`~ptyp`
            - Get or set the PID type:
          * - :py:attr:`~nsid1`
            - Get or set the Node set ID containing the path (weld trajectory) information for the weld source movement.  A sorted node set is requested. The order defines the weld path and the direction (see Remark 1).
          * - :py:attr:`~spd1`
            - Get or set the Speed of the heat source on the weld trajectory
          * - :py:attr:`~nsid2`
            - Get or set the ID of second node set or segment setcontaining information for the weld source aiming direction (see Remark 2)
          * - :py:attr:`~spd2`
            - Get or set the Speed of reference point in NSID2 (ignored unless NSID2 > 0)
          * - :py:attr:`~ncyc`
            - Get or set the Number of substeps for subcycling in evaluation of boundary condition. Allows thermal dumping (see Remark 3).
          * - :py:attr:`~relvel`
            - Get or set the Defines if VEL1 and VEL2 are relative or absolute velocities in coupled simulations
          * - :py:attr:`~iform`
            - Get or set the Geometry description for energy rate density distribution (see Remark 4):
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID for weld energy input rate vs. time
          * - :py:attr:`~q`
            - Get or set the Curve multiplier for weld energy input rate [energy/time]
          * - :py:attr:`~lcrot`
            - Get or set the Load curve defining the rotation (angle in degree) of weld source around the trajectory as function of time(see Remark 2).
          * - :py:attr:`~lcmov`
            - Get or set the Load curve for offset of weld source in direction of the weld beam as function of time (see Remark 2).
          * - :py:attr:`~lclat`
            - Get or set the Load curve for lateral offset of weld sourceas function of time (see Remark 2)
          * - :py:attr:`~disc`
            - Get or set the Resolution for accurate integration, parameter defines edge length for integration cubes.  Default is 5% of weld pool depth.
          * - :py:attr:`~enfor`
            - Get or set the Flag for heat input enforcement option.  If set, the nodal heat input is scaled
          * - :py:attr:`~p1`
            - Get or set the Parameters defining for weld pool geometry, depending on parameter IFORM.See Remark 4 for details.
          * - :py:attr:`~p2`
            - Get or set the Parameters defining for weld pool geometry, depending on parameter IFORM.See Remark 4 for details.
          * - :py:attr:`~p3`
            - Get or set the Parameters defining for weld pool geometry, depending on parameter IFORM.See Remark 4 for details.
          * - :py:attr:`~p4`
            - Get or set the Parameters defining for weld pool geometry, depending on parameter IFORM.See Remark 4 for details.
          * - :py:attr:`~p5`
            - Get or set the Parameters defining for weld pool geometry, depending on parameter IFORM.See Remark 4 for details.
          * - :py:attr:`~p6`
            - Get or set the Parameters defining for weld pool geometry, depending on parameter IFORM.See Remark 4 for details
          * - :py:attr:`~p7`
            - Get or set the Parameters defining for weld pool geometry, depending on parameter IFORM.See Remark 4 for details.
          * - :py:attr:`~p8`
            - Get or set the Parameters defining for weld pool geometry, depending on parameter IFORM.See Remark 4 for details.
          * - :py:attr:`~tx`
            - Get or set the Weld beam direction vector in global coordinates (SID2 = 0 only).
          * - :py:attr:`~ty`
            - Get or set the Weld beam direction vector in global coordinates (SID2 = 0 only).
          * - :py:attr:`~tz`
            - Get or set the Weld beam direction vector in global coordinates (SID2 = 0 only).


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

    from boundary_thermal_weld_trajectory import BoundaryThermalWeldTrajectory

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID or Part Set ID of solids or shells to which weld source is applied.
















   ..
       !! processed by numpydoc !!

.. py:property:: ptyp
   :type: int


   
   Get or set the PID type:
   EQ.1: PID defines a single part ID (default),
   EQ.2: PID defines a part set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsid1
   :type: int


   
   Get or set the Node set ID containing the path (weld trajectory) information for the weld source movement.  A sorted node set is requested. The order defines the weld path and the direction (see Remark 1).
















   ..
       !! processed by numpydoc !!

.. py:property:: spd1
   :type: Optional[float]


   
   Get or set the Speed of the heat source on the weld trajectory
   GT.0.0: constant speed
   LT.0.0 : is a load curve ID defining weld speed as a function of  time.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsid2
   :type: Optional[int]


   
   Get or set the ID of second node set or segment setcontaining information for the weld source aiming direction (see Remark 2)
   GT.0:   SID2 refers to a sorted node set, the order of which defines the direction of the trajectory. The heat source is aimed from current position in SID2to current position in the weld trajectory.
   EQ.0:   beam aiming direction is (tx, ty, tz) input on optional card4.
   LT.0:   |SID2| is a segment set. The heat source is aiming in normal direction to segments in the set.
















   ..
       !! processed by numpydoc !!

.. py:property:: spd2
   :type: Optional[float]


   
   Get or set the Speed of reference point in NSID2 (ignored unless NSID2 > 0)
   GT.0:   constant speed
   LT.0 : is a load curve ID defining weld speed as a function of  time.
















   ..
       !! processed by numpydoc !!

.. py:property:: ncyc
   :type: int


   
   Get or set the Number of substeps for subcycling in evaluation of boundary condition. Allows thermal dumping (see Remark 3).
















   ..
       !! processed by numpydoc !!

.. py:property:: relvel
   :type: int


   
   Get or set the Defines if VEL1 and VEL2 are relative or absolute velocities in coupled simulations
   EQ.0:   absolute velocities
   EQ.1:   relative velocities with respect to underlying structure.
















   ..
       !! processed by numpydoc !!

.. py:property:: iform
   :type: int


   
   Get or set the Geometry description for energy rate density distribution (see Remark 4):
   EQ.1: Goldak-type heat source
   EQ.2: double ellipsoidal heat source with constant density
   EQ.3: double conical heat source with constant density
   EQ.4: frustum-shaped heat source with constant density.
   EQ.5: user-defined function
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID for weld energy input rate vs. time
   EQ.0: use constant multiplier value Q.
















   ..
       !! processed by numpydoc !!

.. py:property:: q
   :type: Optional[float]


   
   Get or set the Curve multiplier for weld energy input rate [energy/time]
   LT.0:   take absolute value and accurate integration of heat using integration cells with edge length DISC
















   ..
       !! processed by numpydoc !!

.. py:property:: lcrot
   :type: Optional[int]


   
   Get or set the Load curve defining the rotation (angle in degree) of weld source around the trajectory as function of time(see Remark 2).
















   ..
       !! processed by numpydoc !!

.. py:property:: lcmov
   :type: Optional[int]


   
   Get or set the Load curve for offset of weld source in direction of the weld beam as function of time (see Remark 2).
















   ..
       !! processed by numpydoc !!

.. py:property:: lclat
   :type: Optional[int]


   
   Get or set the Load curve for lateral offset of weld sourceas function of time (see Remark 2)
















   ..
       !! processed by numpydoc !!

.. py:property:: disc
   :type: Optional[float]


   
   Get or set the Resolution for accurate integration, parameter defines edge length for integration cubes.  Default is 5% of weld pool depth.
















   ..
       !! processed by numpydoc !!

.. py:property:: enfor
   :type: Optional[int]


   
   Get or set the Flag for heat input enforcement option.  If set, the nodal heat input is scaled
   such that the resulting heat inputs equals the user input as given by Q and LCID.
















   ..
       !! processed by numpydoc !!

.. py:property:: p1
   :type: Optional[float]


   
   Get or set the Parameters defining for weld pool geometry, depending on parameter IFORM.See Remark 4 for details.
















   ..
       !! processed by numpydoc !!

.. py:property:: p2
   :type: Optional[float]


   
   Get or set the Parameters defining for weld pool geometry, depending on parameter IFORM.See Remark 4 for details.
















   ..
       !! processed by numpydoc !!

.. py:property:: p3
   :type: Optional[float]


   
   Get or set the Parameters defining for weld pool geometry, depending on parameter IFORM.See Remark 4 for details.
















   ..
       !! processed by numpydoc !!

.. py:property:: p4
   :type: Optional[float]


   
   Get or set the Parameters defining for weld pool geometry, depending on parameter IFORM.See Remark 4 for details.
















   ..
       !! processed by numpydoc !!

.. py:property:: p5
   :type: Optional[float]


   
   Get or set the Parameters defining for weld pool geometry, depending on parameter IFORM.See Remark 4 for details.
















   ..
       !! processed by numpydoc !!

.. py:property:: p6
   :type: Optional[float]


   
   Get or set the Parameters defining for weld pool geometry, depending on parameter IFORM.See Remark 4 for details
















   ..
       !! processed by numpydoc !!

.. py:property:: p7
   :type: Optional[float]


   
   Get or set the Parameters defining for weld pool geometry, depending on parameter IFORM.See Remark 4 for details.
















   ..
       !! processed by numpydoc !!

.. py:property:: p8
   :type: Optional[float]


   
   Get or set the Parameters defining for weld pool geometry, depending on parameter IFORM.See Remark 4 for details.
















   ..
       !! processed by numpydoc !!

.. py:property:: tx
   :type: Optional[float]


   
   Get or set the Weld beam direction vector in global coordinates (SID2 = 0 only).
















   ..
       !! processed by numpydoc !!

.. py:property:: ty
   :type: Optional[float]


   
   Get or set the Weld beam direction vector in global coordinates (SID2 = 0 only).
















   ..
       !! processed by numpydoc !!

.. py:property:: tz
   :type: Optional[float]


   
   Get or set the Weld beam direction vector in global coordinates (SID2 = 0 only).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'THERMAL_WELD_TRAJECTORY'






