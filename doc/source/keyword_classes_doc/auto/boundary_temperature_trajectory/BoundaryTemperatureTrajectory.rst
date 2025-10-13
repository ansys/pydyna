





:class:`BoundaryTemperatureTrajectory`
======================================


.. py:class:: boundary_temperature_trajectory.BoundaryTemperatureTrajectory(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_TEMPERATURE_TRAJECTORY keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryTemperatureTrajectory

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID or part set ID to what the temperature boundary condition will be applied on.
          * - :py:attr:`~pype`
            - Get or set the PID type:
          * - :py:attr:`~nsid1`
            - Get or set the Node set defining the path of the moving volume.  The moving volume travels along the path at speed SPD1.
          * - :py:attr:`~spd1`
            - Get or set the Speed of the moving volume on the trajectory:
          * - :py:attr:`~nsid2`
            - Get or set the Node or segment set that specifies the orientation of the moving volume's center axis.
          * - :py:attr:`~spd2`
            - Get or set the Speed of reference point in NSID2 (ignored unless NSID2 > 0)
          * - :py:attr:`~relvel`
            - Get or set the Defines if SPD1 and SPD2 are relative or absolute speeds in thermo-mechanical coupled analysis.
          * - :py:attr:`~iform`
            - Get or set the Geometric description of the moving volume:
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID for temperature as a function of time
          * - :py:attr:`~tmult`
            - Get or set the Curve multiplier for temperature.
          * - :py:attr:`~lcrot`
            - Get or set the Load curve defining the rotation angle (in degrees) of the moving volume around the trajectory as a function of time.  See Remark 2.
          * - :py:attr:`~lcmov`
            - Get or set the Load curve defining the offset of the moving volume along its center axis as a function of time.  See Remark 2.
          * - :py:attr:`~lclat`
            - Get or set the Load curve defining the lateral offset of the moving volume as a function of time.  See Remark 2.
          * - :py:attr:`~p1`
            - Get or set the Parameters defining the moving volume's geometry.
          * - :py:attr:`~p2`
            - Get or set the Parameters defining the moving volume's geometry.
          * - :py:attr:`~p3`
            - Get or set the Parameters defining the moving volume's geometry.
          * - :py:attr:`~p4`
            - Get or set the Parameters defining the moving volume's geometry.
          * - :py:attr:`~p5`
            - Get or set the Parameters defining the moving volume's geometry.
          * - :py:attr:`~p6`
            - Get or set the Parameters defining the moving volume's geometry.
          * - :py:attr:`~p7`
            - Get or set the Parameters defining the moving volume's geometry.
          * - :py:attr:`~p8`
            - Get or set the Parameters defining the moving volume's geometry.
          * - :py:attr:`~tx`
            - Get or set the Orientation vector of the moving volume's center axis in global coordinates (NSID2 = 0 only).
          * - :py:attr:`~ty`
            - Get or set the Orientation vector of the moving volume's center axis in global coordinates (NSID2 = 0 only).
          * - :py:attr:`~tz`
            - Get or set the Orientation vector of the moving volume's center axis in global coordinates (NSID2 = 0 only).


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

    from boundary_temperature_trajectory import BoundaryTemperatureTrajectory

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID or part set ID to what the temperature boundary condition will be applied on.
















   ..
       !! processed by numpydoc !!

.. py:property:: pype
   :type: int


   
   Get or set the PID type:
   EQ.1:   part ID.
   EQ.2: part set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsid1
   :type: Optional[int]


   
   Get or set the Node set defining the path of the moving volume.  The moving volume travels along the path at speed SPD1.
   The nodes are traversed according to their order in the node set.  See Remark 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: spd1
   :type: Optional[float]


   
   Get or set the Speed of the moving volume on the trajectory:
   GT.0.0: Constant speed
   LT.0.0:  is a load curve ID defining the speed as a function of time.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsid2
   :type: Optional[int]


   
   Get or set the Node or segment set that specifies the orientation of the moving volume's center axis.
   GT.0:   NSID2 together with SPD2 define a curve in the same way that NSID1 and SPD1 define a curve.
   Orientation of the moving volume's center axis is defined as a vector pointing from the current position on NSID2 to the current position on NSID1.
   EQ.0:   The moving volume's center axis is oriented as  input on Card?4.
   LT.0:    specifies a segment set.  The moving volume's center axis is aligned with normals to segments in this set.
   To ensure that the axis orientation can be unambiguously determined at each point of the nodal path,
   LS-DYNA requires that each pair of consecutive nodes in NSID1 must both be in at least one segment of.
   When the center of the moving volume is.
   on a node that is part of more than one segment in |NSID2|, the direction is determined by averaging the adjacent segment normals.
















   ..
       !! processed by numpydoc !!

.. py:property:: spd2
   :type: Optional[float]


   
   Get or set the Speed of reference point in NSID2 (ignored unless NSID2 > 0)
   GT.0:   constant speed
   LT.0:    |SPD2| is a load curve ID defining the speed as a function of time..
















   ..
       !! processed by numpydoc !!

.. py:property:: relvel
   :type: int


   
   Get or set the Defines if SPD1 and SPD2 are relative or absolute speeds in thermo-mechanical coupled analysis.
   EQ.0:   absolute speeds
   EQ.1:   relative speeds with respect to underlying structures.
















   ..
       !! processed by numpydoc !!

.. py:property:: iform
   :type: int


   
   Get or set the Geometric description of the moving volume:
   EQ.1:   cylindrical volume
   EQ.2:   rectangular prism volume.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID for temperature as a function of time
   EQ.0:   temperature is a constant defined by the value TMULT.
















   ..
       !! processed by numpydoc !!

.. py:property:: tmult
   :type: Optional[float]


   
   Get or set the Curve multiplier for temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcrot
   :type: Optional[int]


   
   Get or set the Load curve defining the rotation angle (in degrees) of the moving volume around the trajectory as a function of time.  See Remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcmov
   :type: Optional[int]


   
   Get or set the Load curve defining the offset of the moving volume along its center axis as a function of time.  See Remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: lclat
   :type: Optional[int]


   
   Get or set the Load curve defining the lateral offset of the moving volume as a function of time.  See Remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: p1
   :type: Optional[float]


   
   Get or set the Parameters defining the moving volume's geometry.
   The meaning of each parameter depends on field IFORM.  See Remark 3 for details.
















   ..
       !! processed by numpydoc !!

.. py:property:: p2
   :type: Optional[float]


   
   Get or set the Parameters defining the moving volume's geometry.
   The meaning of each parameter depends on field IFORM.  See Remark 3 for details.
















   ..
       !! processed by numpydoc !!

.. py:property:: p3
   :type: Optional[float]


   
   Get or set the Parameters defining the moving volume's geometry.
   The meaning of each parameter depends on field IFORM.  See Remark 3 for details.
















   ..
       !! processed by numpydoc !!

.. py:property:: p4
   :type: Optional[float]


   
   Get or set the Parameters defining the moving volume's geometry.
   The meaning of each parameter depends on field IFORM.  See Remark 3 for details.
















   ..
       !! processed by numpydoc !!

.. py:property:: p5
   :type: Optional[float]


   
   Get or set the Parameters defining the moving volume's geometry.
   The meaning of each parameter depends on field IFORM.  See Remark 3 for details.
















   ..
       !! processed by numpydoc !!

.. py:property:: p6
   :type: Optional[float]


   
   Get or set the Parameters defining the moving volume's geometry.
   The meaning of each parameter depends on field IFORM.  See Remark 3 for details.
















   ..
       !! processed by numpydoc !!

.. py:property:: p7
   :type: Optional[float]


   
   Get or set the Parameters defining the moving volume's geometry.
   The meaning of each parameter depends on field IFORM.  See Remark 3 for details.
















   ..
       !! processed by numpydoc !!

.. py:property:: p8
   :type: Optional[float]


   
   Get or set the Parameters defining the moving volume's geometry.
   The meaning of each parameter depends on field IFORM.  See Remark 3 for details.
















   ..
       !! processed by numpydoc !!

.. py:property:: tx
   :type: Optional[float]


   
   Get or set the Orientation vector of the moving volume's center axis in global coordinates (NSID2 = 0 only).
















   ..
       !! processed by numpydoc !!

.. py:property:: ty
   :type: Optional[float]


   
   Get or set the Orientation vector of the moving volume's center axis in global coordinates (NSID2 = 0 only).
















   ..
       !! processed by numpydoc !!

.. py:property:: tz
   :type: Optional[float]


   
   Get or set the Orientation vector of the moving volume's center axis in global coordinates (NSID2 = 0 only).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'TEMPERATURE_TRAJECTORY'






