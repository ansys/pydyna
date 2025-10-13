





:class:`BoundaryFluxTrajectory`
===============================


.. py:class:: boundary_flux_trajectory.BoundaryFluxTrajectory(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_FLUX_TRAJECTORY keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryFluxTrajectory

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ssid`
            - Get or set the Segment set ID containing segments that are potentially heated
          * - :py:attr:`~pserod`
            - Get or set the Part set ID for updating boundary segments exposed to the environment as solid elements erode.
          * - :py:attr:`~nsid1`
            - Get or set the Node set defining the path of the weld source. The source travels
          * - :py:attr:`~spd1`
            - Get or set the Speed of the heat source on the weld trajectory
          * - :py:attr:`~nsid2`
            - Get or set the Node or segment set containing information for the weld source aiming direction:
          * - :py:attr:`~spd2`
            - Get or set the Speed of reference point in NSID2 (ignored unless NSID2 > 0)
          * - :py:attr:`~relvel`
            - Get or set the Defines if SPD1 and SPD2 are relative or absolute speeds in
          * - :py:attr:`~erod`
            - Get or set the Flag for updating boundary segments exposed to the
          * - :py:attr:`~loc`
            - Get or set the For a thick thermal shell, the flux will be applied to the surface
          * - :py:attr:`~lcrot`
            - Get or set the Load curve defining the rotation (angle in degrees) of weld source
          * - :py:attr:`~lclat`
            - Get or set the Load curve for lateral offset of weld source as function of time. See Remark 2.
          * - :py:attr:`~iform`
            - Get or set the Geometry description for energy rate density distribution (see Remark 3):
          * - :py:attr:`~lctim`
            - Get or set the Load curve ID for flux energy input rate multiplier q1(t) as a
          * - :py:attr:`~q`
            - Get or set the Base energy input rate Qb [energy/time], see Remark 4.
          * - :py:attr:`~lcinc`
            - Get or set the Load curve ID for flux energy input rate multiplier q2(a) as a. The value of the angle α range from 0° (segment normal and heat source point in the same direction) to 180° (segment normal and heat source point in opposite directions).
          * - :py:attr:`~enfor`
            - Get or set the Flag for heat input enforcement option (see Remark 4)
          * - :py:attr:`~p1`
            - Get or set the Parameters defining flux geometry, depending on field IFORM.
          * - :py:attr:`~p2`
            - Get or set the Parameters defining flux geometry, depending on field IFORM.
          * - :py:attr:`~p3`
            - Get or set the Parameters defining flux geometry, depending on field IFORM.
          * - :py:attr:`~p4`
            - Get or set the Parameters defining flux geometry, depending on field IFORM.
          * - :py:attr:`~p5`
            - Get or set the Parameters defining flux geometry, depending on field IFORM.
          * - :py:attr:`~p6`
            - Get or set the Parameters defining flux geometry, depending on field IFORM.
          * - :py:attr:`~p7`
            - Get or set the Parameters defining flux geometry, depending on field IFORM.
          * - :py:attr:`~p8`
            - Get or set the Parameters defining flux geometry, depending on field IFORM.
          * - :py:attr:`~tx`
            - Get or set the Weld beam direction vector in global coordinates (NSID2 = 0 only).
          * - :py:attr:`~ty`
            - Get or set the Weld beam direction vector in global coordinates (NSID2 = 0 only).
          * - :py:attr:`~tz`
            - Get or set the Weld beam direction vector in global coordinates (NSID2 = 0 only).


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

    from boundary_flux_trajectory import BoundaryFluxTrajectory

Property detail
---------------

.. py:property:: ssid
   :type: Optional[int]


   
   Get or set the Segment set ID containing segments that are potentially heated
   by surface heat source (flux)..
















   ..
       !! processed by numpydoc !!

.. py:property:: pserod
   :type: Optional[int]


   
   Get or set the Part set ID for updating boundary segments exposed to the environment as solid elements erode.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsid1
   :type: Optional[int]


   
   Get or set the Node set defining the path of the weld source. The source travels
   along the path at speed SPD1. The nodes are traversed according
   to their ordering in the node set. See Remark 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: spd1
   :type: float


   
   Get or set the Speed of the heat source on the weld trajectory
   GT.0.0: constant speed
   LT.0.0: |SPD1| is a load curve ID defining weld speed as a function of time.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsid2
   :type: Optional[int]


   
   Get or set the Node or segment set containing information for the weld source aiming direction:
   GT.0: NSID2 together with SPD2 define a curve in the same
   way that NSID1 and SPD1 define a curve. Aiming direction
   is taken to be the vector pointing from the current
   position along NSID2 (for example your hand holding
   the torch) to the current position on NSID1 (the weld source).
   EQ.0: beam aiming direction is (TX, TY, TZ) input on Card 5.
















   ..
       !! processed by numpydoc !!

.. py:property:: spd2
   :type: float


   
   Get or set the Speed of reference point in NSID2 (ignored unless NSID2 > 0)
   GT.0: constant speed
   LT.0: |SPD2| is a load curve ID defining weld speed as a
   function of time.
















   ..
       !! processed by numpydoc !!

.. py:property:: relvel
   :type: int


   
   Get or set the Defines if SPD1 and SPD2 are relative or absolute speeds in
   coupled simulations
   EQ.0: absolute speeds
   EQ.1: relative speeds with respect to underlying structure.
















   ..
       !! processed by numpydoc !!

.. py:property:: erod
   :type: int


   
   Get or set the Flag for updating boundary segments exposed to the
   environment as solid elements (defined in PSEROD) erode; see Remark 5.
   EQ.0: no propagation onto new segments
   EQ.1: propagation onto new segments.
















   ..
       !! processed by numpydoc !!

.. py:property:: loc
   :type: Optional[int]


   
   Get or set the For a thick thermal shell, the flux will be applied to the surface
   identified by LOC. See field THSHEL on the *CONTROL_SHELL keyword.
   EQ.-1: lower surface of thermal shell element
   EQ.0: middle surface of thermal shell element
   EQ.1: upper surface of thermal shell element.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcrot
   :type: Optional[int]


   
   Get or set the Load curve defining the rotation (angle in degrees) of weld source
   around the trajectory as function of time. See Remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: lclat
   :type: Optional[int]


   
   Get or set the Load curve for lateral offset of weld source as function of time. See Remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: iform
   :type: Optional[int]


   
   Get or set the Geometry description for energy rate density distribution (see Remark 3):
   EQ.1: double elliptic with constant density
   EQ.2: double elliptic with Gaussian distribution
   EQ.3: user defined function.
















   ..
       !! processed by numpydoc !!

.. py:property:: lctim
   :type: Optional[int]


   
   Get or set the Load curve ID for flux energy input rate multiplier q1(t) as a
   function of time, see Remark 4.
   EQ.0: use constant multiplier value q1(t) = 1.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: q
   :type: float


   
   Get or set the Base energy input rate Qb [energy/time], see Remark 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcinc
   :type: Optional[int]


   
   Get or set the Load curve ID for flux energy input rate multiplier q2(a) as a. The value of the angle α range from 0° (segment normal and heat source point in the same direction) to 180° (segment normal and heat source point in opposite directions).
   function of inclination angle a, see Remark 4
   EQ.0: use constant multiplier value q2(a) = 1.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: enfor
   :type: int


   
   Get or set the Flag for heat input enforcement option (see Remark 4)
   EQ.0: no additional scaling of heat source
   EQ.1: account for inclination angle a of heat source by energy
   input rate multiplier q3(a) = cos(a)
   EQ.2: scale the nodal fluxes by a multiplier q4(t) such that the
   resulting heat input equals the user input Q(t) =Qb*q1(t)
   EQ.3: apply option 1 and 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: p1
   :type: Optional[float]


   
   Get or set the Parameters defining flux geometry, depending on field IFORM.
   See Remark 3 for details.
















   ..
       !! processed by numpydoc !!

.. py:property:: p2
   :type: Optional[float]


   
   Get or set the Parameters defining flux geometry, depending on field IFORM.
   See Remark 3 for details.
















   ..
       !! processed by numpydoc !!

.. py:property:: p3
   :type: Optional[float]


   
   Get or set the Parameters defining flux geometry, depending on field IFORM.
   See Remark 3 for details.
















   ..
       !! processed by numpydoc !!

.. py:property:: p4
   :type: Optional[float]


   
   Get or set the Parameters defining flux geometry, depending on field IFORM.
   See Remark 3 for details.
















   ..
       !! processed by numpydoc !!

.. py:property:: p5
   :type: Optional[float]


   
   Get or set the Parameters defining flux geometry, depending on field IFORM.
   See Remark 3 for details.
















   ..
       !! processed by numpydoc !!

.. py:property:: p6
   :type: Optional[float]


   
   Get or set the Parameters defining flux geometry, depending on field IFORM.
   See Remark 3 for details.
















   ..
       !! processed by numpydoc !!

.. py:property:: p7
   :type: Optional[float]


   
   Get or set the Parameters defining flux geometry, depending on field IFORM.
   See Remark 3 for details.
















   ..
       !! processed by numpydoc !!

.. py:property:: p8
   :type: Optional[float]


   
   Get or set the Parameters defining flux geometry, depending on field IFORM.
   See Remark 3 for details.
















   ..
       !! processed by numpydoc !!

.. py:property:: tx
   :type: Optional[float]


   
   Get or set the Weld beam direction vector in global coordinates (NSID2 = 0 only).
















   ..
       !! processed by numpydoc !!

.. py:property:: ty
   :type: Optional[float]


   
   Get or set the Weld beam direction vector in global coordinates (NSID2 = 0 only).
















   ..
       !! processed by numpydoc !!

.. py:property:: tz
   :type: Optional[float]


   
   Get or set the Weld beam direction vector in global coordinates (NSID2 = 0 only).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'FLUX_TRAJECTORY'






