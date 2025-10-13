





:class:`MatSeismicIsolator`
===========================


.. py:class:: mat_seismic_isolator.MatSeismicIsolator(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_SEISMIC_ISOLATOR keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatSeismicIsolator

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number has to be chosen.
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~a`
            - Get or set the Nondimensional variable.
          * - :py:attr:`~beta`
            - Get or set the Nondimensional variable.
          * - :py:attr:`~gamma`
            - Get or set the Nondimensional variable.
          * - :py:attr:`~dispy`
            - Get or set the Yield displacement (length units - must be > 0.0).
          * - :py:attr:`~stiffv`
            - Get or set the Vertical stiffness (force/length units).
          * - :py:attr:`~itype`
            - Get or set the Type:
          * - :py:attr:`~preload`
            - Get or set the Vertical preload not explicitly modelled (force units).
          * - :py:attr:`~damp`
            - Get or set the Damping ratio (nondimensional).
          * - :py:attr:`~mx1`
            - Get or set the Moment factor at ends 1 and 2 in local X-direction.
          * - :py:attr:`~mx2`
            - Get or set the Moment factor at ends 1 and 2 in local X-direction.
          * - :py:attr:`~my1`
            - Get or set the Moment factor at ends 1 and 2 in local Y-direction.
          * - :py:attr:`~my2`
            - Get or set the Moment factor at ends 1 and 2 in local Y-direction.
          * - :py:attr:`~cde`
            - Get or set the Viscous damping coefficient (ITYPE=1, 3 or 4).
          * - :py:attr:`~iextra`
            - Get or set the If IEXTRA = 1, optional Card 8 will be read.
          * - :py:attr:`~fmax`
            - Get or set the Maximum friction coefficient (dynamic).
          * - :py:attr:`~delf`
            - Get or set the Difference between maximum friction and static friction coefficient.
          * - :py:attr:`~afric`
            - Get or set the Velocity multiplier in sliding friction equation (time/length units).
          * - :py:attr:`~radx`
            - Get or set the Radius for sliding in local X direction.
          * - :py:attr:`~rady`
            - Get or set the Radius for sliding in local Y direction.
          * - :py:attr:`~radb`
            - Get or set the Radius of retaining ring.
          * - :py:attr:`~stiffl`
            - Get or set the Stiffness for lateral contact against the retaining ring, default is STIFFV.
          * - :py:attr:`~stiffts`
            - Get or set the Stiffness for tensile vertical response (sliding isolator - default = 0).
          * - :py:attr:`~forcey`
            - Get or set the Yield force.
          * - :py:attr:`~alpha`
            - Get or set the Ratio of postyielding stiffness to preyielding stiffness.
          * - :py:attr:`~stifft`
            - Get or set the Stiffness for tensile vertical response (elastomeric isolator), default is 0.5STIFF.
          * - :py:attr:`~dfail`
            - Get or set the Lateral displacement at which the isolator fails.
          * - :py:attr:`~fmaxyc`
            - Get or set the Max friction coefficient (dynamic) for local Y-axis (compression).  (ITYPE=2 only).
          * - :py:attr:`~fmaxxt`
            - Get or set the Max friction coefficient (dynamic) for local X-axis (tension). (ITYPE=2 only)..
          * - :py:attr:`~fmaxyt`
            - Get or set the Max friction coefficient (dynamic) for local Y-axis (tension). (ITYPE=2 only).
          * - :py:attr:`~ylock`
            - Get or set the Stiffness locking the local Y-displacement (optional -single-axis sliding).  (ITYPE=2 only).
          * - :py:attr:`~htcore`
            - Get or set the Height of lead core (length units) (ITYPE=3)
          * - :py:attr:`~rcore`
            - Get or set the Radius of lead core (length units) (ITYPE=3)
          * - :py:attr:`~tshim`
            - Get or set the Total thickness of shim plates (length units) (ITYPE=3)
          * - :py:attr:`~rolcl`
            - Get or set the Mass density times specific heat capacity of lead (units: F.L-2T-1) (ITYPE=3)
          * - :py:attr:`~roscs`
            - Get or set the Mass density times specific heat capacity of steel (units: F.L-2T-1) (ITYPE=3)
          * - :py:attr:`~thcst`
            - Get or set the Thermal conductivity of steel (units: F.t-1T-1) (ITYPE=3)
          * - :py:attr:`~yle2`
            - Get or set the E2 in temperature-dependent yield stress of lead (units: 1/Temperature) (ITYPE=3)
          * - :py:attr:`~pcrini`
            - Get or set the Buckling capacity (force units) (ITYPE=3)
          * - :py:attr:`~diamb`
            - Get or set the External diameter of bearing (length units) (ITYPE=3)
          * - :py:attr:`~fcav0`
            - Get or set the Tensile capacity limited by cavitation (force units) (ITYPE=3)
          * - :py:attr:`~cavk`
            - Get or set the Cavitation parameter (units 1/length) (ITYPE=3)
          * - :py:attr:`~cavtr`
            - Get or set the Total thickness of rubber (length units) (ITYPE=3)
          * - :py:attr:`~cava`
            - Get or set the Strength degradation parameter (dimensionless) (ITYPE=3)
          * - :py:attr:`~phim`
            - Get or set the Maximum cavitation damage index (dimensionless) (ITYPE=3)
          * - :py:attr:`~kthx`
            - Get or set the Rotational stiffness in local x direction (moment per radian)
          * - :py:attr:`~kthy`
            - Get or set the Rotational stiffness in local y direction (moment per radian)
          * - :py:attr:`~kthz`
            - Get or set the Rotational stiffness in local z direction (moment per radian)
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

    from mat_seismic_isolator import MatSeismicIsolator

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number has to be chosen.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density.
















   ..
       !! processed by numpydoc !!

.. py:property:: a
   :type: float


   
   Get or set the Nondimensional variable.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: float


   
   Get or set the Nondimensional variable.
















   ..
       !! processed by numpydoc !!

.. py:property:: gamma
   :type: float


   
   Get or set the Nondimensional variable.
















   ..
       !! processed by numpydoc !!

.. py:property:: dispy
   :type: Optional[float]


   
   Get or set the Yield displacement (length units - must be > 0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: stiffv
   :type: Optional[float]


   
   Get or set the Vertical stiffness (force/length units).
















   ..
       !! processed by numpydoc !!

.. py:property:: itype
   :type: int


   
   Get or set the Type:
   EQ.0:   sliding (spherical or cylindrical)
   EQ.1:   elastomeric
   EQ.2:   sliding (two perpendicular curved beams)
   EQ.3:   lead rubber bearing
   EQ.4: high damping rubber bearing.
















   ..
       !! processed by numpydoc !!

.. py:property:: preload
   :type: float


   
   Get or set the Vertical preload not explicitly modelled (force units).
















   ..
       !! processed by numpydoc !!

.. py:property:: damp
   :type: float


   
   Get or set the Damping ratio (nondimensional).
















   ..
       !! processed by numpydoc !!

.. py:property:: mx1
   :type: float


   
   Get or set the Moment factor at ends 1 and 2 in local X-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: mx2
   :type: float


   
   Get or set the Moment factor at ends 1 and 2 in local X-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: my1
   :type: float


   
   Get or set the Moment factor at ends 1 and 2 in local Y-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: my2
   :type: float


   
   Get or set the Moment factor at ends 1 and 2 in local Y-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: cde
   :type: float


   
   Get or set the Viscous damping coefficient (ITYPE=1, 3 or 4).
















   ..
       !! processed by numpydoc !!

.. py:property:: iextra
   :type: int


   
   Get or set the If IEXTRA = 1, optional Card 8 will be read.
















   ..
       !! processed by numpydoc !!

.. py:property:: fmax
   :type: float


   
   Get or set the Maximum friction coefficient (dynamic).
















   ..
       !! processed by numpydoc !!

.. py:property:: delf
   :type: float


   
   Get or set the Difference between maximum friction and static friction coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: afric
   :type: float


   
   Get or set the Velocity multiplier in sliding friction equation (time/length units).
















   ..
       !! processed by numpydoc !!

.. py:property:: radx
   :type: float


   
   Get or set the Radius for sliding in local X direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: rady
   :type: float


   
   Get or set the Radius for sliding in local Y direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: radb
   :type: float


   
   Get or set the Radius of retaining ring.
















   ..
       !! processed by numpydoc !!

.. py:property:: stiffl
   :type: Optional[float]


   
   Get or set the Stiffness for lateral contact against the retaining ring, default is STIFFV.
















   ..
       !! processed by numpydoc !!

.. py:property:: stiffts
   :type: float


   
   Get or set the Stiffness for tensile vertical response (sliding isolator - default = 0).
















   ..
       !! processed by numpydoc !!

.. py:property:: forcey
   :type: float


   
   Get or set the Yield force.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha
   :type: float


   
   Get or set the Ratio of postyielding stiffness to preyielding stiffness.
















   ..
       !! processed by numpydoc !!

.. py:property:: stifft
   :type: Optional[float]


   
   Get or set the Stiffness for tensile vertical response (elastomeric isolator), default is 0.5STIFF.
















   ..
       !! processed by numpydoc !!

.. py:property:: dfail
   :type: float


   
   Get or set the Lateral displacement at which the isolator fails.
















   ..
       !! processed by numpydoc !!

.. py:property:: fmaxyc
   :type: Optional[float]


   
   Get or set the Max friction coefficient (dynamic) for local Y-axis (compression).  (ITYPE=2 only).
















   ..
       !! processed by numpydoc !!

.. py:property:: fmaxxt
   :type: Optional[float]


   
   Get or set the Max friction coefficient (dynamic) for local X-axis (tension). (ITYPE=2 only)..
















   ..
       !! processed by numpydoc !!

.. py:property:: fmaxyt
   :type: Optional[float]


   
   Get or set the Max friction coefficient (dynamic) for local Y-axis (tension). (ITYPE=2 only).
















   ..
       !! processed by numpydoc !!

.. py:property:: ylock
   :type: Optional[float]


   
   Get or set the Stiffness locking the local Y-displacement (optional -single-axis sliding).  (ITYPE=2 only).
















   ..
       !! processed by numpydoc !!

.. py:property:: htcore
   :type: Optional[float]


   
   Get or set the Height of lead core (length units) (ITYPE=3)
















   ..
       !! processed by numpydoc !!

.. py:property:: rcore
   :type: Optional[float]


   
   Get or set the Radius of lead core (length units) (ITYPE=3)
















   ..
       !! processed by numpydoc !!

.. py:property:: tshim
   :type: Optional[float]


   
   Get or set the Total thickness of shim plates (length units) (ITYPE=3)
















   ..
       !! processed by numpydoc !!

.. py:property:: rolcl
   :type: Optional[float]


   
   Get or set the Mass density times specific heat capacity of lead (units: F.L-2T-1) (ITYPE=3)
















   ..
       !! processed by numpydoc !!

.. py:property:: roscs
   :type: Optional[float]


   
   Get or set the Mass density times specific heat capacity of steel (units: F.L-2T-1) (ITYPE=3)
















   ..
       !! processed by numpydoc !!

.. py:property:: thcst
   :type: Optional[float]


   
   Get or set the Thermal conductivity of steel (units: F.t-1T-1) (ITYPE=3)
















   ..
       !! processed by numpydoc !!

.. py:property:: yle2
   :type: Optional[float]


   
   Get or set the E2 in temperature-dependent yield stress of lead (units: 1/Temperature) (ITYPE=3)
















   ..
       !! processed by numpydoc !!

.. py:property:: pcrini
   :type: Optional[float]


   
   Get or set the Buckling capacity (force units) (ITYPE=3)
















   ..
       !! processed by numpydoc !!

.. py:property:: diamb
   :type: Optional[float]


   
   Get or set the External diameter of bearing (length units) (ITYPE=3)
















   ..
       !! processed by numpydoc !!

.. py:property:: fcav0
   :type: Optional[float]


   
   Get or set the Tensile capacity limited by cavitation (force units) (ITYPE=3)
















   ..
       !! processed by numpydoc !!

.. py:property:: cavk
   :type: Optional[float]


   
   Get or set the Cavitation parameter (units 1/length) (ITYPE=3)
















   ..
       !! processed by numpydoc !!

.. py:property:: cavtr
   :type: Optional[float]


   
   Get or set the Total thickness of rubber (length units) (ITYPE=3)
















   ..
       !! processed by numpydoc !!

.. py:property:: cava
   :type: Optional[float]


   
   Get or set the Strength degradation parameter (dimensionless) (ITYPE=3)
















   ..
       !! processed by numpydoc !!

.. py:property:: phim
   :type: Optional[float]


   
   Get or set the Maximum cavitation damage index (dimensionless) (ITYPE=3)
















   ..
       !! processed by numpydoc !!

.. py:property:: kthx
   :type: Optional[float]


   
   Get or set the Rotational stiffness in local x direction (moment per radian)
















   ..
       !! processed by numpydoc !!

.. py:property:: kthy
   :type: Optional[float]


   
   Get or set the Rotational stiffness in local y direction (moment per radian)
















   ..
       !! processed by numpydoc !!

.. py:property:: kthz
   :type: Optional[float]


   
   Get or set the Rotational stiffness in local z direction (moment per radian)
















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
   :value: 'MAT'


.. py:attribute:: subkeyword
   :value: 'SEISMIC_ISOLATOR'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





