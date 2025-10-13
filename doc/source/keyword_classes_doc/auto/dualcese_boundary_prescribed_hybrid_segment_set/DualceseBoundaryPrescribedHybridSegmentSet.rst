





:class:`DualceseBoundaryPrescribedHybridSegmentSet`
===================================================


.. py:class:: dualcese_boundary_prescribed_hybrid_segment_set.DualceseBoundaryPrescribedHybridSegmentSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DUALCESE_BOUNDARY_PRESCRIBED_HYBRID_SEGMENT_SET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DualceseBoundaryPrescribedHybridSegmentSet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ssid`
            - Get or set the Segment set ID created with *DUALCESE_SEGMENTSET
          * - :py:attr:`~idcomp`
            - Get or set the For inflow boundaries in problems involving chemical reacting flows, the chemical mixture of the fluid entering the domain is defined with a *CHEMISTRY_‌COMPOSITION card with this ID [Not yet available
          * - :py:attr:`~dirx`
            - Get or set the If this vector is non-zero, then it is used as the prescribed flow direction
          * - :py:attr:`~diry`
            - Get or set the If this vector is non-zero, then it is used as the prescribed flow direction
          * - :py:attr:`~dirz`
            - Get or set the If this vector is non-zero, then it is used as the prescribed flow direction
          * - :py:attr:`~lc_z1`
            - Get or set the Load curve ID or function ID to describe the volume fraction of material 1 as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
          * - :py:attr:`~lc_ra`
            - Get or set the Load curve or function ID to describe the mass fraction of reactant (material  ) with respect to the explosive mixture (material 2) as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
          * - :py:attr:`~lc_u`
            - Get or set the Load curve or defined function ID to describe the  -component of the velocity as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
          * - :py:attr:`~lc_v`
            - Get or set the Load curve or defined function ID to describe the  -component of the velocity as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.EQ.0:The y-component of velocity is a constant with value SF_V.
          * - :py:attr:`~lc_w`
            - Get or set the Load curve or defined function ID to describe the  -component of the velocity as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
          * - :py:attr:`~lc_d1`
            - Get or set the Load curve or defined function ID to describe the density of the first multiphase material as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
          * - :py:attr:`~lc_da`
            - Get or set the Load curve or defined function ID to describe the density of the reactant (material  ) as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
          * - :py:attr:`~lc_db`
            - Get or set the Load curve or defined function ID to describe the density of the product (material  ) as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
          * - :py:attr:`~lc_p`
            - Get or set the Load curve or defined function ID to describe the pressure as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
          * - :py:attr:`~lc_t`
            - Get or set the Load curve or defined function ID to describe the temperature as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
          * - :py:attr:`~sf_z1`
            - Get or set the Scale factor for LC_Z1
          * - :py:attr:`~sf_ra`
            - Get or set the Scale factor for LC_RA
          * - :py:attr:`~sf_u`
            - Get or set the Scale factor for LC_U
          * - :py:attr:`~sf_v`
            - Get or set the Scale factor for LC_V
          * - :py:attr:`~sf_w`
            - Get or set the Scale factor for LC_W
          * - :py:attr:`~sf_d1`
            - Get or set the Scale factor for LC_D1
          * - :py:attr:`~sf_da`
            - Get or set the Scale factor for LC_DA
          * - :py:attr:`~sf_db`
            - Get or set the Scale factor for LC_DB


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

    from dualcese_boundary_prescribed_hybrid_segment_set import DualceseBoundaryPrescribedHybridSegmentSet

Property detail
---------------

.. py:property:: ssid
   :type: Optional[int]


   
   Get or set the Segment set ID created with *DUALCESE_SEGMENTSET
















   ..
       !! processed by numpydoc !!

.. py:property:: idcomp
   :type: Optional[int]


   
   Get or set the For inflow boundaries in problems involving chemical reacting flows, the chemical mixture of the fluid entering the domain is defined with a *CHEMISTRY_‌COMPOSITION card with this ID [Not yet available
















   ..
       !! processed by numpydoc !!

.. py:property:: dirx
   :type: Optional[float]


   
   Get or set the If this vector is non-zero, then it is used as the prescribed flow direction
















   ..
       !! processed by numpydoc !!

.. py:property:: diry
   :type: Optional[float]


   
   Get or set the If this vector is non-zero, then it is used as the prescribed flow direction
















   ..
       !! processed by numpydoc !!

.. py:property:: dirz
   :type: Optional[float]


   
   Get or set the If this vector is non-zero, then it is used as the prescribed flow direction
















   ..
       !! processed by numpydoc !!

.. py:property:: lc_z1
   :type: Optional[int]


   
   Get or set the Load curve ID or function ID to describe the volume fraction of material 1 as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
   EQ.0:   The volume fraction is a constant with value SF_?Z1.
   EQ. - 1 : The volume fraction is computed by the solver.
















   ..
       !! processed by numpydoc !!

.. py:property:: lc_ra
   :type: Optional[int]


   
   Get or set the Load curve or function ID to describe the mass fraction of reactant (material  ) with respect to the explosive mixture (material 2) as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
   EQ.0:   The mass fraction is a constant with value SF_RA.
   EQ.-1:  The mass fraction is computed by the solver.
















   ..
       !! processed by numpydoc !!

.. py:property:: lc_u
   :type: Optional[int]


   
   Get or set the Load curve or defined function ID to describe the  -component of the velocity as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
   EQ.0:   The x-component of velocity is a constant with value SF_U.
   EQ.-1:The x-component of velocity is computed by the solver
















   ..
       !! processed by numpydoc !!

.. py:property:: lc_v
   :type: Optional[int]


   
   Get or set the Load curve or defined function ID to describe the  -component of the velocity as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.EQ.0:The y-component of velocity is a constant with value SF_V.

   EQ. - 1:        The  y - component of velocity is computed by the solver.















   ..
       !! processed by numpydoc !!

.. py:property:: lc_w
   :type: Optional[int]


   
   Get or set the Load curve or defined function ID to describe the  -component of the velocity as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
   EQ.0:   The  z-component of velocity is a constant with value SF_W.
   EQ.-1:The  z-component of velocity is computed by the solver.
















   ..
       !! processed by numpydoc !!

.. py:property:: lc_d1
   :type: Optional[int]


   
   Get or set the Load curve or defined function ID to describe the density of the first multiphase material as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
   EQ.0:   The density of the first multiphase material is a constant with value SF_D1
   EQ.-1:The density of the first multiphase material is computed by the solver.
















   ..
       !! processed by numpydoc !!

.. py:property:: lc_da
   :type: Optional[int]


   
   Get or set the Load curve or defined function ID to describe the density of the reactant (material  ) as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
   EQ.0:   The density of the reactant is a constant with value SF_DA.
   EQ.-1:  The density of the reactant is computed by the solver
















   ..
       !! processed by numpydoc !!

.. py:property:: lc_db
   :type: Optional[int]


   
   Get or set the Load curve or defined function ID to describe the density of the product (material  ) as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
   EQ.0:   The density of the product is a constant with value SF_DB.
   EQ.-1:  The density of the product is computed by the solver
















   ..
       !! processed by numpydoc !!

.. py:property:: lc_p
   :type: Optional[float]


   
   Get or set the Load curve or defined function ID to describe the pressure as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
   EQ.0:   The pressure is a constant with value SF_P
   EQ.-1:  The pressure is computed by the solver
















   ..
       !! processed by numpydoc !!

.. py:property:: lc_t
   :type: Optional[float]


   
   Get or set the Load curve or defined function ID to describe the temperature as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
   EQ.0:   The temperature is a constant with value SF_T.
   EQ.-1:  The temperature is computed by the solver
















   ..
       !! processed by numpydoc !!

.. py:property:: sf_z1
   :type: float


   
   Get or set the Scale factor for LC_Z1
















   ..
       !! processed by numpydoc !!

.. py:property:: sf_ra
   :type: float


   
   Get or set the Scale factor for LC_RA
















   ..
       !! processed by numpydoc !!

.. py:property:: sf_u
   :type: float


   
   Get or set the Scale factor for LC_U
















   ..
       !! processed by numpydoc !!

.. py:property:: sf_v
   :type: float


   
   Get or set the Scale factor for LC_V
















   ..
       !! processed by numpydoc !!

.. py:property:: sf_w
   :type: float


   
   Get or set the Scale factor for LC_W
















   ..
       !! processed by numpydoc !!

.. py:property:: sf_d1
   :type: float


   
   Get or set the Scale factor for LC_D1
















   ..
       !! processed by numpydoc !!

.. py:property:: sf_da
   :type: float


   
   Get or set the Scale factor for LC_DA
















   ..
       !! processed by numpydoc !!

.. py:property:: sf_db
   :type: float


   
   Get or set the Scale factor for LC_DB
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DUALCESE'


.. py:attribute:: subkeyword
   :value: 'BOUNDARY_PRESCRIBED_HYBRID_SEGMENT_SET'






