





:class:`DualceseBoundaryPrescribedTwoPhaseMsurf`
================================================


.. py:class:: dualcese_boundary_prescribed_two_phase_msurf.DualceseBoundaryPrescribedTwoPhaseMsurf(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DUALCESE_BOUNDARY_PRESCRIBED_TWO-PHASE_MSURF keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DualceseBoundaryPrescribedTwoPhaseMsurf

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mspid`
            - Get or set the Mesh surface part ID that is referenced by *MESH_SURFACE_ELEMENT cards
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
          * - :py:attr:`~lc_u`
            - Get or set the Load curve ID (see *DEFINE_CURVE) to describe the  -component of the velocity as a function of time or function ID (see *DEFINE_FUNCTION) to give the  -component of the velocity as a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time).
          * - :py:attr:`~lc_v`
            - Get or set the Load curve ID to describe the  -component of the velocity as a function of time or function ID to give the  -component of the velocity as a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time).
          * - :py:attr:`~lc_w`
            - Get or set the Load curve ID to describe the  -component of the velocity as a function of time or function ID to give the  -component of the velocity as a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time).
          * - :py:attr:`~lc_d1`
            - Get or set the Load curve or defined function ID to describe the density of the first multiphase material as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
          * - :py:attr:`~lc_d2`
            - Get or set the Load curve or defined function ID to describe the density of the reactant (material  ) as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
          * - :py:attr:`~lc_p`
            - Get or set the Load curve or defined function ID to describe the pressure as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
          * - :py:attr:`~lc_t`
            - Get or set the Load curve or defined function ID to describe the temperature as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
          * - :py:attr:`~sf_z1`
            - Get or set the Scale factor for LC_Z1
          * - :py:attr:`~sf_u`
            - Get or set the Scale factor for LC_U
          * - :py:attr:`~sf_v`
            - Get or set the Scale factor for LC_V
          * - :py:attr:`~sf_w`
            - Get or set the Scale factor for LC_W
          * - :py:attr:`~sf_d1`
            - Get or set the Scale factor for LC_D1
          * - :py:attr:`~sf_d2`
            - Get or set the Scale factor for LC_D2
          * - :py:attr:`~sf_p`
            - Get or set the Scale factor for LC_P
          * - :py:attr:`~sf_t`
            - Get or set the Scale factor for LC_T


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

    from dualcese_boundary_prescribed_two_phase_msurf import DualceseBoundaryPrescribedTwoPhaseMsurf

Property detail
---------------

.. py:property:: mspid
   :type: Optional[int]


   
   Get or set the Mesh surface part ID that is referenced by *MESH_SURFACE_ELEMENT cards
















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

.. py:property:: lc_u
   :type: Optional[int]


   
   Get or set the Load curve ID (see *DEFINE_CURVE) to describe the  -component of the velocity as a function of time or function ID (see *DEFINE_FUNCTION) to give the  -component of the velocity as a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time).
   EQ.0:    -component of velocity is a constant with value SF_U.
   EQ.-1:   -component of velocity is computed by the solver
















   ..
       !! processed by numpydoc !!

.. py:property:: lc_v
   :type: Optional[int]


   
   Get or set the Load curve ID to describe the  -component of the velocity as a function of time or function ID to give the  -component of the velocity as a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time).
   EQ.0:    -component of velocity is a constant with value SF_V.
   EQ.-1:   -component of velocity is computed by the solver.
















   ..
       !! processed by numpydoc !!

.. py:property:: lc_w
   :type: Optional[int]


   
   Get or set the Load curve ID to describe the  -component of the velocity as a function of time or function ID to give the  -component of the velocity as a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time).
   EQ.0:    -component of velocity is a constant with value SF_W.
   EQ.-1:   -component of velocity is computed by the solver
















   ..
       !! processed by numpydoc !!

.. py:property:: lc_d1
   :type: Optional[int]


   
   Get or set the Load curve or defined function ID to describe the density of the first multiphase material as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
   EQ.0:   The density of the first multiphase material is a constant with value SF_D1
   EQ.-1:The density of the first multiphase material is computed by the solver.
















   ..
       !! processed by numpydoc !!

.. py:property:: lc_d2
   :type: Optional[int]


   
   Get or set the Load curve or defined function ID to describe the density of the reactant (material  ) as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
   EQ.0:   The density of the reactant is a constant with value SF_D2.
   EQ.-1:  The density of the reactant is computed by the solver
















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

.. py:property:: sf_d2
   :type: float


   
   Get or set the Scale factor for LC_D2
















   ..
       !! processed by numpydoc !!

.. py:property:: sf_p
   :type: float


   
   Get or set the Scale factor for LC_P
















   ..
       !! processed by numpydoc !!

.. py:property:: sf_t
   :type: float


   
   Get or set the Scale factor for LC_T
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DUALCESE'


.. py:attribute:: subkeyword
   :value: 'BOUNDARY_PRESCRIBED_TWO-PHASE_MSURF'






