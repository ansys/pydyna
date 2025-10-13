





:class:`DualceseBoundaryPrescribedMsurf`
========================================


.. py:class:: dualcese_boundary_prescribed_msurf.DualceseBoundaryPrescribedMsurf(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DUALCESE_BOUNDARY_PRESCRIBED_MSURF keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DualceseBoundaryPrescribedMsurf

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
            - Get or set the For inflow boundaries in problems involving chemical reacting flows, the chemical mixture of the fluid entering the domain is defined with a *CHEMISTRY_?COMPOSITION card with this ID
          * - :py:attr:`~lc_u`
            - Get or set the Load curve ID (see *DEFINE_CURVE) to describe the  -component of the velocity as a function of time or function ID (see *DEFINE_FUNCTION) to give the  -component of the velocity as a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time).
          * - :py:attr:`~lc_v`
            - Get or set the Load curve ID to describe the  -component of the velocity as a function of time or function ID to give the  -component of the velocity as a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time).
          * - :py:attr:`~lc_w`
            - Get or set the Load curve ID to describe the  -component of the velocity as a function of time or function ID to give the  -component of the velocity as a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time).
          * - :py:attr:`~lc_rho`
            - Get or set the Load curve ID to describe the density as a function of time or function ID to give the density as a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time).
          * - :py:attr:`~lc_p`
            - Get or set the Load curve ID to describe the pressure as a function of time or function ID to give the pressure as a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time).
          * - :py:attr:`~lc_t`
            - Get or set the Load curve ID to describe the temperature as a function of time or function ID to give the temperature as a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time).
          * - :py:attr:`~sf_u`
            - Get or set the Scale factor for LC_U (default = 1.0).
          * - :py:attr:`~sf_v`
            - Get or set the Scale factor for LC_V (default = 1.0).
          * - :py:attr:`~sf_w`
            - Get or set the Scale factor for LC_W (default = 1.0).
          * - :py:attr:`~sf_rho`
            - Get or set the Scale factor for LC_RHO (default = 1.0).
          * - :py:attr:`~sf_p`
            - Get or set the Scale factor for LC_P (default = 1.0).
          * - :py:attr:`~sf_t`
            - Get or set the Scale factor for LC_T (default = 1.0).


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

    from dualcese_boundary_prescribed_msurf import DualceseBoundaryPrescribedMsurf

Property detail
---------------

.. py:property:: mspid
   :type: Optional[int]


   
   Get or set the Mesh surface part ID that is referenced by *MESH_SURFACE_ELEMENT cards
















   ..
       !! processed by numpydoc !!

.. py:property:: idcomp
   :type: Optional[int]


   
   Get or set the For inflow boundaries in problems involving chemical reacting flows, the chemical mixture of the fluid entering the domain is defined with a *CHEMISTRY_?COMPOSITION card with this ID
















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

.. py:property:: lc_rho
   :type: Optional[int]


   
   Get or set the Load curve ID to describe the density as a function of time or function ID to give the density as a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time).
   EQ.0:   Density is a constant with value SF_RHO.
   EQ.-1:  Density is computed by the solver
















   ..
       !! processed by numpydoc !!

.. py:property:: lc_p
   :type: Optional[int]


   
   Get or set the Load curve ID to describe the pressure as a function of time or function ID to give the pressure as a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time).
   EQ.0:   Pressure is a constant with value SF_P.
   EQ.-1:  Pressure is computed by the solver
















   ..
       !! processed by numpydoc !!

.. py:property:: lc_t
   :type: Optional[int]


   
   Get or set the Load curve ID to describe the temperature as a function of time or function ID to give the temperature as a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time).
   EQ.0:   Temperature is a constant with value SF_T.
   EQ.-1:  Temperature is computed by the solver
















   ..
       !! processed by numpydoc !!

.. py:property:: sf_u
   :type: float


   
   Get or set the Scale factor for LC_U (default = 1.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: sf_v
   :type: float


   
   Get or set the Scale factor for LC_V (default = 1.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: sf_w
   :type: float


   
   Get or set the Scale factor for LC_W (default = 1.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: sf_rho
   :type: float


   
   Get or set the Scale factor for LC_RHO (default = 1.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: sf_p
   :type: float


   
   Get or set the Scale factor for LC_P (default = 1.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: sf_t
   :type: float


   
   Get or set the Scale factor for LC_T (default = 1.0).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DUALCESE'


.. py:attribute:: subkeyword
   :value: 'BOUNDARY_PRESCRIBED_MSURF'






