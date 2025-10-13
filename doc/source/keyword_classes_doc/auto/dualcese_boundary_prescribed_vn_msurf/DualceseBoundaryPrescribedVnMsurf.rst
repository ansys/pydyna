





:class:`DualceseBoundaryPrescribedVnMsurf`
==========================================


.. py:class:: dualcese_boundary_prescribed_vn_msurf.DualceseBoundaryPrescribedVnMsurf(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DUALCESE_BOUNDARY_PRESCRIBED_VN_MSURF keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DualceseBoundaryPrescribedVnMsurf

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
          * - :py:attr:`~dirx`
            - Get or set the If this vector is non-zero, then it is used as the prescribed flow direction
          * - :py:attr:`~diry`
            - Get or set the If this vector is non-zero, then it is used as the prescribed flow direction
          * - :py:attr:`~dirz`
            - Get or set the If this vector is non-zero, then it is used as the prescribed flow direction
          * - :py:attr:`~lc_vn`
            - Get or set the Load curve or function ID to describe the normal velocity as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
          * - :py:attr:`~lc_rho`
            - Get or set the Load curve ID to describe the density versus time
          * - :py:attr:`~lc_p`
            - Get or set the Load curve ID to describe the pressure versus time
          * - :py:attr:`~lc_t`
            - Get or set the Load curve ID to describe the temperature versus time
          * - :py:attr:`~sf_vn`
            - Get or set the Scale factor for LC_VN (default = 1.0)
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

    from dualcese_boundary_prescribed_vn_msurf import DualceseBoundaryPrescribedVnMsurf

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

.. py:property:: lc_vn
   :type: Optional[int]


   
   Get or set the Load curve or function ID to describe the normal velocity as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
   EQ.0:   The normal velocity is a constant with value SF_‌VN.
   EQ. - 1 : The normal velocity is computed by the solver.
















   ..
       !! processed by numpydoc !!

.. py:property:: lc_rho
   :type: Optional[int]


   
   Get or set the Load curve ID to describe the density versus time
















   ..
       !! processed by numpydoc !!

.. py:property:: lc_p
   :type: Optional[int]


   
   Get or set the Load curve ID to describe the pressure versus time
















   ..
       !! processed by numpydoc !!

.. py:property:: lc_t
   :type: Optional[int]


   
   Get or set the Load curve ID to describe the temperature versus time
















   ..
       !! processed by numpydoc !!

.. py:property:: sf_vn
   :type: float


   
   Get or set the Scale factor for LC_VN (default = 1.0)
















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
   :value: 'BOUNDARY_PRESCRIBED_VN_MSURF'






