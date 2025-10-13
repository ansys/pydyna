





:class:`IcfdBoundaryPrescribedTurbulence`
=========================================


.. py:class:: icfd_boundary_prescribed_turbulence.IcfdBoundaryPrescribedTurbulence(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_BOUNDARY_PRESCRIBED_TURBULENCE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdBoundaryPrescribedTurbulence

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the PID for a fluid surface.
          * - :py:attr:`~vtype`
            - Get or set the Variable type.
          * - :py:attr:`~imp`
            - Get or set the Imposition method:
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID to describe the variable value as a function of time; see *DEFINE_‌CURVE, *DEFINE_‌CURVE_‌FUNCTION or *DEFINE_‌FUNCTION. If a *DEFINE_‌FUNCTION is used, the following parameters are allowed: f(x, y, z, vx, vy, vz, temp, pres, time, k, e, mut).
          * - :py:attr:`~ks`
            - Get or set the Roughness physical height and roughness constant. When defined, the global values of *ICFD_CONTROL_TURBULENCE are replaced for this surface part.
          * - :py:attr:`~cs`
            - Get or set the Roughness physical height and roughness constant. When defined, the global values of *ICFD_CONTROL_TURBULENCE are replaced for this surface part.


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

    from icfd_boundary_prescribed_turbulence import IcfdBoundaryPrescribedTurbulence

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the PID for a fluid surface.
















   ..
       !! processed by numpydoc !!

.. py:property:: vtype
   :type: int


   
   Get or set the Variable type.
   EQ.1:Turbulence kineticenergy.
   EQ.2:Turbulent dissipation rate.
   EQ.3:Specific dissipation rate.
   EQ.4:Modified turbulent viscosity.
















   ..
       !! processed by numpydoc !!

.. py:property:: imp
   :type: int


   
   Get or set the Imposition method:
   EQ.0:Direct imposition through value specified by LCID.
   EQ.1:Using turbulent Intensity specified by LCID if VTYPE = 1.Using turbulence length scale specified by LCID if VTYPE = 2,3 and 4.
   EQ.2:Using turbulence viscosity ratio specified by LCID.Only available for VTYPE = 2 and VTYPE = 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID to describe the variable value as a function of time; see *DEFINE_‌CURVE, *DEFINE_‌CURVE_‌FUNCTION or *DEFINE_‌FUNCTION. If a *DEFINE_‌FUNCTION is used, the following parameters are allowed: f(x, y, z, vx, vy, vz, temp, pres, time, k, e, mut).
















   ..
       !! processed by numpydoc !!

.. py:property:: ks
   :type: Optional[float]


   
   Get or set the Roughness physical height and roughness constant. When defined, the global values of *ICFD_CONTROL_TURBULENCE are replaced for this surface part.
















   ..
       !! processed by numpydoc !!

.. py:property:: cs
   :type: Optional[float]


   
   Get or set the Roughness physical height and roughness constant. When defined, the global values of *ICFD_CONTROL_TURBULENCE are replaced for this surface part.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'BOUNDARY_PRESCRIBED_TURBULENCE'






