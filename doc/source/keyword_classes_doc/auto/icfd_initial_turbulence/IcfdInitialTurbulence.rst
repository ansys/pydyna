





:class:`IcfdInitialTurbulence`
==============================


.. py:class:: icfd_initial_turbulence.IcfdInitialTurbulence(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_INITIAL_TURBULENCE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdInitialTurbulence

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID for the volume elements or the surface elements where the values are initialized (see *ICFD_PART_VOL and *ICFD_PART).PID = 0 to assign the initial condition to all nodes at once.
          * - :py:attr:`~i`
            - Get or set the Initial turbulent intensity.
          * - :py:attr:`~r`
            - Get or set the Initial turbulent viscosity to laminar viscosity ratio.
          * - :py:attr:`~k`
            - Get or set the Initial kinetic energy. When defined, it replaces the choice of I. A negative integer will point to a *DEFINE_FUNCTION ID. The following parameters are allowed : f(x,y,z), allowing to define initial profiles function of coordinates
          * - :py:attr:`~ew`
            - Get or set the Initial turbulent specific dissipation rate or dissipation rate depending on the choice of turbulence model. When defined, it replaces the choice of R. A negative integer will point to a *DEFINE_FUNCTION ID. The following parameters are allowed : f(x,y,z), allowing to define initial profiles function of coordinates


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

    from icfd_initial_turbulence import IcfdInitialTurbulence

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID for the volume elements or the surface elements where the values are initialized (see *ICFD_PART_VOL and *ICFD_PART).PID = 0 to assign the initial condition to all nodes at once.
















   ..
       !! processed by numpydoc !!

.. py:property:: i
   :type: Optional[float]


   
   Get or set the Initial turbulent intensity.
















   ..
       !! processed by numpydoc !!

.. py:property:: r
   :type: Optional[float]


   
   Get or set the Initial turbulent viscosity to laminar viscosity ratio.
















   ..
       !! processed by numpydoc !!

.. py:property:: k
   :type: Optional[float]


   
   Get or set the Initial kinetic energy. When defined, it replaces the choice of I. A negative integer will point to a *DEFINE_FUNCTION ID. The following parameters are allowed : f(x,y,z), allowing to define initial profiles function of coordinates
















   ..
       !! processed by numpydoc !!

.. py:property:: ew
   :type: Optional[float]


   
   Get or set the Initial turbulent specific dissipation rate or dissipation rate depending on the choice of turbulence model. When defined, it replaces the choice of R. A negative integer will point to a *DEFINE_FUNCTION ID. The following parameters are allowed : f(x,y,z), allowing to define initial profiles function of coordinates
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'INITIAL_TURBULENCE'






