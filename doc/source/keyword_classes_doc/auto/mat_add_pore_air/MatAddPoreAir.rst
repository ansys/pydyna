





:class:`MatAddPoreAir`
======================


.. py:class:: mat_add_pore_air.MatAddPoreAir(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_ADD_PORE_AIR keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatAddPoreAir

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification - must be same as the structural material.
          * - :py:attr:`~pa_rho`
            - Get or set the Initial density of pore air, default to atmospheric air density, AIR_RO, defined in *CONTROL_PORE_AIR
          * - :py:attr:`~pa_pre`
            - Get or set the Initial pressure of pore air, default to atmospheric air pressure, AIR_P, defined in *CONTROL_PORE_AIR.
          * - :py:attr:`~pore`
            - Get or set the Porosity, ratio of pores to total volume, default to 1.
          * - :py:attr:`~dvimin`
            - Get or set the Optional parameters to trigger air flow analysis.  Pore air flow analysis is performed only for these nodes having incremental volume change ratio, abs(V(t)-V(t-dt))/V(t-dt), larger than DVMIN, where V(t) is the nodal volume at time=t.  This parameter may be needed for the material that has very high permeability.  Caution has to be exercised when using DVMIN, a reasonable starting value may equal the inverse of the total number of analysis time steps.  Another option to control pore air analysis can be found in *SENSOR_CONTROL.
          * - :py:attr:`~perm1`
            - Get or set the Permeability of pore air along x-direction, <0 when ABS(PERMX) is the curve defining permeability coefficient as a function of volume ratio,     current-volume)/volume-at-stress-free-state.
          * - :py:attr:`~perm2`
            - Get or set the Permeability of pore air along y-direction, <0 when ABS(PERMY) is the curve defining permeability coefficient as a function of volume ratio,     current-volume)/volume-at-stress-free-state
          * - :py:attr:`~perm3`
            - Get or set the Permeability of pore air along z-direction, <0 when ABS(PERMZ) is the curve defining permeability coefficient as a function of volume ratio,     current-volume)/volume-at-stress-free-state.
          * - :py:attr:`~cdarcy`
            - Get or set the Coefficient of Darcy's law
          * - :py:attr:`~cdf`
            - Get or set the Coefficient of Dupuit-Forchheimer law
          * - :py:attr:`~lcpgd1`
            - Get or set the Curves defining non-linear Darcy's laws along x, y and z-directions
          * - :py:attr:`~lcpgd2`
            - Get or set the Curves defining non-linear Darcy's laws along x, y and z-directions
          * - :py:attr:`~lcpgd3`
            - Get or set the Curves defining non-linear Darcy's laws along x, y and z-directions
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

    from mat_add_pore_air import MatAddPoreAir

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification - must be same as the structural material.
















   ..
       !! processed by numpydoc !!

.. py:property:: pa_rho
   :type: Optional[float]


   
   Get or set the Initial density of pore air, default to atmospheric air density, AIR_RO, defined in *CONTROL_PORE_AIR
















   ..
       !! processed by numpydoc !!

.. py:property:: pa_pre
   :type: Optional[float]


   
   Get or set the Initial pressure of pore air, default to atmospheric air pressure, AIR_P, defined in *CONTROL_PORE_AIR.
















   ..
       !! processed by numpydoc !!

.. py:property:: pore
   :type: float


   
   Get or set the Porosity, ratio of pores to total volume, default to 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: dvimin
   :type: Optional[float]


   
   Get or set the Optional parameters to trigger air flow analysis.  Pore air flow analysis is performed only for these nodes having incremental volume change ratio, abs(V(t)-V(t-dt))/V(t-dt), larger than DVMIN, where V(t) is the nodal volume at time=t.  This parameter may be needed for the material that has very high permeability.  Caution has to be exercised when using DVMIN, a reasonable starting value may equal the inverse of the total number of analysis time steps.  Another option to control pore air analysis can be found in *SENSOR_CONTROL.
















   ..
       !! processed by numpydoc !!

.. py:property:: perm1
   :type: float


   
   Get or set the Permeability of pore air along x-direction, <0 when ABS(PERMX) is the curve defining permeability coefficient as a function of volume ratio,     current-volume)/volume-at-stress-free-state.
















   ..
       !! processed by numpydoc !!

.. py:property:: perm2
   :type: Optional[float]


   
   Get or set the Permeability of pore air along y-direction, <0 when ABS(PERMY) is the curve defining permeability coefficient as a function of volume ratio,     current-volume)/volume-at-stress-free-state
















   ..
       !! processed by numpydoc !!

.. py:property:: perm3
   :type: Optional[float]


   
   Get or set the Permeability of pore air along z-direction, <0 when ABS(PERMZ) is the curve defining permeability coefficient as a function of volume ratio,     current-volume)/volume-at-stress-free-state.
















   ..
       !! processed by numpydoc !!

.. py:property:: cdarcy
   :type: float


   
   Get or set the Coefficient of Darcy's law
















   ..
       !! processed by numpydoc !!

.. py:property:: cdf
   :type: float


   
   Get or set the Coefficient of Dupuit-Forchheimer law
















   ..
       !! processed by numpydoc !!

.. py:property:: lcpgd1
   :type: int


   
   Get or set the Curves defining non-linear Darcy's laws along x, y and z-directions
















   ..
       !! processed by numpydoc !!

.. py:property:: lcpgd2
   :type: Optional[int]


   
   Get or set the Curves defining non-linear Darcy's laws along x, y and z-directions
















   ..
       !! processed by numpydoc !!

.. py:property:: lcpgd3
   :type: Optional[int]


   
   Get or set the Curves defining non-linear Darcy's laws along x, y and z-directions
















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
   :value: 'ADD_PORE_AIR'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





