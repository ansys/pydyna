





:class:`LsoVariableGroup`
=========================


.. py:class:: lso_variable_group.LsoVariableGroup(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LSO_VARIABLE_GROUP keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LsoVariableGroup

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~solver_name`
            - Get or set the Name of the solver.
          * - :py:attr:`~domain_type`
            - Get or set the One of the following must be used:
          * - :py:attr:`~group_name`
            - Get or set the Group name representing the list of variable_name names.
          * - :py:attr:`~var_name`
            - Get or set the Either the name of another variable group or one of the following known edit variables:


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

    from lso_variable_group import LsoVariableGroup

Property detail
---------------

.. py:property:: solver_name
   :type: Optional[str]


   
   Get or set the Name of the solver.
















   ..
       !! processed by numpydoc !!

.. py:property:: domain_type
   :type: str


   
   Get or set the One of the following must be used:
   NODE
   BEAM_ELEMENT
   SHELL_ELEMENT
   THICK_SHELL_ELEMENT
   SOLID_ELEMENT
   SEGMENT
   PART
   GLOBAL
   SURFACE_ELEMENT
   VOLUME_ELEMENT.
















   ..
       !! processed by numpydoc !!

.. py:property:: group_name
   :type: Optional[str]


   
   Get or set the Group name representing the list of variable_name names.
















   ..
       !! processed by numpydoc !!

.. py:property:: var_name
   :type: Optional[str]


   
   Get or set the Either the name of another variable group or one of the following known edit variables:
   NODE (MECH) displacement
   BEAM (MECH)
   SHELL (MECH)
   TSHELL (MECH)
   PART (MECH)
   NODE (ICFD) temperature,
   pressure,
   species_1_density,
   species_2_density,
   enstrophy,
   helicity,
   x_velocity, y_velocity, z_velocity,
   velocity,
   x_vorticity, y_vorticity, z_vorticity,
   vorticity,      heat_flux,
   NODE (CESE)
   SURFACE_ELEMENT (ICFD)
   element_pressure
   VOLUME_ELEMENT (ICFD)
   element_pressure
   VOLUME_ELEMENT (CESE)
   density,
   temperature, pressure, internal energy,
   species_1_density, species_2_density,
   enstrophy,
   helicity,
   x_velocity, y_velocity, z_velocity,
   velocity,
   x_vorticity, y_vorticity, z_vorticity,
   vorticity,
   x_centroid, y_centroid, z_centroid,
   centroid,
   density_var,
   ddx_density,
   ddy_density,
   ddz_density,
   density,
   x_momentum_var,
   ddx_momentum,
   ddy_momentum,
   ddz_momentum,
   x_momentum,
   ddx_momentum,
   ddy_momentum,
   ddz_momentum,
   y_momentum,
   z_momentum_var,
   ddx_momentum,
   ddy_momentum,
   ddz_momentum,
   z_momentum,
   momentum,
   total_e_var,
   ddx_total_e,
   ddy_total_e,
   ddz_total_e,
   total_e,
   SEGMENT (MECH)
   SEGMENT (ICFD) drag_force_magnitude,
   ave_drag_force_magnitude,
   tot_drag_force_magnitude,
   mass_outflow,
   tot_mass_outflow,
   tot_mass-outflow_rate,
   SEGMENT (CESE)
   SURFACE_PART (ICFD)
   VOLUME_PART (ICFD)
   PART (CESE) mat_mass, mat_internal_energy
   GLOBAL (MECH)
   GLOBAL (ICFD) div(u), total_ke, total_enstrophy, total_drag
   GLOBAL (CESE) total_ke, tot_enstrophy.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LSO'


.. py:attribute:: subkeyword
   :value: 'VARIABLE_GROUP'






