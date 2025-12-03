Using PyDPF-post for LS-DYNA
============================

Operators
~~~~~~~~~

1. ``d3plot``

-  ``lsdyna::d3plot::meshes_provider``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      # for regular mesh
      meshOP = dpf.Operator("lsdyna::d3plot::meshes_provider")
      meshOP.inputs.data_sources.connect(ds)
      meshes = meshOP.outputs.meshes()
      mesh = meshes.get_mesh({})

      # for adaptive mesh
      meshOP = dpf.Operator("lsdyna::d3plot::meshes_provider")
      meshOP.inputs.data_sources.connect(ds)
      timeScoping = dpf.Scoping()
      timeScoping.ids = list(range(1, 21))
      meshOP.inputs.time_scoping.connect(timeScoping)
      meshes = meshOP.outputs.meshes()
      mesh = meshes.get_mesh({'time':1})

-  ``lsdyna::d3plot::U``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      coord = dpf.Operator("lsdyna::d3plot::U")
      coord.inputs.data_sources.connect(ds)
      # set the time
      coord.inputs.time_scoping.connect([3])
      fields = coord.outputs.displacement()

-  ``lsdyna::d3plot::TimeFreqSupportProvider``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      time = dpf.Operator("lsdyna::d3plot::TimeFreqSupportProvider")
      time.inputs.data_sources.connect(ds)
      result_time_freq_support = time.outputs.time_freq_support()

-  ``lsdyna::d3plot::result_info_provider``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultInfoOp = dpf.Operator("lsdyna::d3plot::result_info_provider")
      resultInfoOp.inputs.data_sources(ds)
      result_info = resultInfoOp.outputs.result_info()

-  ``lsdyna::d3plot::eng_ke``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::eng_ke")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.global_kinetic_energy()

-  ``lsdyna::d3plot::global_internal_energy``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::global_internal_energy")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.global_internal_energy()

-  ``lsdyna::d3plot::global_total_energy``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::global_total_energy")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.global_total_energy()

-  ``lsdyna::d3plot::global_velocity``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::global_velocity")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.global_velocity()

-  ``lsdyna::d3plot::node_initial_coordinates``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::node_initial_coordinates")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.node_initial_coordinates()

-  ``lsdyna::d3plot::node_coordinates``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::node_coordinates")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.node_coordinates()

-  ``lsdyna::d3plot::V``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::V")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.node_velocities()

-  ``lsdyna::d3plot::A``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::A")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.node_accelerations()

-  ``lsdyna::d3plot::node_temperature``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::node_temperature")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.node_temperature()

-  ``lsdyna::d3plot::node_heat_flux``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::node_heat_flux")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.node_heat_flux()

-  ``lsdyna::d3plot::node_mass_scaling``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::node_mass_scaling")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.node_mass_scaling()

-  ``lsdyna::d3plot::node_temperature_divide_time``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::node_temperature_divide_time")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.node_temperature_divide_time()

-  ``lsdyna::d3plot::node_residual_force``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::node_residual_force")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.node_residual_force()

-  ``lsdyna::d3plot::node_residual_moment``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::node_residual_moment")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.node_residual_moment()

-  ``lsdyna::d3plot::node_penetration``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::node_penetration")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.node_penetration()

-  ``lsdyna::d3plot::node_relative_penetration``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::node_relative_penetration")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.node_relative_penetration()

-  ``lsdyna::d3plot::node_contact_energy_density``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::node_contact_energy_density")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.node_contact_energy_density()

-  ``lsdyna::d3plot::S``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::S")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.stress()

-  ``lsdyna::d3plot::stress_von_mises``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::stress_von_mises")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.stress_von_mises()

-  ``lsdyna::d3plot::effective_plastic_strain``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::effective_plastic_strain")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.effective_plastic_strain()

-  ``lsdyna::d3plot::EPEL``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::EPEL")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.strain()

-  ``lsdyna::d3plot::strain_von_mises``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::strain_von_mises")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.strain_von_mises()

-  ``lsdyna::d3plot::history_var``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::history_var")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.history_var()

-  ``lsdyna::d3plot::thickness``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::thickness")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.thickness()

-  ``lsdyna::d3plot::element_dependent_var_1``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::element_dependent_var_1")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.element_dependent_var_1()

-  ``lsdyna::d3plot::element_dependent_var_2``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::element_dependent_var_2")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.element_dependent_var_2()

-  ``lsdyna::d3plot::mx``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::mx")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.mx()

-  ``lsdyna::d3plot::my``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::my")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.my()

-  ``lsdyna::d3plot::mxy``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::mxy")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.mxy()

-  ``lsdyna::d3plot::qx``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::qx")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.qx()

-  ``lsdyna::d3plot::qy``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::qy")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.qy()

-  ``lsdyna::d3plot::nx``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::nx")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.nx()

-  ``lsdyna::d3plot::ny``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::ny")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.ny()

-  ``lsdyna::d3plot::nxy``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::nxy")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.nxy()

-  ``lsdyna::d3plot::axial_force``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::axial_force")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.axial_force()

-  ``lsdyna::d3plot::s_shear_resultant``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::s_shear_resultant")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.s_shear_resultant()

-  ``lsdyna::d3plot::t_shear_resultant``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::t_shear_resultant")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.t_shear_resultant()

-  ``lsdyna::d3plot::s_bending_moment``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::s_bending_moment")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.s_bending_moment()

-  ``lsdyna::d3plot::t_bending_moment``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::t_bending_moment")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.t_bending_moment()

-  ``lsdyna::d3plot::torsional_resultant``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::torsional_resultant")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.torsional_resultant()

-  ``lsdyna::d3plot::axial_stress``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::axial_stress")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.axial_stress()

-  ``lsdyna::d3plot::rs_shear_stress``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::rs_shear_stress")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.rs_shear_stress()

-  ``lsdyna::d3plot::tr_shear_stress``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::tr_shear_stress")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.tr_shear_stress()

-  ``lsdyna::d3plot::axial_plastic_strain``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::axial_plastic_strain")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.axial_plastic_strain()

-  ``lsdyna::d3plot::axial_strain``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./d3plot', 'd3plot')

      resultOp = dpf.Operator("lsdyna::d3plot::axial_strain")
      resultOp.inputs.data_sources(ds)
      # set the time
      resultOp.inputs.time_scoping.connect([3])
      result = resultOp.outputs.axial_strain()

2. ``binout``

-  ``lsdyna::binout::meshes_provider``
  
   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources() ds.set_result_file_path(r'./binout', 'binout')

      meshOP = dpf.Operator("lsdyna::binout::meshes_provider")
      meshOP.inputs.data_sources.connect(ds) meshes = meshOP.outputs.meshes()
      mesh = meshes.get_mesh({})

-  ``lsdyna::binout::U``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources() ds.set_result_file_path(r'./binout', 'binout')

      coord = dpf.Operator("lsdyna::binout::U")
      coord.inputs.data_sources.connect(ds)
      # set the time
      coord.inputs.time_scoping.connect([3])
      fields = coord.outputs.displacement()

-  ``lsdyna::binout::TimeFreqSupportProvider``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r"./binout", "binout")

      op = dpf.Operator("lsdyna::binout::TimeFreqSupportProvider")
      op.inputs.data_sources(ds)
      result_time_freq_support = op.outputs.time_freq_support()

-  ``lsdyna::binout::glstat::TimeFreqSupportProvider``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./binout', 'binout')

      op = dpf.Operator("llsdyna::binout::glstat::TimeFreqSupportProvider")
      op.inputs.data_sources(ds)
      result_time_freq_support = op.outputs.time_freq_support()

-  ``lsdyna::binout::matsum::TimeFreqSupportProvider``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./binout', 'binout')

      op = dpf.Operator("llsdyna::binout::matsum::TimeFreqSupportProvider")
      op.inputs.data_sources(ds)
      result_time_freq_support = op.outputs.time_freq_support()

-  ``lsdyna::binout::rcforc::TimeFreqSupportProvider``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./binout', 'binout')

      op = dpf.Operator("llsdyna::binout::rcforc::TimeFreqSupportProvider")
      op.inputs.data_sources(ds)
      result_time_freq_support = op.outputs.time_freq_support()

-  ``lsdyna::binout::result_info_provider``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./binout', 'binout')

      resultInfoOp = dpf.Operator("lsdyna::binout::result_info_provider")
      resultInfoOp.inputs.data_sources(ds)
      result_info = resultInfoOp.outputs.result_info()

-  ``lsdyna::binout::S``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./binout', 'binout')

      stressOp = dpf.Operator("lsdyna::binout::S")
      stressOp.inputs.data_sources(ds)
      fields = stressOp.outputs.stress()

-  ``lsdyna::binout::EPEL``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./binout', 'binout')

      resultOp = dpf.Operator("lsdyna::binout::EPEL")
      resultOp.inputs.data_sources(ds)
      result = resultOp.outputs.strain()

-  ``lsdyna::binout::glstat``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./binout', 'binout')

      glstat_op = dpf.Operator("lsdyna::binout::glstat")
      glstat_op.inputs.data_sources(ds)
      fields = glstat_op.outputs.results()

      # get correponding result by component(from lsdyna::binout::result_info_provider)
      field0 = fields.get_field({"component":0})
      field1 = fields.get_field({"component":1})
      ...

-  ``lsdyna::binout::matsum``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./binout', 'binout')

      glstat_op = dpf.Operator("lsdyna::binout::matsum")
      glstat_op.inputs.data_sources(ds)
      fields = glstat_op.outputs.results()

      # get correponding result by component(from lsdyna::binout::result_info_provider)
      field0 = fields.get_field({"component":0})
      field1 = fields.get_field({"component":1})
      ...

-  ``lsdyna::binout::rcforc``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./binout', 'binout')

      glstat_op = dpf.Operator("lsdyna::binout::rcforc")
      glstat_op.inputs.data_sources(ds)
      fields = glstat_op.outputs.results()

      # get correponding result by component(from lsdyna::binout::result_info_provider)
      field0 = fields.get_field({"component":0})
      field1 = fields.get_field({"component":1})
      ...

1. ``nvh``

-  ``lsdyna::d3ssd::meshes_provider``

-  ``lsdyna::d3spcm::meshes_provider``

-  ``lsdyna::d3psd::meshes_provider``

-  ``lsdyna::d3rms::meshes_provider``

-  ``lsdyna::d3zcf::meshes_provider``

-  ``lsdyna::d3ssd::result_info_provider``

-  ``lsdyna::d3ssd::U``

-  ``lsdyna::d3ssd::V``

-  ``lsdyna::d3ssd::A``

-  ``lsdyna::d3ssd::S``

-  ``lsdyna::d3ssd::EPEL``

-  ``lsdyna::d3ssd::TimeFreqSupportProvider``

   same as d3plot

-  ``lsdyna::moddynout::TimeFreqSupportProvider``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./moddynout', 'moddynout')

      resultOp = dpf.Operator("lsdyna::moddynout::TimeFreqSupportProvider")
      resultOp.inputs.data_sources(ds)
      result = resultOp.outputs.time_freq_support()

-  ``lsdyna::moddynout::result_info_provider``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./moddynout', 'moddynout')

      resultInfoOp = dpf.Operator("lsdyna::moddynout::result_info_provider")
      resultInfoOp.inputs.data_sources(ds)
      result_info = resultInfoOp.outputs.result_info()

-  ``lsdyna::moddynout::F``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./moddynout', 'moddynout')

      times = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
      disOp = dpf.Operator("lsdyna::moddynout::F")
      disOp.inputs.data_sources(ds)
      disOp.inputs.time_scoping(times)
      fields = disOp.outputs.moddynout_force()

-  ``lsdyna::moddynout::A``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./moddynout', 'moddynout')

      times = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
      disOp = dpf.Operator("lsdyna::moddynout::A")
      disOp.inputs.data_sources(ds)
      disOp.inputs.time_scoping(times)
      fields = disOp.outputs.moddynout_acceleration()

-  ``lsdyna::moddynout::V``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./moddynout', 'moddynout')

      times = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
      disOp = dpf.Operator("lsdyna::moddynout::V")
      disOp.inputs.data_sources(ds)
      disOp.inputs.time_scoping(times)
      fields = disOp.outputs.moddynout_velocity()

-  ``lsdyna::moddynout::U``

   .. code:: python

      from ansys.dpf import core as dpf

      ds = dpf.DataSources()
      ds.set_result_file_path(r'./moddynout', 'moddynout')

      times = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
      disOp = dpf.Operator("lsdyna::moddynout::U")
      disOp.inputs.data_sources(ds)
      disOp.inputs.time_scoping(times)
      fields = disOp.outputs.moddynout_disp()
