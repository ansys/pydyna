





:class:`ControlThermalSolver`
=============================


.. py:class:: control_thermal_solver.ControlThermalSolver(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_THERMAL_SOLVER keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlThermalSolver

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~atype`
            - Get or set the Thermal analysis type:
          * - :py:attr:`~ptype`
            - Get or set the Thermal problem type: (see *CONTROL_THERMAL_NONLINEAR if no-zero)
          * - :py:attr:`~solver`
            - Get or set the Thermal analysis solver type (see Remarks 1 and 2):
          * - :py:attr:`~gpt`
            - Get or set the Number of Gauss points to be used in the solid elements:
          * - :py:attr:`~eqheat`
            - Get or set the Mechanical equivalent of heat (default set to 1.0).
          * - :py:attr:`~fwork`
            - Get or set the Fraction of mechnical work converted into heat (default set to 1.0).
          * - :py:attr:`~sbc`
            - Get or set the Stefan Boltzmann constant. Value is used with enclosure radiation surfaces, see *BOUNDARY_RADIATION_...
          * - :py:attr:`~msglvl`
            - Get or set the Output message level  (For SOLVER > 10)
          * - :py:attr:`~maxitr`
            - Get or set the Maximum number of iterations.  For SOLVER >11.
          * - :py:attr:`~abstol`
            - Get or set the Absolute convergence tolerance.  For SOLVER >11.
          * - :py:attr:`~reltol`
            - Get or set the Relative convergence tolerance.  Replaces CGTOL for SOLVER >11.
          * - :py:attr:`~omega`
            - Get or set the Relaxation parameter omega for SOLVER 14 and 16.
          * - :py:attr:`~tsf`
            - Get or set the Thermal Speedup Factor. This factor multiplies all thermal parameters with units of time in the denominator (e.g., thermal conductivity, convection heat transfer coefficients). It is used to artificially time scale the problem.
          * - :py:attr:`~mxdmp`
            - Get or set the Matrix Dumping for SOLVER > 11
          * - :py:attr:`~dtvf`
            - Get or set the Time interval between view factor updates.
          * - :py:attr:`~varden`
            - Get or set the Variable thermal density for solid elements in a coupled thermal - structural analysis.Setting VARDEN to 1 or 2 will adjust the material density in the thermal solver to account for changes in element volume, for example, due to material compaction, thermal expansion, etc.In applications where volume changes are small, the default is recommended.
          * - :py:attr:`~ncycl`
            - Get or set the Thermal matrix reassembly frequency. Default is at every thermal cycle.


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

    from control_thermal_solver import ControlThermalSolver

Property detail
---------------

.. py:property:: atype
   :type: int


   
   Get or set the Thermal analysis type:
   EQ.0: Steady state analysis,
   EQ.1: transient analysis.
















   ..
       !! processed by numpydoc !!

.. py:property:: ptype
   :type: int


   
   Get or set the Thermal problem type: (see *CONTROL_THERMAL_NONLINEAR if no-zero)
   EQ.0: linear problem,
   EQ.1: nonlinear problem with material properties evaluated at gauss point temperature,
   EQ.2: nonlinear problem with material properties evaluated at element average temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: solver
   :type: int


   
   Get or set the Thermal analysis solver type (see Remarks 1 and 2):
   EQ.11:  Direct solver
   EQ.12 : Diagonal scaling(default for MPP) conjugate gradient iterative
   EQ.13 : Symmetric Gauss - Seidel conjugate gradient iterative
   EQ.14 : SSOR conjugate gradient iterative
   EQ.15 : ILDLT0(incomplete factorization) conjugate gradient iterative
   EQ.16 : Modified ILDLT0(incomplete factorization) conjugate gradient iterative
   EQ.17 : GMRES solver for conjugate heat transfer problems
   EQ.18 : ILDLT(T) (incomplete factorization with threshold pivoting)
   EQ.19 : Preconditioned conjugate gradient with MUMPS(see Remark Error!Reference source not found.in * CONTROL_IMPlICIT_SOLVER)
   EQ.30 : Direct nonsymmetric factorization
















   ..
       !! processed by numpydoc !!

.. py:property:: gpt
   :type: int


   
   Get or set the Number of Gauss points to be used in the solid elements:
   EQ.8: Use default is set to 8,
   EQ. 1: one point quadrature is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: eqheat
   :type: float


   
   Get or set the Mechanical equivalent of heat (default set to 1.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: fwork
   :type: float


   
   Get or set the Fraction of mechnical work converted into heat (default set to 1.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: sbc
   :type: float


   
   Get or set the Stefan Boltzmann constant. Value is used with enclosure radiation surfaces, see *BOUNDARY_RADIATION_...
















   ..
       !! processed by numpydoc !!

.. py:property:: msglvl
   :type: int


   
   Get or set the Output message level  (For SOLVER > 10)
   EQ.0:no output (default),
   EQ.1:summary information,
   EQ.2:detailed information, use only for debugging.
















   ..
       !! processed by numpydoc !!

.. py:property:: maxitr
   :type: int


   
   Get or set the Maximum number of iterations.  For SOLVER >11.
   EQ.0:use default value 500.
















   ..
       !! processed by numpydoc !!

.. py:property:: abstol
   :type: float


   
   Get or set the Absolute convergence tolerance.  For SOLVER >11.
   EQ.0.0:use default value 1.e-10.
















   ..
       !! processed by numpydoc !!

.. py:property:: reltol
   :type: float


   
   Get or set the Relative convergence tolerance.  Replaces CGTOL for SOLVER >11.
   EQ.0.0:use default value 1.e-06.
















   ..
       !! processed by numpydoc !!

.. py:property:: omega
   :type: float


   
   Get or set the Relaxation parameter omega for SOLVER 14 and 16.
   EQ.0.0:use default value 1.0 for Solver 14, use default value 0.0 for Solver 16..
















   ..
       !! processed by numpydoc !!

.. py:property:: tsf
   :type: float


   
   Get or set the Thermal Speedup Factor. This factor multiplies all thermal parameters with units of time in the denominator (e.g., thermal conductivity, convection heat transfer coefficients). It is used to artificially time scale the problem.
   EQ.0.0: Default value 1.0,
   LT.0.0 : | TSF | is a load curve ID.Curve defines speedup factor as a function of time.
   Its main use is in metal stamping.If the velocity of the stamping punch is artificially increased by 1000, then set TSF = 1000 to scale the thermal parameters.
















   ..
       !! processed by numpydoc !!

.. py:property:: mxdmp
   :type: int


   
   Get or set the Matrix Dumping for SOLVER > 11
   EQ.0:   No Dumping
   GT.0:   Dump using ASCII format every MXDMP time steps.
   LT.0:   Dump using binary format every |MXDMP| time steps.
















   ..
       !! processed by numpydoc !!

.. py:property:: dtvf
   :type: float


   
   Get or set the Time interval between view factor updates.
















   ..
       !! processed by numpydoc !!

.. py:property:: varden
   :type: int


   
   Get or set the Variable thermal density for solid elements in a coupled thermal - structural analysis.Setting VARDEN to 1 or 2 will adjust the material density in the thermal solver to account for changes in element volume, for example, due to material compaction, thermal expansion, etc.In applications where volume changes are small, the default is recommended.
   EQ.0:   Thermal density remains constant and equal to TRO as given in * MAT_THERMAL_option(default).
   EQ.1 : Thermal density varies to account for change in volume.If an equation of state(*EOS) is used, the initial internal energy specified therein is taken into account.
   EQ.2 : Thermal density varies to account for change in volume.The initial internal energy is not considered.
















   ..
       !! processed by numpydoc !!

.. py:property:: ncycl
   :type: int


   
   Get or set the Thermal matrix reassembly frequency. Default is at every thermal cycle.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'THERMAL_SOLVER'






