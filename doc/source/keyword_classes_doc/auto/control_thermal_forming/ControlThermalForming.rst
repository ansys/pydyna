





:class:`ControlThermalForming`
==============================


.. py:class:: control_thermal_forming.ControlThermalForming(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_THERMAL_FORMING keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlThermalForming

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~its`
            - Get or set the Initial thermal time step size.
          * - :py:attr:`~ptype`
            - Get or set the Thermal problem type (see Remark 1 for determining the type of problem):
          * - :py:attr:`~tsf`
            - Get or set the Thermal Speedup Factor. This factor multiplies all thermal
          * - :py:attr:`~thshel`
            - Get or set the Thermal shell option:
          * - :py:attr:`~ithoff`
            - Get or set the Flag for offsetting thermal contact surfaces for thick thermal shells:
          * - :py:attr:`~solver`
            - Get or set the Thermal analysis solver type (see *CONTROL_THERMAL_SOLVER).
          * - :py:attr:`~fwork`
            - Get or set the Fraction of mechanical work converted into heat
          * - :py:attr:`~k`
            - Get or set the Thermal conductivity of fluid between the contact surfaces.
          * - :py:attr:`~frad`
            - Get or set the Radiation factor between the contact surfaces.
          * - :py:attr:`~h0`
            - Get or set the Heat transfer conductance for closed gaps. Use this heat transfer
          * - :py:attr:`~lmin`
            - Get or set the Minimum gap, 𝑙min; use the heat transfer conductance defined
          * - :py:attr:`~lmax`
            - Get or set the No thermal contact if gap is greater than this value (..max).
          * - :py:attr:`~ftoslv`
            - Get or set the Fraction, .. , of sliding friction energy partitioned to the slave
          * - :py:attr:`~bc_flg`
            - Get or set the Thermal boundary condition flag:
          * - :py:attr:`~algo`
            - Get or set the Contact algorithm type.
          * - :py:attr:`~lcfst`
            - Get or set the Load curve number for static coefficient of friction as a function of
          * - :py:attr:`~lcfdt`
            - Get or set the Load curve number for dynamic coefficient of friction as a
          * - :py:attr:`~formula`
            - Get or set the Formula that defines the contact heat conductance as a function of
          * - :py:attr:`~a`
            - Get or set the Load curve ID for the a coefficient used in the formula.
          * - :py:attr:`~b`
            - Get or set the Load curve ID for the b coefficient used in the formula
          * - :py:attr:`~c`
            - Get or set the Load curve ID for the c coefficient used in the formula
          * - :py:attr:`~d`
            - Get or set the Load curve ID for the d coefficient used in the formula.
          * - :py:attr:`~lch`
            - Get or set the Load curve ID for h. This parameter can refer to a curve ID (see


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

    from control_thermal_forming import ControlThermalForming

Property detail
---------------

.. py:property:: its
   :type: Optional[float]


   
   Get or set the Initial thermal time step size.
















   ..
       !! processed by numpydoc !!

.. py:property:: ptype
   :type: int


   
   Get or set the Thermal problem type (see Remark 1 for determining the type of problem):
   EQ.0: linear problem
   EQ.1: nonlinear problem with material properties evaluated at
   the temperature of the gauss point
   EQ.2: nonlinear problem with material properties evaluated at
   the average temperature of the element.
















   ..
       !! processed by numpydoc !!

.. py:property:: tsf
   :type: float


   
   Get or set the Thermal Speedup Factor. This factor multiplies all thermal
   parameters with units of time in the denominator (such as
   thermal conductivity and convection heat transfer coefficients). It
   is used to artificially scale the problem in time. For example, if
   the velocity of the stamping punch is artificially increased by
   1000, then set TSF = 1000 to scale the thermal parameters.
















   ..
       !! processed by numpydoc !!

.. py:property:: thshel
   :type: int


   
   Get or set the Thermal shell option:
   EQ.0: no temperature gradient is considered through the shell
   thickness.
   EQ.1: a temperature gradient is calculated through the shell    thickness.
















   ..
       !! processed by numpydoc !!

.. py:property:: ithoff
   :type: int


   
   Get or set the Flag for offsetting thermal contact surfaces for thick thermal shells:
   EQ.0: no offset; if thickness is not included in the contact, the
   heat will be transferred between the mid-surfaces of the
   corresponding contact segments (shells).
   EQ.1: offsets are applied so that contact heat transfer is always
   between the outer surfaces of the contact segments (shells).
















   ..
       !! processed by numpydoc !!

.. py:property:: solver
   :type: int


   
   Get or set the Thermal analysis solver type (see *CONTROL_THERMAL_SOLVER).
   For SMP only:
   EQ.1: using solver 11 (enter -1 to use the old ACTCOL solver)
   EQ.2: nonsymmetric direct solver
   EQ.3: diagonal scaled conjugate gradient iterative (default)
   EQ.4: incomplete choleski conjugate gradient iterativeEQ.5: nonsymmetric diagonal scaled bi-conjugate gradient
   For SMP or MPP:
   EQ.11: direct solver
   EQ.12: diagonal scaling (default for MPP) conjugate gradient iterative
   EQ.13: symmetric Gauss-Siedel conjugate gradient iterative
   EQ.14: SSOR conjugate gradient iterative
   EQ.15: ILDLT0 (incomplete factorization) conjugate gradient iterative
   EQ.16: modified ILDLT0 (incomplete factorization) conjugate gradient iterative
   For Conjugate Heat transfer problems in SMP or MPP:
   EQ.17: GMRES solver.
















   ..
       !! processed by numpydoc !!

.. py:property:: fwork
   :type: float


   
   Get or set the Fraction of mechanical work converted into heat
















   ..
       !! processed by numpydoc !!

.. py:property:: k
   :type: Optional[float]


   
   Get or set the Thermal conductivity of fluid between the contact surfaces.
















   ..
       !! processed by numpydoc !!

.. py:property:: frad
   :type: Optional[float]


   
   Get or set the Radiation factor between the contact surfaces.
















   ..
       !! processed by numpydoc !!

.. py:property:: h0
   :type: Optional[float]


   
   Get or set the Heat transfer conductance for closed gaps. Use this heat transfer
   conductance for gaps in the range.
















   ..
       !! processed by numpydoc !!

.. py:property:: lmin
   :type: Optional[float]


   
   Get or set the Minimum gap, 𝑙min; use the heat transfer conductance defined
   (H0) for gap thicknesses less than this value.
   LT.0.0: -LMIN is a load curve ID defining 𝑙min as a function of time.
















   ..
       !! processed by numpydoc !!

.. py:property:: lmax
   :type: Optional[float]


   
   Get or set the No thermal contact if gap is greater than this value (..max).
















   ..
       !! processed by numpydoc !!

.. py:property:: ftoslv
   :type: float


   
   Get or set the Fraction, .. , of sliding friction energy partitioned to the slave
   surface. Energy partitioned to the master surface is (1 . .. ).EQ.0: Default set to 0.5: The sliding friction energy is
   partitioned 50% - 50% to the slave and master surfaces in contact.
















   ..
       !! processed by numpydoc !!

.. py:property:: bc_flg
   :type: int


   
   Get or set the Thermal boundary condition flag:
   EQ.0: thermal boundary conditions are on when parts are in contact.t
   EQ.1: thermal boundary conditions are off when parts are in contact.
















   ..
       !! processed by numpydoc !!

.. py:property:: algo
   :type: int


   
   Get or set the Contact algorithm type.
   EQ.0: two way contact; both surfaces change temperature due to contact.
   EQ.1: one way contact; master surface does not change
   temperature due to contact. Slave surface does change temperature
















   ..
       !! processed by numpydoc !!

.. py:property:: lcfst
   :type: Optional[int]


   
   Get or set the Load curve number for static coefficient of friction as a function of
   temperature. The load curve value multiplies the coefficient value FS
















   ..
       !! processed by numpydoc !!

.. py:property:: lcfdt
   :type: Optional[int]


   
   Get or set the Load curve number for dynamic coefficient of friction as a
   function of temperature. The load curve value multiplies the coefficient value FD.
















   ..
       !! processed by numpydoc !!

.. py:property:: formula
   :type: int


   
   Get or set the Formula that defines the contact heat conductance as a function of
   temperature and pressure. See the manual
   This is equivalent to defining the keyword *USER_INTERFACE_CONDUCTIVITY. The user subroutine usrhcon will be called for this contact interface to define the contact heat transfer coefficient
















   ..
       !! processed by numpydoc !!

.. py:property:: a
   :type: Optional[int]


   
   Get or set the Load curve ID for the a coefficient used in the formula.
















   ..
       !! processed by numpydoc !!

.. py:property:: b
   :type: Optional[int]


   
   Get or set the Load curve ID for the b coefficient used in the formula
















   ..
       !! processed by numpydoc !!

.. py:property:: c
   :type: Optional[int]


   
   Get or set the Load curve ID for the c coefficient used in the formula
















   ..
       !! processed by numpydoc !!

.. py:property:: d
   :type: Optional[int]


   
   Get or set the Load curve ID for the d coefficient used in the formula.
















   ..
       !! processed by numpydoc !!

.. py:property:: lch
   :type: Optional[int]


   
   Get or set the Load curve ID for h. This parameter can refer to a curve ID (see
   *DEFINE_CURVE) or a function ID (see *DEFINE_FUNCTION).
   When LCH is a curve ID (and a function ID) it is interpreted as follows:
   GT.0: the heat transfer coefficient is defined as a function of
   time, 𝑡, by a curve consisting of (𝑡, ℎ(𝑡)) data pairs.
   LT.0: the heat transfer coefficient is defined as a function of
   temperature, 𝑇, by a curve consisting of (𝑇, ℎ(𝑇)) data pairs
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'THERMAL_FORMING'






