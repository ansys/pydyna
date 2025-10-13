





:class:`ControlPoreFluid`
=========================


.. py:class:: control_pore_fluid.ControlPoreFluid(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_PORE_FLUID keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlPoreFluid

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~atype`
            - Get or set the Analysis type for pore water pressure calculations:
          * - :py:attr:`~wtable`
            - Get or set the Default z-coordinate of water table (where pore pressure is zero).
          * - :py:attr:`~pf_rho`
            - Get or set the Default density for pore water.
          * - :py:attr:`~grav`
            - Get or set the Gravitational acceleration used to calculate hydrostatic pore water pressure.
          * - :py:attr:`~pf_bulk`
            - Get or set the Default bulk modulus of pore fluid (stress units).
          * - :py:attr:`~output`
            - Get or set the Output flag controlling stresses to D3PLOT and D3THDT binary files:
          * - :py:attr:`~tmf`
            - Get or set the Initial Time Magnification factor on seepage (ATYPE=3,4 only).
          * - :py:attr:`~targ`
            - Get or set the Target for maximum change of excess pore pressure at any node, per timestep. If the actual change falls below the target, the time factor on the seepage calculation will be increased (see notes). If zero, the constant value of TMF is used. If non-zero, TMF is taken as the initial factor. .
          * - :py:attr:`~fmin`
            - Get or set the Minimum time factor on seepage calculation
          * - :py:attr:`~fmax`
            - Get or set the Maximum time factor on seepage calculation
          * - :py:attr:`~ftied_`
            - Get or set the Analysis type for pore water pressure calculations (see Remark 1):
          * - :py:attr:`~conv`
            - Get or set the Convergence tolerance for ATYPE=4 - maximum head change per timestep at any node (length units)
          * - :py:attr:`~conmax`
            - Get or set the Maximum factor on permeability with ATYPE=-4
          * - :py:attr:`~eterm`
            - Get or set the Event time termination (ATYPE=3)
          * - :py:attr:`~therm`
            - Get or set the Thermal expansion:  Volumetric strain per degree increase for undrained soil
          * - :py:attr:`~etfag`
            - Get or set the Flag for interpretation of time (see Time Factoring):


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

    from control_pore_fluid import ControlPoreFluid

Property detail
---------------

.. py:property:: atype
   :type: int


   
   Get or set the Analysis type for pore water pressure calculations:
   EQ.0:  No pore water pressure calculation.
   EQ.1:  Undrained analysis,
   EQ.2:  Drained analysis,
   EQ.3:  Time dependent consolidation (coupled)
   EQ.4:  Consolidate to steady state (uncoupled)
   EQ.5:  Drained in dynamic relaxation, undrained in transient.
   EQ.6:  As 4 but do not check convergence, continue to end time .
















   ..
       !! processed by numpydoc !!

.. py:property:: wtable
   :type: float


   
   Get or set the Default z-coordinate of water table (where pore pressure is zero).
















   ..
       !! processed by numpydoc !!

.. py:property:: pf_rho
   :type: Optional[float]


   
   Get or set the Default density for pore water.
















   ..
       !! processed by numpydoc !!

.. py:property:: grav
   :type: Optional[float]


   
   Get or set the Gravitational acceleration used to calculate hydrostatic pore water pressure.
















   ..
       !! processed by numpydoc !!

.. py:property:: pf_bulk
   :type: Optional[float]


   
   Get or set the Default bulk modulus of pore fluid (stress units).
















   ..
       !! processed by numpydoc !!

.. py:property:: output
   :type: int


   
   Get or set the Output flag controlling stresses to D3PLOT and D3THDT binary files:
   EQ.0:  total stresses are output
   EQ.1:  effective stresses are output, see notes
















   ..
       !! processed by numpydoc !!

.. py:property:: tmf
   :type: float


   
   Get or set the Initial Time Magnification factor on seepage (ATYPE=3,4 only).
   GT.0:   Factor (can be used with automatic control, see TARG, FMIN, FMAX).
   LT.0:  Load Curve ID (see *DEFINE_CURVE) giving Time Magnification Factor versus analysis time.
















   ..
       !! processed by numpydoc !!

.. py:property:: targ
   :type: float


   
   Get or set the Target for maximum change of excess pore pressure at any node, per timestep. If the actual change falls below the target, the time factor on the seepage calculation will be increased (see notes). If zero, the constant value of TMF is used. If non-zero, TMF is taken as the initial factor. .
















   ..
       !! processed by numpydoc !!

.. py:property:: fmin
   :type: float


   
   Get or set the Minimum time factor on seepage calculation
















   ..
       !! processed by numpydoc !!

.. py:property:: fmax
   :type: float


   
   Get or set the Maximum time factor on seepage calculation
















   ..
       !! processed by numpydoc !!

.. py:property:: ftied_
   :type: float


   
   Get or set the Analysis type for pore water pressure calculations (see Remark 1):
   EQ.0.0: Tied contacts act as impermeable membranes,
   EQ.1.0 : Fluid may flow freely through tied contacts.
















   ..
       !! processed by numpydoc !!

.. py:property:: conv
   :type: float


   
   Get or set the Convergence tolerance for ATYPE=4 - maximum head change per timestep at any node (length units)
















   ..
       !! processed by numpydoc !!

.. py:property:: conmax
   :type: float


   
   Get or set the Maximum factor on permeability with ATYPE=-4
















   ..
       !! processed by numpydoc !!

.. py:property:: eterm
   :type: float


   
   Get or set the Event time termination (ATYPE=3)
















   ..
       !! processed by numpydoc !!

.. py:property:: therm
   :type: float


   
   Get or set the Thermal expansion:  Volumetric strain per degree increase for undrained soil
















   ..
       !! processed by numpydoc !!

.. py:property:: etfag
   :type: int


   
   Get or set the Flag for interpretation of time (see Time Factoring):
   EQ.0:   Time means analysis time,
   EQ.1 : Time means event time..
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'PORE_FLUID'






