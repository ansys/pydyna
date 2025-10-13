





:class:`DefinePressureTube`
===========================


.. py:class:: define_pressure_tube.DefinePressureTube(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_PRESSURE_TUBE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefinePressureTube

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID of tube. The tube(s) consists of all the beam elements in the
          * - :py:attr:`~ws`
            - Get or set the Wave propagation speed.
          * - :py:attr:`~pr`
            - Get or set the Initial tube pressure.
          * - :py:attr:`~mtd`
            - Get or set the Solution method:
          * - :py:attr:`~type`
            - Get or set the Tube elements:
          * - :py:attr:`~visc`
            - Get or set the MTD.EQ.0: Artificial viscosity multiplier (VISC > 0.0); see Remark 2. A smaller value gives a more resolved pulse at shorter wavelengths but may lead to instabilities. For typical automotive crash applications (tube length ~2m, diameter ~5mm, pressure pulse width ~5ms) the default value is recommended.
          * - :py:attr:`~cfl`
            - Get or set the Stability factor (CFL > 0.0); see Remark 1. A smaller value leads to increased stability at the expense of increased computational cost.
          * - :py:attr:`~damp`
            - Get or set the Linear damping (DAMP ≥ 0.0); see Remark 1.
          * - :py:attr:`~bndl`
            - Get or set the Left boundary condition (0 ≤ BNDi ≤ 1); see Remark 2. Special cases are:
          * - :py:attr:`~bndr`
            - Get or set the Right boundary condition (0 ≤ BNDi ≤ 1); see Remark 2. Special cases are:
          * - :py:attr:`~cavl`
            - Get or set the Left cavity; see Remark 3.
          * - :py:attr:`~cavr`
            - Get or set the Right cavity; see Remark 3.
          * - :py:attr:`~snode`
            - Get or set the Optional starting node. This node determines the left end of the tube. If not set, the tube starts at the lowest numbered beam node
          * - :py:attr:`~nshl`
            - Get or set the Number of automatically generated shells/solids on circumference of tube
          * - :py:attr:`~elform`
            - Get or set the ELFORM for automatically generated shells/solids; see *SECTION_‌SHELL/SOLID.
          * - :py:attr:`~nip`
            - Get or set the Number of through thickness integration points for automatically generated shells; see NIP in *SECTION_‌SHELL.
          * - :py:attr:`~shrf`
            - Get or set the Shear correction factor for automatically generated shells; see SHRF in *SECTION_‌SHELL
          * - :py:attr:`~bpid`
            - Get or set the Optional PID given to beam elements when automatically generating shells/solids.
          * - :py:attr:`~nsld`
            - Get or set the Number of automatically generated shells/solids on circumference of tube
          * - :py:attr:`~nthk`
            - Get or set the Number of solid elements in thickness of tube for automatically generated solids.
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

    from define_pressure_tube import DefinePressureTube

Property detail
---------------

.. py:property:: pid
   :type: int


   
   Get or set the Part ID of tube. The tube(s) consists of all the beam elements in the
   part. Only ELFORM = 1,4,5,11 are allowed. Each set of joint beam
   elements in the part will model a tube and the beam elements may
   not contain junctions. Also, two different parts where
   *DEFINE_PRESSURE_TUBE is applied may not share beam nodes.
   For MPP all elements in the part will be on a single processor, so it is
   recommended that the part should only contain beam elements.
















   ..
       !! processed by numpydoc !!

.. py:property:: ws
   :type: float


   
   Get or set the Wave propagation speed.
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: float


   
   Get or set the Initial tube pressure.
















   ..
       !! processed by numpydoc !!

.. py:property:: mtd
   :type: int


   
   Get or set the Solution method:
   EQ.0:   Standard Galerkin FEM.
   EQ.1: Discontinuous Galerkin
   EQ.2: Discontinuous Galerkin on isothermal Euler equations
















   ..
       !! processed by numpydoc !!

.. py:property:: type
   :type: int


   
   Get or set the Tube elements:
   EQ.0:   The tube is entirely simulated with beam elements. Cross section area is given from contact penetration of the beam elements. The mechanical response in radial direction of the beam elements is governed by contact stiffness. Only mortar contacts are supported.
   EQ.1:   The tube is simulated by automatic generation of shell elements, which are assigned the beam part ID and the beam material model. A new part ID is given to the beam elements, and those are no longer part of the mechanical solution. Contacts and other properties associated with the old beam part ID will now apply to the new shell part. Cross section area is given from the shell element nodes, and the mechanical response is governed entirely by the shells. Supports all contact definitions.
   EQ.2:   the tube is simulated by automatic generation of solid elements, similarly to TYPE = 1 above.
   LT.0:   automatic generation of elements as above, but the beam nodes are given new nodal IDs.
   The old beam NIDs are moved to the automatically generated tube (one row of nodes along the length).
   Any nodal constraints will thus apply to the new tube instead of the beam element tube.
   See Figure 0-1 for an example of different values of TYPE and how they affect nodal constraints
















   ..
       !! processed by numpydoc !!

.. py:property:: visc
   :type: float


   
   Get or set the MTD.EQ.0: Artificial viscosity multiplier (VISC > 0.0); see Remark 2. A smaller value gives a more resolved pulse at shorter wavelengths but may lead to instabilities. For typical automotive crash applications (tube length ~2m, diameter ~5mm, pressure pulse width ~5ms) the default value is recommended.
   MTD.GT.0: Slope limiter smoothing factor; see Remark 2. Smaller value gives a more resolved pulse at shorter wavelengths but may lead to instabilities.Larger value leads to a smeared pulse.
















   ..
       !! processed by numpydoc !!

.. py:property:: cfl
   :type: float


   
   Get or set the Stability factor (CFL > 0.0); see Remark 1. A smaller value leads to increased stability at the expense of increased computational cost.
   For typical automotive crash applications, the default value is recommended.
















   ..
       !! processed by numpydoc !!

.. py:property:: damp
   :type: float


   
   Get or set the Linear damping (DAMP ≥ 0.0); see Remark 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: bndl
   :type: float


   
   Get or set the Left boundary condition (0 ≤ BNDi ≤ 1); see Remark 2. Special cases are:
   EQ.0.0: closed tube end, that is, zero velocity boundary condition
   EQ:0.5: non-reflecting boundary condition
   EQ:1.0: open tube end, that is, constant pressure boundary condition
   Left tube end is automatically assigned to the lowest/highest beam node number on the tube, respectively.
















   ..
       !! processed by numpydoc !!

.. py:property:: bndr
   :type: float


   
   Get or set the Right boundary condition (0 ≤ BNDi ≤ 1); see Remark 2. Special cases are:
   EQ.0.0: closed tube end, that is, zero velocity boundary condition
   EQ:0.5: non-reflecting boundary condition
   EQ:1.0: open tube end, that is, constant pressure boundary condition
   Right tube end is automatically assigned to the lowest/highest beam node number on the tube, respectively.
















   ..
       !! processed by numpydoc !!

.. py:property:: cavl
   :type: float


   
   Get or set the Left cavity; see Remark 3.
   GT.0.0: elements near the end of the tube are replaced with a cavity.
   The integer part of CAVi determines the number of beam elements that belong to the cavity.
   The remainder of CAVi determines the boundary condition on the interface between the tube and the cavity.
   LT:0.0: the tube is extended with a cavity by adding new beam elements.
   The length of the added cavity is given by  where  truncates the decimal portion of  (leaving an integer).
   The remainder of  determines the boundary condition on the interface between the tube and the cavity.
















   ..
       !! processed by numpydoc !!

.. py:property:: cavr
   :type: float


   
   Get or set the Right cavity; see Remark 3.
   GT.0.0: elements near the end of the tube are replaced with a cavity.
   The integer part of CAVi determines the number of beam elements that belong to the cavity.
   The remainder of CAVi determines the boundary condition on the interface between the tube and the cavity.
   LT:0.0: the tube is extended with a cavity by adding new beam elements.
   The length of the added cavity is given by  where  truncates the decimal portion of  (leaving an integer).
   The remainder of  determines the boundary condition on the interface between the tube and the cavity.
















   ..
       !! processed by numpydoc !!

.. py:property:: snode
   :type: int


   
   Get or set the Optional starting node. This node determines the left end of the tube. If not set, the tube starts at the lowest numbered beam node
















   ..
       !! processed by numpydoc !!

.. py:property:: nshl
   :type: int


   
   Get or set the Number of automatically generated shells/solids on circumference of tube
















   ..
       !! processed by numpydoc !!

.. py:property:: elform
   :type: int


   
   Get or set the ELFORM for automatically generated shells/solids; see *SECTION_‌SHELL/SOLID.
















   ..
       !! processed by numpydoc !!

.. py:property:: nip
   :type: int


   
   Get or set the Number of through thickness integration points for automatically generated shells; see NIP in *SECTION_‌SHELL.
















   ..
       !! processed by numpydoc !!

.. py:property:: shrf
   :type: float


   
   Get or set the Shear correction factor for automatically generated shells; see SHRF in *SECTION_‌SHELL
















   ..
       !! processed by numpydoc !!

.. py:property:: bpid
   :type: Optional[int]


   
   Get or set the Optional PID given to beam elements when automatically generating shells/solids.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsld
   :type: int


   
   Get or set the Number of automatically generated shells/solids on circumference of tube
















   ..
       !! processed by numpydoc !!

.. py:property:: nthk
   :type: int


   
   Get or set the Number of solid elements in thickness of tube for automatically generated solids.
















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
   :value: 'DEFINE'


.. py:attribute:: subkeyword
   :value: 'PRESSURE_TUBE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





