





:class:`BatteryEchemControlSolver`
==================================


.. py:class:: battery_echem_control_solver.BatteryEchemControlSolver(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BATTERY_ECHEM_CONTROL_SOLVER keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BatteryEchemControlSolver

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~imodel`
            - Get or set the Sets the battery model.
          * - :py:attr:`~igeom`
            - Get or set the Sets the geometric dimension:
          * - :py:attr:`~ncycle`
            - Get or set the The number of cycles to run. Default is 1 cycle
          * - :py:attr:`~aging`
            - Get or set the Aging model. 1 for ON, and 0 for OFF
          * - :py:attr:`~tra`
            - Get or set the Thermal runaway model. 1for ON, and 0 for OFF.
          * - :py:attr:`~gas`
            - Get or set the Gas generation model (scheduled)
          * - :py:attr:`~esolid`
            - Get or set the The concentration of the electrode particle model
          * - :py:attr:`~irun`
            - Get or set the Battery simulation cycle termination criterion
          * - :py:attr:`~lcur`
            - Get or set the Running current.
          * - :py:attr:`~curv`
            - Get or set the Current value to run
          * - :py:attr:`~ctime`
            - Get or set the Running time for the cycle
          * - :py:attr:`~vcut`
            - Get or set the A voltage to terminate
          * - :py:attr:`~mws`
            - Get or set the Molecular weight of the SEI
          * - :py:attr:`~dens`
            - Get or set the Density of the SEI
          * - :py:attr:`~brugs`
            - Get or set the The Brugmann constant of the SEI
          * - :py:attr:`~epss`
            - Get or set the Initial SEI porosity
          * - :py:attr:`~cseio`
            - Get or set the Initial SEI concentration, [mol/m3]
          * - :py:attr:`~tseio`
            - Get or set the Initial thickness of the SEI layer
          * - :py:attr:`~ecdo`
            - Get or set the The exchange current density for the SEI reaction
          * - :py:attr:`~kfs`
            - Get or set the The reaction rate constant for the SEI reaction
          * - :py:attr:`~ceco`
            - Get or set the Initial concentration of EC (Ethylene Carbonate)
          * - :py:attr:`~ecdf`
            - Get or set the Diffusion coefficient of EC
          * - :py:attr:`~hofeln`
            - Get or set the Formation enthalpy of electrolyte, [KJ/mol]
          * - :py:attr:`~hofli`
            - Get or set the Formation enthalpy of Li+, [KJ/mol]
          * - :py:attr:`~hofsei`
            - Get or set the Formation enthalpy of the SEI layer, [KJ/mol]
          * - :py:attr:`~hofc2h4`
            - Get or set the Formation enthalpy of ethylene, [KJ/mol]
          * - :py:attr:`~afi`
            - Get or set the Frequency factor for the reaction
          * - :py:attr:`~eat`
            - Get or set the Activation energy for the reaction
          * - :py:attr:`~hoflc`
            - Get or set the Formation enthalpy of LC (Li2CO3)
          * - :py:attr:`~hofco2`
            - Get or set the Formation enthalpy of CO2
          * - :py:attr:`~hofo2`
            - Get or set the Formation enthalpy of O2
          * - :py:attr:`~file1`
            - Get or set the
          * - :py:attr:`~file2`
            - Get or set the


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

    from battery_echem_control_solver import BatteryEchemControlSolver

Property detail
---------------

.. py:property:: imodel
   :type: Optional[int]


   
   Get or set the Sets the battery model.
   EQ.1:   A single insertion model
   EQ.2 : Dual insertion model
















   ..
       !! processed by numpydoc !!

.. py:property:: igeom
   :type: Optional[int]


   
   Get or set the Sets the geometric dimension:
   EQ.1:   1D Electrochemical model
   EQ.11 : 1D Aging and Thermal Runaway model
   EQ.101 : 1D ECTM model
   EQ.111 : 1D ECTM with A & T model
















   ..
       !! processed by numpydoc !!

.. py:property:: ncycle
   :type: int


   
   Get or set the The number of cycles to run. Default is 1 cycle
















   ..
       !! processed by numpydoc !!

.. py:property:: aging
   :type: int


   
   Get or set the Aging model. 1 for ON, and 0 for OFF
















   ..
       !! processed by numpydoc !!

.. py:property:: tra
   :type: int


   
   Get or set the Thermal runaway model. 1for ON, and 0 for OFF.
















   ..
       !! processed by numpydoc !!

.. py:property:: gas
   :type: int


   
   Get or set the Gas generation model (scheduled)
















   ..
       !! processed by numpydoc !!

.. py:property:: esolid
   :type: int


   
   Get or set the The concentration of the electrode particle model
   EQ.0: Superposition method.
   EQ.1 : Full equation method.
















   ..
       !! processed by numpydoc !!

.. py:property:: irun
   :type: Optional[int]


   
   Get or set the Battery simulation cycle termination criterion
   EQ.1:   The current cycle runs for a given time
   EQ.2 : The current cycle runs until the cell voltage reaches VCUT
















   ..
       !! processed by numpydoc !!

.. py:property:: lcur
   :type: Optional[int]


   
   Get or set the Running current.
   EQ.0:   Constant current.
   EQ.1 : Variable current
















   ..
       !! processed by numpydoc !!

.. py:property:: curv
   :type: Optional[float]


   
   Get or set the Current value to run
















   ..
       !! processed by numpydoc !!

.. py:property:: ctime
   :type: float


   
   Get or set the Running time for the cycle
















   ..
       !! processed by numpydoc !!

.. py:property:: vcut
   :type: float


   
   Get or set the A voltage to terminate
















   ..
       !! processed by numpydoc !!

.. py:property:: mws
   :type: Optional[float]


   
   Get or set the Molecular weight of the SEI
















   ..
       !! processed by numpydoc !!

.. py:property:: dens
   :type: Optional[float]


   
   Get or set the Density of the SEI
















   ..
       !! processed by numpydoc !!

.. py:property:: brugs
   :type: Optional[float]


   
   Get or set the The Brugmann constant of the SEI
















   ..
       !! processed by numpydoc !!

.. py:property:: epss
   :type: Optional[float]


   
   Get or set the Initial SEI porosity
















   ..
       !! processed by numpydoc !!

.. py:property:: cseio
   :type: Optional[float]


   
   Get or set the Initial SEI concentration, [mol/m3]
















   ..
       !! processed by numpydoc !!

.. py:property:: tseio
   :type: Optional[float]


   
   Get or set the Initial thickness of the SEI layer
















   ..
       !! processed by numpydoc !!

.. py:property:: ecdo
   :type: Optional[float]


   
   Get or set the The exchange current density for the SEI reaction
















   ..
       !! processed by numpydoc !!

.. py:property:: kfs
   :type: Optional[float]


   
   Get or set the The reaction rate constant for the SEI reaction
















   ..
       !! processed by numpydoc !!

.. py:property:: ceco
   :type: Optional[float]


   
   Get or set the Initial concentration of EC (Ethylene Carbonate)
















   ..
       !! processed by numpydoc !!

.. py:property:: ecdf
   :type: Optional[float]


   
   Get or set the Diffusion coefficient of EC
















   ..
       !! processed by numpydoc !!

.. py:property:: hofeln
   :type: Optional[int]


   
   Get or set the Formation enthalpy of electrolyte, [KJ/mol]
















   ..
       !! processed by numpydoc !!

.. py:property:: hofli
   :type: Optional[int]


   
   Get or set the Formation enthalpy of Li+, [KJ/mol]
















   ..
       !! processed by numpydoc !!

.. py:property:: hofsei
   :type: Optional[float]


   
   Get or set the Formation enthalpy of the SEI layer, [KJ/mol]
















   ..
       !! processed by numpydoc !!

.. py:property:: hofc2h4
   :type: Optional[float]


   
   Get or set the Formation enthalpy of ethylene, [KJ/mol]
















   ..
       !! processed by numpydoc !!

.. py:property:: afi
   :type: Optional[float]


   
   Get or set the Frequency factor for the reaction
















   ..
       !! processed by numpydoc !!

.. py:property:: eat
   :type: Optional[float]


   
   Get or set the Activation energy for the reaction
















   ..
       !! processed by numpydoc !!

.. py:property:: hoflc
   :type: Optional[float]


   
   Get or set the Formation enthalpy of LC (Li2CO3)
















   ..
       !! processed by numpydoc !!

.. py:property:: hofco2
   :type: Optional[float]


   
   Get or set the Formation enthalpy of CO2
















   ..
       !! processed by numpydoc !!

.. py:property:: hofo2
   :type: Optional[float]


   
   Get or set the Formation enthalpy of O2
















   ..
       !! processed by numpydoc !!

.. py:property:: file1
   :type: Optional[str]


   
   Get or set the 
















   ..
       !! processed by numpydoc !!

.. py:property:: file2
   :type: Optional[str]


   
   Get or set the 
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BATTERY'


.. py:attribute:: subkeyword
   :value: 'ECHEM_CONTROL_SOLVER'






