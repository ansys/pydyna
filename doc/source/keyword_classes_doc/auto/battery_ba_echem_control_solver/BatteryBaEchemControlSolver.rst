





:class:`BatteryBaEchemControlSolver`
====================================


.. py:class:: battery_ba_echem_control_solver.BatteryBaEchemControlSolver(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BATTERY_BA_ECHEM_CONTROL_SOLVER keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BatteryBaEchemControlSolver

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

    from battery_ba_echem_control_solver import BatteryBaEchemControlSolver

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



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BATTERY'


.. py:attribute:: subkeyword
   :value: 'BA_ECHEM_CONTROL_SOLVER'






