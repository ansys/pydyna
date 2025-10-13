





:class:`BoundaryPoreFluidSet`
=============================


.. py:class:: boundary_pore_fluid_set.BoundaryPoreFluidSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_PORE_FLUID_SET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryPoreFluidSet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~psid`
            - Get or set the Partset ID (PID),see *PART_SET.  All elements within the part must lie below the water table..
          * - :py:attr:`~wtable`
            - Get or set the Z-coordinate at which pore pressure = 0 (water table)
          * - :py:attr:`~pf_rho`
            - Get or set the Density of pore water in soil skeleton:  EQ.0:  Default density specified on *CONTROL_PORE_FLUID card is used.
          * - :py:attr:`~atype`
            - Get or set the Analysis type for Parts:
          * - :py:attr:`~pf_bulk`
            - Get or set the Bulk modulus of pore fluid:EQ.0: Default to value specified on *CONTROL_PORE_FLUID
          * - :py:attr:`~acurve`
            - Get or set the Curve of analysis type vs time (see notes below)
          * - :py:attr:`~wtcur`
            - Get or set the Curve of water table (z-coordinate) vs time
          * - :py:attr:`~suclim`
            - Get or set the Suction limit (defined in head, i.e. length units). Must not be negative. See notes


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

    from boundary_pore_fluid_set import BoundaryPoreFluidSet

Property detail
---------------

.. py:property:: psid
   :type: Optional[int]


   
   Get or set the Partset ID (PID),see *PART_SET.  All elements within the part must lie below the water table..
















   ..
       !! processed by numpydoc !!

.. py:property:: wtable
   :type: Optional[float]


   
   Get or set the Z-coordinate at which pore pressure = 0 (water table)
















   ..
       !! processed by numpydoc !!

.. py:property:: pf_rho
   :type: Optional[float]


   
   Get or set the Density of pore water in soil skeleton:  EQ.0:  Default density specified on *CONTROL_PORE_FLUID card is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: atype
   :type: int


   
   Get or set the Analysis type for Parts:
   EQ.0: Default to value specified on *CONTROL_PORE_FLUID
   EQ 1: Undrained analysis
   EQ 2: Drained analysis
   EQ 3:Time dependent consolidation (coupled)
   EQ 4:Consolidate to steady state (uncoupled)
   EQ 5:Drained in dynamic relaxation, undrained in transient
















   ..
       !! processed by numpydoc !!

.. py:property:: pf_bulk
   :type: Optional[float]


   
   Get or set the Bulk modulus of pore fluid:EQ.0: Default to value specified on *CONTROL_PORE_FLUID
















   ..
       !! processed by numpydoc !!

.. py:property:: acurve
   :type: Optional[int]


   
   Get or set the Curve of analysis type vs time (see notes below)
















   ..
       !! processed by numpydoc !!

.. py:property:: wtcur
   :type: Optional[int]


   
   Get or set the Curve of water table (z-coordinate) vs time
















   ..
       !! processed by numpydoc !!

.. py:property:: suclim
   :type: Optional[float]


   
   Get or set the Suction limit (defined in head, i.e. length units). Must not be negative. See notes
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'PORE_FLUID_SET'






