





:class:`ControlImplicitTermination`
===================================


.. py:class:: control_implicit_termination.ControlImplicitTermination(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_IMPLICIT_TERMINATION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlImplicitTermination

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~deltau`
            - Get or set the Alternate termination criteria for implicit transient simulation.
          * - :py:attr:`~delta1`
            - Get or set the If _| max displacement for single dof |_characteristic length of model is less than DELTA1 then implicit will terminate
          * - :py:attr:`~ketol`
            - Get or set the If Kinetic Energy drops below KETOL for STEP  consecutive implicit time steps then implicit will terminate
          * - :py:attr:`~ietol`
            - Get or set the If Internal Energy drops below IETOL for STEP consecutive implicit time steps then implicit will terminate
          * - :py:attr:`~tetol`
            - Get or set the If Total Energy drops below TETOL for NSTEP  consecutive implicit time steps then implicit will terminate
          * - :py:attr:`~nstep`
            - Get or set the Number of time steps used in tests for Kinetic Energy, Internal Energy, and/or Total Energy.


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

    from control_implicit_termination import ControlImplicitTermination

Property detail
---------------

.. py:property:: deltau
   :type: float


   
   Get or set the Alternate termination criteria for implicit transient simulation.
   EQ.0.0:  terminate based on ENDTIM (default)
   NE.0.0:  terminate when displacement for last time step relative to the total displacement is less than DELTAU.
















   ..
       !! processed by numpydoc !!

.. py:property:: delta1
   :type: float


   
   Get or set the If _| max displacement for single dof |_characteristic length of model is less than DELTA1 then implicit will terminate
















   ..
       !! processed by numpydoc !!

.. py:property:: ketol
   :type: float


   
   Get or set the If Kinetic Energy drops below KETOL for STEP  consecutive implicit time steps then implicit will terminate
















   ..
       !! processed by numpydoc !!

.. py:property:: ietol
   :type: float


   
   Get or set the If Internal Energy drops below IETOL for STEP consecutive implicit time steps then implicit will terminate
















   ..
       !! processed by numpydoc !!

.. py:property:: tetol
   :type: float


   
   Get or set the If Total Energy drops below TETOL for NSTEP  consecutive implicit time steps then implicit will terminate
















   ..
       !! processed by numpydoc !!

.. py:property:: nstep
   :type: int


   
   Get or set the Number of time steps used in tests for Kinetic Energy, Internal Energy, and/or Total Energy.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'IMPLICIT_TERMINATION'






