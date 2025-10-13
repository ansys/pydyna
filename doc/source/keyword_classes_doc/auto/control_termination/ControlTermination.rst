





:class:`ControlTermination`
===========================


.. py:class:: control_termination.ControlTermination(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_TERMINATION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlTermination

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~endtim`
            - Get or set the Termination time. Mandatory.
          * - :py:attr:`~endcyc`
            - Get or set the Termination cycle.
          * - :py:attr:`~dtmin`
            - Get or set the Reduction (or scale) factor for initial time step size to determine minimum time step.
          * - :py:attr:`~endeng`
            - Get or set the Percent change in energy ratio for termination of calculation. If undefined, this option is inactive.
          * - :py:attr:`~endmas`
            - Get or set the Percent change in the total mass for termination of calculation.  This option is relevant if and only if mass scaling is used to limit the minimum time step size; see *CONTROL_TIMESTEP field DT2MS.
          * - :py:attr:`~nosol`
            - Get or set the Flag for a non-solution run, i.e. normal termination directly after initialization.


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

    from control_termination import ControlTermination

Property detail
---------------

.. py:property:: endtim
   :type: float


   
   Get or set the Termination time. Mandatory.
















   ..
       !! processed by numpydoc !!

.. py:property:: endcyc
   :type: int


   
   Get or set the Termination cycle.
















   ..
       !! processed by numpydoc !!

.. py:property:: dtmin
   :type: float


   
   Get or set the Reduction (or scale) factor for initial time step size to determine minimum time step.
















   ..
       !! processed by numpydoc !!

.. py:property:: endeng
   :type: float


   
   Get or set the Percent change in energy ratio for termination of calculation. If undefined, this option is inactive.
















   ..
       !! processed by numpydoc !!

.. py:property:: endmas
   :type: float


   
   Get or set the Percent change in the total mass for termination of calculation.  This option is relevant if and only if mass scaling is used to limit the minimum time step size; see *CONTROL_TIMESTEP field DT2MS.
   LT.0.0: |ENDMAS| is the load curve ID defining the percent change in the total mass as a function of the total mass.
















   ..
       !! processed by numpydoc !!

.. py:property:: nosol
   :type: int


   
   Get or set the Flag for a non-solution run, i.e. normal termination directly after initialization.
   EQ.0: off (default),
   EQ.1: on.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'TERMINATION'






