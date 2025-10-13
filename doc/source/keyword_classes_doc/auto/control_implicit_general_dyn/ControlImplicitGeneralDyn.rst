





:class:`ControlImplicitGeneralDyn`
==================================


.. py:class:: control_implicit_general_dyn.ControlImplicitGeneralDyn(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_IMPLICIT_GENERAL_DYN keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlImplicitGeneralDyn

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~imflag`
            - Get or set the Implicit/Explicit switching flag
          * - :py:attr:`~dt0`
            - Get or set the Initial time step size for implicit analysis.  See Remarks 2 and 5.
          * - :py:attr:`~imform`
            - Get or set the Element formulation switching flag
          * - :py:attr:`~nsbs`
            - Get or set the Number of steps in nonlinear springback (default = 1).
          * - :py:attr:`~igs`
            - Get or set the Geometric (initial stress) stiffness flag
          * - :py:attr:`~cnstn`
            - Get or set the Indicator for consistent tangent stiffness:
          * - :py:attr:`~form`
            - Get or set the Element formulation when using IMFORM flag.
          * - :py:attr:`~zero_v`
            - Get or set the Zero out the velocity before switching from explicit to implicit.


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

    from control_implicit_general_dyn import ControlImplicitGeneralDyn

Property detail
---------------

.. py:property:: imflag
   :type: int


   
   Get or set the Implicit/Explicit switching flag
   EQ.0: explicit analysis (default)
   EQ.1: implicit analysis
   EQ.2: explicit followed by one implicit step (springback analysis)
   EQ.4: implicit with automatic implicit-explicit switching
   EQ.5: implicit with automatic switching and mandatory implicit finish
   EQ.6: explicit with intermittent eigenvalue extraction
   EQ.-n: curve ID=n gives IMFLAG as a function of time.
















   ..
       !! processed by numpydoc !!

.. py:property:: dt0
   :type: Optional[float]


   
   Get or set the Initial time step size for implicit analysis.  See Remarks 2 and 5.
   LT.0:   eliminate negative principal stresses in geometric(initial stress) stiffness.Initial time step is |DT0|.
















   ..
       !! processed by numpydoc !!

.. py:property:: imform
   :type: int


   
   Get or set the Element formulation switching flag
   EQ.1: switch to fully integrated formulation for implicit springback
   EQ.2: retain original element formulation (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: nsbs
   :type: int


   
   Get or set the Number of steps in nonlinear springback (default = 1).
















   ..
       !! processed by numpydoc !!

.. py:property:: igs
   :type: int


   
   Get or set the Geometric (initial stress) stiffness flag
   EQ.2: ignore(default)
   EQ.1: include
   LT.0:   include on part set |IGS|
















   ..
       !! processed by numpydoc !!

.. py:property:: cnstn
   :type: int


   
   Get or set the Indicator for consistent tangent stiffness:
   EQ.0: do not use (default)
   EQ.1: use.
















   ..
       !! processed by numpydoc !!

.. py:property:: form
   :type: int


   
   Get or set the Element formulation when using IMFORM flag.
   EQ.0: type 16
   EQ.1: type 6.
















   ..
       !! processed by numpydoc !!

.. py:property:: zero_v
   :type: int


   
   Get or set the Zero out the velocity before switching from explicit to implicit.
   EQ.0: The velocities are not zeroed out.
   EQ.1: The velocities are set to zero.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'IMPLICIT_GENERAL_DYN'






