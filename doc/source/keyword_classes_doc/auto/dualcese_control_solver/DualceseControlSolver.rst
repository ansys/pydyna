





:class:`DualceseControlSolver`
==============================


.. py:class:: dualcese_control_solver.DualceseControlSolver(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DUALCESE_CONTROL_SOLVER keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DualceseControlSolver

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~eqns`
            - Get or set the Select the equations being solved with the dual CESE solver.
          * - :py:attr:`~igeom`
            - Get or set the Sets the geometric dimension:
          * - :py:attr:`~iframe`
            - Get or set the Choose the frame of reference:
          * - :py:attr:`~mixtype`
            - Get or set the Select the mix or multiphase model solver (if any):
          * - :py:attr:`~idc`
            - Get or set the Contact interaction detection coefficient (for FSI and conjugate heat transfer problems).
          * - :py:attr:`~isnan`
            - Get or set the Flag to check for a NaN in the dual CESE solver solution arrays at the completion of each time step. This option can be useful for debugging purposes. There is a cost overhead when this option is active.


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

    from dualcese_control_solver import DualceseControlSolver

Property detail
---------------

.. py:property:: eqns
   :type: str


   
   Get or set the Select the equations being solved with the dual CESE solver.
   EQ.NS:  Navier - Stokes equations
   EQ.EULER : Euler equations
















   ..
       !! processed by numpydoc !!

.. py:property:: igeom
   :type: Optional[str]


   
   Get or set the Sets the geometric dimension:
   EQ.2D:  Two - dimensional(2D) problem
   EQ.3D : Three - dimensional(3D) problem
   EQ.AXI : 2D axisymmetric
















   ..
       !! processed by numpydoc !!

.. py:property:: iframe
   :type: str


   
   Get or set the Choose the frame of reference:
   EQ.FIXED:       Usual non - moving reference frame(default).
   EQ.ROT : Non - inertial rotating reference frame.
   EQ.ROTATING : Non - inertial rotating reference frame
















   ..
       !! processed by numpydoc !!

.. py:property:: mixtype
   :type: Optional[str]


   
   Get or set the Select the mix or multiphase model solver (if any):
   EQ.<blank>: No mix or multiphase model(default).
   EQ.HYBRID : Hybrid multiphase model solver.
   EQ.TWO - PHASE : Two - phase multiphase solver.
















   ..
       !! processed by numpydoc !!

.. py:property:: idc
   :type: float


   
   Get or set the Contact interaction detection coefficient (for FSI and conjugate heat transfer problems).
















   ..
       !! processed by numpydoc !!

.. py:property:: isnan
   :type: int


   
   Get or set the Flag to check for a NaN in the dual CESE solver solution arrays at the completion of each time step. This option can be useful for debugging purposes. There is a cost overhead when this option is active.
   EQ.0: No checking,
   EQ.1 : Checking is active
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DUALCESE'


.. py:attribute:: subkeyword
   :value: 'CONTROL_SOLVER'






