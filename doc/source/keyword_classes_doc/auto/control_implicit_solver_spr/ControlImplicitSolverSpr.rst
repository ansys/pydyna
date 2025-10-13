





:class:`ControlImplicitSolverSpr`
=================================


.. py:class:: control_implicit_solver_spr.ControlImplicitSolverSpr(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_IMPLICIT_SOLVER_SPR keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlImplicitSolverSpr

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~lsolvr`
            - Get or set the Linear equation solver method (see Remarks below).
          * - :py:attr:`~lprint`
            - Get or set the Linear solver print flag controls screen and message file output (see Remarks below).
          * - :py:attr:`~negev`
            - Get or set the Negative eigenvalue flag.  Selects procedure when negative eigenvalues are detected during stiffness matrix inversion (see Remarks below).
          * - :py:attr:`~order`
            - Get or set the Ordering option (see Remarks below)
          * - :py:attr:`~drcm`
            - Get or set the Drilling rotation constraint method for shells (see Remarks below).
          * - :py:attr:`~drcprm`
            - Get or set the Drilling rotation constraint parameter for shells. This parameter scales the drilling stiffness.
          * - :py:attr:`~autospc`
            - Get or set the Automatic Constraint Scan flag
          * - :py:attr:`~autotol`
            - Get or set the AUTOSPC tolerance.  The test for singularity is the ratio of the smallest singular value and the largest singular value.
          * - :py:attr:`~lcpack`
            - Get or set the Matrix assembly package:
          * - :py:attr:`~mtxdmp`
            - Get or set the Matrix and right-hand-side dumping.  LS-DYNA has the option of dumping the globally assembled stiffness matrix and right-hand-side vectors files in Harwell-Boeing sparse matrix format.
          * - :py:attr:`~iparm1`
            - Get or set the For 22 <= LSOLVR <= 26 only, maximum number of iterations.  Default is 500
          * - :py:attr:`~rparm1`
            - Get or set the For 22 <= LSOLVR <= 26 only, absolute tolerance for convergence.  Default is 10e-10.
          * - :py:attr:`~rparm2`
            - Get or set the For 22 <= LSOLVR <= 26 only, relative tolerance for convergence.  Default is 10e-4.
          * - :py:attr:`~rparm5`
            - Get or set the For LSOLVR = 30 only, compression tolerance used to compute a low - rank factorization with the MUMPS solver.Default is 0.0.
          * - :py:attr:`~emxdmp`
            - Get or set the Flag for dumping elemental stiffness and mass matrices:
          * - :py:attr:`~rdcmem`
            - Get or set the Starting with LS-DYNA R11, the memory for linear algebra has been moved from static memory allocation to dynamic memory allocation.
          * - :py:attr:`~absmem`
            - Get or set the Absolute upper bound for the dynamic memory allocated for factorization. The allocated memory will be bounded above by the min⁡(RDCME ×NWORDS ,ABSMEM ) where NWORDS is the number of available words as determined by the operating system. If the predicted amount of required memory is less than this value, then less memory than this bound may be allocated.


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

    from control_implicit_solver_spr import ControlImplicitSolverSpr

Property detail
---------------

.. py:property:: lsolvr
   :type: int


   
   Get or set the Linear equation solver method (see Remarks below).
   EQ.2:   Parallel multi-frontal sparse solver (default)
   EQ.22:  iterative, CG with diagonal preconditioner
   EQ.23:  iterative, CG with SGS preconditioner
   EQ.24:  iterative, CG with SSOR preconditioner
   EQ.25:  iterative, CG with modified ILDLTD preconditioner
   EQ.26:  iterative, CG with modified ILDLTO preconditioner that requires extra storage
   EQ.30 Parallel direct/hybrid solver MUMPS
   EQ.90:  User Supplied Linear Equation Solver SMP only:
   EQ.6:   BCSLIB-EXT, direct, sparse, double precision
















   ..
       !! processed by numpydoc !!

.. py:property:: lprint
   :type: int


   
   Get or set the Linear solver print flag controls screen and message file output (see Remarks below).
   EQ.0:   no printing
   EQ.1:   output summary statistics on memory, cpu requirements
   EQ.2:   more statistics
   EQ.3:   even more statistics and debug checking
   During execution, use the interactive command "<ctrl-c>lprint" to toggle this print flag between 0 and 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: negev
   :type: int


   
   Get or set the Negative eigenvalue flag.  Selects procedure when negative eigenvalues are detected during stiffness matrix inversion (see Remarks below).
   EQ.1:   stop, or retry step if auto step control is active
   EQ.2:   print warning message, try to continue (default)
















   ..
       !! processed by numpydoc !!

.. py:property:: order
   :type: int


   
   Get or set the Ordering option (see Remarks below)
   EQ.0:   Method set automatically by LS-DYNA
   EQ.1:   MMD, Multiple Minimum Degree.
   EQ.2:   Metis
   EQ.4:   LSGpart.
















   ..
       !! processed by numpydoc !!

.. py:property:: drcm
   :type: int


   
   Get or set the Drilling rotation constraint method for shells (see Remarks below).
   EQ.1:   add drilling stiffness (old Version 970 method)
   EQ.2:   same as 4 below
   EQ.3:   add no drilling stiffness
   EQ.4:   add drilling stiffness (improved method) (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: drcprm
   :type: Optional[float]


   
   Get or set the Drilling rotation constraint parameter for shells. This parameter scales the drilling stiffness.
   For the old method (DRCM = 1) the default value of DRCPRM is 1.0 for linear analysis,
   100.0 for nonlinear implicit analysis, and either 1.E-12 or 1.E-8 for eigenvalue analysis depending on the shell element type.
   For eigenvalue analysis, the input value for DRCPRM is ignored.  For the improved method (default, DRCM = 4),
   the default value of DRCPRM is as described above for the old method except default DRCPRM is 1.0 for nonlinear implicit analysis.
















   ..
       !! processed by numpydoc !!

.. py:property:: autospc
   :type: int


   
   Get or set the Automatic Constraint Scan flag
   EQ.1:   scan the assembled stiffness matrix looking for unconstrained, unattached degrees of freedom.
   Generate additional constraints as necessary to avoid negative eigenvalues.
   EQ.2:   do not add constraints.
















   ..
       !! processed by numpydoc !!

.. py:property:: autotol
   :type: Optional[float]


   
   Get or set the AUTOSPC tolerance.  The test for singularity is the ratio of the smallest singular value and the largest singular value.
   If this ratio is less than AUTOTOL, then the triple of columns is declared singular and a constraint is generated.
   Default values in single and double precision are 1e-4 and 10e-8, respectively.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcpack
   :type: int


   
   Get or set the Matrix assembly package:
   EQ.2:   Default.
   EQ.3:   Same as 2, but incorporates a non-symmetric linear solver; see Remarks below
















   ..
       !! processed by numpydoc !!

.. py:property:: mtxdmp
   :type: int


   
   Get or set the Matrix and right-hand-side dumping.  LS-DYNA has the option of dumping the globally assembled stiffness matrix and right-hand-side vectors files in Harwell-Boeing sparse matrix format.
   Such output may be useful for comparing to other linear equation solution packages.
   EQ.0:   No dumping
   GT.0:   Dump all matrices and right-hand-side vectors every MTXDMP time steps.
   Output is written as ASCII text and the involved filenames are of the following form:   K_xxxx_yyy.mtx.rb
   This file contains the stiffness matrix at step xxxx, iteration yyy.            M_xxxx_yyy.mtx.rb
   This file contains the mass matrix at step xxxx, iteration yyy.  Only for eigenvalue analysis.          W_xxxx_yyy.mtx.rb
   This file contains the damping matrix at step xxxx, iteration yyy.  Only for simulations with damping. K_xxxx_yyy_zzz.rhs.rb
   This file contains the right hand side at step xxxx, iteration yyy, where yyyis the iteration at which a stiffness matrix is formed; and zzz is the cumulative iteration number for the step.
   The values of yyy and zzz don’t always coincide because the stiffness matrix is not necessarily reformed every iteration. Node_Data_xxxx_yyy
   This file maps stiffness matrix to nodes and provides nodal coordinates.
   LT.0:   Like positive values of MTXDMP but dumped data is binary.
   EQ.|9999|:      Simulation is terminated after dumping matrices and right hand side prior to factorization
















   ..
       !! processed by numpydoc !!

.. py:property:: iparm1
   :type: int


   
   Get or set the For 22 <= LSOLVR <= 26 only, maximum number of iterations.  Default is 500
















   ..
       !! processed by numpydoc !!

.. py:property:: rparm1
   :type: float


   
   Get or set the For 22 <= LSOLVR <= 26 only, absolute tolerance for convergence.  Default is 10e-10.
















   ..
       !! processed by numpydoc !!

.. py:property:: rparm2
   :type: float


   
   Get or set the For 22 <= LSOLVR <= 26 only, relative tolerance for convergence.  Default is 10e-4.
















   ..
       !! processed by numpydoc !!

.. py:property:: rparm5
   :type: float


   
   Get or set the For LSOLVR = 30 only, compression tolerance used to compute a low - rank factorization with the MUMPS solver.Default is 0.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: emxdmp
   :type: int


   
   Get or set the Flag for dumping elemental stiffness and mass matrices:
   EQ.0:   No dumping
   GT.0:   Dump all elemental matrices every EMXDMP time steps.
   Output is written as ASCII text and the involved filenames are of the following form: ElmStfMtx_xxxx_yyy
   This file contains the elemental stiffness matrix at step xxxx, iteration yyy. ElmMssMtx_xxxx_yyy
   This file contains the elemental mass matrix at step xxxx, iteration yyy.
   LT.0:   Like positive values of MTXDMP but dumped data is binary.
   EQ.|9999|:      Simulation is terminated after dumping matrices and right hand side prior to factorization
















   ..
       !! processed by numpydoc !!

.. py:property:: rdcmem
   :type: float


   
   Get or set the Starting with LS-DYNA R11, the memory for linear algebra has been moved from static memory allocation to dynamic memory allocation.
   For implicit applications we have found that some operating systems are not “robust” when queried about how much dynamic memory is free.
   This factor caps the amount of dynamic memory requested for linear algebra applications to RDCMEM times the amount that the operating system declares available.
   0.85 seems to work well for most systems. If you are using a workstation and starting up other applications while running LS-DYNA, you may need to use a number like 0.50
















   ..
       !! processed by numpydoc !!

.. py:property:: absmem
   :type: Optional[float]


   
   Get or set the Absolute upper bound for the dynamic memory allocated for factorization. The allocated memory will be bounded above by the min⁡(RDCME ×NWORDS ,ABSMEM ) where NWORDS is the number of available words as determined by the operating system. If the predicted amount of required memory is less than this value, then less memory than this bound may be allocated.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'IMPLICIT_SOLVER_SPR'






