





:class:`ControlImplicitSolution`
================================


.. py:class:: control_implicit_solution.ControlImplicitSolution(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_IMPLICIT_SOLUTION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlImplicitSolution

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nsolvr`
            - Get or set the Solution method for implicit analysis:
          * - :py:attr:`~ilimit`
            - Get or set the Iteration limit between automatic stiffness reformations.
          * - :py:attr:`~maxref`
            - Get or set the Stiffness reformation limit per time step.
          * - :py:attr:`~dctol`
            - Get or set the Displacement relative convergence tolerance (see Remark 5).
          * - :py:attr:`~ectol`
            - Get or set the Energy relative convergence tolerance (see Remark 5).
          * - :py:attr:`~rctol`
            - Get or set the Residual (force) relative convergence tolerance (see Remark 5).
          * - :py:attr:`~lstol`
            - Get or set the Line search convergence tolerance.
          * - :py:attr:`~abstol`
            - Get or set the Absolute convergence tolerance.
          * - :py:attr:`~dnorm`
            - Get or set the Displacement norm for convergence test:
          * - :py:attr:`~diverg`
            - Get or set the Divergence flag (force imbalance increase during equilibrium iterations):
          * - :py:attr:`~istif`
            - Get or set the Initial stiffness formation flag:
          * - :py:attr:`~nlprint`
            - Get or set the Nonlinear solver print flag:
          * - :py:attr:`~nlnorm`
            - Get or set the Nonlinear convergence norm type:
          * - :py:attr:`~d3itctl`
            - Get or set the Control D3ITER database.  If nonzero, the search directions for the nonlinear implicit solution are written to the D3ITER database.  To reduce the size of the D3ITER database the database is reset every n time steps where n=D3ITCTL
          * - :py:attr:`~cpchk`
            - Get or set the Contact penetration check flag
          * - :py:attr:`~dmtol`
            - Get or set the Maximum displacement convergence tolerance; convergence is detected when the relative maximum nodal or rigid body displacement is less than this value.
          * - :py:attr:`~emtol`
            - Get or set the Maximum energy convergence tolerance; convergence is detected when the relative maximum nodal or rigid body energy increment is less than this value.
          * - :py:attr:`~rmtol`
            - Get or set the Maximum residual convergence tolerance; convergence is detected when the relative maximum nodal or rigid body residual is less than this value.
          * - :py:attr:`~nttol`
            - Get or set the Nodal translational convergence tolerance; convergence is detected when the absolute maximum nodal translational residual is less than this value.
          * - :py:attr:`~nrtol`
            - Get or set the Nodal rotational convergence tolerance; convergence is detected when the absolute maximum nodal rotational residual is less than this value.
          * - :py:attr:`~rttol`
            - Get or set the Rigid body translational convergence tolerance; convergence is detected when the absolute maximum rigid body translational residual is less than this value.
          * - :py:attr:`~rrtol`
            - Get or set the Rigid body rotational convergence tolerance; convergence is detected when the absolute maximum rigid body rotational residual is less than this value.
          * - :py:attr:`~arcctl`
            - Get or set the Arc length controlling node ID:
          * - :py:attr:`~arcdir`
            - Get or set the Arc length controlling node direction (ignored if ARCCTL=0 above):
          * - :py:attr:`~arclen`
            - Get or set the Arc length size
          * - :py:attr:`~arcmth`
            - Get or set the Arc length method:
          * - :py:attr:`~arcdmp`
            - Get or set the Arc length damping option:
          * - :py:attr:`~arcpsi`
            - Get or set the Relative influence of load/time parameter in spherical arclength constraint,
          * - :py:attr:`~arcalf`
            - Get or set the Relative influence of predictor step direction for positioning of the arc
          * - :py:attr:`~arctim`
            - Get or set the Optional time when arc length method is initiated. Applies to ARCMTH = 3.
          * - :py:attr:`~lsmtd`
            - Get or set the Line search convergence method:
          * - :py:attr:`~lsdir`
            - Get or set the Line search direction method:
          * - :py:attr:`~irad`
            - Get or set the Normalized curvature factor for curved line search, where 0 indicates a straight line search and 1 indicates full curved line search.
          * - :py:attr:`~srad`
            - Get or set the Radius of influence for determining curve in curved line search. For each independent node, all nodes within this radius are used for determining the curve. If 0, then all nodes connected to the same element as the independent node are used.
          * - :py:attr:`~awgt`
            - Get or set the Adaptive line search weight factor between 0 and 1. A high value tends to restrict the motion of oscillating nodes during the implicit process.
          * - :py:attr:`~sred`
            - Get or set the Initial step reduction between 0 and 1 for adaptive line search, use large number for conservative start in  implicit procedure.


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

    from control_implicit_solution import ControlImplicitSolution

Property detail
---------------

.. py:property:: nsolvr
   :type: int


   
   Get or set the Solution method for implicit analysis:
   EQ.-1: Multistep linear,
   EQ.1: Linear,
   EQ.6: Nonlinear with BFGS updates + arclength,
   EQ.7: Nonlinear with Broyden updates + arclength,
   EQ.8: Nonlinear with DFP updates + arclength,
   EQ.9: Nonlinear with Davidon updates + arclength.
   EQ.12: Nonlinear with BFGS updates.This solver incorporates different line search and integration schemes as compared to obsolete NSOLVR=2.  Inclusion of an arc length method is optional and is invoked by setting ARCMTH=3.
















   ..
       !! processed by numpydoc !!

.. py:property:: ilimit
   :type: int


   
   Get or set the Iteration limit between automatic stiffness reformations.
   Default is set to ILIMIT = 11.
















   ..
       !! processed by numpydoc !!

.. py:property:: maxref
   :type: int


   
   Get or set the Stiffness reformation limit per time step.
   LT.0:   If  matrix reformations occur, convergence for that time step is forced; see Remark 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: dctol
   :type: float


   
   Get or set the Displacement relative convergence tolerance (see Remark 5).
   LT.0:   -DCTOL references a curve that defines tolerance as a function of time.
















   ..
       !! processed by numpydoc !!

.. py:property:: ectol
   :type: float


   
   Get or set the Energy relative convergence tolerance (see Remark 5).
   LT.0:   -ECTOL references a curve that defines tolerance as a function of time.
















   ..
       !! processed by numpydoc !!

.. py:property:: rctol
   :type: float


   
   Get or set the Residual (force) relative convergence tolerance (see Remark 5).
   LT.0:   -RCTOL references a curve that defines tolerance as a function of time
















   ..
       !! processed by numpydoc !!

.. py:property:: lstol
   :type: float


   
   Get or set the Line search convergence tolerance.
   Default is set to LSTOL = 0.9.
   LT.0: -LSTOL is the line search tolerance, but this option  activates an alternate strategy where line search acts only on the independent degrees of freedom. This is opposed to the default strategy, where prescribed motions on nodes and rigid bodies are also incorporated, sometimes leading to unnecessarily small time steps because of the requirement of fulfilling these boundary conditions
















   ..
       !! processed by numpydoc !!

.. py:property:: abstol
   :type: float


   
   Get or set the Absolute convergence tolerance.
   LT.0:   Convergence detected when the residual norm is less than.Note : To drive convergence based on , set DCTOLand ECTOL to 10 - 20
















   ..
       !! processed by numpydoc !!

.. py:property:: dnorm
   :type: int


   
   Get or set the Displacement norm for convergence test:
   EQ.1: Increment vs. displacement over current step,
   EQ.2: Increment vs. total displacement (default).
   LT.0: |"DNORM" |; also activates reading of optional Card 2.1
















   ..
       !! processed by numpydoc !!

.. py:property:: diverg
   :type: int


   
   Get or set the Divergence flag (force imbalance increase during equilibrium iterations):
   EQ.1: Reform stiffness if divergence detected (default),
   EQ.2: Ignore divergence.
















   ..
       !! processed by numpydoc !!

.. py:property:: istif
   :type: int


   
   Get or set the Initial stiffness formation flag:
   EQ.1: Reform stiffness at start of each step (default),
   EQ.n: Reform stiffness at start of every n'th step.
















   ..
       !! processed by numpydoc !!

.. py:property:: nlprint
   :type: int


   
   Get or set the Nonlinear solver print flag:
   EQ.0: No nolinear iteration information printed(new v970 default).
   EQ.1: Print iteration information to screen, messag, d3hsp files,
   EQ.2: Print extra norm information (NLNORM = 1).
   EQ.3: Same as 2, but also print information from line search.
   NOTE: during execution, sense switch nlprt can also be used to toggle this print flag on and off.
















   ..
       !! processed by numpydoc !!

.. py:property:: nlnorm
   :type: float


   
   Get or set the Nonlinear convergence norm type:
   LT.0: Same as 4, but rotational degrees of freedom are scaled appropriately with characteristic length ABS(NLNORM) to account for units.
   EQ.1: consider translational and rotational degrees of freedom
   EQ.2: consider translational degrees of freedom only (default)
   EQ.4: consider sum of translational and rotational degrees of freedom, i.e., no separate treatment.
















   ..
       !! processed by numpydoc !!

.. py:property:: d3itctl
   :type: int


   
   Get or set the Control D3ITER database.  If nonzero, the search directions for the nonlinear implicit solution are written to the D3ITER database.  To reduce the size of the D3ITER database the database is reset every n time steps where n=D3ITCTL
















   ..
       !! processed by numpydoc !!

.. py:property:: cpchk
   :type: int


   
   Get or set the Contact penetration check flag
   EQ.0: no contact penetration is performed (default)
   EQ.1: check for contact penetration during the nonlinear solution
   procedure. If such penetration is found modify the line search to
   prevent unnecessary penetration.
















   ..
       !! processed by numpydoc !!

.. py:property:: dmtol
   :type: float


   
   Get or set the Maximum displacement convergence tolerance; convergence is detected when the relative maximum nodal or rigid body displacement is less than this value.
















   ..
       !! processed by numpydoc !!

.. py:property:: emtol
   :type: float


   
   Get or set the Maximum energy convergence tolerance; convergence is detected when the relative maximum nodal or rigid body energy increment is less than this value.
















   ..
       !! processed by numpydoc !!

.. py:property:: rmtol
   :type: float


   
   Get or set the Maximum residual convergence tolerance; convergence is detected when the relative maximum nodal or rigid body residual is less than this value.
















   ..
       !! processed by numpydoc !!

.. py:property:: nttol
   :type: float


   
   Get or set the Nodal translational convergence tolerance; convergence is detected when the absolute maximum nodal translational residual is less than this value.
















   ..
       !! processed by numpydoc !!

.. py:property:: nrtol
   :type: float


   
   Get or set the Nodal rotational convergence tolerance; convergence is detected when the absolute maximum nodal rotational residual is less than this value.
















   ..
       !! processed by numpydoc !!

.. py:property:: rttol
   :type: float


   
   Get or set the Rigid body translational convergence tolerance; convergence is detected when the absolute maximum rigid body translational residual is less than this value.
















   ..
       !! processed by numpydoc !!

.. py:property:: rrtol
   :type: float


   
   Get or set the Rigid body rotational convergence tolerance; convergence is detected when the absolute maximum rigid body rotational residual is less than this value.
















   ..
       !! processed by numpydoc !!

.. py:property:: arcctl
   :type: int


   
   Get or set the Arc length controlling node ID:
   EQ.0: generalized arc length method (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: arcdir
   :type: int


   
   Get or set the Arc length controlling node direction (ignored if ARCCTL=0 above):
   EQ.1: global X-translation (default),
   EQ.2: global Y-translation,
   EQ.3: global Z-translation.
















   ..
       !! processed by numpydoc !!

.. py:property:: arclen
   :type: float


   
   Get or set the Arc length size
   LE.0.0: chosen automatically using initial step size
   Default is set to ARCLEN = 0.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: arcmth
   :type: int


   
   Get or set the Arc length method:
   EQ.1: Crisfield (default),
   EQ.2: Ramm.
   EQ.3: Modified Crisfield (used with NSOLVR = 12 only)
















   ..
       !! processed by numpydoc !!

.. py:property:: arcdmp
   :type: int


   
   Get or set the Arc length damping option:
   EQ.1: On, oscillations in static solution are supressed,
   EQ.2: Off (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: arcpsi
   :type: float


   
   Get or set the Relative influence of load/time parameter in spherical arclength constraint,
   default value is 0 which corresponds to a cylindrical arclength
   constraint. Applies to ARCMTH = 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: arcalf
   :type: float


   
   Get or set the Relative influence of predictor step direction for positioning of the arc
   center, default is 0 which means that the center is at the origin. Applies
   to ARCMTH = 3..
















   ..
       !! processed by numpydoc !!

.. py:property:: arctim
   :type: float


   
   Get or set the Optional time when arc length method is initiated. Applies to ARCMTH = 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: lsmtd
   :type: int


   
   Get or set the Line search convergence method:
   EQ.1: Energy method using only translational variables,
   EQ.2: Residual method,
   EQ.3: Energy method using both translational and rotational variables.
   EQ.4: Energy method using sum of translational and rotational degrees of freedom, i.e., no separate treatment (default)
   EQ.5: Same as 4, but account for residual norm growth to be extra conservative in step length (applies to NSOLVR=12)
















   ..
       !! processed by numpydoc !!

.. py:property:: lsdir
   :type: int


   
   Get or set the Line search direction method:
   EQ.1: Search on all variables (traditional approach used in versions prior to 971),
   EQ.2: Search only on the independent (unconstrained) variables,
   EQ.3: Use adaptive line search (see AWGT, SRED),
   EQ.4: Use curved line search (see IRAD, SRAD).
















   ..
       !! processed by numpydoc !!

.. py:property:: irad
   :type: float


   
   Get or set the Normalized curvature factor for curved line search, where 0 indicates a straight line search and 1 indicates full curved line search.
















   ..
       !! processed by numpydoc !!

.. py:property:: srad
   :type: float


   
   Get or set the Radius of influence for determining curve in curved line search. For each independent node, all nodes within this radius are used for determining the curve. If 0, then all nodes connected to the same element as the independent node are used.
















   ..
       !! processed by numpydoc !!

.. py:property:: awgt
   :type: float


   
   Get or set the Adaptive line search weight factor between 0 and 1. A high value tends to restrict the motion of oscillating nodes during the implicit process.
















   ..
       !! processed by numpydoc !!

.. py:property:: sred
   :type: float


   
   Get or set the Initial step reduction between 0 and 1 for adaptive line search, use large number for conservative start in  implicit procedure.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'IMPLICIT_SOLUTION'






