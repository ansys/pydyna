





:class:`ControlImplicitEigenvalue`
==================================


.. py:class:: control_implicit_eigenvalue.ControlImplicitEigenvalue(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_IMPLICIT_EIGENVALUE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlImplicitEigenvalue

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~neig`
            - Get or set the Number of eigenvalues to extract. This must be specified. The other parameters below are optional.
          * - :py:attr:`~center`
            - Get or set the Center frequency. This option finds the nearest NEIG eigenvalues located about this value.
          * - :py:attr:`~lflag`
            - Get or set the Left end point finite flag.
          * - :py:attr:`~lftend`
            - Get or set the Left end point of interval. Only used when LFLAG = 1.
          * - :py:attr:`~rflag`
            - Get or set the Right end point finite flag:
          * - :py:attr:`~rhtend`
            - Get or set the Right end point of interval. Only used when RFLAG = 1.
          * - :py:attr:`~eigmth`
            - Get or set the Eigenvalue extraction method:
          * - :py:attr:`~shfscl`
            - Get or set the Shift scale.
          * - :py:attr:`~isolid`
            - Get or set the If nonzero, reset all solid element formulations to ISOLID for the implicit computations.  Can be used for all implicit computations not just eigenvalue computations..
          * - :py:attr:`~ibeam`
            - Get or set the If nonzero, reset all beam element formulations to IBEAM for the implicit computations.  Can be used for all implicit computations not just eigenvalue computations.
          * - :py:attr:`~ishell`
            - Get or set the If nonzero, reset all shell element formulations to ISHELL for the implicit computations.  Can be used for all implicit computations not just eigenvalue computations.
          * - :py:attr:`~itshell`
            - Get or set the If nonzero, reset all thick shell element formulations to ITSHELL for the implicit computations.  Can be used for all implicit computations not just eigenvalue computations.
          * - :py:attr:`~mstres`
            - Get or set the Flag for computing the stresses for the eigenmodes:
          * - :py:attr:`~evdump`
            - Get or set the Flag for writing eigenvalues and eigenvectors to file Eigen_Vectors(SMP only):
          * - :py:attr:`~mstrscl`
            - Get or set the Scaling for computing the velocity based on the mode shape for the stress computation.
          * - :py:attr:`~iparm1`
            - Get or set the Minimum block size for the Cholesky factorization
          * - :py:attr:`~iparm2`
            - Get or set the Maximum block size for the Cholesky factorization.  Default is the model size
          * - :py:attr:`~iparm3`
            - Get or set the Node set ID specifying special nodes in the model where increased accuracy is desired
          * - :py:attr:`~iparm4`
            - Get or set the MCMS minimum group/substructure size
          * - :py:attr:`~rparm1`
            - Get or set the Eigenvalue expansion factor
          * - :py:attr:`~rparm2`
            - Get or set the BLR preconditioner tolerance
          * - :py:attr:`~rparm5`
            - Get or set the Harmonic index
          * - :py:attr:`~rparm6`
            - Get or set the Vector ID for the axis of rotation


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

    from control_implicit_eigenvalue import ControlImplicitEigenvalue

Property detail
---------------

.. py:property:: neig
   :type: int


   
   Get or set the Number of eigenvalues to extract. This must be specified. The other parameters below are optional.
   LT.0: curve ID = (-NEIG) used for intermittent eigenvalue analysis
















   ..
       !! processed by numpydoc !!

.. py:property:: center
   :type: float


   
   Get or set the Center frequency. This option finds the nearest NEIG eigenvalues located about this value.
















   ..
       !! processed by numpydoc !!

.. py:property:: lflag
   :type: int


   
   Get or set the Left end point finite flag.
   EQ.0: left end point is -infinity
   EQ.1: left end point is LFTEND.
















   ..
       !! processed by numpydoc !!

.. py:property:: lftend
   :type: float


   
   Get or set the Left end point of interval. Only used when LFLAG = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: rflag
   :type: int


   
   Get or set the Right end point finite flag:
   EQ.0: right end point is +infinity
   EQ.1: right end point is RHTEND.
















   ..
       !! processed by numpydoc !!

.. py:property:: rhtend
   :type: float


   
   Get or set the Right end point of interval. Only used when RFLAG = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: eigmth
   :type: int


   
   Get or set the Eigenvalue extraction method:
   EQ.2: Block Shift and Invert Lanczos (default).
   EQ.3: Lanczos with [M] = [I](for debug only).
   EQ.5: Same as 3 but include Dynamic Terms.
   EQ.6:   Same as 2 but include Dynamic Terms
   EQ.101: MCMS.  See Remark 4.
   EQ.102: LOBPCG.See Remark 5.
   EQ.111 : Sectoral Symmetry.See Remark 10
















   ..
       !! processed by numpydoc !!

.. py:property:: shfscl
   :type: float


   
   Get or set the Shift scale.
















   ..
       !! processed by numpydoc !!

.. py:property:: isolid
   :type: int


   
   Get or set the If nonzero, reset all solid element formulations to ISOLID for the implicit computations.  Can be used for all implicit computations not just eigenvalue computations..
















   ..
       !! processed by numpydoc !!

.. py:property:: ibeam
   :type: int


   
   Get or set the If nonzero, reset all beam element formulations to IBEAM for the implicit computations.  Can be used for all implicit computations not just eigenvalue computations.
















   ..
       !! processed by numpydoc !!

.. py:property:: ishell
   :type: int


   
   Get or set the If nonzero, reset all shell element formulations to ISHELL for the implicit computations.  Can be used for all implicit computations not just eigenvalue computations.
















   ..
       !! processed by numpydoc !!

.. py:property:: itshell
   :type: int


   
   Get or set the If nonzero, reset all thick shell element formulations to ITSHELL for the implicit computations.  Can be used for all implicit computations not just eigenvalue computations.
















   ..
       !! processed by numpydoc !!

.. py:property:: mstres
   :type: int


   
   Get or set the Flag for computing the stresses for the eigenmodes:
   EQ.0: Do not compute the stresses.
   EQ.1: Compute the stresses.
















   ..
       !! processed by numpydoc !!

.. py:property:: evdump
   :type: Optional[int]


   
   Get or set the Flag for writing eigenvalues and eigenvectors to file Eigen_Vectors(SMP only):
   EQ.0: Do not write eigenvalues and eigenvectors.
   GT.0: Write eigenvalues and eigenvectors using an ASCII format.
   LT.0: Write eigenvalues and eigenvectors using a binary format.
















   ..
       !! processed by numpydoc !!

.. py:property:: mstrscl
   :type: float


   
   Get or set the Scaling for computing the velocity based on the mode shape for the stress computation.
















   ..
       !! processed by numpydoc !!

.. py:property:: iparm1
   :type: int


   
   Get or set the Minimum block size for the Cholesky factorization
















   ..
       !! processed by numpydoc !!

.. py:property:: iparm2
   :type: Optional[int]


   
   Get or set the Maximum block size for the Cholesky factorization.  Default is the model size
















   ..
       !! processed by numpydoc !!

.. py:property:: iparm3
   :type: Optional[int]


   
   Get or set the Node set ID specifying special nodes in the model where increased accuracy is desired
















   ..
       !! processed by numpydoc !!

.. py:property:: iparm4
   :type: int


   
   Get or set the MCMS minimum group/substructure size
















   ..
       !! processed by numpydoc !!

.. py:property:: rparm1
   :type: float


   
   Get or set the Eigenvalue expansion factor
















   ..
       !! processed by numpydoc !!

.. py:property:: rparm2
   :type: int


   
   Get or set the BLR preconditioner tolerance
















   ..
       !! processed by numpydoc !!

.. py:property:: rparm5
   :type: Optional[int]


   
   Get or set the Harmonic index
















   ..
       !! processed by numpydoc !!

.. py:property:: rparm6
   :type: int


   
   Get or set the Vector ID for the axis of rotation
   EQ.0 Axis of rotation if the global z-axis(default)
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'IMPLICIT_EIGENVALUE'






