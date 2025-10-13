





:class:`ControlImplicitAuto`
============================


.. py:class:: control_implicit_auto.ControlImplicitAuto(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_IMPLICIT_AUTO keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlImplicitAuto

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~iauto`
            - Get or set the Automatic time step control flag
          * - :py:attr:`~iteopt`
            - Get or set the Optimum equilibrium iteration count per time step
          * - :py:attr:`~itewin`
            - Get or set the Allowable iteration window. If iteration count is within ITEWIN iterations of ITEOPT, step size will not be adjusted.
          * - :py:attr:`~dtmin`
            - Get or set the Minimum allowable time step size.  Simulation stops with error termination if time step falls below DTMIN.
          * - :py:attr:`~dtmax`
            - Get or set the Maximum allowable time step size (default = DT*10).
          * - :py:attr:`~dtexp`
            - Get or set the Time interval to run in explicit mode before returning to implicit mode.
          * - :py:attr:`~kfail`
            - Get or set the Number of failed attempts to converge implicitly for the current time
          * - :py:attr:`~kcycle`
            - Get or set the Number of explicit cycles to run in explicit mode before returning to
          * - :py:attr:`~hcmin`
            - Get or set the Mid-point relative Euclidian residual norm min and max tolerance, to be seen as a confidence interval, see Remark for IAUTO = 3.
          * - :py:attr:`~hcmax`
            - Get or set the Mid-point relative Euclidian residual norm min and max tolerance, to be seen as a confidence interval, see Remark for IAUTO = 3.
          * - :py:attr:`~hmmin`
            - Get or set the Mid-point relative maximum residual norm min and max tolerance, to be seen as a confidence interval, see Remark for IAUTO = 3.
          * - :py:attr:`~hmmax`
            - Get or set the Mid-point relative maximum residual norm min and max tolerance, to be seen as a confidence interval, see Remark for IAUTO = 3.
          * - :py:attr:`~hntmax`
            - Get or set the Mid-point absolute Nodal Translational norm tolerance, see Remark for IAUTO = 3.
          * - :py:attr:`~hnrmax`
            - Get or set the Mid-point absolute Nodal Rotational norm tolerance, see Remark for IAUTO = 3.
          * - :py:attr:`~hrtmax`
            - Get or set the Mid-point absolute Rigid body Translational norm tolerance, see Remark for IAUTO = 3.
          * - :py:attr:`~hrrmax`
            - Get or set the Mid-point absolute Rigid body Rotational norm tolerance, see Remark for IAUTO=3.


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

    from control_implicit_auto import ControlImplicitAuto

Property detail
---------------

.. py:property:: iauto
   :type: int


   
   Get or set the Automatic time step control flag
   EQ.0:   constant time step size
   EQ.1 : automatically adjust time step size
   EQ.2 : automatically adjust time step size and synchronize with thermal mechanical time step.
   EQ.3 : same as 1, but accounting for mid step residual values with respect to parameters on card 2 and according to the Remark for IAUTO.
   LT.0 : Curve ID = (-IAUTO) gives time step size as a function of time.If specified, DTMIN and DTMAX will still be applied
















   ..
       !! processed by numpydoc !!

.. py:property:: iteopt
   :type: int


   
   Get or set the Optimum equilibrium iteration count per time step
















   ..
       !! processed by numpydoc !!

.. py:property:: itewin
   :type: int


   
   Get or set the Allowable iteration window. If iteration count is within ITEWIN iterations of ITEOPT, step size will not be adjusted.
















   ..
       !! processed by numpydoc !!

.. py:property:: dtmin
   :type: Optional[float]


   
   Get or set the Minimum allowable time step size.  Simulation stops with error termination if time step falls below DTMIN.
   LT.0:   enable automatic key point generation.Minimum allowable time step is |DTMIN|.
















   ..
       !! processed by numpydoc !!

.. py:property:: dtmax
   :type: Optional[float]


   
   Get or set the Maximum allowable time step size (default = DT*10).
















   ..
       !! processed by numpydoc !!

.. py:property:: dtexp
   :type: Optional[float]


   
   Get or set the Time interval to run in explicit mode before returning to implicit mode.
   Applies only when automatic implicit-explicit switching is active (IMFLAG= 4 or 5 on *CONTROL_IMPLICIT_GENERAL).  Also, see KCYCLE.
   EQ.0:   defaults to the current implicit time step size.
   LT.0 : curve ID = (-DTEXP) gives the time interval as a function of time.
















   ..
       !! processed by numpydoc !!

.. py:property:: kfail
   :type: Optional[int]


   
   Get or set the Number of failed attempts to converge implicitly for the current time
   step before automatically switching to explicit time integration.
   Applies only when automatic implicit-explicit switching is active. The
   default is one attempt. If IAUTO = 0, any input value is reset to unity
















   ..
       !! processed by numpydoc !!

.. py:property:: kcycle
   :type: Optional[int]


   
   Get or set the Number of explicit cycles to run in explicit mode before returning to
   the implicit mode. The actual time interval that is used will be the
   maximum between DTEXP and KCYCLE*(latest estimate of the explicit time step size).
















   ..
       !! processed by numpydoc !!

.. py:property:: hcmin
   :type: Optional[float]


   
   Get or set the Mid-point relative Euclidian residual norm min and max tolerance, to be seen as a confidence interval, see Remark for IAUTO = 3.
   Only active if RCTOL on *CONTROL_‌IMPLICIT_‌SOLUTION is set.
















   ..
       !! processed by numpydoc !!

.. py:property:: hcmax
   :type: Optional[float]


   
   Get or set the Mid-point relative Euclidian residual norm min and max tolerance, to be seen as a confidence interval, see Remark for IAUTO = 3.
   Only active if RCTOL on *CONTROL_‌IMPLICIT_‌SOLUTION is set.
















   ..
       !! processed by numpydoc !!

.. py:property:: hmmin
   :type: Optional[float]


   
   Get or set the Mid-point relative maximum residual norm min and max tolerance, to be seen as a confidence interval, see Remark for IAUTO = 3.
   Only active if RMTOL on *CONTROL_‌IMPLICIT_‌SOLUTION is set.
















   ..
       !! processed by numpydoc !!

.. py:property:: hmmax
   :type: Optional[float]


   
   Get or set the Mid-point relative maximum residual norm min and max tolerance, to be seen as a confidence interval, see Remark for IAUTO = 3.
   Only active if RMTOL on *CONTROL_‌IMPLICIT_‌SOLUTION is set.
















   ..
       !! processed by numpydoc !!

.. py:property:: hntmax
   :type: Optional[float]


   
   Get or set the Mid-point absolute Nodal Translational norm tolerance, see Remark for IAUTO = 3.
   Only active if NTTOL on *CONTROL_‌IMPLICIT_‌SOLUTION is set.
















   ..
       !! processed by numpydoc !!

.. py:property:: hnrmax
   :type: Optional[float]


   
   Get or set the Mid-point absolute Nodal Rotational norm tolerance, see Remark for IAUTO = 3.
   Only active if NRTOL on *CONTROL_‌IMPLICIT_‌SOLUTION is set.
















   ..
       !! processed by numpydoc !!

.. py:property:: hrtmax
   :type: Optional[float]


   
   Get or set the Mid-point absolute Rigid body Translational norm tolerance, see Remark for IAUTO = 3.
   Only active if RTTOL on *CONTROL_‌IMPLICIT_‌SOLUTION is set.
















   ..
       !! processed by numpydoc !!

.. py:property:: hrrmax
   :type: Optional[float]


   
   Get or set the Mid-point absolute Rigid body Rotational norm tolerance, see Remark for IAUTO=3.
   Only active if RRTOL on *CONTROL_‌IMPLICIT_‌SOLUTION is set.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'IMPLICIT_AUTO'






