





:class:`ControlTimestep`
========================


.. py:class:: control_timestep.ControlTimestep(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_TIMESTEP keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlTimestep

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~dtinit`
            - Get or set the Initial time step size:
          * - :py:attr:`~tssfac`
            - Get or set the Scale factor for computed time step.
          * - :py:attr:`~isdo`
            - Get or set the Basis of time size calculation.
          * - :py:attr:`~tslimt`
            - Get or set the Shell element minimum time step assignment. Applies only to *MAT_PLASTIC_KINEMATIC, *MAT_PONER_LAW_PLASTICITY, *MAT_STRAIN_RATE_DEPENDENT_PLASTICITY, *MAT_PIECE-WISE_LINEAR_PLASTICITY.
          * - :py:attr:`~dt2ms`
            - Get or set the Time step size for mass scaled solutions (default set to 0.0).
          * - :py:attr:`~lctm`
            - Get or set the Load curve ID that limits the maximum time step size (optional).
          * - :py:attr:`~erode`
            - Get or set the Erosion flag for elements with small time step.  See Remark 5.
          * - :py:attr:`~ms1st`
            - Get or set the Limit mass scaling to the first step and fix the mass vector according to the time steps. The time step will not be fixed but may drop during the calculation from the specified minimum:
          * - :py:attr:`~dt2msf`
            - Get or set the Reduction factor for initial time step size to determine the minimum time step size permitted.
          * - :py:attr:`~dt2mslc`
            - Get or set the Load curve specifying DT2MS as a function of time during the explicit solutions phase. The load curve can only be used for increasing the magnitude of DT2MS. Consequently, the magnitude of DT2MS is taken as the maximum of the current value and the value from the load curve.
          * - :py:attr:`~imscl`
            - Get or set the Flag for selective mass scaling if and only if mass scaling active.  Selective mass scaling does not scale the rigid body mass and is therefore more accurate.  Since it is memory and CPU intensive, it should be applied only to small finely meshed parts.  This option is available starting with the third revision of version 971.
          * - :py:attr:`~rmscl`
            - Get or set the Flag for using rotational option in selective mass scaling
          * - :py:attr:`~emscl`
            - Get or set the Fraction of added mass from mass scaling that contributes to gravity loads, in addition to the physical mass. See also *LOAD_BODY, this number should be between 0 and 1.
          * - :py:attr:`~ihdo`
            - Get or set the Method for calculating solid element time steps:
          * - :py:attr:`~igado`
            - Get or set the Method for calculating time steps for IGA elements:


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

    from control_timestep import ControlTimestep

Property detail
---------------

.. py:property:: dtinit
   :type: float


   
   Get or set the Initial time step size:
   EQ.0.0: LS-DYNA determines initial step size (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: tssfac
   :type: float


   
   Get or set the Scale factor for computed time step.
   LT.0:   |TSSFAC| is the load curve or function defining the scale factor as a function of time.
















   ..
       !! processed by numpydoc !!

.. py:property:: isdo
   :type: int


   
   Get or set the Basis of time size calculation.
   EQ.0: characteristic length=area/(longest side),
   EQ.1: characteristic length=area/(longest diagonal),
   EQ.2: based on bar wave speed and MAX [shortest side, area/longest side].
















   ..
       !! processed by numpydoc !!

.. py:property:: tslimt
   :type: float


   
   Get or set the Shell element minimum time step assignment. Applies only to *MAT_PLASTIC_KINEMATIC, *MAT_PONER_LAW_PLASTICITY, *MAT_STRAIN_RATE_DEPENDENT_PLASTICITY, *MAT_PIECE-WISE_LINEAR_PLASTICITY.
















   ..
       !! processed by numpydoc !!

.. py:property:: dt2ms
   :type: float


   
   Get or set the Time step size for mass scaled solutions (default set to 0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: lctm
   :type: int


   
   Get or set the Load curve ID that limits the maximum time step size (optional).
















   ..
       !! processed by numpydoc !!

.. py:property:: erode
   :type: int


   
   Get or set the Erosion flag for elements with small time step.  See Remark 5.
   EQ.0:   calculation will terminate if the solution time step drops to  (see *CONTROL_‌TERMINATION).
   EQ.1:   solid elements or thick shell elements that cause the time step to drop to  will erode; similarly, SPH particles that cause the time step to drop will be deactivated.
   EQ.10:  shell elements with time step below  will erode.
   EQ.11:  same as ERODE = 1 but shell elements will also erode
   EQ.100: beam elements with time step below  will erode.
   EQ.101: same as ERODE = 1 but beam elements will also erode
   EQ.110: beam and shell elements will erode.
   EQ.111: same as ERODE = 1 but beam and shell elements will also erode
















   ..
       !! processed by numpydoc !!

.. py:property:: ms1st
   :type: int


   
   Get or set the Limit mass scaling to the first step and fix the mass vector according to the time steps. The time step will not be fixed but may drop during the calculation from the specified minimum:
   EQ.0: no,
   EQ.1: yes.
















   ..
       !! processed by numpydoc !!

.. py:property:: dt2msf
   :type: Optional[float]


   
   Get or set the Reduction factor for initial time step size to determine the minimum time step size permitted.
















   ..
       !! processed by numpydoc !!

.. py:property:: dt2mslc
   :type: Optional[int]


   
   Get or set the Load curve specifying DT2MS as a function of time during the explicit solutions phase. The load curve can only be used for increasing the magnitude of DT2MS. Consequently, the magnitude of DT2MS is taken as the maximum of the current value and the value from the load curve.
















   ..
       !! processed by numpydoc !!

.. py:property:: imscl
   :type: Optional[int]


   
   Get or set the Flag for selective mass scaling if and only if mass scaling active.  Selective mass scaling does not scale the rigid body mass and is therefore more accurate.  Since it is memory and CPU intensive, it should be applied only to small finely meshed parts.  This option is available starting with the third revision of version 971.
   EQ.0: no selective mass scaling.
   EQ.1: all parts undergo selective mass scaling.
   LT.0: recommended.  |IMSCL| is the part set ID of the parts that undergo selective mass scaling; all other parts are mass scaled the usual way
















   ..
       !! processed by numpydoc !!

.. py:property:: rmscl
   :type: float


   
   Get or set the Flag for using rotational option in selective mass scaling
   EQ.0.: Only translational inertia are selectively mass scaled
   NE.0.: Both translational and rotational inertia are selectively mass scaled.
















   ..
       !! processed by numpydoc !!

.. py:property:: emscl
   :type: float


   
   Get or set the Fraction of added mass from mass scaling that contributes to gravity loads, in addition to the physical mass. See also *LOAD_BODY, this number should be between 0 and 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: ihdo
   :type: int


   
   Get or set the Method for calculating solid element time steps:
   EQ.0:   default method
   EQ.1:   modified method to improve time step continuity.
















   ..
       !! processed by numpydoc !!

.. py:property:: igado
   :type: int


   
   Get or set the Method for calculating time steps for IGA elements:
   EQ.0:   Default method(conservative)
   EQ.1 : account for interelement continuity(usually leads to larger time steps)
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'TIMESTEP'






