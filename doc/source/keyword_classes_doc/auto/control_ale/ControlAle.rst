





:class:`ControlAle`
===================


.. py:class:: control_ale.ControlAle(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_ALE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlAle

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~dct`
            - Get or set the Flag to invoke alternate advection logic for ALE (see Remark 2):
          * - :py:attr:`~nadv`
            - Get or set the Number of cycles between advections.
          * - :py:attr:`~meth`
            - Get or set the Advection method:
          * - :py:attr:`~afac`
            - Get or set the ALE smoothing weight factor - Simple average:
          * - :py:attr:`~bfac`
            - Get or set the ALE smoothing weight factor for Volume weighting.
          * - :py:attr:`~cfac`
            - Get or set the ALE smoothing weight factor for Isoparametric.
          * - :py:attr:`~dfac`
            - Get or set the ALE smoothing weight factor for Equipotential.
          * - :py:attr:`~efac`
            - Get or set the ALE smoothing weight factor for Equilibrium.
          * - :py:attr:`~start`
            - Get or set the Start time for ALE smoothing (default = 0.0).
          * - :py:attr:`~end`
            - Get or set the END End time for ALE smoothing (default = 1.0E+20).
          * - :py:attr:`~aafac`
            - Get or set the ALE advection factor (donor cell options, default=1.0).This field is obsolete
          * - :py:attr:`~vfact`
            - Get or set the Void factor. This is the definition of a void. A void is obtained by multiplying the time zero density of an element by a factor called the void factor (default = 1.0E-06).
          * - :py:attr:`~prit`
            - Get or set the A floag to turn on or off the pressure equilibrium iteration option for multimaterial elements.
          * - :py:attr:`~ebc`
            - Get or set the Automatic Euler boundary condition:
          * - :py:attr:`~pref`
            - Get or set the A pseudo reference pressure equivalent to an environmental pressure that is being applied to the free surfaces of the ALE domain or meh
          * - :py:attr:`~nsidebc`
            - Get or set the A node set ID (NSID) which is to be excluded from the EBC constraint.
          * - :py:attr:`~ncpl`
            - Get or set the Number of Lagrangian cycles between coupling calculations.  This is typically done every cycle; therefore, its default is 1.  This is on optional card 3.
          * - :py:attr:`~nbkt`
            - Get or set the Number of Lagrangian cycles between global bucket-sort searches to locate the position of the Lagrangian structure (mesh) relative to the ALE fluid (mesh).  Default is 50.  This is on optional card 3.
          * - :py:attr:`~imascl`
            - Get or set the A flag for turning ON/OFF mass scaling for ALE parts.  The global mass scaling control (parameter DT2MS under *CONTROL_ TIMESTEP card) must be ON.  If the run dt is lower than the mass scaling dt, then IMASCL has the following effects:
          * - :py:attr:`~checkr`
            - Get or set the A parameter for reducing or eliminating an ALE pressure locking pattern.  It may range from 0.01 to 0.1.
          * - :py:attr:`~beamin`
            - Get or set the Flag to align the dynamics of plain strain and axisymmetric
          * - :py:attr:`~mmgpref`
            - Get or set the A flag to select the method for assigning a reference pressure to multiple ALE multi-material groups (see Remark 3).
          * - :py:attr:`~pdifmx`
            - Get or set the Maximum of pressure difference between neighboring ALE
          * - :py:attr:`~dtmufac`
            - Get or set the Scale a time step called DTMU that depends on the dynamic viscosity
          * - :py:attr:`~optimpp`
            - Get or set the Optimize the MPP communications in the penalty coupling
          * - :py:attr:`~ialedr`
            - Get or set the Include ALE computations in the dynamic relaxation analysis (*CONTROL_DYNAMIC_RELAXATION).
          * - :py:attr:`~bndflx`
            - Get or set the Multi-Material ALE group set ID selecting only the materials in elements at mesh boundaries with influxes that can flow in. By default, when the flow is inwards at boundary faces of ALE elements, every materials in these elements flow in. This option can select only a few of these ALE groups.
          * - :py:attr:`~minmas`
            - Get or set the Factor of the minimum mass allowed in an element: MINMAS*initial_density*element_volume.


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

    from control_ale import ControlAle

Property detail
---------------

.. py:property:: dct
   :type: int


   
   Get or set the Flag to invoke alternate advection logic for ALE (see Remark 2):
   NE. - 1:        Use default advection logic.
   EQ. - 1 : Use alternate(improved) advection logic; generally recommended, especially for simulation of explosives.
   Note that for S - ALE DCT is ignored and the alternative advection option is always used.
















   ..
       !! processed by numpydoc !!

.. py:property:: nadv
   :type: int


   
   Get or set the Number of cycles between advections.
















   ..
       !! processed by numpydoc !!

.. py:property:: meth
   :type: int


   
   Get or set the Advection method:
   EQ.1: donor cell + HIS (first order accurate),
   EQ.2: Van Leer + half index shift (second order).
   EQ.-2 Modified Van Leer
   EQ.3: donor cell + HIS, first order accurate, conserving total energy over each advection step instead of conserving internal energy
   EQ.6:   Finite Volume Method with a Flux Corrected Transport. Only supported by ideal gases: the finite volume method is only applied to ALE elements fully filled with materials using *EOS_IDEAL_GAS or *EOS_001 for ideal gases. The advection in mixed ALE elements is handled by a donor cell method.
















   ..
       !! processed by numpydoc !!

.. py:property:: afac
   :type: float


   
   Get or set the ALE smoothing weight factor - Simple average:
   EQ.-1: turn smoothing off.
















   ..
       !! processed by numpydoc !!

.. py:property:: bfac
   :type: float


   
   Get or set the ALE smoothing weight factor for Volume weighting.
















   ..
       !! processed by numpydoc !!

.. py:property:: cfac
   :type: float


   
   Get or set the ALE smoothing weight factor for Isoparametric.
















   ..
       !! processed by numpydoc !!

.. py:property:: dfac
   :type: float


   
   Get or set the ALE smoothing weight factor for Equipotential.
















   ..
       !! processed by numpydoc !!

.. py:property:: efac
   :type: float


   
   Get or set the ALE smoothing weight factor for Equilibrium.
















   ..
       !! processed by numpydoc !!

.. py:property:: start
   :type: float


   
   Get or set the Start time for ALE smoothing (default = 0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: end
   :type: float


   
   Get or set the END End time for ALE smoothing (default = 1.0E+20).
















   ..
       !! processed by numpydoc !!

.. py:property:: aafac
   :type: float


   
   Get or set the ALE advection factor (donor cell options, default=1.0).This field is obsolete
















   ..
       !! processed by numpydoc !!

.. py:property:: vfact
   :type: float


   
   Get or set the Void factor. This is the definition of a void. A void is obtained by multiplying the time zero density of an element by a factor called the void factor (default = 1.0E-06).
















   ..
       !! processed by numpydoc !!

.. py:property:: prit
   :type: int


   
   Get or set the A floag to turn on or off the pressure equilibrium iteration option for multimaterial elements.
   EQ. 0. Off (default)
   EQ. 1. On
















   ..
       !! processed by numpydoc !!

.. py:property:: ebc
   :type: int


   
   Get or set the Automatic Euler boundary condition:
   EQ.0: off (default),
   EQ.1: on with stick condition,
   EQ.2: on with slip condition.
















   ..
       !! processed by numpydoc !!

.. py:property:: pref
   :type: float


   
   Get or set the A pseudo reference pressure equivalent to an environmental pressure that is being applied to the free surfaces of the ALE domain or meh
















   ..
       !! processed by numpydoc !!

.. py:property:: nsidebc
   :type: Optional[int]


   
   Get or set the A node set ID (NSID) which is to be excluded from the EBC constraint.
















   ..
       !! processed by numpydoc !!

.. py:property:: ncpl
   :type: int


   
   Get or set the Number of Lagrangian cycles between coupling calculations.  This is typically done every cycle; therefore, its default is 1.  This is on optional card 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: nbkt
   :type: int


   
   Get or set the Number of Lagrangian cycles between global bucket-sort searches to locate the position of the Lagrangian structure (mesh) relative to the ALE fluid (mesh).  Default is 50.  This is on optional card 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: imascl
   :type: int


   
   Get or set the A flag for turning ON/OFF mass scaling for ALE parts.  The global mass scaling control (parameter DT2MS under *CONTROL_ TIMESTEP card) must be ON.  If the run dt is lower than the mass scaling dt, then IMASCL has the following effects:
   EQ.0: (Default) No mass scaling for ALE parts.  Print out maximum 20 warnings.
   EQ.1: No mass scaling for ALE parts.  Stop the run.
   EQ.2: Do mass scaling for ALE parts (the result may not be correct due to this scaling)
















   ..
       !! processed by numpydoc !!

.. py:property:: checkr
   :type: float


   
   Get or set the A parameter for reducing or eliminating an ALE pressure locking pattern.  It may range from 0.01 to 0.1.
















   ..
       !! processed by numpydoc !!

.. py:property:: beamin
   :type: float


   
   Get or set the Flag to align the dynamics of plain strain and axisymmetric
   beams in 2D FSI ALE models to their shell counterparts in 3D FSI ALE models:
   EQ.0.0: Off (default)
   EQ.1.0: On
















   ..
       !! processed by numpydoc !!

.. py:property:: mmgpref
   :type: int


   
   Get or set the A flag to select the method for assigning a reference pressure to multiple ALE multi-material groups (see Remark 3).
   EQ.0: OFF(default) PREF applies to every AMMG in the model.
   LT.0 : Then | MMGPREF | is an ID of either(a) a curve defined via DEFINE_CURVE or (b)a table defined via DEFINE_TABLE.Since by convention Tables and Load curves may not share common ID's, there will be no confusion about which ID is to be read
















   ..
       !! processed by numpydoc !!

.. py:property:: pdifmx
   :type: float


   
   Get or set the Maximum of pressure difference between neighboring ALE
   elements under which the stresses are zeroed out:
   EQ.0: Off (default)
   GT.0: On
















   ..
       !! processed by numpydoc !!

.. py:property:: dtmufac
   :type: float


   
   Get or set the Scale a time step called DTMU that depends on the dynamic viscosity
   DTMU is emitted by the element to the solver as an element time
   step, thereby making DTMU an upper bound on the global time step.
   EQ.0: Off (default)
   GT.0: On.
















   ..
       !! processed by numpydoc !!

.. py:property:: optimpp
   :type: int


   
   Get or set the Optimize the MPP communications in the penalty coupling
   (*CONSTRAINED_LAGRANGE_IN_SOLID, CTYPE = 4) and
   group ALE parts together for the element processing.
   EQ.0: Off (default)
   EQ.1: On.
















   ..
       !! processed by numpydoc !!

.. py:property:: ialedr
   :type: int


   
   Get or set the Include ALE computations in the dynamic relaxation analysis (*CONTROL_DYNAMIC_RELAXATION).
   EQ.0:   Off (default)
   EQ.1:   On.
















   ..
       !! processed by numpydoc !!

.. py:property:: bndflx
   :type: int


   
   Get or set the Multi-Material ALE group set ID selecting only the materials in elements at mesh boundaries with influxes that can flow in. By default, when the flow is inwards at boundary faces of ALE elements, every materials in these elements flow in. This option can select only a few of these ALE groups.
   EQ.0:   Off (default)
   GT.0:   *SET_MULTI-MATERIAL_GROUP_LIST ID
   EQ.-1: No influx.
















   ..
       !! processed by numpydoc !!

.. py:property:: minmas
   :type: float


   
   Get or set the Factor of the minimum mass allowed in an element: MINMAS*initial_density*element_volume.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'ALE'






