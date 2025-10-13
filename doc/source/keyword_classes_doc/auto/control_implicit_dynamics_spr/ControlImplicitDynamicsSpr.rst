





:class:`ControlImplicitDynamicsSpr`
===================================


.. py:class:: control_implicit_dynamics_spr.ControlImplicitDynamicsSpr(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_IMPLICIT_DYNAMICS_SPR keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlImplicitDynamicsSpr

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~imass`
            - Get or set the Implicit analysis type:
          * - :py:attr:`~gamma`
            - Get or set the Newmark time integration constant (default = 0.50).
          * - :py:attr:`~beta`
            - Get or set the Newmark time integration constant (default = 0.25).
          * - :py:attr:`~tdybir`
            - Get or set the Birth time for application of dynamic terms.
          * - :py:attr:`~tdydth`
            - Get or set the Death time for application of dynamic terms.
          * - :py:attr:`~tdybur`
            - Get or set the Burial time for application of dynamic terms.
          * - :py:attr:`~irate`
            - Get or set the Rate effects switch:
          * - :py:attr:`~alpha`
            - Get or set the Composite time integration constant (see Remark 2).
          * - :py:attr:`~psid`
            - Get or set the Part set ID for a body undergoing rotational (spinning) motion.
          * - :py:attr:`~angle`
            - Get or set the Target angle increment during a single time step, in degrees.


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

    from control_implicit_dynamics_spr import ControlImplicitDynamicsSpr

Property detail
---------------

.. py:property:: imass
   :type: int


   
   Get or set the Implicit analysis type:
   LT.0: curve ID=(-IMASS) used to control amount of implicit dynamic effect applied to the analysis. TDYBIR, TDYDTH and TDYBUR are ignored with this option
   EQ.0: static analysis
   EQ.1: dynamic analysis using Newmark time integration.
   EQ.2: dynamic analysis by modal superposition following the solution of the eigenvalue problem.
   EQ.3: dynamic analysis by modal superposition using the eigenvalue solution in d3eigv files that are in the runtime directory.
















   ..
       !! processed by numpydoc !!

.. py:property:: gamma
   :type: float


   
   Get or set the Newmark time integration constant (default = 0.50).
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: float


   
   Get or set the Newmark time integration constant (default = 0.25).
















   ..
       !! processed by numpydoc !!

.. py:property:: tdybir
   :type: float


   
   Get or set the Birth time for application of dynamic terms.
















   ..
       !! processed by numpydoc !!

.. py:property:: tdydth
   :type: float


   
   Get or set the Death time for application of dynamic terms.
















   ..
       !! processed by numpydoc !!

.. py:property:: tdybur
   :type: float


   
   Get or set the Burial time for application of dynamic terms.
















   ..
       !! processed by numpydoc !!

.. py:property:: irate
   :type: int


   
   Get or set the Rate effects switch:
   EQ.-1: rate effects are on in constitutive models even in implicit statics
   EQ.0: rate effects are on in constitutive models, except implicit statics
   EQ.1: rate effects are off in constitutive models
   EQ.2: rate effects are off in constitutive models for both explicit and implicit
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha
   :type: float


   
   Get or set the Composite time integration constant (see Remark 2).
   GT.0: Bathe composite scheme is activated
   LT.0.AND.GT. - 1 : HHT scheme is activated
   LE. - 1 : Specify part sets for finite rotational dynamics
















   ..
       !! processed by numpydoc !!

.. py:property:: psid
   :type: Optional[int]


   
   Get or set the Part set ID for a body undergoing rotational (spinning) motion.
















   ..
       !! processed by numpydoc !!

.. py:property:: angle
   :type: float


   
   Get or set the Target angle increment during a single time step, in degrees.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'IMPLICIT_DYNAMICS_SPR'






