





:class:`ControlDiscreteElement`
===============================


.. py:class:: control_discrete_element.ControlDiscreteElement(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_DISCRETE_ELEMENT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlDiscreteElement

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ndamp`
            - Get or set the Normal damping coefficient.
          * - :py:attr:`~tdamp`
            - Get or set the Tangential damping coefficient.
          * - :py:attr:`~frics`
            - Get or set the Friction coefficient
          * - :py:attr:`~fricr`
            - Get or set the Rolling friction coefficient.
          * - :py:attr:`~normk`
            - Get or set the Optional: user defined normal spring constant
          * - :py:attr:`~sheark`
            - Get or set the Optional: user defined shear spring constant
          * - :py:attr:`~cap`
            - Get or set the EQ.0: dry particles
          * - :py:attr:`~vtk`
            - Get or set the Maximum number of subcycling cycles
          * - :py:attr:`~gamma`
            - Get or set the Liquid surface tension
          * - :py:attr:`~vol`
            - Get or set the Volume fraction
          * - :py:attr:`~ang`
            - Get or set the Contact angle
          * - :py:attr:`~gap`
            - Get or set the Optional parameter affecting the spatial limit of the liquid bridge.
          * - :py:attr:`~ignore`
            - Get or set the Ignore initial penetration option
          * - :py:attr:`~nbuf`
            - Get or set the GE.0:    Factor of memory use for asynchronous message buffer (Default = 6)
          * - :py:attr:`~parallel`
            - Get or set the Flag for calculating contact force between bonded DES:
          * - :py:attr:`~lnorm`
            - Get or set the Load curve ID of a curve that defines function for normal stiffness with respect to norm penetration ratio
          * - :py:attr:`~lshear`
            - Get or set the Load curve ID of a curve that defines function for shear stiffness with respect to norm penetration ratio
          * - :py:attr:`~fricd`
            - Get or set the Dynamic coefficient of friction. By default, FRICD = FRICS
          * - :py:attr:`~dc`
            - Get or set the Exponential decay coefficient
          * - :py:attr:`~ncrb`
            - Get or set the Rebalancing frequency, that is, the number of cycles between each rebalancing.  This parameter only applies to MPP.
          * - :py:attr:`~bt`
            - Get or set the Birth time.
          * - :py:attr:`~dt`
            - Get or set the Death time
          * - :py:attr:`~cp`
            - Get or set the DES thermal properties
          * - :py:attr:`~tc`
            - Get or set the DES thermal properties
          * - :py:attr:`~tfac`
            - Get or set the DES thermal properties
          * - :py:attr:`~idesoft`
            - Get or set the Flag for soft constraint formulation:
          * - :py:attr:`~sofscl`
            - Get or set the Scale factor applied to the contact stiffness in the soft constrain formulation.
          * - :py:attr:`~iskip`
            - Get or set the Flag for skipping the calculation of contact force between DES:
          * - :py:attr:`~maxnei`
            - Get or set the Number of neighbors to be tracked for DES contact and capillary force calculation (default = 20).


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

    from control_discrete_element import ControlDiscreteElement

Property detail
---------------

.. py:property:: ndamp
   :type: float


   
   Get or set the Normal damping coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: tdamp
   :type: float


   
   Get or set the Tangential damping coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: frics
   :type: float


   
   Get or set the Friction coefficient
   EQ.0: 3 DOF
   NE.0: 6 DOF (consider rotational DOF)
















   ..
       !! processed by numpydoc !!

.. py:property:: fricr
   :type: float


   
   Get or set the Rolling friction coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: normk
   :type: float


   
   Get or set the Optional: user defined normal spring constant
















   ..
       !! processed by numpydoc !!

.. py:property:: sheark
   :type: float


   
   Get or set the Optional: user defined shear spring constant
















   ..
       !! processed by numpydoc !!

.. py:property:: cap
   :type: int


   
   Get or set the EQ.0: dry particles
   NE.0: wet particles, consider capillary force and need additional
   input card
















   ..
       !! processed by numpydoc !!

.. py:property:: vtk
   :type: int


   
   Get or set the Maximum number of subcycling cycles
















   ..
       !! processed by numpydoc !!

.. py:property:: gamma
   :type: float


   
   Get or set the Liquid surface tension
















   ..
       !! processed by numpydoc !!

.. py:property:: vol
   :type: float


   
   Get or set the Volume fraction
















   ..
       !! processed by numpydoc !!

.. py:property:: ang
   :type: float


   
   Get or set the Contact angle
















   ..
       !! processed by numpydoc !!

.. py:property:: gap
   :type: float


   
   Get or set the Optional parameter affecting the spatial limit of the liquid bridge.
   CAP.EQ.0:       GAP is ignored, if the CAP field is 0 and the simulation is modeling dry particles.
   CAP.NE.0 : A liquid bridge exists when δ, as illustrated in Figure 0 - 2, is less or equal to min⁡(GAP ,d_rup) where d_rup is the rupture distance of the bridge automatically calculated by LS - DYNA
















   ..
       !! processed by numpydoc !!

.. py:property:: ignore
   :type: int


   
   Get or set the Ignore initial penetration option
   EQ.0:   Calculate the contact force for DES with initial penetration
   GT.0 : Ignore the contact force calculation for DES with initial penetration
















   ..
       !! processed by numpydoc !!

.. py:property:: nbuf
   :type: int


   
   Get or set the GE.0:    Factor of memory use for asynchronous message buffer (Default = 6)
   LT.0:   Disable asynchronous scheme and use minimum memory for data transfer
















   ..
       !! processed by numpydoc !!

.. py:property:: parallel
   :type: int


   
   Get or set the Flag for calculating contact force between bonded DES:
   EQ.0:   skip contact force calculation for bonded DES(Default)
   EQ.1 : consider contact force calculation for bonded DES
















   ..
       !! processed by numpydoc !!

.. py:property:: lnorm
   :type: int


   
   Get or set the Load curve ID of a curve that defines function for normal stiffness with respect to norm penetration ratio
















   ..
       !! processed by numpydoc !!

.. py:property:: lshear
   :type: int


   
   Get or set the Load curve ID of a curve that defines function for shear stiffness with respect to norm penetration ratio
















   ..
       !! processed by numpydoc !!

.. py:property:: fricd
   :type: float


   
   Get or set the Dynamic coefficient of friction. By default, FRICD = FRICS
















   ..
       !! processed by numpydoc !!

.. py:property:: dc
   :type: float


   
   Get or set the Exponential decay coefficient
















   ..
       !! processed by numpydoc !!

.. py:property:: ncrb
   :type: int


   
   Get or set the Rebalancing frequency, that is, the number of cycles between each rebalancing.  This parameter only applies to MPP.
   EQ.0:   no rebalancing is performed
















   ..
       !! processed by numpydoc !!

.. py:property:: bt
   :type: float


   
   Get or set the Birth time.
















   ..
       !! processed by numpydoc !!

.. py:property:: dt
   :type: float


   
   Get or set the Death time
















   ..
       !! processed by numpydoc !!

.. py:property:: cp
   :type: float


   
   Get or set the DES thermal properties
















   ..
       !! processed by numpydoc !!

.. py:property:: tc
   :type: float


   
   Get or set the DES thermal properties
















   ..
       !! processed by numpydoc !!

.. py:property:: tfac
   :type: float


   
   Get or set the DES thermal properties
















   ..
       !! processed by numpydoc !!

.. py:property:: idesoft
   :type: int


   
   Get or set the Flag for soft constraint formulation:
   EQ.1:   Soft constraint formulation.The contact stiffness is based on the nodal mass and the global time step size.
   This input provides a different way for calculating NORMK.NORMK is ignored if IDESOFT = 1. IDESOFT is ignored if LNORM ≠ 0.
















   ..
       !! processed by numpydoc !!

.. py:property:: sofscl
   :type: float


   
   Get or set the Scale factor applied to the contact stiffness in the soft constrain formulation.
















   ..
       !! processed by numpydoc !!

.. py:property:: iskip
   :type: int


   
   Get or set the Flag for skipping the calculation of contact force between DES:
   EQ.0:   Consider the particle - particle contact calculation(default).
   EQ.1 : Skip the particle - particle contact calculation.
















   ..
       !! processed by numpydoc !!

.. py:property:: maxnei
   :type: int


   
   Get or set the Number of neighbors to be tracked for DES contact and capillary force calculation (default = 20).
   If particle sizes are very different, MAXNEI needs to be increased to capture more neighbors
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'DISCRETE_ELEMENT'






