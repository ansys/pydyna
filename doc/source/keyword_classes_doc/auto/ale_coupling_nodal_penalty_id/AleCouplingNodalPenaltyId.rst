





:class:`AleCouplingNodalPenaltyId`
==================================


.. py:class:: ale_coupling_nodal_penalty_id.AleCouplingNodalPenaltyId(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ALE_COUPLING_NODAL_PENALTY_ID keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: AleCouplingNodalPenaltyId

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~coupid`
            - Get or set the Coupling (card) ID number (I10). If not defined, LSDYNA will assign an internal coupling ID based on the order of appearance in the input deck.
          * - :py:attr:`~title`
            - Get or set the A description of this coupling definition (A70).
          * - :py:attr:`~strsid`
            - Get or set the Set ID defining a part, part set or segment set ID of structure (see *PART, *SET_PART or *SET_SEGMENT). The structure may include Lagrangian elements, EFG, SPG, or SPH.
          * - :py:attr:`~alesid`
            - Get or set the Master set ID defining a part or part set ID of the ALE or master solid elements (see *PART or *SET_PART)
          * - :py:attr:`~strsty`
            - Get or set the Slave set type of "SLAVE"
          * - :py:attr:`~alesty`
            - Get or set the Master set type of "MASTER"
          * - :py:attr:`~mcoup`
            - Get or set the Multi-material option
          * - :py:attr:`~start`
            - Get or set the Start time for coupling.
          * - :py:attr:`~end`
            - Get or set the End time for coupling.
          * - :py:attr:`~pform`
            - Get or set the Penalty stiffness formulations:
          * - :py:attr:`~pfac`
            - Get or set the Penalty stiffness factor (PFORM = 0 or 1) for scaling the estimated stiffness of the interacting (coupling) system or load curve ID (PFORM = 2).
          * - :py:attr:`~frcmin`
            - Get or set the Only to be used with nonzero MCOUP. Minimum volume fraction of the fluid materials included in the list of AMMGs to activate coupling. Default value is 0.5. Reducing FRCMIN (typically, between 0.1 and 0.3) would turn on coupling earlier to prevent leakage in hypervelocity impact cases.


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

    from ale_coupling_nodal_penalty_id import AleCouplingNodalPenaltyId

Property detail
---------------

.. py:property:: coupid
   :type: Optional[int]


   
   Get or set the Coupling (card) ID number (I10). If not defined, LSDYNA will assign an internal coupling ID based on the order of appearance in the input deck.
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the A description of this coupling definition (A70).
















   ..
       !! processed by numpydoc !!

.. py:property:: strsid
   :type: Optional[int]


   
   Get or set the Set ID defining a part, part set or segment set ID of structure (see *PART, *SET_PART or *SET_SEGMENT). The structure may include Lagrangian elements, EFG, SPG, or SPH.
















   ..
       !! processed by numpydoc !!

.. py:property:: alesid
   :type: Optional[int]


   
   Get or set the Master set ID defining a part or part set ID of the ALE or master solid elements (see *PART or *SET_PART)
















   ..
       !! processed by numpydoc !!

.. py:property:: strsty
   :type: int


   
   Get or set the Slave set type of "SLAVE"
   EQ.0: Part set ID (PSID).
   EQ.1: Part ID (PID).
   EQ.2: Segment set ID (SSID).
   EQ 3: Node set ID (NSID)
















   ..
       !! processed by numpydoc !!

.. py:property:: alesty
   :type: int


   
   Get or set the Master set type of "MASTER"
   EQ.0: Part set ID (PSID).
   EQ.1: Part ID (PID).
















   ..
       !! processed by numpydoc !!

.. py:property:: mcoup
   :type: int


   
   Get or set the Multi-material option
   EQ.0: Couple with all multi-material groups,
   EQ.-n: refers to a set ID of an ALE multi-material groups defined in *SET_MULTI-MATERIAL_GROUP card in which its set ID=n.
















   ..
       !! processed by numpydoc !!

.. py:property:: start
   :type: float


   
   Get or set the Start time for coupling.
















   ..
       !! processed by numpydoc !!

.. py:property:: end
   :type: float


   
   Get or set the End time for coupling.
















   ..
       !! processed by numpydoc !!

.. py:property:: pform
   :type: Optional[int]


   
   Get or set the Penalty stiffness formulations:
   EQ.0:   Mass based penalty stiffness
   EQ.1 : Bulk modulus based penalty stiffness
   amespace
   Q.2 : Penalty stiffness is determined by the user - provided load curve between penetration and penalty pressure.
















   ..
       !! processed by numpydoc !!

.. py:property:: pfac
   :type: Optional[int]


   
   Get or set the Penalty stiffness factor (PFORM = 0 or 1) for scaling the estimated stiffness of the interacting (coupling) system or load curve ID (PFORM = 2).
















   ..
       !! processed by numpydoc !!

.. py:property:: frcmin
   :type: float


   
   Get or set the Only to be used with nonzero MCOUP. Minimum volume fraction of the fluid materials included in the list of AMMGs to activate coupling. Default value is 0.5. Reducing FRCMIN (typically, between 0.1 and 0.3) would turn on coupling earlier to prevent leakage in hypervelocity impact cases.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ALE'


.. py:attribute:: subkeyword
   :value: 'COUPLING_NODAL_PENALTY_ID'






