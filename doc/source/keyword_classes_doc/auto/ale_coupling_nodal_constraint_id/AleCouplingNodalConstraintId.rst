





:class:`AleCouplingNodalConstraintId`
=====================================


.. py:class:: ale_coupling_nodal_constraint_id.AleCouplingNodalConstraintId(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ALE_COUPLING_NODAL_CONSTRAINT_ID keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: AleCouplingNodalConstraintId

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
            - Get or set the Set ID defining a part, part set or segment set ID of the structure (see *PART, *SET_‌PART or *SET_‌SEGMENT). The structure may include Lagrangian solid, shell, beam, thick shell, or discrete sphere elements. EFG, SPH, or EFG nodes may be used, but the boundary conditions may not be satisfied
          * - :py:attr:`~alesid`
            - Get or set the Set ID defining a part or part set ID of the ALE solid elements (see *PART or *SET_‌PART).
          * - :py:attr:`~strsty`
            - Get or set the Set type of STRSID
          * - :py:attr:`~alesty`
            - Get or set the Master set type of "MASTER"
          * - :py:attr:`~ctype`
            - Get or set the Coupling type:
          * - :py:attr:`~mcoup`
            - Get or set the Multi-material option (CTYPE 4, 5, 6, 11 and 12, ).
          * - :py:attr:`~start`
            - Get or set the Start time for coupling.
          * - :py:attr:`~end`
            - Get or set the End time for coupling.
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

    from ale_coupling_nodal_constraint_id import AleCouplingNodalConstraintId

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


   
   Get or set the Set ID defining a part, part set or segment set ID of the structure (see *PART, *SET_‌PART or *SET_‌SEGMENT). The structure may include Lagrangian solid, shell, beam, thick shell, or discrete sphere elements. EFG, SPH, or EFG nodes may be used, but the boundary conditions may not be satisfied
















   ..
       !! processed by numpydoc !!

.. py:property:: alesid
   :type: Optional[int]


   
   Get or set the Set ID defining a part or part set ID of the ALE solid elements (see *PART or *SET_‌PART).
















   ..
       !! processed by numpydoc !!

.. py:property:: strsty
   :type: int


   
   Get or set the Set type of STRSID
   EQ.0: Part set ID (PSID).
   EQ.1: Part ID (PID).
   EQ.2: Segment set ID (SGSID).
   EQ.3: Node set ID(NSID)
















   ..
       !! processed by numpydoc !!

.. py:property:: alesty
   :type: int


   
   Get or set the Master set type of "MASTER"
   EQ.0: Part set ID (PSID).
   EQ.1: Part ID (PID).
















   ..
       !! processed by numpydoc !!

.. py:property:: ctype
   :type: int


   
   Get or set the Coupling type:
   EQ.1: Constrained acceleration.
   EQ.2: Constrained acceleration and velocity.
















   ..
       !! processed by numpydoc !!

.. py:property:: mcoup
   :type: Optional[int]


   
   Get or set the Multi-material option (CTYPE 4, 5, 6, 11 and 12, ).
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
   :value: 'COUPLING_NODAL_CONSTRAINT_ID'






