





:class:`AleStructuredFsi`
=========================


.. py:class:: ale_structured_fsi.AleStructuredFsi(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ALE_STRUCTURED_FSI keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: AleStructuredFsi

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~coupid`
            - Get or set the Coupling (card) ID number.  If not defined, LS-DYNA will assign an internal coupling ID based on the order of appearance in the input deck.
          * - :py:attr:`~title`
            - Get or set the A description of this coupling definition (A70).
          * - :py:attr:`~lstrsid`
            - Get or set the Set ID defining a part, part set, or segment set ID of the Lagrangian structure (see *PART, *SET_PART or *SET_SEGMENT).
          * - :py:attr:`~alesid`
            - Get or set the Set ID defining a part or part set ID of the Structured ALE mesh (see *PART).
          * - :py:attr:`~lstrstyp`
            - Get or set the Set type of LSTRSID:
          * - :py:attr:`~alestyp`
            - Get or set the Set type of ALESID:
          * - :py:attr:`~mcoup`
            - Get or set the Which Multi-material(s) to be coupled (Remark 1):
          * - :py:attr:`~start`
            - Get or set the Start time for coupling.
          * - :py:attr:`~end`
            - Get or set the End time for coupling.
          * - :py:attr:`~pfac`
            - Get or set the Penalty factor. PFAC is a scale factor for scaling the estimated stiffness of the interacting(coupling) system.It is used to compute the coupling forces to be distributed on the Lagrangian and ALE parts.
          * - :py:attr:`~fric`
            - Get or set the Friction Coefficient.  Friction force is evaluated as normal force multiplied by friction coefficient.
          * - :py:attr:`~flip`
            - Get or set the A Lagrangian segment will couple to fluid on only one side of the segment.The assump tion is segment normal points to f luids to be coupl ed. If that is not the case , set flip to 1.


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

    from ale_structured_fsi import AleStructuredFsi

Property detail
---------------

.. py:property:: coupid
   :type: Optional[int]


   
   Get or set the Coupling (card) ID number.  If not defined, LS-DYNA will assign an internal coupling ID based on the order of appearance in the input deck.
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the A description of this coupling definition (A70).
















   ..
       !! processed by numpydoc !!

.. py:property:: lstrsid
   :type: Optional[int]


   
   Get or set the Set ID defining a part, part set, or segment set ID of the Lagrangian structure (see *PART, *SET_PART or *SET_SEGMENT).
















   ..
       !! processed by numpydoc !!

.. py:property:: alesid
   :type: Optional[int]


   
   Get or set the Set ID defining a part or part set ID of the Structured ALE mesh (see *PART).
















   ..
       !! processed by numpydoc !!

.. py:property:: lstrstyp
   :type: int


   
   Get or set the Set type of LSTRSID:
   EQ.0:   part set ID (PSID).
   EQ.1:   part ID (PID).
   EQ.2:   segment set ID (SGSID).
















   ..
       !! processed by numpydoc !!

.. py:property:: alestyp
   :type: int


   
   Get or set the Set type of ALESID:
   EQ.0:   part set ID (PSID).
   EQ.1:   part ID (PID).
















   ..
       !! processed by numpydoc !!

.. py:property:: mcoup
   :type: Optional[int]


   
   Get or set the Which Multi-material(s) to be coupled (Remark 1):
   EQ.0:   couple with all multi-material groups,
   EQ.-N:  -N is the ID of *SET_MULTI-MATERIAL_GROUP.
















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

.. py:property:: pfac
   :type: float


   
   Get or set the Penalty factor. PFAC is a scale factor for scaling the estimated stiffness of the interacting(coupling) system.It is used to compute the coupling forces to be distributed on the Lagrangian and ALE parts.
   GT.0:Fraction of estimated critical stiffness.
   LT.1:PFAC must be an integer, and PFAC is a load curve ID. The curve defines the coupling pressure on the axis as a function of the penetration along the axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: fric
   :type: float


   
   Get or set the Friction Coefficient.  Friction force is evaluated as normal force multiplied by friction coefficient.
   GT.0:   Constant friction coefficient
   EQ. - N : Variable friction coefficient; defined by a TABLE ID = N.The table is to look up the friction coefficient value given a pair of(coupling pressure, relative velocity)..
















   ..
       !! processed by numpydoc !!

.. py:property:: flip
   :type: int


   
   Get or set the A Lagrangian segment will couple to fluid on only one side of the segment.The assump tion is segment normal points to f luids to be coupl ed. If that is not the case , set flip to 1.
   EQ.0:N o action.
   EQ.1:Flip the segment normal so it points to fluids to be coupled.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ALE'


.. py:attribute:: subkeyword
   :value: 'STRUCTURED_FSI'






