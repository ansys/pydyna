





:class:`AleReferenceSystemGroup`
================================


.. py:class:: ale_reference_system_group.AleReferenceSystemGroup(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ALE_REFERENCE_SYSTEM_GROUP keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: AleReferenceSystemGroup

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the Set ID.
          * - :py:attr:`~stype`
            - Get or set the Set type:
          * - :py:attr:`~prtype`
            - Get or set the Reference system type :
          * - :py:attr:`~prid`
            - Get or set the A parameter giving additional information depending on the reference system (PRTYPE) choice:
          * - :py:attr:`~bctran`
            - Get or set the For PRTYPE 4 & 5:  BCTRAN is a translational constraint (remark 3).
          * - :py:attr:`~bcexp`
            - Get or set the For PRTYPE= 4 & 7:  BCTRAN is an expansion constraint (remark 3).
          * - :py:attr:`~bcrot`
            - Get or set the For PRTYPE= 4:  BCROT is a rotational constraint (remark 3).
          * - :py:attr:`~icoord`
            - Get or set the PRTYPE=4: ICR is a center of mesh expansion and rotation flag,


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

    from ale_reference_system_group import AleReferenceSystemGroup

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: stype
   :type: int


   
   Get or set the Set type:
   EQ.0: part set (default),
   EQ.1: part,
   EQ.2: node set,
   EQ.3: segment set.
















   ..
       !! processed by numpydoc !!

.. py:property:: prtype
   :type: int


   
   Get or set the Reference system type :
   EQ.0: Eulerian,
   EQ.1: Lagrangian,
   EQ.2: Normal ALE mesh smoothing,
   EQ.3: Prescribed motion following load curves, see *ALE_REFERENCE_ SYSTEM_CURVE,
   EQ.4: Automatic mesh motion following mass weighted average velocity in ALE mesh,
   EQ.5: Automatic mesh motion following a local coordinate system defined by three user defined nodes, see *ALE_REFERENCE_SYSTEM_NODE,
   EQ.6: Switching in time between different reference system types, see *ALE_REFERENCE_SYSTEM_SWITCH,
   EQ.7: Automatic mesh expansion in order to enclose up to twelve user defined nodes, see *ALE_REFERENCE_SYSTEM_NODE.
   EQ.8: Mesh smoothing option for shock waves, where the element grid contracts in the vicinity of the shock front.  This may be referred to as the Delayed-ALE option.  It controls how much the mesh is to be moved during the remap step.  This option requires the definition of the 5th parameter in the 2nd card, EFAC; see below for definition.
   EQ.9: Allowing the ALE mesh(es) to:
   -Translate and/or rotate to follow a local Lagrangian reference coordinate system (whose *ALE_REFERENCE_SYSTEM_NODE card ID is defined by the BCTRAN parameter)
   -Expand or contract to enclose a Lagrangian part-set ID defined by the PRID parameter.
   -Has a Lagrangian node ID be defined by the ICR/NID parameter to be the center of the ALE mesh expansion.
















   ..
       !! processed by numpydoc !!

.. py:property:: prid
   :type: Optional[int]


   
   Get or set the A parameter giving additional information depending on the reference system (PRTYPE) choice:
   PRTYPE.EQ.3:    PRID defines a load curve group ID specifying an * ALE_‌REFERENCE_‌SYSTEM_‌CURVE card for mesh translation.This defines up to 12 curves which prescribe the motion of the system.
   PRTYPE.EQ.4 : PRID defines a node set ID(*SET_‌NODE), for which a mass average velocity is computed.This velocity controls the mesh motion.
   PRTYPE.EQ.5 : PRID defines a node group ID specifying an * ALE_‌REFERENCE_‌SYSTEM_‌NODE card, via which, three nodes forming a local coordinate system are defined.
   PRTYPE.EQ.6 : PRID defines a switch list ID specifying an* ALE_‌REFERENCE_‌SYSTEM_‌SWITCH card.This defines the switch timesand the reference system choices for each time interval between the switches.
   PRTYPE.EQ.7 : PRID defines a node group ID specifying an * ALE_‌REFERENCE_‌SYSTEM_‌NODE card.Up to 12 nodes in space forming a region to be enveloped by the ALE mesh are defined.
   PRTYPE.EQ.9 : PRID defines a Lagrangian part set ID(PSID) defining the Lagrangian part(s) whose range of motion is to be enveloped by the ALE mesh(es).This is useful for airbag modeling.
















   ..
       !! processed by numpydoc !!

.. py:property:: bctran
   :type: int


   
   Get or set the For PRTYPE 4 & 5:  BCTRAN is a translational constraint (remark 3).
   EQ.0: no constraints,
   EQ.1: constrained x translation,
   EQ.2: constrained y translation,
   EQ.3: constrained z translation,
   EQ.4: constrained x and y translation,
   EQ.5: constrained y and z translation,
   EQ.6: constrained z and x translation,
   EQ.7: constrained x, y, and z translation
















   ..
       !! processed by numpydoc !!

.. py:property:: bcexp
   :type: int


   
   Get or set the For PRTYPE= 4 & 7:  BCTRAN is an expansion constraint (remark 3).
   EQ.0: no constraints,
   EQ.1: constrained x expansion,
   EQ.2: constrained y expansion,
   EQ.3: constrained z expansion,
   EQ.4: constrained x and y expansion,
   EQ.5: constrained y and z expansion,
   EQ.6: constrained z and x expansion,
   EQ.7: constrained x, y, and z expansion
















   ..
       !! processed by numpydoc !!

.. py:property:: bcrot
   :type: int


   
   Get or set the For PRTYPE= 4:  BCROT is a rotational constraint (remark 3).
   EQ.0: no constraints,
   EQ.1: constrained x rotation,
   EQ.2: constrained y rotation,
   EQ.3: constrained z rotation,
   EQ.4: constrained x and y rotation,
   EQ.5: constrained y and z rotation,
   EQ.6: constrained z and x rotation,
   EQ.7: constrained x, y, and z rotation
















   ..
       !! processed by numpydoc !!

.. py:property:: icoord
   :type: int


   
   Get or set the PRTYPE=4: ICR is a center of mesh expansion and rotation flag,
   EQ.0:  The center is at center of gravity of the ALE mesh.
   EQ.1:  The center is at (XC, YC, ZC), just a point in space (it does not have to be a defined node)
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ALE'


.. py:attribute:: subkeyword
   :value: 'REFERENCE_SYSTEM_GROUP'






