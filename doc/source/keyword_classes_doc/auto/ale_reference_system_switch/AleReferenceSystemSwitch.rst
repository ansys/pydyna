





:class:`AleReferenceSystemSwitch`
=================================


.. py:class:: ale_reference_system_switch.AleReferenceSystemSwitch(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ALE_REFERENCE_SYSTEM_SWITCH keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: AleReferenceSystemSwitch

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Switch list ID, see *ALE_REFERENCE_SYSTEM_GROUP.
          * - :py:attr:`~t1`
            - Get or set the Time one for switching reference system type.
          * - :py:attr:`~t2`
            - Get or set the Time two for switching reference system type.
          * - :py:attr:`~t3`
            - Get or set the Time three for switching reference system type.
          * - :py:attr:`~t4`
            - Get or set the Time four for switching reference system type.
          * - :py:attr:`~t5`
            - Get or set the Time five for switching reference system type.
          * - :py:attr:`~t6`
            - Get or set the Time six for switching reference system type.
          * - :py:attr:`~t7`
            - Get or set the Time seven for switching reference system type.
          * - :py:attr:`~type1`
            - Get or set the Reference system types:
          * - :py:attr:`~type2`
            - Get or set the Reference system types:
          * - :py:attr:`~type3`
            - Get or set the Reference system types:
          * - :py:attr:`~type4`
            - Get or set the Reference system types:
          * - :py:attr:`~type5`
            - Get or set the Reference system types:
          * - :py:attr:`~type6`
            - Get or set the Reference system types:
          * - :py:attr:`~type7`
            - Get or set the Reference system types:
          * - :py:attr:`~type8`
            - Get or set the Reference system types:
          * - :py:attr:`~id1`
            - Get or set the ID of node or curve group (PRTYPE 3, 5, or 7).
          * - :py:attr:`~id2`
            - Get or set the ID of node or curve group (PRTYPE 3, 5, or 7).
          * - :py:attr:`~id3`
            - Get or set the ID of node or curve group (PRTYPE 3, 5, or 7).
          * - :py:attr:`~id4`
            - Get or set the ID of node or curve group (PRTYPE 3, 5, or 7).
          * - :py:attr:`~id5`
            - Get or set the ID of node or curve group (PRTYPE 3, 5, or 7).
          * - :py:attr:`~id6`
            - Get or set the ID of node or curve group (PRTYPE 3, 5, or 7).
          * - :py:attr:`~id7`
            - Get or set the ID of node or curve group (PRTYPE 3, 5, or 7).
          * - :py:attr:`~id8`
            - Get or set the ID of node or curve group (PRTYPE 3, 5, or 7).


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

    from ale_reference_system_switch import AleReferenceSystemSwitch

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Switch list ID, see *ALE_REFERENCE_SYSTEM_GROUP.
















   ..
       !! processed by numpydoc !!

.. py:property:: t1
   :type: float


   
   Get or set the Time one for switching reference system type.
















   ..
       !! processed by numpydoc !!

.. py:property:: t2
   :type: float


   
   Get or set the Time two for switching reference system type.
















   ..
       !! processed by numpydoc !!

.. py:property:: t3
   :type: float


   
   Get or set the Time three for switching reference system type.
















   ..
       !! processed by numpydoc !!

.. py:property:: t4
   :type: float


   
   Get or set the Time four for switching reference system type.
















   ..
       !! processed by numpydoc !!

.. py:property:: t5
   :type: float


   
   Get or set the Time five for switching reference system type.
















   ..
       !! processed by numpydoc !!

.. py:property:: t6
   :type: float


   
   Get or set the Time six for switching reference system type.
















   ..
       !! processed by numpydoc !!

.. py:property:: t7
   :type: float


   
   Get or set the Time seven for switching reference system type.
















   ..
       !! processed by numpydoc !!

.. py:property:: type1
   :type: int


   
   Get or set the Reference system types:
   EQ.0: Eulerian,
   EQ.1: Lagrangian,
   EQ.2: Normal ALE mesh smoothing,
   EQ.3: Prescribed motion following load curves, see *ALE_REFERENCE_SYSTEM_CURVE,
   EQ.4: Automatic mesh motion following mass weighted average velocity in ALE mesh,
   EQ.5: Automatic mesh motion following coordinate system defined by three user-defined nodes, see *ALE_REFERENCE_SYSTEM_NODE,
   EQ.7: Automatic mesh expansion in order to enclose up to twelve user-defined nodes, see *ALE_REFERENCE_SYSTEM_NODE.
















   ..
       !! processed by numpydoc !!

.. py:property:: type2
   :type: int


   
   Get or set the Reference system types:
   EQ.0: Eulerian,
   EQ.1: Lagrangian,
   EQ.2: Normal ALE mesh smoothing,
   EQ.3: Prescribed motion following load curves, see *ALE_REFERENCE_SYSTEM_CURVE,
   EQ.4: Automatic mesh motion following mass weighted average velocity in ALE mesh,
   EQ.5: Automatic mesh motion following coordinate system defined by three user-defined nodes, see *ALE_REFERENCE_SYSTEM_NODE,
   EQ.7: Automatic mesh expansion in order to enclose up to twelve user-defined nodes, see *ALE_REFERENCE_SYSTEM_NODE.
















   ..
       !! processed by numpydoc !!

.. py:property:: type3
   :type: int


   
   Get or set the Reference system types:
   EQ.0: Eulerian,
   EQ.1: Lagrangian,
   EQ.2: Normal ALE mesh smoothing,
   EQ.3: Prescribed motion following load curves, see *ALE_REFERENCE_SYSTEM_CURVE,
   EQ.4: Automatic mesh motion following mass weighted average velocity in ALE mesh,
   EQ.5: Automatic mesh motion following coordinate system defined by three user-defined nodes, see *ALE_REFERENCE_SYSTEM_NODE,
   EQ.7: Automatic mesh expansion in order to enclose up to twelve user-defined nodes, see *ALE_REFERENCE_SYSTEM_NODE.
















   ..
       !! processed by numpydoc !!

.. py:property:: type4
   :type: int


   
   Get or set the Reference system types:
   EQ.0: Eulerian,
   EQ.1: Lagrangian,
   EQ.2: Normal ALE mesh smoothing,
   EQ.3: Prescribed motion following load curves, see *ALE_REFERENCE_SYSTEM_CURVE,
   EQ.4: Automatic mesh motion following mass weighted average velocity in ALE mesh,
   EQ.5: Automatic mesh motion following coordinate system defined by three user-defined nodes, see *ALE_REFERENCE_SYSTEM_NODE,
   EQ.7: Automatic mesh expansion in order to enclose up to twelve user-defined nodes, see *ALE_REFERENCE_SYSTEM_NODE.
















   ..
       !! processed by numpydoc !!

.. py:property:: type5
   :type: int


   
   Get or set the Reference system types:
   EQ.0: Eulerian,
   EQ.1: Lagrangian,
   EQ.2: Normal ALE mesh smoothing,
   EQ.3: Prescribed motion following load curves, see *ALE_REFERENCE_SYSTEM_CURVE,
   EQ.4: Automatic mesh motion following mass weighted average velocity in ALE mesh,
   EQ.5: Automatic mesh motion following coordinate system defined by three user-defined nodes, see *ALE_REFERENCE_SYSTEM_NODE,
   EQ.7: Automatic mesh expansion in order to enclose up to twelve user-defined nodes, see *ALE_REFERENCE_SYSTEM_NODE.
















   ..
       !! processed by numpydoc !!

.. py:property:: type6
   :type: int


   
   Get or set the Reference system types:
   EQ.0: Eulerian,
   EQ.1: Lagrangian,
   EQ.2: Normal ALE mesh smoothing,
   EQ.3: Prescribed motion following load curves, see *ALE_REFERENCE_SYSTEM_CURVE,
   EQ.4: Automatic mesh motion following mass weighted average velocity in ALE mesh,
   EQ.5: Automatic mesh motion following coordinate system defined by three user-defined nodes, see *ALE_REFERENCE_SYSTEM_NODE,
   EQ.7: Automatic mesh expansion in order to enclose up to twelve user-defined nodes, see *ALE_REFERENCE_SYSTEM_NODE.
















   ..
       !! processed by numpydoc !!

.. py:property:: type7
   :type: int


   
   Get or set the Reference system types:
   EQ.0: Eulerian,
   EQ.1: Lagrangian,
   EQ.2: Normal ALE mesh smoothing,
   EQ.3: Prescribed motion following load curves, see *ALE_REFERENCE_SYSTEM_CURVE,
   EQ.4: Automatic mesh motion following mass weighted average velocity in ALE mesh,
   EQ.5: Automatic mesh motion following coordinate system defined by three user-defined nodes, see *ALE_REFERENCE_SYSTEM_NODE,
   EQ.7: Automatic mesh expansion in order to enclose up to twelve user-defined nodes, see *ALE_REFERENCE_SYSTEM_NODE.
















   ..
       !! processed by numpydoc !!

.. py:property:: type8
   :type: int


   
   Get or set the Reference system types:
   EQ.0: Eulerian,
   EQ.1: Lagrangian,
   EQ.2: Normal ALE mesh smoothing,
   EQ.3: Prescribed motion following load curves, see *ALE_REFERENCE_SYSTEM_CURVE,
   EQ.4: Automatic mesh motion following mass weighted average velocity in ALE mesh,
   EQ.5: Automatic mesh motion following coordinate system defined by three user-defined nodes, see *ALE_REFERENCE_SYSTEM_NODE,
   EQ.7: Automatic mesh expansion in order to enclose up to twelve user-defined nodes, see *ALE_REFERENCE_SYSTEM_NODE.
















   ..
       !! processed by numpydoc !!

.. py:property:: id1
   :type: Optional[int]


   
   Get or set the ID of node or curve group (PRTYPE 3, 5, or 7).
















   ..
       !! processed by numpydoc !!

.. py:property:: id2
   :type: Optional[int]


   
   Get or set the ID of node or curve group (PRTYPE 3, 5, or 7).
















   ..
       !! processed by numpydoc !!

.. py:property:: id3
   :type: Optional[int]


   
   Get or set the ID of node or curve group (PRTYPE 3, 5, or 7).
















   ..
       !! processed by numpydoc !!

.. py:property:: id4
   :type: Optional[int]


   
   Get or set the ID of node or curve group (PRTYPE 3, 5, or 7).
















   ..
       !! processed by numpydoc !!

.. py:property:: id5
   :type: Optional[int]


   
   Get or set the ID of node or curve group (PRTYPE 3, 5, or 7).
















   ..
       !! processed by numpydoc !!

.. py:property:: id6
   :type: Optional[int]


   
   Get or set the ID of node or curve group (PRTYPE 3, 5, or 7).
















   ..
       !! processed by numpydoc !!

.. py:property:: id7
   :type: Optional[int]


   
   Get or set the ID of node or curve group (PRTYPE 3, 5, or 7).
















   ..
       !! processed by numpydoc !!

.. py:property:: id8
   :type: Optional[int]


   
   Get or set the ID of node or curve group (PRTYPE 3, 5, or 7).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ALE'


.. py:attribute:: subkeyword
   :value: 'REFERENCE_SYSTEM_SWITCH'






