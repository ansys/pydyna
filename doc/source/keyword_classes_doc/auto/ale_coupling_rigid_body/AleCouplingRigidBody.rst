





:class:`AleCouplingRigidBody`
=============================


.. py:class:: ale_coupling_rigid_body.AleCouplingRigidBody(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ALE_COUPLING_RIGID_BODY keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: AleCouplingRigidBody

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Rigid body part ID.
          * - :py:attr:`~esid`
            - Get or set the Node set ID defining ALE boundary nodes to follow rigidRigid body motion.
          * - :py:attr:`~id`
            - Get or set the Set ID defining a part, part set or segment set ID of the ALE coupling interface.
          * - :py:attr:`~idtype`
            - Get or set the Type of set ID:
          * - :py:attr:`~ictype`
            - Get or set the Constraint type:EQ.1:   No flow through all directions.
          * - :py:attr:`~iexcle`
            - Get or set the Segment Set ID to be excluded from applying ALE essential boundary condition. For example, inlet/outlet segments.


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

    from ale_coupling_rigid_body import AleCouplingRigidBody

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Rigid body part ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: esid
   :type: Optional[int]


   
   Get or set the Node set ID defining ALE boundary nodes to follow rigidRigid body motion.
















   ..
       !! processed by numpydoc !!

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Set ID defining a part, part set or segment set ID of the ALE coupling interface.
















   ..
       !! processed by numpydoc !!

.. py:property:: idtype
   :type: int


   
   Get or set the Type of set ID:
   EQ.0:   Partpart set ID(PSID)).
   EQ.1:   Partpart ID(PID)).
   EQ.2:   Segmentsegment set ID(SGSID)).
















   ..
       !! processed by numpydoc !!

.. py:property:: ictype
   :type: int


   
   Get or set the Constraint type:EQ.1:   No flow through all directions.
   EQ.2 : No flow through normal direction. (slip condition)
















   ..
       !! processed by numpydoc !!

.. py:property:: iexcle
   :type: Optional[int]


   
   Get or set the Segment Set ID to be excluded from applying ALE essential boundary condition. For example, inlet/outlet segments.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ALE'


.. py:attribute:: subkeyword
   :value: 'COUPLING_RIGID_BODY'






