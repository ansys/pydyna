





:class:`BoundarySpcNodeBirthDeath`
==================================


.. py:class:: boundary_spc_node_birth_death.BoundarySpcNodeBirthDeath(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_SPC_NODE_BIRTH_DEATH keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundarySpcNodeBirthDeath

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nid`
            - Get or set the Node ID.
          * - :py:attr:`~cid`
            - Get or set the Coordinate system ID, see *DEFINE_COORDINATE_SYSTEM.
          * - :py:attr:`~dofx`
            - Get or set the EQ.0: no translational constraint in local x-direction,
          * - :py:attr:`~dofy`
            - Get or set the EQ.0: no translational constraint in local y-direction,
          * - :py:attr:`~dofz`
            - Get or set the EQ.0: no translational constraint in local z-direction,
          * - :py:attr:`~dofrx`
            - Get or set the EQ.0: no rotational constraint about the local x-axis,
          * - :py:attr:`~dofry`
            - Get or set the EQ.0: no rotational constraint about the local y-axis,
          * - :py:attr:`~dofrz`
            - Get or set the EQ.0: no rotational constraint about the local z-axiis
          * - :py:attr:`~birth`
            - Get or set the Activation time for constraint
          * - :py:attr:`~death`
            - Get or set the Deactivation time for constraint.
          * - :py:attr:`~id`
            - Get or set the ID keyword option
          * - :py:attr:`~heading`
            - Get or set the Descriptor. We suggest using unique descriptions.


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~keyword`
            - 
          * - :py:attr:`~subkeyword`
            - 
          * - :py:attr:`~option_specs`
            - Get the card format type.






Import detail
-------------

.. code-block:: python

    from boundary_spc_node_birth_death import BoundarySpcNodeBirthDeath

Property detail
---------------

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Node ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: int


   
   Get or set the Coordinate system ID, see *DEFINE_COORDINATE_SYSTEM.
















   ..
       !! processed by numpydoc !!

.. py:property:: dofx
   :type: int


   
   Get or set the EQ.0: no translational constraint in local x-direction,
   EQ.1: translational constraint in local x-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: dofy
   :type: int


   
   Get or set the EQ.0: no translational constraint in local y-direction,
   EQ.1: translational constraint in local y-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: dofz
   :type: int


   
   Get or set the EQ.0: no translational constraint in local z-direction,
   EQ.1: translational constraint in local z-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: dofrx
   :type: int


   
   Get or set the EQ.0: no rotational constraint about the local x-axis,
   EQ.1: rotational constraint about local x-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: dofry
   :type: int


   
   Get or set the EQ.0: no rotational constraint about the local y-axis,
   EQ.1: rotational constraint about local y-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: dofrz
   :type: int


   
   Get or set the EQ.0: no rotational constraint about the local z-axiis
   EQ.1: rotational constraint about local z-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: birth
   :type: float


   
   Get or set the Activation time for constraint
















   ..
       !! processed by numpydoc !!

.. py:property:: death
   :type: float


   
   Get or set the Deactivation time for constraint.
















   ..
       !! processed by numpydoc !!

.. py:property:: id
   :type: Optional[int]


   
   Get or set the ID keyword option
















   ..
       !! processed by numpydoc !!

.. py:property:: heading
   :type: Optional[str]


   
   Get or set the Descriptor. We suggest using unique descriptions.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'SPC_NODE_BIRTH_DEATH'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





