





:class:`ConstrainedRigidBodiesSet`
==================================


.. py:class:: constrained_rigid_bodies_set.ConstrainedRigidBodiesSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_RIGID_BODIES_SET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedRigidBodiesSet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pidl`
            - Get or set the Lead rigid body part ID, see *PART.
          * - :py:attr:`~pidc`
            - Get or set the Constrained rigid body part set ID, see *SET_PART (If _SET option is used, this input references to a part set ID, see *SET_PART.).
          * - :py:attr:`~iflag`
            - Get or set the This flag is meaningful if and only if the inertia properties of the lead part, PIDL, are defined in *PART_‌INERTIA.  See Remark 1.


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

    from constrained_rigid_bodies_set import ConstrainedRigidBodiesSet

Property detail
---------------

.. py:property:: pidl
   :type: Optional[int]


   
   Get or set the Lead rigid body part ID, see *PART.
















   ..
       !! processed by numpydoc !!

.. py:property:: pidc
   :type: Optional[int]


   
   Get or set the Constrained rigid body part set ID, see *SET_PART (If _SET option is used, this input references to a part set ID, see *SET_PART.).
















   ..
       !! processed by numpydoc !!

.. py:property:: iflag
   :type: Optional[int]


   
   Get or set the This flag is meaningful if and only if the inertia properties of the lead part, PIDL, are defined in *PART_‌INERTIA.  See Remark 1.
   EQ.1:   Update the center - of - gravity, the translational mass,and the inertia matrix of PIDL to reflect its merging with the constrained rigid body(PIDC).
   EQ.0 : The merged PIDC will not affect the properties defined in * PART_‌INERTIA for PIDL since the properties are assumed to already account for merged parts.If the properties are not defined in a* PART_‌INERTIA definition, the inertia properties of PIDC will be computed from its nodal masses.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'RIGID_BODIES_SET'






