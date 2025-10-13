





:class:`ChangeRigidBodyConstraint`
==================================


.. py:class:: change_rigid_body_constraint.ChangeRigidBodyConstraint(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CHANGE_RIGID_BODY_CONSTRAINT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ChangeRigidBodyConstraint

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID, see *PART.
          * - :py:attr:`~tc`
            - Get or set the Translational constraint:
          * - :py:attr:`~rc`
            - Get or set the Rotational constraint:


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

    from change_rigid_body_constraint import ChangeRigidBodyConstraint

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID, see *PART.
















   ..
       !! processed by numpydoc !!

.. py:property:: tc
   :type: int


   
   Get or set the Translational constraint:
   EQ.0: no constraints,
   EQ.1: constrained x displacement,
   EQ.2: constrained y displacement,
   EQ.3: constrained z displacement,
   EQ.4: constrained x and y displacements,
   EQ.5: constrained y and z displacements,
   EQ.6: constrained z and x displacements,
   EQ.7: constrained x, y, and z displacements.
















   ..
       !! processed by numpydoc !!

.. py:property:: rc
   :type: int


   
   Get or set the Rotational constraint:
   EQ.0: no constraints,
   EQ.1: constrained x rotation,
   EQ.2: constrained y rotation,
   EQ.3: constrained z rotation,
   EQ.4: constrained x and y rotations,
   EQ.5: constrained y and z rotations,
   EQ.6: constrained z and x rotations,
   EQ.7: constrained x, y, and z rotations.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CHANGE'


.. py:attribute:: subkeyword
   :value: 'RIGID_BODY_CONSTRAINT'






