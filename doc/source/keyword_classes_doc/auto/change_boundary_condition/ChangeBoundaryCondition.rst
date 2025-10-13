





:class:`ChangeBoundaryCondition`
================================


.. py:class:: change_boundary_condition.ChangeBoundaryCondition(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CHANGE_BOUNDARY_CONDITION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ChangeBoundaryCondition

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nid`
            - Get or set the Nodal point ID, see also *NODE.
          * - :py:attr:`~bcc`
            - Get or set the New translational boundary condition code:


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

    from change_boundary_condition import ChangeBoundaryCondition

Property detail
---------------

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Nodal point ID, see also *NODE.
















   ..
       !! processed by numpydoc !!

.. py:property:: bcc
   :type: int


   
   Get or set the New translational boundary condition code:
   EQ.1: constrained x displacement,
   EQ.2: constrained y displacement,
   EQ.3: constrained z displacement,
   EQ.4: constrained x and y displacements,
   EQ.5: constrained y and z displacements,
   EQ.6: constrained z and x displacements,
   EQ.7: constrained x, y, and z displacements.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CHANGE'


.. py:attribute:: subkeyword
   :value: 'BOUNDARY_CONDITION'






