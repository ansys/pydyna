





:class:`RigidDeformableControl`
===============================


.. py:class:: rigid_deformable_control.RigidDeformableControl(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA RIGID_DEFORMABLE_CONTROL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: RigidDeformableControl

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nrbf`
            - Get or set the Flag to delete or activate nodal rigid bodies. If nodal rigid bodies or generalized, weld definitions are active in the deformable bodies that are switched to rigid, then the definitions should be deleted to avoid instabilities:
          * - :py:attr:`~ncsf`
            - Get or set the Flag to delete or activate nodal constraint set. If nodal constraint/spotweld definitions are active in the deformable bodies that are switched to rigid, then the definitions should be deleted to avoid instabilities:
          * - :py:attr:`~rwf`
            - Get or set the Flag to delete or activate rigid walls:
          * - :py:attr:`~dtmax`
            - Get or set the Maximum permitted time step size after restart.


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

    from rigid_deformable_control import RigidDeformableControl

Property detail
---------------

.. py:property:: nrbf
   :type: int


   
   Get or set the Flag to delete or activate nodal rigid bodies. If nodal rigid bodies or generalized, weld definitions are active in the deformable bodies that are switched to rigid, then the definitions should be deleted to avoid instabilities:
   EQ.0: no change,
   EQ.1: delete,
   EQ.2: activate.
















   ..
       !! processed by numpydoc !!

.. py:property:: ncsf
   :type: int


   
   Get or set the Flag to delete or activate nodal constraint set. If nodal constraint/spotweld definitions are active in the deformable bodies that are switched to rigid, then the definitions should be deleted to avoid instabilities:
   EQ.0: no change,
   EQ.1: delete,
   EQ.2: activate.
















   ..
       !! processed by numpydoc !!

.. py:property:: rwf
   :type: int


   
   Get or set the Flag to delete or activate rigid walls:
   EQ.0: no change,
   EQ.1: delete,
   EQ.2: activate.
















   ..
       !! processed by numpydoc !!

.. py:property:: dtmax
   :type: Optional[float]


   
   Get or set the Maximum permitted time step size after restart.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'RIGID'


.. py:attribute:: subkeyword
   :value: 'DEFORMABLE_CONTROL'






