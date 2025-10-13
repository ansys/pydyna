





:class:`ControlFormingOnestepAutoConstraint`
============================================


.. py:class:: control_forming_onestep_auto_constraint.ControlFormingOnestepAutoConstraint(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_FORMING_ONESTEP_AUTO_CONSTRAINT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlFormingOnestepAutoConstraint

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~icon`
            - Get or set the Automatic nodal constraining option to eliminate the rigid body motion:EQ. 1: Apply.
          * - :py:attr:`~node1`
            - Get or set the Node IDs on the final part which will be used to transform the unfolded blank, so the same node ID on the final part and unfolded blank will be coincident.  Without defining these nodes, the unfolded blank will be in an arbitrary orientation with respect to the final part.
          * - :py:attr:`~node2`
            - Get or set the Node IDs on the final part which will be used to transform the unfolded blank, so the same node ID on the final part and unfolded blank will be coincident.  Without defining these nodes, the unfolded blank will be in an arbitrary orientation with respect to the final part.
          * - :py:attr:`~node3`
            - Get or set the Node IDs on the final part which will be used to transform the unfolded blank, so the same node ID on the final part and unfolded blank will be coincident.  Without defining these nodes, the unfolded blank will be in an arbitrary orientation with respect to the final part.


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

    from control_forming_onestep_auto_constraint import ControlFormingOnestepAutoConstraint

Property detail
---------------

.. py:property:: icon
   :type: Optional[int]


   
   Get or set the Automatic nodal constraining option to eliminate the rigid body motion:EQ. 1: Apply.
















   ..
       !! processed by numpydoc !!

.. py:property:: node1
   :type: Optional[int]


   
   Get or set the Node IDs on the final part which will be used to transform the unfolded blank, so the same node ID on the final part and unfolded blank will be coincident.  Without defining these nodes, the unfolded blank will be in an arbitrary orientation with respect to the final part.
















   ..
       !! processed by numpydoc !!

.. py:property:: node2
   :type: Optional[int]


   
   Get or set the Node IDs on the final part which will be used to transform the unfolded blank, so the same node ID on the final part and unfolded blank will be coincident.  Without defining these nodes, the unfolded blank will be in an arbitrary orientation with respect to the final part.
















   ..
       !! processed by numpydoc !!

.. py:property:: node3
   :type: Optional[int]


   
   Get or set the Node IDs on the final part which will be used to transform the unfolded blank, so the same node ID on the final part and unfolded blank will be coincident.  Without defining these nodes, the unfolded blank will be in an arbitrary orientation with respect to the final part.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'FORMING_ONESTEP_AUTO_CONSTRAINT'






