





:class:`ControlFormingOnestepFriction`
======================================


.. py:class:: control_forming_onestep_friction.ControlFormingOnestepFriction(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_FORMING_ONESTEP_FRICTION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlFormingOnestepFriction

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ndset`
            - Get or set the Node set ID along the periphery of the part, as defined by keyword *SET_NODE_LIST.
          * - :py:attr:`~bdton`
            - Get or set the Binder tonnage used to calculate friction force.
          * - :py:attr:`~frict`
            - Get or set the Coefficient of friction.


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

    from control_forming_onestep_friction import ControlFormingOnestepFriction

Property detail
---------------

.. py:property:: ndset
   :type: Optional[int]


   
   Get or set the Node set ID along the periphery of the part, as defined by keyword *SET_NODE_LIST.
















   ..
       !! processed by numpydoc !!

.. py:property:: bdton
   :type: float


   
   Get or set the Binder tonnage used to calculate friction force.
















   ..
       !! processed by numpydoc !!

.. py:property:: frict
   :type: float


   
   Get or set the Coefficient of friction.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'FORMING_ONESTEP_FRICTION'






