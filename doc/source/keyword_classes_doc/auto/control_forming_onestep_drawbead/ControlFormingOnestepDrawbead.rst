





:class:`ControlFormingOnestepDrawbead`
======================================


.. py:class:: control_forming_onestep_drawbead.ControlFormingOnestepDrawbead(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_FORMING_ONESTEP_DRAWBEAD keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlFormingOnestepDrawbead

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ndset`
            - Get or set the Node set ID along the periphery of the part, as defined by keyword *SET_NODE_LIST.
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID that defines the material hardening curve.
          * - :py:attr:`~th`
            - Get or set the Thickness of the unformed sheet blank.
          * - :py:attr:`~percnt`
            - Get or set the Draw bead lock force fraction of the fully locked bead force.


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

    from control_forming_onestep_drawbead import ControlFormingOnestepDrawbead

Property detail
---------------

.. py:property:: ndset
   :type: Optional[int]


   
   Get or set the Node set ID along the periphery of the part, as defined by keyword *SET_NODE_LIST.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID that defines the material hardening curve.
















   ..
       !! processed by numpydoc !!

.. py:property:: th
   :type: float


   
   Get or set the Thickness of the unformed sheet blank.
















   ..
       !! processed by numpydoc !!

.. py:property:: percnt
   :type: float


   
   Get or set the Draw bead lock force fraction of the fully locked bead force.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'FORMING_ONESTEP_DRAWBEAD'






