





:class:`ControlFormingProjection`
=================================


.. py:class:: control_forming_projection.ControlFormingProjection(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_FORMING_PROJECTION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlFormingProjection

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pidb`
            - Get or set the Part ID for the blank.
          * - :py:attr:`~pidt`
            - Get or set the Part ID for the tool.
          * - :py:attr:`~gap`
            - Get or set the A distance, which defines the minimum gap required.
          * - :py:attr:`~nrbst`
            - Get or set the Specify whether the blank will move along its normal direction. If its moves along the normal of blank, then this flag also specifies the direction the normal is pointing with respect to the tool.
          * - :py:attr:`~nrtst`
            - Get or set the Normal direction of tool:


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

    from control_forming_projection import ControlFormingProjection

Property detail
---------------

.. py:property:: pidb
   :type: Optional[int]


   
   Get or set the Part ID for the blank.
















   ..
       !! processed by numpydoc !!

.. py:property:: pidt
   :type: Optional[int]


   
   Get or set the Part ID for the tool.
















   ..
       !! processed by numpydoc !!

.. py:property:: gap
   :type: Optional[float]


   
   Get or set the A distance, which defines the minimum gap required.
















   ..
       !! processed by numpydoc !!

.. py:property:: nrbst
   :type: int


   
   Get or set the Specify whether the blank will move along its normal direction. If its moves along the normal of blank, then this flag also specifies the direction the normal is pointing with respect to the tool.
   EQ.0: Move the blank’s nodes along the blank’s normal.The normal to the surface of the blank is pointing towards the tool.
   EQ.1 : Move the blank’s nodes along the blank’s normal.The normal to the surface of the blank is pointing away from the tool.
   EQ.2 : Move the blank nodes along the tool's normal direction.This case is useful for contact between a guide pinand blank.
















   ..
       !! processed by numpydoc !!

.. py:property:: nrtst
   :type: int


   
   Get or set the Normal direction of tool:
   EQ.0: the normal to the surface of the tool is pointing towards the blank,
   EQ.1: the normal to the surface of the tool is pointing away from blank.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'FORMING_PROJECTION'






