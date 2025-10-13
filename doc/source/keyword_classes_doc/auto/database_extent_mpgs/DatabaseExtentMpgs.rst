





:class:`DatabaseExtentMpgs`
===========================


.. py:class:: database_extent_mpgs.DatabaseExtentMpgs(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DATABASE_EXTENT_MPGS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DatabaseExtentMpgs

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~vtype`
            - Get or set the Variable type:
          * - :py:attr:`~comp`
            - Get or set the Component number ID. For the corresponding VTYPE, integer components from the following tables can be chosen:


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

    from database_extent_mpgs import DatabaseExtentMpgs

Property detail
---------------

.. py:property:: vtype
   :type: int


   
   Get or set the Variable type:
   EQ.0: node,
   EQ.1: brick
   EQ.2: beam,
   EQ.3: shell,
   EQ.4: thick shell.
















   ..
       !! processed by numpydoc !!

.. py:property:: comp
   :type: Optional[int]


   
   Get or set the Component number ID. For the corresponding VTYPE, integer components from the following tables can be chosen:
   VTYPE.EQ.0: Table 9.1,
   VTYPE.EQ.1: Table 9.2,
   VTYPE.EQ.2: not supported,
   VTYPE.EQ.3: Table 9.3,
   VTYPE.EQ.4: not supported.
   Please see keyword manual 9.12-9.15.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DATABASE'


.. py:attribute:: subkeyword
   :value: 'EXTENT_MPGS'






