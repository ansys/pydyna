





:class:`DatabaseExtentAvs`
==========================


.. py:class:: database_extent_avs.DatabaseExtentAvs(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DATABASE_EXTENT_AVS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DatabaseExtentAvs

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

    from database_extent_avs import DatabaseExtentAvs

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
   VTYPE.EQ.0: use Table 9.1,
   VTYPE.EQ.1: use Table 9.2,
   VTYPE.EQ.2: not supported,
   VTYPE.EQ.3: use Table 9.3,
   VTYPE.EQ.4: not supported.
   Please see keyword manual 9.12-9.15.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DATABASE'


.. py:attribute:: subkeyword
   :value: 'EXTENT_AVS'






