





:class:`DatabaseNodalForceGroup`
================================


.. py:class:: database_nodal_force_group.DatabaseNodalForceGroup(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DATABASE_NODAL_FORCE_GROUP keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DatabaseNodalForceGroup

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nsid`
            - Get or set the Nodal set ID, see *SET_NODE.
          * - :py:attr:`~cid`
            - Get or set the Coordinate system ID for output of data in local system, see *DEFINE_COORDINATE.


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

    from database_nodal_force_group import DatabaseNodalForceGroup

Property detail
---------------

.. py:property:: nsid
   :type: Optional[int]


   
   Get or set the Nodal set ID, see *SET_NODE.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: Optional[int]


   
   Get or set the Coordinate system ID for output of data in local system, see *DEFINE_COORDINATE.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DATABASE'


.. py:attribute:: subkeyword
   :value: 'NODAL_FORCE_GROUP'






