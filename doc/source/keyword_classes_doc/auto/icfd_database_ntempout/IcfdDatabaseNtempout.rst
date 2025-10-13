





:class:`IcfdDatabaseNtempout`
=============================


.. py:class:: icfd_database_ntempout.IcfdDatabaseNtempout(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_DATABASE_NTEMPOUT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdDatabaseNtempout

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nid`
            - Get or set the Internal ICFD node ID.
          * - :py:attr:`~dtout`
            - Get or set the Output frequency. If 0., the ICFD timestep will be used.


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

    from icfd_database_ntempout import IcfdDatabaseNtempout

Property detail
---------------

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Internal ICFD node ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: dtout
   :type: float


   
   Get or set the Output frequency. If 0., the ICFD timestep will be used.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'DATABASE_NTEMPOUT'






