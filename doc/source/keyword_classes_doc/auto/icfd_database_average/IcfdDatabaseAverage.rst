





:class:`IcfdDatabaseAverage`
============================


.. py:class:: icfd_database_average.IcfdDatabaseAverage(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_DATABASE_AVERAGE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdDatabaseAverage

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~dt`
            - Get or set the Over each DT time interval, an average of the different fluid variables will be calculated and then reset when moving to the next DT interval.


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

    from icfd_database_average import IcfdDatabaseAverage

Property detail
---------------

.. py:property:: dt
   :type: Optional[float]


   
   Get or set the Over each DT time interval, an average of the different fluid variables will be calculated and then reset when moving to the next DT interval.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'DATABASE_AVERAGE'






