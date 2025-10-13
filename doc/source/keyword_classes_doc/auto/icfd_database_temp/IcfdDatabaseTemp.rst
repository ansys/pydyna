





:class:`IcfdDatabaseTemp`
=========================


.. py:class:: icfd_database_temp.IcfdDatabaseTemp(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_DATABASE_TEMP keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdDatabaseTemp

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID of the surface where the average temperature and heat flux will be computed.
          * - :py:attr:`~dtout`
            - Get or set the Output frequency. Default is at every fluid timestep.


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

    from icfd_database_temp import IcfdDatabaseTemp

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID of the surface where the average temperature and heat flux will be computed.
















   ..
       !! processed by numpydoc !!

.. py:property:: dtout
   :type: Optional[float]


   
   Get or set the Output frequency. Default is at every fluid timestep.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'DATABASE_TEMP'






