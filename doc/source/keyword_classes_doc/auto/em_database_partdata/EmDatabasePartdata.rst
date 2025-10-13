





:class:`EmDatabasePartdata`
===========================


.. py:class:: em_database_partdata.EmDatabasePartdata(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_DATABASE_PARTDATA keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmDatabasePartdata

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~outlv`
            - Get or set the Determines if the output file should be dumped.
          * - :py:attr:`~dtout`
            - Get or set the Time interval to print the output. If DTOUT is equal to 0.0, then the EM timestep will be used.


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

    from em_database_partdata import EmDatabasePartdata

Property detail
---------------

.. py:property:: outlv
   :type: int


   
   Get or set the Determines if the output file should be dumped.
   EQ.0: No output file is generated.
   EQ.1: The output file is generated.
















   ..
       !! processed by numpydoc !!

.. py:property:: dtout
   :type: float


   
   Get or set the Time interval to print the output. If DTOUT is equal to 0.0, then the EM timestep will be used.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'DATABASE_PARTDATA'






