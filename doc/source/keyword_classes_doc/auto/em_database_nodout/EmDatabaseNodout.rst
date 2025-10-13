





:class:`EmDatabaseNodout`
=========================


.. py:class:: em_database_nodout.EmDatabaseNodout(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_DATABASE_NODOUT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmDatabaseNodout

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
          * - :py:attr:`~nsid`
            - Get or set the Node Set ID.


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

    from em_database_nodout import EmDatabaseNodout

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

.. py:property:: nsid
   :type: Optional[int]


   
   Get or set the Node Set ID.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'DATABASE_NODOUT'






