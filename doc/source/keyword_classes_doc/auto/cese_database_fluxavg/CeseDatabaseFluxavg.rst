





:class:`CeseDatabaseFluxavg`
============================


.. py:class:: cese_database_fluxavg.CeseDatabaseFluxavg(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CESE_DATABASE_FLUXAVG keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: CeseDatabaseFluxavg

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~outlv`
            - Get or set the Determines if the output file should be dumped
          * - :py:attr:`~dtout`
            - Get or set the Time interval to print the output. If DTOUT is equal to 0.0, then the CESE timestep will be used.
          * - :py:attr:`~elsid`
            - Get or set the Solid Elements Set ID.


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

    from cese_database_fluxavg import CeseDatabaseFluxavg

Property detail
---------------

.. py:property:: outlv
   :type: int


   
   Get or set the Determines if the output file should be dumped
   EQ.0: No output file is generated.
   EQ.1: The output file giving the average fluxes is generated.
















   ..
       !! processed by numpydoc !!

.. py:property:: dtout
   :type: float


   
   Get or set the Time interval to print the output. If DTOUT is equal to 0.0, then the CESE timestep will be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: elsid
   :type: Optional[int]


   
   Get or set the Solid Elements Set ID.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CESE'


.. py:attribute:: subkeyword
   :value: 'DATABASE_FLUXAVG'






