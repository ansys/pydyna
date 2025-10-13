





:class:`DatabaseSuperplasticForming`
====================================


.. py:class:: database_superplastic_forming.DatabaseSuperplasticForming(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DATABASE_SUPERPLASTIC_FORMING keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DatabaseSuperplasticForming

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~dtout`
            - Get or set the Output time interval for output to PRESSURE, CURVE1 and CURVE2 files. The PRESSURE file contains general information from the analysis and the files CURVE1 and CURVE2 contain pressure versus time from phases 1 and 2 of the analysis. The pressure file may be plotted in Phase 3 of LS-TAURUS using the SUPERPL option.


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

    from database_superplastic_forming import DatabaseSuperplasticForming

Property detail
---------------

.. py:property:: dtout
   :type: Optional[float]


   
   Get or set the Output time interval for output to PRESSURE, CURVE1 and CURVE2 files. The PRESSURE file contains general information from the analysis and the files CURVE1 and CURVE2 contain pressure versus time from phases 1 and 2 of the analysis. The pressure file may be plotted in Phase 3 of LS-TAURUS using the SUPERPL option.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DATABASE'


.. py:attribute:: subkeyword
   :value: 'SUPERPLASTIC_FORMING'






