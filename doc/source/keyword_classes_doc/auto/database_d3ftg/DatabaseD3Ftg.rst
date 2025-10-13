





:class:`DatabaseD3Ftg`
======================


.. py:class:: database_d3ftg.DatabaseD3Ftg(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DATABASE_D3FTG keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DatabaseD3Ftg

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~binary`
            - Get or set the Flag for writing the binary plot file
          * - :py:attr:`~dt`
            - Get or set the Time interval between output states in time domain fatigue analysis (see *FATIGUE_OPTION)


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

    from database_d3ftg import DatabaseD3Ftg

Property detail
---------------

.. py:property:: binary
   :type: int


   
   Get or set the Flag for writing the binary plot file
   EQ.0: off
   EQ.1:write the binary plot file d3ftg
















   ..
       !! processed by numpydoc !!

.. py:property:: dt
   :type: float


   
   Get or set the Time interval between output states in time domain fatigue analysis (see *FATIGUE_OPTION)
   EQ.0.0: only fatigue results at the end of the analysis are output
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DATABASE'


.. py:attribute:: subkeyword
   :value: 'D3FTG'






