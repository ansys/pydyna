





:class:`DatabaseRve`
====================


.. py:class:: database_rve.DatabaseRve(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DATABASE_RVE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DatabaseRve

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~dt`
            - Get or set the Time interval for the output of RVE homogenization results to the rveout file.
          * - :py:attr:`~bina`
            - Get or set the Type of the output file:


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

    from database_rve import DatabaseRve

Property detail
---------------

.. py:property:: dt
   :type: float


   
   Get or set the Time interval for the output of RVE homogenization results to the rveout file.
















   ..
       !! processed by numpydoc !!

.. py:property:: bina
   :type: int


   
   Get or set the Type of the output file:
   EQ. 0:  ASCII database file named “rveout”.
   EQ. 1 : LS - DYNA binary database.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DATABASE'


.. py:attribute:: subkeyword
   :value: 'RVE'






