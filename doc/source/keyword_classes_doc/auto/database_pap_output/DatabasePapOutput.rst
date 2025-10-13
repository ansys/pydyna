





:class:`DatabasePapOutput`
==========================


.. py:class:: database_pap_output.DatabasePapOutput(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DATABASE_PAP_OUTPUT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DatabasePapOutput

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ivel`
            - Get or set the Meaning of "velocity" in d3plot and d3thdt output files
          * - :py:attr:`~iaccx`
            - Get or set the Meaning of "X-Acceleration" in d3plot and d3thdt output files
          * - :py:attr:`~iaccy`
            - Get or set the Meaning of "X-Acceleration" in d3plot and d3thdt output files
          * - :py:attr:`~iaccz`
            - Get or set the Meaning of "X-Acceleration" in d3plot and d3thdt output files
          * - :py:attr:`~ncyout`
            - Get or set the Number of cycles between outputs of calculation status to d3hsp and log file


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

    from database_pap_output import DatabasePapOutput

Property detail
---------------

.. py:property:: ivel
   :type: int


   
   Get or set the Meaning of "velocity" in d3plot and d3thdt output files
   0:  Nodal velocity vector
   1:  Seepage velocity vector
















   ..
       !! processed by numpydoc !!

.. py:property:: iaccx
   :type: int


   
   Get or set the Meaning of "X-Acceleration" in d3plot and d3thdt output files
   0:  Not written
   21: Nodal air density
   22: Nodal pore air pressure
   24: Nodal air mass
   25: Nodal air mass flow rate
















   ..
       !! processed by numpydoc !!

.. py:property:: iaccy
   :type: int


   
   Get or set the Meaning of "X-Acceleration" in d3plot and d3thdt output files
   0:  Not written
   21: Nodal air density
   22: Nodal pore air pressure
   24: Nodal air mass
   25: Nodal air mass flow rate
















   ..
       !! processed by numpydoc !!

.. py:property:: iaccz
   :type: int


   
   Get or set the Meaning of "X-Acceleration" in d3plot and d3thdt output files
   0:  Not written
   21: Nodal air density
   22: Nodal pore air pressure
   24: Nodal air mass
   25: Nodal air mass flow rate
















   ..
       !! processed by numpydoc !!

.. py:property:: ncyout
   :type: int


   
   Get or set the Number of cycles between outputs of calculation status to d3hsp and log file
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DATABASE'


.. py:attribute:: subkeyword
   :value: 'PAP_OUTPUT'






