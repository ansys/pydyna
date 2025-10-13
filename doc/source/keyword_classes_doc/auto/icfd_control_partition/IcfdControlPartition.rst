





:class:`IcfdControlPartition`
=============================


.. py:class:: icfd_control_partition.IcfdControlPartition(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_CONTROL_PARTITION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdControlPartition

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ptech`
            - Get or set the Indicates the type of partition.


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

    from icfd_control_partition import IcfdControlPartition

Property detail
---------------

.. py:property:: ptech
   :type: int


   
   Get or set the Indicates the type of partition.
   EQ. 1: the library Metis is used.
   EQ. 2: partition along the axis with higher aspect ratio.
   EQ. 3: partition along X axis.
   EQ. 4: partition along Y axis.
   EQ. 5: partition along Z axis
   .
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'CONTROL_PARTITION'






