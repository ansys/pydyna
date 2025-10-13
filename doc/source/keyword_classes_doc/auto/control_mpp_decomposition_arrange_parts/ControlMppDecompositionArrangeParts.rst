





:class:`ControlMppDecompositionArrangeParts`
============================================


.. py:class:: control_mpp_decomposition_arrange_parts.ControlMppDecompositionArrangeParts(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_MPP_DECOMPOSITION_ARRANGE_PARTS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlMppDecompositionArrangeParts

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Part ID/Part set ID
          * - :py:attr:`~type`
            - Get or set the EQ. 0: Part ID to be distributed to all processors
          * - :py:attr:`~nproc`
            - Get or set the Used only for TYPE equal to 0 or 1. Number of processors will
          * - :py:attr:`~frstp`
            - Get or set the Used only for TYPE equal to 0 or 1. Starting MPP rank ID (rank


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

    from control_mpp_decomposition_arrange_parts import ControlMppDecompositionArrangeParts

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Part ID/Part set ID
















   ..
       !! processed by numpydoc !!

.. py:property:: type
   :type: int


   
   Get or set the EQ. 0: Part ID to be distributed to all processors
   1: Part Set ID to be distributed to all processors
   10: Part ID to be lumped into one processor
   11: Part Set ID to be lumped into one processor.
   EQ.20: Part ID to be lumped into one processor with MPP load balanced
   EQ.21: Part Set ID to be lumped into one processor with MPP load balanced
















   ..
       !! processed by numpydoc !!

.. py:property:: nproc
   :type: Optional[int]


   
   Get or set the Used only for TYPE equal to 0 or 1. Number of processors will
   be used for decomposition and this Part ID/Part set ID will be
   distributed to NPROC of processors.
















   ..
       !! processed by numpydoc !!

.. py:property:: frstp
   :type: Optional[int]


   
   Get or set the Used only for TYPE equal to 0 or 1. Starting MPP rank ID (rank
   starts from 0).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'MPP_DECOMPOSITION_ARRANGE_PARTS'






