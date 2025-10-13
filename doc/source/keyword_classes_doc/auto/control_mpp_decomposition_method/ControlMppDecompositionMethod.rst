





:class:`ControlMppDecompositionMethod`
======================================


.. py:class:: control_mpp_decomposition_method.ControlMppDecompositionMethod(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_MPP_DECOMPOSITION_METHOD keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlMppDecompositionMethod

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~name`
            - Get or set the Name of the decomposition method to use. There are currently two options:


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

    from control_mpp_decomposition_method import ControlMppDecompositionMethod

Property detail
---------------

.. py:property:: name
   :type: Optional[str]


   
   Get or set the Name of the decomposition method to use. There are currently two options:
   RCB = recursive coordinate besection
   GREEDY=a simple heuristic method
   In almost all cases the RCB method is superior and should be used.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'MPP_DECOMPOSITION_METHOD'






