





:class:`ControlMppDecompositionOutdecomp`
=========================================


.. py:class:: control_mpp_decomposition_outdecomp.ControlMppDecompositionOutdecomp(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_MPP_DECOMPOSITION_OUTDECOMP keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlMppDecompositionOutdecomp

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~itype`
            - Get or set the 1: database in ls-prepost format to file decomp_parts.lsprepost.


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

    from control_mpp_decomposition_outdecomp import ControlMppDecompositionOutdecomp

Property detail
---------------

.. py:property:: itype
   :type: int


   
   Get or set the 1: database in ls-prepost format to file decomp_parts.lsprepost.
   2: database in animator format to file decomp_parts.ses
   EQ.3:   database in LS-PrePost format with d3plot state number.
   This allows lsprepost to show the matching d3plot with the decomposition for
   *CONTROL_MPP_DECOMPOSITION_REDECOMPOSITION decomp_parts.lsprepost_s######
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'MPP_DECOMPOSITION_OUTDECOMP'






