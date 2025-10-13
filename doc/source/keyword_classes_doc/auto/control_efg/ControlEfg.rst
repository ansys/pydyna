





:class:`ControlEfg`
===================


.. py:class:: control_efg.ControlEfg(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_EFG keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlEfg

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~isplane`
            - Get or set the Optional choice for the mesh-free kernal functions:
          * - :py:attr:`~idila`
            - Get or set the Optional choice for the normalized dilation parameter:
          * - :py:attr:`~inint`
            - Get or set the This is the factor needed for the estimation of maximum workspace (MWSPAC) that can be used during the initialization phase.
          * - :py:attr:`~imlm`
            - Get or set the Optional choice for the matrix operation, linear solving and memory usage:
          * - :py:attr:`~etol`
            - Get or set the Error tolerance in the IMLM. When IMLM=2 is used, ININT in card one becomes redundant. IMLM=2 is recommended.


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

    from control_efg import ControlEfg

Property detail
---------------

.. py:property:: isplane
   :type: int


   
   Get or set the Optional choice for the mesh-free kernal functions:
   EQ.0: Cubic spline function (default)
   EQ.1: Quadratic spline function
















   ..
       !! processed by numpydoc !!

.. py:property:: idila
   :type: int


   
   Get or set the Optional choice for the normalized dilation parameter:
   .EQ.0: use individual dilation parameter for each mesh-free particle (default)
   EQ.1: use the largest dilation parameter. The EFG processor will calculate the largest dilation parameter for each mesh-free part. Every particle associated with that mesh-free part will be assigned to the same value for the mesh-free computation.
















   ..
       !! processed by numpydoc !!

.. py:property:: inint
   :type: int


   
   Get or set the This is the factor needed for the estimation of maximum workspace (MWSPAC) that can be used during the initialization phase.
















   ..
       !! processed by numpydoc !!

.. py:property:: imlm
   :type: int


   
   Get or set the Optional choice for the matrix operation, linear solving and memory usage:
   EQ.1: Original BCSLIB-EXT solvers.
   EQ.2: EFGPACK
















   ..
       !! processed by numpydoc !!

.. py:property:: etol
   :type: float


   
   Get or set the Error tolerance in the IMLM. When IMLM=2 is used, ININT in card one becomes redundant. IMLM=2 is recommended.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'EFG'






