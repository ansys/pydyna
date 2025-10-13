





:class:`ControlImplicitBuckle`
==============================


.. py:class:: control_implicit_buckle.ControlImplicitBuckle(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_IMPLICIT_BUCKLE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlImplicitBuckle

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nmode`
            - Get or set the Number of buckling modes to compute:
          * - :py:attr:`~bckmth`
            - Get or set the Method used to extract buckling modes


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

    from control_implicit_buckle import ControlImplicitBuckle

Property detail
---------------

.. py:property:: nmode
   :type: int


   
   Get or set the Number of buckling modes to compute:
   EQ.0: none (default)
   EQ.n: compute n lowest buckling modes
   LT.0: curve ID = (-NEIG) used for intermittent buckling analysis
















   ..
       !! processed by numpydoc !!

.. py:property:: bckmth
   :type: int


   
   Get or set the Method used to extract buckling modes
   EQ.1: Use Block Shift and Invert Lanczos. Default of all problems
   not using *CONTROL_IMPLICIT_INERTIA_RELIEF.
   EQ.2: Use Power Method. Only valid option for problems using
   *CONTROL_IMPLICIT_INERTIA_RELIEF. Optional for other problems. See Remarks
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'IMPLICIT_BUCKLE'






