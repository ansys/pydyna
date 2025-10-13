





:class:`ControlParallel`
========================


.. py:class:: control_parallel.ControlParallel(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_PARALLEL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlParallel

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ncpu`
            - Get or set the Number of cpus used.
          * - :py:attr:`~numrhs`
            - Get or set the Number of right-hand sides allocated in memory:
          * - :py:attr:`~const`
            - Get or set the Consistency flag for parallel solution (NCPU >1).
          * - :py:attr:`~para`
            - Get or set the Flag for parallel force assembly if CONST=1.


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

    from control_parallel import ControlParallel

Property detail
---------------

.. py:property:: ncpu
   :type: int


   
   Get or set the Number of cpus used.
















   ..
       !! processed by numpydoc !!

.. py:property:: numrhs
   :type: int


   
   Get or set the Number of right-hand sides allocated in memory:
   EQ.0: same as NCPU, always recommended,
   EQ.1: allocate only one.
















   ..
       !! processed by numpydoc !!

.. py:property:: const
   :type: int


   
   Get or set the Consistency flag for parallel solution (NCPU >1).
   EQ.1: on
   EQ.2: off, for a faster solution (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: para
   :type: int


   
   Get or set the Flag for parallel force assembly if CONST=1.
   EQ.0: off
   EQ.1: on
   EQ.2: on
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'PARALLEL'






