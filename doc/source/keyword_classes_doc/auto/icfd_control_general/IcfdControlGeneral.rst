





:class:`IcfdControlGeneral`
===========================


.. py:class:: icfd_control_general.IcfdControlGeneral(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_CONTROL_GENERAL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdControlGeneral

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~atype`
            - Get or set the Analysis type:
          * - :py:attr:`~mtype`
            - Get or set the Solving Method type:
          * - :py:attr:`~dvcl`
            - Get or set the Divergence Cleaning Flag:
          * - :py:attr:`~rdvcl`
            - Get or set the Remeshing divergence cleaning :


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

    from icfd_control_general import IcfdControlGeneral

Property detail
---------------

.. py:property:: atype
   :type: int


   
   Get or set the Analysis type:
   EQ.-1:Turns off the ICFD solver after initial keyword reading.
   EQ.0:Transient analysis
   EQ.1:Steady state analysis
















   ..
       !! processed by numpydoc !!

.. py:property:: mtype
   :type: int


   
   Get or set the Solving Method type:
   EQ.0:Fractional Step Method
   EQ.1:Monolithic solve
   EQ.2:Potential flow solve (Steady state only)
















   ..
       !! processed by numpydoc !!

.. py:property:: dvcl
   :type: int


   
   Get or set the Divergence Cleaning Flag:
   EQ.0: Default.Initialize the solution with divergence cleaning
   EQ.1 : No divergence cleaning
   EQ.2 : Initial divergence cleaning using potential flow
   EQ.3 : Divergence cleaning after each remeshing step
















   ..
       !! processed by numpydoc !!

.. py:property:: rdvcl
   :type: int


   
   Get or set the Remeshing divergence cleaning :
   EQ.0:    Default.No divergence cleaning after remesh(default)
   EQ.1 : Divergence cleaning after each remeshing step.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'CONTROL_GENERAL'






