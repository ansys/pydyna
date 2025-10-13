





:class:`ControlStagedConstruction`
==================================


.. py:class:: control_staged_construction.ControlStagedConstruction(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_STAGED_CONSTRUCTION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlStagedConstruction

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~tstart`
            - Get or set the Time at start of analysis (normally leave blank)
          * - :py:attr:`~stgs`
            - Get or set the Construction stage at start of analysis.
          * - :py:attr:`~stge`
            - Get or set the Construction stage at end of analysis.
          * - :py:attr:`~accel`
            - Get or set the Default acceleration for gravity loading.
          * - :py:attr:`~fact`
            - Get or set the Default stiffness and gravity factor for parts before they are added.
          * - :py:attr:`~dordel`
            - Get or set the Dormant part treatment in d3plot file.
          * - :py:attr:`~nopdel`
            - Get or set the Treatment of pressure loads on deleted elements.
          * - :py:attr:`~itime`
            - Get or set the Treatment of “Real Time” on *DEFINE_‌CONSTRUCTION_‌STAGES (see Remark 9):
          * - :py:attr:`~idynain`
            - Get or set the Flag to control output of dynain file at the end of every stage


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

    from control_staged_construction import ControlStagedConstruction

Property detail
---------------

.. py:property:: tstart
   :type: float


   
   Get or set the Time at start of analysis (normally leave blank)
















   ..
       !! processed by numpydoc !!

.. py:property:: stgs
   :type: int


   
   Get or set the Construction stage at start of analysis.
















   ..
       !! processed by numpydoc !!

.. py:property:: stge
   :type: int


   
   Get or set the Construction stage at end of analysis.
















   ..
       !! processed by numpydoc !!

.. py:property:: accel
   :type: float


   
   Get or set the Default acceleration for gravity loading.
















   ..
       !! processed by numpydoc !!

.. py:property:: fact
   :type: float


   
   Get or set the Default stiffness and gravity factor for parts before they are added.
















   ..
       !! processed by numpydoc !!

.. py:property:: dordel
   :type: int


   
   Get or set the Dormant part treatment in d3plot file.
   EQ 0: Parts not shown when dormant (flagged as  deleted ),
   EQ 1: Parts shown normally when dormant..
















   ..
       !! processed by numpydoc !!

.. py:property:: nopdel
   :type: int


   
   Get or set the Treatment of pressure loads on deleted elements.
   EQ 0: Pressure loads automatically deleted,
   EQ 1: No automatic deletion.
















   ..
       !! processed by numpydoc !!

.. py:property:: itime
   :type: int


   
   Get or set the Treatment of “Real Time” on *DEFINE_‌CONSTRUCTION_‌STAGES (see Remark 9):
   EQ.0:   Real Time is ignored.
   EQ.1:   Time in output files (d3plot, d3thdt, binout…) is converted to Real Time
















   ..
       !! processed by numpydoc !!

.. py:property:: idynain
   :type: int


   
   Get or set the Flag to control output of dynain file at the end of every stage
   EQ.0:   write dynain file
   EQ.1:   do not write dynain file .
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'STAGED_CONSTRUCTION'






