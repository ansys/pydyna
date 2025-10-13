





:class:`ControlCpu`
===================


.. py:class:: control_cpu.ControlCpu(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_CPU keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlCpu

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~cputim`
            - Get or set the Seconds of cpu time:
          * - :py:attr:`~iglst`
            - Get or set the Flag for outputting CPU and elapsed times in the glstat file:


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

    from control_cpu import ControlCpu

Property detail
---------------

.. py:property:: cputim
   :type: float


   
   Get or set the Seconds of cpu time:
   EQ:0.0 no cpu time limit set
















   ..
       !! processed by numpydoc !!

.. py:property:: iglst
   :type: int


   
   Get or set the Flag for outputting CPU and elapsed times in the glstat file:
   EQ.0:   no
   EQ.1 : yes
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'CPU'






