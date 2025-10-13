





:class:`ControlImplicitStabilizationSpr`
========================================


.. py:class:: control_implicit_stabilization_spr.ControlImplicitStabilizationSpr(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_IMPLICIT_STABILIZATION_SPR keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlImplicitStabilizationSpr

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ias`
            - Get or set the Artificial Stabilization flag
          * - :py:attr:`~scale`
            - Get or set the Scale factor for artificial stabilization. Values greater than 1.0 cause less springback in the first few steps, while values less than 1.0 allow the part to springback more freely over the first few steps.
          * - :py:attr:`~tstart`
            - Get or set the Start time. (default: immediately upon entering implicit mode)
          * - :py:attr:`~tend`
            - Get or set the End time. (default: termination time)


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

    from control_implicit_stabilization_spr import ControlImplicitStabilizationSpr

Property detail
---------------

.. py:property:: ias
   :type: int


   
   Get or set the Artificial Stabilization flag
   EQ.1: active
   EQ.2: inactive (default)
















   ..
       !! processed by numpydoc !!

.. py:property:: scale
   :type: float


   
   Get or set the Scale factor for artificial stabilization. Values greater than 1.0 cause less springback in the first few steps, while values less than 1.0 allow the part to springback more freely over the first few steps.
















   ..
       !! processed by numpydoc !!

.. py:property:: tstart
   :type: float


   
   Get or set the Start time. (default: immediately upon entering implicit mode)
















   ..
       !! processed by numpydoc !!

.. py:property:: tend
   :type: float


   
   Get or set the End time. (default: termination time)
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'IMPLICIT_STABILIZATION_SPR'






