





:class:`DualceseControlLimiter`
===============================


.. py:class:: dualcese_control_limiter.DualceseControlLimiter(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DUALCESE_CONTROL_LIMITER keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DualceseControlLimiter

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~idlmt`
            - Get or set the Set the stability limiter option (See dual CESE theory manual):
          * - :py:attr:`~alfa`
            - Get or set the Re-weighting coefficient (See dual CESE theory manual)
          * - :py:attr:`~beta`
            - Get or set the Numerical viscosity control coefficient (See dual CESE theory manual)
          * - :py:attr:`~epsr`
            - Get or set the Stability control coefficient (See dual CESE theory manual)


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

    from dualcese_control_limiter import DualceseControlLimiter

Property detail
---------------

.. py:property:: idlmt
   :type: int


   
   Get or set the Set the stability limiter option (See dual CESE theory manual):
   EQ.0:   limiter format 1 (Re - weighting).
   EQ.1 : limiter format 2 (Relaxing).
















   ..
       !! processed by numpydoc !!

.. py:property:: alfa
   :type: float


   
   Get or set the Re-weighting coefficient (See dual CESE theory manual)
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: float


   
   Get or set the Numerical viscosity control coefficient (See dual CESE theory manual)
















   ..
       !! processed by numpydoc !!

.. py:property:: epsr
   :type: float


   
   Get or set the Stability control coefficient (See dual CESE theory manual)
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DUALCESE'


.. py:attribute:: subkeyword
   :value: 'CONTROL_LIMITER'






