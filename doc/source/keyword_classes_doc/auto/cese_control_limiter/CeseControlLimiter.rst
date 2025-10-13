





:class:`CeseControlLimiter`
===========================


.. py:class:: cese_control_limiter.CeseControlLimiter(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CESE_CONTROL_LIMITER keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: CeseControlLimiter

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~idlmt`
            - Get or set the Set the stability limiter option (See CESE theory manual):
          * - :py:attr:`~alfa`
            - Get or set the Re-weighting coefficient (see <theory> Eq.4 for alfa ).
          * - :py:attr:`~beta_`
            - Get or set the Numerical viscosity control coefficient (see <theory> Eq.6 for beta ).
          * - :py:attr:`~epsr`
            - Get or set the Stability control coefficient (see <theory> Eq.5 for epsr ).


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

    from cese_control_limiter import CeseControlLimiter

Property detail
---------------

.. py:property:: idlmt
   :type: int


   
   Get or set the Set the stability limiter option (See CESE theory manual):
   EQ.0: limiter format 1 : Re-weighting
   EQ.1: limiter format 2 : Relaxing.
















   ..
       !! processed by numpydoc !!

.. py:property:: alfa
   :type: float


   
   Get or set the Re-weighting coefficient (see <theory> Eq.4 for alfa ).
















   ..
       !! processed by numpydoc !!

.. py:property:: beta_
   :type: float


   
   Get or set the Numerical viscosity control coefficient (see <theory> Eq.6 for beta ).
















   ..
       !! processed by numpydoc !!

.. py:property:: epsr
   :type: float


   
   Get or set the Stability control coefficient (see <theory> Eq.5 for epsr ).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CESE'


.. py:attribute:: subkeyword
   :value: 'CONTROL_LIMITER'






