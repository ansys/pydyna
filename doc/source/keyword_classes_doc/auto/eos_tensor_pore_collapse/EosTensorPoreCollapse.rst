





:class:`EosTensorPoreCollapse`
==============================


.. py:class:: eos_tensor_pore_collapse.EosTensorPoreCollapse(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EOS_TENSOR_PORE_COLLAPSE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EosTensorPoreCollapse

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~eosid`
            - Get or set the Equation of state label.
          * - :py:attr:`~nld`
            - Get or set the Virgin loading load curve ID.
          * - :py:attr:`~ncr`
            - Get or set the Completely crushed load curve ID.
          * - :py:attr:`~mu1`
            - Get or set the Excess compression required before any pores can collapse.
          * - :py:attr:`~mu2`
            - Get or set the Excess compression point where the virgin loading curve and the completely crushed curve intersect.
          * - :py:attr:`~ie0`
            - Get or set the Initial internal Energy.
          * - :py:attr:`~ec0`
            - Get or set the Initial excess Compression.


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

    from eos_tensor_pore_collapse import EosTensorPoreCollapse

Property detail
---------------

.. py:property:: eosid
   :type: Optional[int]


   
   Get or set the Equation of state label.
















   ..
       !! processed by numpydoc !!

.. py:property:: nld
   :type: Optional[int]


   
   Get or set the Virgin loading load curve ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: ncr
   :type: Optional[int]


   
   Get or set the Completely crushed load curve ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: mu1
   :type: float


   
   Get or set the Excess compression required before any pores can collapse.
















   ..
       !! processed by numpydoc !!

.. py:property:: mu2
   :type: float


   
   Get or set the Excess compression point where the virgin loading curve and the completely crushed curve intersect.
















   ..
       !! processed by numpydoc !!

.. py:property:: ie0
   :type: float


   
   Get or set the Initial internal Energy.
















   ..
       !! processed by numpydoc !!

.. py:property:: ec0
   :type: float


   
   Get or set the Initial excess Compression.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EOS'


.. py:attribute:: subkeyword
   :value: 'TENSOR_PORE_COLLAPSE'






