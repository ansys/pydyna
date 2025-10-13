





:class:`DualceseEosStiffenedGas`
================================


.. py:class:: dualcese_eos_stiffened_gas.DualceseEosStiffenedGas(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DUALCESE_EOS_STIFFENED_GAS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DualceseEosStiffenedGas

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~eosid`
            - Get or set the Wquation of state ID for this dual CESE solver EOS
          * - :py:attr:`~ga`
            - Get or set the Adiabatic exponent, Must be>1.0.
          * - :py:attr:`~gb`
            - Get or set the Reference pressure, Must be>+0.0


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

    from dualcese_eos_stiffened_gas import DualceseEosStiffenedGas

Property detail
---------------

.. py:property:: eosid
   :type: Optional[int]


   
   Get or set the Wquation of state ID for this dual CESE solver EOS
















   ..
       !! processed by numpydoc !!

.. py:property:: ga
   :type: Optional[float]


   
   Get or set the Adiabatic exponent, Must be>1.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: gb
   :type: Optional[float]


   
   Get or set the Reference pressure, Must be>+0.0
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DUALCESE'


.. py:attribute:: subkeyword
   :value: 'EOS_STIFFENED_GAS'






