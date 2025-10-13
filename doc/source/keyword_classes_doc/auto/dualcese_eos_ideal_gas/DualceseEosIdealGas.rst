





:class:`DualceseEosIdealGas`
============================


.. py:class:: dualcese_eos_ideal_gas.DualceseEosIdealGas(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DUALCESE_EOS_IDEAL_GAS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DualceseEosIdealGas

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~eosid`
            - Get or set the Equation of state identifier
          * - :py:attr:`~cv`
            - Get or set the Specific heat at constant volume
          * - :py:attr:`~cp`
            - Get or set the Specific heat at constant pressure
          * - :py:attr:`~e0`
            - Get or set the Represents the heat of detonation released during the reactions, or the constant rate of afterburn energy added (E0 = 0.0 is the default), e_0


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

    from dualcese_eos_ideal_gas import DualceseEosIdealGas

Property detail
---------------

.. py:property:: eosid
   :type: Optional[int]


   
   Get or set the Equation of state identifier
















   ..
       !! processed by numpydoc !!

.. py:property:: cv
   :type: float


   
   Get or set the Specific heat at constant volume
















   ..
       !! processed by numpydoc !!

.. py:property:: cp
   :type: float


   
   Get or set the Specific heat at constant pressure
















   ..
       !! processed by numpydoc !!

.. py:property:: e0
   :type: float


   
   Get or set the Represents the heat of detonation released during the reactions, or the constant rate of afterburn energy added (E0 = 0.0 is the default), e_0
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DUALCESE'


.. py:attribute:: subkeyword
   :value: 'EOS_IDEAL_GAS'






