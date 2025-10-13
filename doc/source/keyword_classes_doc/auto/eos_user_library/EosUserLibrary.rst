





:class:`EosUserLibrary`
=======================


.. py:class:: eos_user_library.EosUserLibrary(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EOS_USER_LIBRARY keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EosUserLibrary

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~eosid`
            - Get or set the Equation of state ID. A unique number or label must be specified (see *PART)..
          * - :py:attr:`~sesmid`
            - Get or set the Material ID
          * - :py:attr:`~e0`
            - Get or set the Initial internal energy per unit reference volume (see the beginning of the *EOS section)
          * - :py:attr:`~v0`
            - Get or set the Initial relative volume (see the beginning of the *EOS section)


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

    from eos_user_library import EosUserLibrary

Property detail
---------------

.. py:property:: eosid
   :type: Optional[int]


   
   Get or set the Equation of state ID. A unique number or label must be specified (see *PART)..
















   ..
       !! processed by numpydoc !!

.. py:property:: sesmid
   :type: Optional[int]


   
   Get or set the Material ID
















   ..
       !! processed by numpydoc !!

.. py:property:: e0
   :type: Optional[float]


   
   Get or set the Initial internal energy per unit reference volume (see the beginning of the *EOS section)
















   ..
       !! processed by numpydoc !!

.. py:property:: v0
   :type: Optional[float]


   
   Get or set the Initial relative volume (see the beginning of the *EOS section)
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EOS'


.. py:attribute:: subkeyword
   :value: 'USER_LIBRARY'






