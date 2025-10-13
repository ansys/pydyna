





:class:`EosLinearPolynomial`
============================


.. py:class:: eos_linear_polynomial.EosLinearPolynomial(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EOS_LINEAR_POLYNOMIAL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EosLinearPolynomial

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~eosid`
            - Get or set the Equation of state ID. A unique number or label must be specified (see *PART).
          * - :py:attr:`~c0`
            - Get or set the The 0th polynomial equation coefficient
          * - :py:attr:`~c1`
            - Get or set the The 1st polynomial equation coefficient (when used by itself, this is the elastic bulk modulus, meaning it cannot be used for deformation that is beyond the elastic regime).
          * - :py:attr:`~c2`
            - Get or set the The 2st polynomial equation coefficient
          * - :py:attr:`~c3`
            - Get or set the The 3rd polynomial equation coefficient
          * - :py:attr:`~c4`
            - Get or set the The 4th polynomial equation coefficient
          * - :py:attr:`~c5`
            - Get or set the The 5th polynomial equation coefficient
          * - :py:attr:`~c6`
            - Get or set the The 6th polynomial equation coefficient
          * - :py:attr:`~e0`
            - Get or set the Initial internal energy per unit reference volume (see the beginning of the *EOS section)
          * - :py:attr:`~v0`
            - Get or set the Initial relative volume (see the beginning of the *EOS section).


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

    from eos_linear_polynomial import EosLinearPolynomial

Property detail
---------------

.. py:property:: eosid
   :type: Optional[int]


   
   Get or set the Equation of state ID. A unique number or label must be specified (see *PART).
















   ..
       !! processed by numpydoc !!

.. py:property:: c0
   :type: float


   
   Get or set the The 0th polynomial equation coefficient
















   ..
       !! processed by numpydoc !!

.. py:property:: c1
   :type: float


   
   Get or set the The 1st polynomial equation coefficient (when used by itself, this is the elastic bulk modulus, meaning it cannot be used for deformation that is beyond the elastic regime).
















   ..
       !! processed by numpydoc !!

.. py:property:: c2
   :type: float


   
   Get or set the The 2st polynomial equation coefficient
















   ..
       !! processed by numpydoc !!

.. py:property:: c3
   :type: float


   
   Get or set the The 3rd polynomial equation coefficient
















   ..
       !! processed by numpydoc !!

.. py:property:: c4
   :type: float


   
   Get or set the The 4th polynomial equation coefficient
















   ..
       !! processed by numpydoc !!

.. py:property:: c5
   :type: float


   
   Get or set the The 5th polynomial equation coefficient
















   ..
       !! processed by numpydoc !!

.. py:property:: c6
   :type: float


   
   Get or set the The 6th polynomial equation coefficient
















   ..
       !! processed by numpydoc !!

.. py:property:: e0
   :type: Optional[float]


   
   Get or set the Initial internal energy per unit reference volume (see the beginning of the *EOS section)
















   ..
       !! processed by numpydoc !!

.. py:property:: v0
   :type: Optional[float]


   
   Get or set the Initial relative volume (see the beginning of the *EOS section).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EOS'


.. py:attribute:: subkeyword
   :value: 'LINEAR_POLYNOMIAL'






