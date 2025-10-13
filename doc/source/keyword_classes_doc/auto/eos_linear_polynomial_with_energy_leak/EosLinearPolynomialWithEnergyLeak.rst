





:class:`EosLinearPolynomialWithEnergyLeak`
==========================================


.. py:class:: eos_linear_polynomial_with_energy_leak.EosLinearPolynomialWithEnergyLeak(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EOS_LINEAR_POLYNOMIAL_WITH_ENERGY_LEAK keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EosLinearPolynomialWithEnergyLeak

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~eosid`
            - Get or set the Equation of state label.
          * - :py:attr:`~c0`
            - Get or set the
          * - :py:attr:`~c1`
            - Get or set the
          * - :py:attr:`~c2`
            - Get or set the
          * - :py:attr:`~c3`
            - Get or set the
          * - :py:attr:`~c4`
            - Get or set the
          * - :py:attr:`~c5`
            - Get or set the
          * - :py:attr:`~c6`
            - Get or set the
          * - :py:attr:`~e0`
            - Get or set the Initial internal energy.
          * - :py:attr:`~v0`
            - Get or set the Initial relative volume.
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID, which can be the ID of *DEFINE_‌CURVE,


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

    from eos_linear_polynomial_with_energy_leak import EosLinearPolynomialWithEnergyLeak

Property detail
---------------

.. py:property:: eosid
   :type: Optional[int]


   
   Get or set the Equation of state label.
















   ..
       !! processed by numpydoc !!

.. py:property:: c0
   :type: Optional[float]


   
   Get or set the 
















   ..
       !! processed by numpydoc !!

.. py:property:: c1
   :type: Optional[float]


   
   Get or set the 
















   ..
       !! processed by numpydoc !!

.. py:property:: c2
   :type: Optional[float]


   
   Get or set the 
















   ..
       !! processed by numpydoc !!

.. py:property:: c3
   :type: Optional[float]


   
   Get or set the 
















   ..
       !! processed by numpydoc !!

.. py:property:: c4
   :type: Optional[float]


   
   Get or set the 
















   ..
       !! processed by numpydoc !!

.. py:property:: c5
   :type: Optional[float]


   
   Get or set the 
















   ..
       !! processed by numpydoc !!

.. py:property:: c6
   :type: Optional[float]


   
   Get or set the 
















   ..
       !! processed by numpydoc !!

.. py:property:: e0
   :type: Optional[float]


   
   Get or set the Initial internal energy.
















   ..
       !! processed by numpydoc !!

.. py:property:: v0
   :type: Optional[float]


   
   Get or set the Initial relative volume.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID, which can be the ID of *DEFINE_‌CURVE,
   *DEFINE_‌CURVE_‌FUNCTION or *DEFINE_‌FUNCTION, defining the energy deposition rate.
   If an energy leak rate is intended, do not specify a negative ordinate in LCID,
   rather, use the constant(s) in the equation of state, e.g., set C4 to a negative value.
   If *DEFINE_‌FUNCTION is used, the input of the defined function is time.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EOS'


.. py:attribute:: subkeyword
   :value: 'LINEAR_POLYNOMIAL_WITH_ENERGY_LEAK'






