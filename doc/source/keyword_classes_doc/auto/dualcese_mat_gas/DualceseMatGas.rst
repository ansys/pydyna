





:class:`DualceseMatGas`
=======================


.. py:class:: dualcese_mat_gas.DualceseMatGas(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DUALCESE_MAT_GAS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DualceseMatGas

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material ID
          * - :py:attr:`~c1`
            - Get or set the Two coefficients in the Sutherland’s formula for viscosity
          * - :py:attr:`~c2`
            - Get or set the Two coefficients in the Sutherland’s formula for viscosity
          * - :py:attr:`~prnd`
            - Get or set the The Prandtl Number (used to determine the coefficient of thermal conductivity). It is approximately constant for most gases. For air at standard conditions PRND = 0.72


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

    from dualcese_mat_gas import DualceseMatGas

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material ID
















   ..
       !! processed by numpydoc !!

.. py:property:: c1
   :type: float


   
   Get or set the Two coefficients in the Sutherland’s formula for viscosity
















   ..
       !! processed by numpydoc !!

.. py:property:: c2
   :type: float


   
   Get or set the Two coefficients in the Sutherland’s formula for viscosity
















   ..
       !! processed by numpydoc !!

.. py:property:: prnd
   :type: float


   
   Get or set the The Prandtl Number (used to determine the coefficient of thermal conductivity). It is approximately constant for most gases. For air at standard conditions PRND = 0.72
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DUALCESE'


.. py:attribute:: subkeyword
   :value: 'MAT_GAS'






