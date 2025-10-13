





:class:`CeseMatGas`
===================


.. py:class:: cese_mat_gas.CeseMatGas(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CESE_MAT_GAS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: CeseMatGas

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification.
          * - :py:attr:`~c1`
            - Get or set the First coefficient in the Sutherlands formula for viscosity, its constant for a given gas.
          * - :py:attr:`~c2`
            - Get or set the Second coefficient in the Sutherlands formula for viscosity, its constant for a given gas.
          * - :py:attr:`~pr`
            - Get or set the The Prandtl Number (used to determine the coefficient of thermal conductivity).


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

    from cese_mat_gas import CeseMatGas

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification.
















   ..
       !! processed by numpydoc !!

.. py:property:: c1
   :type: float


   
   Get or set the First coefficient in the Sutherlands formula for viscosity, its constant for a given gas.
















   ..
       !! processed by numpydoc !!

.. py:property:: c2
   :type: float


   
   Get or set the Second coefficient in the Sutherlands formula for viscosity, its constant for a given gas.
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: float


   
   Get or set the The Prandtl Number (used to determine the coefficient of thermal conductivity).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CESE'


.. py:attribute:: subkeyword
   :value: 'MAT_GAS'






