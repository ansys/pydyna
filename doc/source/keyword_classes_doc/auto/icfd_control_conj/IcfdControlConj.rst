





:class:`IcfdControlConj`
========================


.. py:class:: icfd_control_conj.IcfdControlConj(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_CONTROL_CONJ keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdControlConj

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ctype`
            - Get or set the Indicates the thermal coupling type.
          * - :py:attr:`~tsf`
            - Get or set the Thermal Speedup Factor. This factor multiplies all thermal parameters present in the heat equation with  units  of  time  in  the  denominator  (e.g.,  thermal  conductivity,  convection  heat  transfer  coefficients).    It  is  used  to artificially  time  scale  the thermal problem. A negative value will refer to a time dependent load curve.


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

    from icfd_control_conj import IcfdControlConj

Property detail
---------------

.. py:property:: ctype
   :type: int


   
   Get or set the Indicates the thermal coupling type.
   EQ.0: Robust and accurate monolithic coupling where the temperature field are solved simultaneously between the fluid and the structure.
   EQ.1: Weak thermal coupling. The fluid passes the heat flux to the solid at the fluid-structure interface and the solid returns the temperature which is applied as a Dirichlet condition.
















   ..
       !! processed by numpydoc !!

.. py:property:: tsf
   :type: Optional[float]


   
   Get or set the Thermal Speedup Factor. This factor multiplies all thermal parameters present in the heat equation with  units  of  time  in  the  denominator  (e.g.,  thermal  conductivity,  convection  heat  transfer  coefficients).    It  is  used  to artificially  time  scale  the thermal problem. A negative value will refer to a time dependent load curve.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'CONTROL_CONJ'






