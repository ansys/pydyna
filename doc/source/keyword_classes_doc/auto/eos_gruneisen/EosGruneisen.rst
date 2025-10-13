





:class:`EosGruneisen`
=====================


.. py:class:: eos_gruneisen.EosGruneisen(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EOS_GRUNEISEN keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EosGruneisen

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~eosid`
            - Get or set the Equation of state ID.
          * - :py:attr:`~c`
            - Get or set the
          * - :py:attr:`~s1`
            - Get or set the
          * - :py:attr:`~s2`
            - Get or set the
          * - :py:attr:`~s3`
            - Get or set the
          * - :py:attr:`~gamao`
            - Get or set the
          * - :py:attr:`~a`
            - Get or set the
          * - :py:attr:`~e0`
            - Get or set the E0 Initial internal energy.
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

    from eos_gruneisen import EosGruneisen

Property detail
---------------

.. py:property:: eosid
   :type: Optional[int]


   
   Get or set the Equation of state ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: c
   :type: Optional[float]


   
   Get or set the 
















   ..
       !! processed by numpydoc !!

.. py:property:: s1
   :type: Optional[float]


   
   Get or set the 
















   ..
       !! processed by numpydoc !!

.. py:property:: s2
   :type: Optional[float]


   
   Get or set the 
















   ..
       !! processed by numpydoc !!

.. py:property:: s3
   :type: Optional[float]


   
   Get or set the 
















   ..
       !! processed by numpydoc !!

.. py:property:: gamao
   :type: Optional[float]


   
   Get or set the 
















   ..
       !! processed by numpydoc !!

.. py:property:: a
   :type: Optional[float]


   
   Get or set the 
















   ..
       !! processed by numpydoc !!

.. py:property:: e0
   :type: Optional[float]


   
   Get or set the E0 Initial internal energy.
















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
   If an energy leak rate is intended, do not specify a negative ordinate in LCID, rather,
   use the constant(s) in the equation of state, e.g., set GAMMA0 or/and A to a negative value.
   If *DEFINE_‌FUNCTION is used, the input of the defined function is time.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EOS'


.. py:attribute:: subkeyword
   :value: 'GRUNEISEN'






