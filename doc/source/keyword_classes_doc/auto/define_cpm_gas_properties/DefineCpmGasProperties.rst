





:class:`DefineCpmGasProperties`
===============================


.. py:class:: define_cpm_gas_properties.DefineCpmGasProperties(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_CPM_GAS_PROPERTIES keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineCpmGasProperties

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Unique ID for this card
          * - :py:attr:`~xmm`
            - Get or set the Molar mass
          * - :py:attr:`~cp0`
            - Get or set the Coefficients of temperature dependent specific heat with constant pressure
          * - :py:attr:`~cp1`
            - Get or set the Coefficients of temperature dependent specific heat with constant pressure
          * - :py:attr:`~cp2`
            - Get or set the Coefficients of temperature dependent specific heat with constant pressure
          * - :py:attr:`~cp3`
            - Get or set the Coefficients of temperature dependent specific heat with constant pressure
          * - :py:attr:`~cp4`
            - Get or set the Coefficients of temperature dependent specific heat with constant pressure
          * - :py:attr:`~mut0`
            - Get or set the Coefficients of temperature dependent Joule-Thomson effect
          * - :py:attr:`~mut1`
            - Get or set the Coefficients of temperature dependent Joule-Thomson effect
          * - :py:attr:`~mut2`
            - Get or set the Coefficients of temperature dependent Joule-Thomson effect
          * - :py:attr:`~mut3`
            - Get or set the Coefficients of temperature dependent Joule-Thomson effect
          * - :py:attr:`~mut4`
            - Get or set the Coefficients of temperature dependent Joule-Thomson effect
          * - :py:attr:`~chm_id`
            - Get or set the Chamber ID (see Remark 1)
          * - :py:attr:`~vini`
            - Get or set the Initial volume for user defined inflator (see Remark 1):
          * - :py:attr:`~title`
            - Get or set the Additional title line


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~keyword`
            - 
          * - :py:attr:`~subkeyword`
            - 
          * - :py:attr:`~option_specs`
            - Get the card format type.






Import detail
-------------

.. code-block:: python

    from define_cpm_gas_properties import DefineCpmGasProperties

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Unique ID for this card
















   ..
       !! processed by numpydoc !!

.. py:property:: xmm
   :type: Optional[float]


   
   Get or set the Molar mass
















   ..
       !! processed by numpydoc !!

.. py:property:: cp0
   :type: Optional[float]


   
   Get or set the Coefficients of temperature dependent specific heat with constant pressure
















   ..
       !! processed by numpydoc !!

.. py:property:: cp1
   :type: Optional[float]


   
   Get or set the Coefficients of temperature dependent specific heat with constant pressure
















   ..
       !! processed by numpydoc !!

.. py:property:: cp2
   :type: Optional[float]


   
   Get or set the Coefficients of temperature dependent specific heat with constant pressure
















   ..
       !! processed by numpydoc !!

.. py:property:: cp3
   :type: Optional[float]


   
   Get or set the Coefficients of temperature dependent specific heat with constant pressure
















   ..
       !! processed by numpydoc !!

.. py:property:: cp4
   :type: Optional[float]


   
   Get or set the Coefficients of temperature dependent specific heat with constant pressure
















   ..
       !! processed by numpydoc !!

.. py:property:: mut0
   :type: Optional[float]


   
   Get or set the Coefficients of temperature dependent Joule-Thomson effect
















   ..
       !! processed by numpydoc !!

.. py:property:: mut1
   :type: Optional[float]


   
   Get or set the Coefficients of temperature dependent Joule-Thomson effect
















   ..
       !! processed by numpydoc !!

.. py:property:: mut2
   :type: Optional[float]


   
   Get or set the Coefficients of temperature dependent Joule-Thomson effect
















   ..
       !! processed by numpydoc !!

.. py:property:: mut3
   :type: Optional[float]


   
   Get or set the Coefficients of temperature dependent Joule-Thomson effect
















   ..
       !! processed by numpydoc !!

.. py:property:: mut4
   :type: Optional[float]


   
   Get or set the Coefficients of temperature dependent Joule-Thomson effect
















   ..
       !! processed by numpydoc !!

.. py:property:: chm_id
   :type: Optional[int]


   
   Get or set the Chamber ID (see Remark 1)
















   ..
       !! processed by numpydoc !!

.. py:property:: vini
   :type: float


   
   Get or set the Initial volume for user defined inflator (see Remark 1):
   EQ.0.0: user defined inflator disabled
   GT.0.0: initial volume
   LT.0.0: calculate volume based on chamber geometry.
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Additional title line
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DEFINE'


.. py:attribute:: subkeyword
   :value: 'CPM_GAS_PROPERTIES'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





