





:class:`AleStructuredMultiMaterialGroup`
========================================


.. py:class:: ale_structured_multi_material_group.AleStructuredMultiMaterialGroup(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ALE_STRUCTURED_MULTI_MATERIAL_GROUP keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: AleStructuredMultiMaterialGroup

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ammg_name`
            - Get or set the AMMG name. Required to identify the AMMG (S-ALE fluid); Not case sensitive and need to be unique; See remark 2.
          * - :py:attr:`~mid`
            - Get or set the Material ID
          * - :py:attr:`~eosid`
            - Get or set the Equation-of-state ID.
          * - :py:attr:`~pref`
            - Get or set the Defines reference pressure of this AMMG; See remark 3


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

    from ale_structured_multi_material_group import AleStructuredMultiMaterialGroup

Property detail
---------------

.. py:property:: ammg_name
   :type: Optional[str]


   
   Get or set the AMMG name. Required to identify the AMMG (S-ALE fluid); Not case sensitive and need to be unique; See remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material ID
















   ..
       !! processed by numpydoc !!

.. py:property:: eosid
   :type: Optional[int]


   
   Get or set the Equation-of-state ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: pref
   :type: float


   
   Get or set the Defines reference pressure of this AMMG; See remark 3
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ALE'


.. py:attribute:: subkeyword
   :value: 'STRUCTURED_MULTI_MATERIAL_GROUP'






