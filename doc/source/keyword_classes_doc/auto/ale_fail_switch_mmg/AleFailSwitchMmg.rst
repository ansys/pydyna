





:class:`AleFailSwitchMmg`
=========================


.. py:class:: ale_fail_switch_mmg.AleFailSwitchMmg(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ALE_FAIL_SWITCH_MMG keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: AleFailSwitchMmg

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Switch list ID,
          * - :py:attr:`~title`
            - Get or set the Switch list title .
          * - :py:attr:`~fr_mmg`
            - Get or set the This is the AMMG-SID before the switch.  The AMMG-SID corresponds to the SID defined under the *SET_MULTI-MATERIAL_GROUP_LIST (SMMGL) card.  This SID points to one or more AMMGs (remark 1).
          * - :py:attr:`~to_mmg`
            - Get or set the This is the AMMG-SID after the switch.  The AMMG-SID corresponds to the SID defined under the *SET_MULTI-MATERIAL_GROUP_LIST card. This SID points to one or more AMMGs (remark 1).


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

    from ale_fail_switch_mmg import AleFailSwitchMmg

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Switch list ID,
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Switch list title .
















   ..
       !! processed by numpydoc !!

.. py:property:: fr_mmg
   :type: Optional[int]


   
   Get or set the This is the AMMG-SID before the switch.  The AMMG-SID corresponds to the SID defined under the *SET_MULTI-MATERIAL_GROUP_LIST (SMMGL) card.  This SID points to one or more AMMGs (remark 1).
















   ..
       !! processed by numpydoc !!

.. py:property:: to_mmg
   :type: Optional[int]


   
   Get or set the This is the AMMG-SID after the switch.  The AMMG-SID corresponds to the SID defined under the *SET_MULTI-MATERIAL_GROUP_LIST card. This SID points to one or more AMMGs (remark 1).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ALE'


.. py:attribute:: subkeyword
   :value: 'FAIL_SWITCH_MMG'






