





:class:`InitialFatigueDamageRatioD3Plot`
========================================


.. py:class:: initial_fatigue_damage_ratio_d3plot.InitialFatigueDamageRatioD3Plot(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INITIAL_FATIGUE_DAMAGE_RATIO_D3PLOT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InitialFatigueDamageRatioD3Plot

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~filename`
            - Get or set the Path and name of existing binary database for fatigue information.
          * - :py:attr:`~nstate`
            - Get or set the State ID in binary database (e.g. d3plot) for reading damage variables.
          * - :py:attr:`~neiphd`
            - Get or set the ID of additional integration point history variable which saves the damage for solid elements.
          * - :py:attr:`~neipsd`
            - Get or set the ID of additional integration point history variable which saves the damage for shell and thick shell elements.


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

    from initial_fatigue_damage_ratio_d3plot import InitialFatigueDamageRatioD3Plot

Property detail
---------------

.. py:property:: filename
   :type: Optional[str]


   
   Get or set the Path and name of existing binary database for fatigue information.
















   ..
       !! processed by numpydoc !!

.. py:property:: nstate
   :type: Optional[int]


   
   Get or set the State ID in binary database (e.g. d3plot) for reading damage variables.
















   ..
       !! processed by numpydoc !!

.. py:property:: neiphd
   :type: Optional[int]


   
   Get or set the ID of additional integration point history variable which saves the damage for solid elements.
















   ..
       !! processed by numpydoc !!

.. py:property:: neipsd
   :type: Optional[int]


   
   Get or set the ID of additional integration point history variable which saves the damage for shell and thick shell elements.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INITIAL'


.. py:attribute:: subkeyword
   :value: 'FATIGUE_DAMAGE_RATIO_D3PLOT'






