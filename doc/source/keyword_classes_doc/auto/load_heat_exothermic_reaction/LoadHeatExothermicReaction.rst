





:class:`LoadHeatExothermicReaction`
===================================


.. py:class:: load_heat_exothermic_reaction.LoadHeatExothermicReaction(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_HEAT_EXOTHERMIC_REACTION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadHeatExothermicReaction

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~hsid`
            - Get or set the Heat Source ID.
          * - :py:attr:`~stype`
            - Get or set the Heat Source model type:
          * - :py:attr:`~nsid`
            - Get or set the Node Set ID.
          * - :py:attr:`~bt`
            - Get or set the Birth time for application of heat source term.
          * - :py:attr:`~dt`
            - Get or set the Death time for application of heat source term.
          * - :py:attr:`~tmin`
            - Get or set the Minimum temperature before heat source activation is triggered.
          * - :py:attr:`~tmax`
            - Get or set the Maximum temperature before heat source activation is triggered.
          * - :py:attr:`~toff`
            - Get or set the Option offset for temperature used in heat source calculation.
          * - :py:attr:`~csei0`
            - Get or set the Initial concentration of Solid Electrolyte Interphase (SEI)
          * - :py:attr:`~asei`
            - Get or set the SEI-decomposition frequency factor.
          * - :py:attr:`~easei`
            - Get or set the SEI-decomposition activation energy.
          * - :py:attr:`~msei`
            - Get or set the Reaction order for CSEI.
          * - :py:attr:`~hsei`
            - Get or set the SEI-decomposition heat release.
          * - :py:attr:`~wc`
            - Get or set the Specific carbon content in jellyroll.
          * - :py:attr:`~ru`
            - Get or set the Reaction constant.
          * - :py:attr:`~cne0`
            - Get or set the Initial concentration value of NE
          * - :py:attr:`~ane`
            - Get or set the Negative Solvent frequency factor.
          * - :py:attr:`~eane`
            - Get or set the Negative Solvent Activation Energy.
          * - :py:attr:`~mne`
            - Get or set the Reaction order for CNE.
          * - :py:attr:`~hne`
            - Get or set the Negative Solvent Heat release.
          * - :py:attr:`~wcne`
            - Get or set the Specific carbon content in jellyroll.
          * - :py:attr:`~tsei0`
            - Get or set the Initial value of TSEI.
          * - :py:attr:`~tseir`
            - Get or set the Reference TSEI value.
          * - :py:attr:`~alpha0`
            - Get or set the Initial value of alfa.
          * - :py:attr:`~ape`
            - Get or set the Positive solvent frequency factor.
          * - :py:attr:`~eape`
            - Get or set the Positive solvent activation energy.
          * - :py:attr:`~mpep1`
            - Get or set the Reaction order for alfa.
          * - :py:attr:`~hpe`
            - Get or set the Positive solvent heat release.
          * - :py:attr:`~wpe`
            - Get or set the Specific positive active content.
          * - :py:attr:`~mpep2`
            - Get or set the Reaction order for (1-ALFA).
          * - :py:attr:`~ce0`
            - Get or set the Initial concentration value of CE.
          * - :py:attr:`~ae`
            - Get or set the Electrolyte decomposition frequency factor.
          * - :py:attr:`~eae`
            - Get or set the Electrolyte activation energy.
          * - :py:attr:`~me`
            - Get or set the Reaction order for CE.
          * - :py:attr:`~he`
            - Get or set the Electrolyte decomposition heat release.
          * - :py:attr:`~we`
            - Get or set the Specific Electrolyte content.


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

    from load_heat_exothermic_reaction import LoadHeatExothermicReaction

Property detail
---------------

.. py:property:: hsid
   :type: Optional[int]


   
   Get or set the Heat Source ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: stype
   :type: Optional[int]


   
   Get or set the Heat Source model type:
   EQ.0 or EQ 1 : heat source defined by NREL's 4 - Equation model.See Remark 1.
   EQ.2 : heat source defined by 1 - Equation model.See Remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsid
   :type: Optional[int]


   
   Get or set the Node Set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: bt
   :type: float


   
   Get or set the Birth time for application of heat source term.
















   ..
       !! processed by numpydoc !!

.. py:property:: dt
   :type: float


   
   Get or set the Death time for application of heat source term.
















   ..
       !! processed by numpydoc !!

.. py:property:: tmin
   :type: float


   
   Get or set the Minimum temperature before heat source activation is triggered.
















   ..
       !! processed by numpydoc !!

.. py:property:: tmax
   :type: float


   
   Get or set the Maximum temperature before heat source activation is triggered.
















   ..
       !! processed by numpydoc !!

.. py:property:: toff
   :type: float


   
   Get or set the Option offset for temperature used in heat source calculation.
















   ..
       !! processed by numpydoc !!

.. py:property:: csei0
   :type: float


   
   Get or set the Initial concentration of Solid Electrolyte Interphase (SEI)
















   ..
       !! processed by numpydoc !!

.. py:property:: asei
   :type: float


   
   Get or set the SEI-decomposition frequency factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: easei
   :type: float


   
   Get or set the SEI-decomposition activation energy.
















   ..
       !! processed by numpydoc !!

.. py:property:: msei
   :type: float


   
   Get or set the Reaction order for CSEI.
















   ..
       !! processed by numpydoc !!

.. py:property:: hsei
   :type: float


   
   Get or set the SEI-decomposition heat release.
















   ..
       !! processed by numpydoc !!

.. py:property:: wc
   :type: float


   
   Get or set the Specific carbon content in jellyroll.
















   ..
       !! processed by numpydoc !!

.. py:property:: ru
   :type: float


   
   Get or set the Reaction constant.
















   ..
       !! processed by numpydoc !!

.. py:property:: cne0
   :type: float


   
   Get or set the Initial concentration value of NE
















   ..
       !! processed by numpydoc !!

.. py:property:: ane
   :type: float


   
   Get or set the Negative Solvent frequency factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: eane
   :type: float


   
   Get or set the Negative Solvent Activation Energy.
















   ..
       !! processed by numpydoc !!

.. py:property:: mne
   :type: float


   
   Get or set the Reaction order for CNE.
















   ..
       !! processed by numpydoc !!

.. py:property:: hne
   :type: float


   
   Get or set the Negative Solvent Heat release.
















   ..
       !! processed by numpydoc !!

.. py:property:: wcne
   :type: float


   
   Get or set the Specific carbon content in jellyroll.
















   ..
       !! processed by numpydoc !!

.. py:property:: tsei0
   :type: float


   
   Get or set the Initial value of TSEI.
















   ..
       !! processed by numpydoc !!

.. py:property:: tseir
   :type: float


   
   Get or set the Reference TSEI value.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha0
   :type: float


   
   Get or set the Initial value of alfa.
















   ..
       !! processed by numpydoc !!

.. py:property:: ape
   :type: float


   
   Get or set the Positive solvent frequency factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: eape
   :type: float


   
   Get or set the Positive solvent activation energy.
















   ..
       !! processed by numpydoc !!

.. py:property:: mpep1
   :type: float


   
   Get or set the Reaction order for alfa.
















   ..
       !! processed by numpydoc !!

.. py:property:: hpe
   :type: float


   
   Get or set the Positive solvent heat release.
















   ..
       !! processed by numpydoc !!

.. py:property:: wpe
   :type: float


   
   Get or set the Specific positive active content.
















   ..
       !! processed by numpydoc !!

.. py:property:: mpep2
   :type: float


   
   Get or set the Reaction order for (1-ALFA).
















   ..
       !! processed by numpydoc !!

.. py:property:: ce0
   :type: float


   
   Get or set the Initial concentration value of CE.
















   ..
       !! processed by numpydoc !!

.. py:property:: ae
   :type: float


   
   Get or set the Electrolyte decomposition frequency factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: eae
   :type: float


   
   Get or set the Electrolyte activation energy.
















   ..
       !! processed by numpydoc !!

.. py:property:: me
   :type: float


   
   Get or set the Reaction order for CE.
















   ..
       !! processed by numpydoc !!

.. py:property:: he
   :type: float


   
   Get or set the Electrolyte decomposition heat release.
















   ..
       !! processed by numpydoc !!

.. py:property:: we
   :type: float


   
   Get or set the Specific Electrolyte content.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'HEAT_EXOTHERMIC_REACTION'






