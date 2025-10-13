





:class:`ChemistryControlTbx`
============================


.. py:class:: chemistry_control_tbx.ChemistryControlTbx(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CHEMISTRY_CONTROL_TBX keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ChemistryControlTbx

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~idchem`
            - Get or set the Identifier for this chemistry solver.
          * - :py:attr:`~usepar`
            - Get or set the Coupling flag indicating if a *STOCHASTIC_TBX_PARTICLES card is provided for this model:


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

    from chemistry_control_tbx import ChemistryControlTbx

Property detail
---------------

.. py:property:: idchem
   :type: Optional[int]


   
   Get or set the Identifier for this chemistry solver.
















   ..
       !! processed by numpydoc !!

.. py:property:: usepar
   :type: int


   
   Get or set the Coupling flag indicating if a *STOCHASTIC_TBX_PARTICLES card is provided for this model:
   EQ.1:uses a *STOCHASTIC_TBX_PARTICLES card (default).
   EQ.0: does not use such a card.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CHEMISTRY'


.. py:attribute:: subkeyword
   :value: 'CONTROL_TBX'






