





:class:`ChemistryComposition`
=============================


.. py:class:: chemistry_composition.ChemistryComposition(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CHEMISTRY_COMPOSITION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ChemistryComposition

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the A unique identifier among all chemistry compositions.
          * - :py:attr:`~modelid`
            - Get or set the Identifier of a Chemkin-compatible chemistry model.
          * - :py:attr:`~molfr`
            - Get or set the The mole number corresponding to the species named in the SPECIES field.
          * - :py:attr:`~species`
            - Get or set the The Chemkin-compatible name of a chemical species that is defined in the chemistry model identified by MODELID (see *CHEMISTRY_MODEL).


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

    from chemistry_composition import ChemistryComposition

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the A unique identifier among all chemistry compositions.
















   ..
       !! processed by numpydoc !!

.. py:property:: modelid
   :type: Optional[int]


   
   Get or set the Identifier of a Chemkin-compatible chemistry model.
















   ..
       !! processed by numpydoc !!

.. py:property:: molfr
   :type: Optional[float]


   
   Get or set the The mole number corresponding to the species named in the SPECIES field.
















   ..
       !! processed by numpydoc !!

.. py:property:: species
   :type: Optional[str]


   
   Get or set the The Chemkin-compatible name of a chemical species that is defined in the chemistry model identified by MODELID (see *CHEMISTRY_MODEL).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CHEMISTRY'


.. py:attribute:: subkeyword
   :value: 'COMPOSITION'






