





:class:`CeseChemistryD3Plot`
============================


.. py:class:: cese_chemistry_d3plot.CeseChemistryD3Plot(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CESE_CHEMISTRY_D3PLOT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: CeseChemistryD3Plot

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~modelid`
            - Get or set the Identifier of a Chemkin-compatible chemistry model.
          * - :py:attr:`~species`
            - Get or set the Name of a chemical species that is defined in the chemistry model identified by MODELID (see *CHEMISTRY_MODEL).


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

    from cese_chemistry_d3plot import CeseChemistryD3Plot

Property detail
---------------

.. py:property:: modelid
   :type: Optional[int]


   
   Get or set the Identifier of a Chemkin-compatible chemistry model.
















   ..
       !! processed by numpydoc !!

.. py:property:: species
   :type: Optional[str]


   
   Get or set the Name of a chemical species that is defined in the chemistry model identified by MODELID (see *CHEMISTRY_MODEL).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CESE'


.. py:attribute:: subkeyword
   :value: 'CHEMISTRY_D3PLOT'






