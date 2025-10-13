





:class:`ControlEnergy`
======================


.. py:class:: control_energy.ControlEnergy(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_ENERGY keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlEnergy

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~hgen`
            - Get or set the Hourglass energy calculation option.
          * - :py:attr:`~rwen`
            - Get or set the Stonewall energy dissipation option:
          * - :py:attr:`~slnten`
            - Get or set the Sliding interface energy dissipation option:
          * - :py:attr:`~rylen`
            - Get or set the Rayleigh energy dissipation option (damping energy dissipation):
          * - :py:attr:`~irgen`
            - Get or set the Initial reference geometry energy option (included in internal energy, resulting from *INITIAL_FOAM_REFERENCE_GEOMETRY):
          * - :py:attr:`~maten`
            - Get or set the Detailed material energies option. For a choice of material models (currently supported are 3, 4, 15, 19, 24, 63,81, 82, 98, 104, 105, 106, 107, 123, 124, 188, 224, 225, 240, and 251 for shell and solid elements), internal energy is additionally split into elastic, plastic and damage portions:
          * - :py:attr:`~drlen`
            - Get or set the Drilling energy calculation option, for implicit and with use of DRCPSID/DRCPRM on *CONTROL_SHELL:
          * - :py:attr:`~disen`
            - Get or set the Dissipation energy calculation option, for implicit:


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

    from control_energy import ControlEnergy

Property detail
---------------

.. py:property:: hgen
   :type: int


   
   Get or set the Hourglass energy calculation option.
   EQ.1: hourglass energy is not computed (default),
   EQ.2: hourglass energy is computed and included in the energy balance.
















   ..
       !! processed by numpydoc !!

.. py:property:: rwen
   :type: int


   
   Get or set the Stonewall energy dissipation option:
   EQ.1: energy dissipation is not computed,
   EQ.2: energy dissipation is computed and included in the energy balance (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: slnten
   :type: int


   
   Get or set the Sliding interface energy dissipation option:
   EQ.1: energy dissipation is not computed,
   EQ.2: energy dissipation is computed and included in the energy balance.
















   ..
       !! processed by numpydoc !!

.. py:property:: rylen
   :type: int


   
   Get or set the Rayleigh energy dissipation option (damping energy dissipation):
   EQ.1: energy dissipation is not computed (default),
   EQ.2: energy dissipation is computed and included in the energy balance.
















   ..
       !! processed by numpydoc !!

.. py:property:: irgen
   :type: int


   
   Get or set the Initial reference geometry energy option (included in internal energy, resulting from *INITIAL_FOAM_REFERENCE_GEOMETRY):
   EQ.1:   initial reference geometry energy is not computed,
   EQ.2:   initial reference geometry energy is computed and included in the energy balance as part of the internal energy (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: maten
   :type: int


   
   Get or set the Detailed material energies option. For a choice of material models (currently supported are 3, 4, 15, 19, 24, 63,81, 82, 98, 104, 105, 106, 107, 123, 124, 188, 224, 225, 240, and 251 for shell and solid elements), internal energy is additionally split into elastic, plastic and damage portions:
   EQ.1:   detailed material energies are not computed(default).
   EQ.2 : detailed material energies are computed and reported as mat_energy_elastic, mat_energy_plastic,and mat_energy_ damage in ASCII file glstatand matsum
















   ..
       !! processed by numpydoc !!

.. py:property:: drlen
   :type: int


   
   Get or set the Drilling energy calculation option, for implicit and with use of DRCPSID/DRCPRM on *CONTROL_SHELL:
   EQ.1:   Drilling energy is not computed(default).
   EQ.2 : Drilling energy is computed and included in the energy balance.The drilling energies are reported in the ASCII file glstat, see* DATABASE_OPTION.
















   ..
       !! processed by numpydoc !!

.. py:property:: disen
   :type: int


   
   Get or set the Dissipation energy calculation option, for implicit:
   EQ.1:   Dissipated energy is not computed(default).
   EQ.2 : Dissipated kinetic and internal energy is computed and included in the energy balance.The dissipation energies are reported in the ASCII file glstat, see* DATABASE_OPTION.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'ENERGY'






