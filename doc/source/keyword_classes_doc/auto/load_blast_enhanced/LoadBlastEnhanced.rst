





:class:`LoadBlastEnhanced`
==========================


.. py:class:: load_blast_enhanced.LoadBlastEnhanced(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_BLAST_ENHANCED keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadBlastEnhanced

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~bid`
            - Get or set the Blast ID.  A unique number must be defined for each blast source (charge).  Multiple charges may be defined, however, interaction of the waves in air is not considered.
          * - :py:attr:`~m`
            - Get or set the Equivalent mass of TNT.
          * - :py:attr:`~xbo`
            - Get or set the x-coordinate of charge center.
          * - :py:attr:`~ybo`
            - Get or set the y-coordinate of charge center.
          * - :py:attr:`~zbo`
            - Get or set the z-coordinate of charge center.
          * - :py:attr:`~tbo`
            - Get or set the Time of detonation.
          * - :py:attr:`~unit`
            - Get or set the Unit conversion flag.    EQ.1:  feet, pound-mass, seconds, psi    EQ.2:  meters, kilograms, seconds, Pascals (default)    EQ.3:  inch, dozens of slugs, seconds, psi    EQ.4:  centimeters, grams, microseconds, Megabars    EQ.5:  user conversions will be supplied(see Card 2).    EQ.6: kilogram, millimeter, millisecond, GPa    EQ.7: metric ton, millimeter, second, Mpa    EQ.8: gram, millimeter, millisecond, MPa
          * - :py:attr:`~blast`
            - Get or set the Type of blast source    EQ.1:  hemispherical surface burst - charge is located on or very near the ground surface, initial shock wave is reflected and reinforced by the ground    EQ.2:  spherical free-air burst (default) - no amplification of the initial shock wave due to interaction with the ground surface    EQ.3:  air burst - moving non-sperhical warhead    EQ.4:  air burst with ground reflection - initial shock wave impinges on the ground surface and is reinforced by the reflected wave to produce a Mach stem.
          * - :py:attr:`~cfm`
            - Get or set the Conversion factor - pounds per LS-DYNA mass unit.
          * - :py:attr:`~cfl`
            - Get or set the Conversion factor - feet per LS-DYNA length units.
          * - :py:attr:`~cft`
            - Get or set the Conversion factor - milliseconds per LS-DYNA time unit.
          * - :py:attr:`~cfp`
            - Get or set the Conversion factor - psi per LS-DYNA pressure unit.
          * - :py:attr:`~nidbo`
            - Get or set the Optional node ID representing the charge center.  If defined then XBO, YBO and XBO are ignored.
          * - :py:attr:`~death`
            - Get or set the Death time.  Blast pressures are deactivated at this time.
          * - :py:attr:`~negphs`
            - Get or set the Treament of negative phase.


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

    from load_blast_enhanced import LoadBlastEnhanced

Property detail
---------------

.. py:property:: bid
   :type: Optional[int]


   
   Get or set the Blast ID.  A unique number must be defined for each blast source (charge).  Multiple charges may be defined, however, interaction of the waves in air is not considered.
















   ..
       !! processed by numpydoc !!

.. py:property:: m
   :type: float


   
   Get or set the Equivalent mass of TNT.
















   ..
       !! processed by numpydoc !!

.. py:property:: xbo
   :type: float


   
   Get or set the x-coordinate of charge center.
















   ..
       !! processed by numpydoc !!

.. py:property:: ybo
   :type: float


   
   Get or set the y-coordinate of charge center.
















   ..
       !! processed by numpydoc !!

.. py:property:: zbo
   :type: float


   
   Get or set the z-coordinate of charge center.
















   ..
       !! processed by numpydoc !!

.. py:property:: tbo
   :type: float


   
   Get or set the Time of detonation.
















   ..
       !! processed by numpydoc !!

.. py:property:: unit
   :type: int


   
   Get or set the Unit conversion flag.    EQ.1:  feet, pound-mass, seconds, psi    EQ.2:  meters, kilograms, seconds, Pascals (default)    EQ.3:  inch, dozens of slugs, seconds, psi    EQ.4:  centimeters, grams, microseconds, Megabars    EQ.5:  user conversions will be supplied(see Card 2).    EQ.6: kilogram, millimeter, millisecond, GPa    EQ.7: metric ton, millimeter, second, Mpa    EQ.8: gram, millimeter, millisecond, MPa
















   ..
       !! processed by numpydoc !!

.. py:property:: blast
   :type: int


   
   Get or set the Type of blast source    EQ.1:  hemispherical surface burst - charge is located on or very near the ground surface, initial shock wave is reflected and reinforced by the ground    EQ.2:  spherical free-air burst (default) - no amplification of the initial shock wave due to interaction with the ground surface    EQ.3:  air burst - moving non-sperhical warhead    EQ.4:  air burst with ground reflection - initial shock wave impinges on the ground surface and is reinforced by the reflected wave to produce a Mach stem.
















   ..
       !! processed by numpydoc !!

.. py:property:: cfm
   :type: float


   
   Get or set the Conversion factor - pounds per LS-DYNA mass unit.
















   ..
       !! processed by numpydoc !!

.. py:property:: cfl
   :type: float


   
   Get or set the Conversion factor - feet per LS-DYNA length units.
















   ..
       !! processed by numpydoc !!

.. py:property:: cft
   :type: float


   
   Get or set the Conversion factor - milliseconds per LS-DYNA time unit.
















   ..
       !! processed by numpydoc !!

.. py:property:: cfp
   :type: float


   
   Get or set the Conversion factor - psi per LS-DYNA pressure unit.
















   ..
       !! processed by numpydoc !!

.. py:property:: nidbo
   :type: Optional[int]


   
   Get or set the Optional node ID representing the charge center.  If defined then XBO, YBO and XBO are ignored.
















   ..
       !! processed by numpydoc !!

.. py:property:: death
   :type: float


   
   Get or set the Death time.  Blast pressures are deactivated at this time.
















   ..
       !! processed by numpydoc !!

.. py:property:: negphs
   :type: int


   
   Get or set the Treament of negative phase.
   EQ.0:  negative dictated by the Friedlander equation.
   EQ.1:  negative phase ignored as in ConWep.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'BLAST_ENHANCED'






