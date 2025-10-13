





:class:`MatFhwaSoil`
====================


.. py:class:: mat_fhwa_soil.MatFhwaSoil(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_FHWA_SOIL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatFhwaSoil

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique unmber has to be chosen.
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~nplot`
            - Get or set the Plotting options
          * - :py:attr:`~spgrav`
            - Get or set the Specific Gravity of Soil used to get porosity.
          * - :py:attr:`~rhowat`
            - Get or set the Density of water in model units - used to determine air void strain (saturation).
          * - :py:attr:`~vn`
            - Get or set the Viscoplasticity parameter (strain-rate enhanced strength).
          * - :py:attr:`~gammar`
            - Get or set the Viscoplasticity parameter (strain-rate enhanced strength).
          * - :py:attr:`~intrmx`
            - Get or set the Maximum number of plasticity iterations (default 1).
          * - :py:attr:`~k`
            - Get or set the Bulk Modulus (non-zero).
          * - :py:attr:`~g`
            - Get or set the Shear modulus (non-zero).
          * - :py:attr:`~phimax`
            - Get or set the Peak Shear Strength Angle (friction angle) (radians).
          * - :py:attr:`~ahyp`
            - Get or set the Coefficient A for modified Drucker-Prager Surface.
          * - :py:attr:`~coh`
            - Get or set the Cohesion n Shear Strength at zero confinement (overburden).
          * - :py:attr:`~eccen`
            - Get or set the Eccentricity parameter for third invariant effects.
          * - :py:attr:`~an`
            - Get or set the Strain hardening percent of phimax where non-linear effects start.
          * - :py:attr:`~et`
            - Get or set the Strain Hardening Amount of non-linear effects.
          * - :py:attr:`~mcont`
            - Get or set the Moisture Content of Soil (Determines amount of air voids) (0-1.00)
          * - :py:attr:`~pwd1`
            - Get or set the Parameter for pore water effects on bulk modulus.
          * - :py:attr:`~pwksk`
            - Get or set the Skeleton bulk modulus- Pore water parameter n set to zero to eliminate effects.
          * - :py:attr:`~pwd2`
            - Get or set the Parameter for pore waterr effects on the effective pressure (confinement).
          * - :py:attr:`~phires`
            - Get or set the The minimum internal friction angle, radians (residual shear strength).
          * - :py:attr:`~dint`
            - Get or set the Volumetric Strain at Initial damage threshold, EMBED Equation.3.
          * - :py:attr:`~vdfm`
            - Get or set the Void formation energy (like fracture energy).
          * - :py:attr:`~damlev`
            - Get or set the Level of damage that will cause element deletion (0.0-1.0).
          * - :py:attr:`~epsmax`
            - Get or set the Maximum principle failure strain.
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

    from mat_fhwa_soil import MatFhwaSoil

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique unmber has to be chosen.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density.
















   ..
       !! processed by numpydoc !!

.. py:property:: nplot
   :type: int


   
   Get or set the Plotting options
   EQ.1: Effective Strain.
   EQ.2: Damage Criterion Threshold.
   EQ.3: Damage (diso).
   EQ.4: Current Damage Criterion.
   EQ.5: Not used.
   EQ.6: Current Friction Angle (phi).
















   ..
       !! processed by numpydoc !!

.. py:property:: spgrav
   :type: Optional[float]


   
   Get or set the Specific Gravity of Soil used to get porosity.
















   ..
       !! processed by numpydoc !!

.. py:property:: rhowat
   :type: float


   
   Get or set the Density of water in model units - used to determine air void strain (saturation).
















   ..
       !! processed by numpydoc !!

.. py:property:: vn
   :type: Optional[float]


   
   Get or set the Viscoplasticity parameter (strain-rate enhanced strength).
















   ..
       !! processed by numpydoc !!

.. py:property:: gammar
   :type: Optional[float]


   
   Get or set the Viscoplasticity parameter (strain-rate enhanced strength).
















   ..
       !! processed by numpydoc !!

.. py:property:: intrmx
   :type: int


   
   Get or set the Maximum number of plasticity iterations (default 1).
















   ..
       !! processed by numpydoc !!

.. py:property:: k
   :type: Optional[float]


   
   Get or set the Bulk Modulus (non-zero).
















   ..
       !! processed by numpydoc !!

.. py:property:: g
   :type: Optional[float]


   
   Get or set the Shear modulus (non-zero).
















   ..
       !! processed by numpydoc !!

.. py:property:: phimax
   :type: Optional[float]


   
   Get or set the Peak Shear Strength Angle (friction angle) (radians).
















   ..
       !! processed by numpydoc !!

.. py:property:: ahyp
   :type: Optional[float]


   
   Get or set the Coefficient A for modified Drucker-Prager Surface.
















   ..
       !! processed by numpydoc !!

.. py:property:: coh
   :type: Optional[float]


   
   Get or set the Cohesion n Shear Strength at zero confinement (overburden).
















   ..
       !! processed by numpydoc !!

.. py:property:: eccen
   :type: Optional[float]


   
   Get or set the Eccentricity parameter for third invariant effects.
















   ..
       !! processed by numpydoc !!

.. py:property:: an
   :type: Optional[float]


   
   Get or set the Strain hardening percent of phimax where non-linear effects start.
















   ..
       !! processed by numpydoc !!

.. py:property:: et
   :type: Optional[float]


   
   Get or set the Strain Hardening Amount of non-linear effects.
















   ..
       !! processed by numpydoc !!

.. py:property:: mcont
   :type: Optional[float]


   
   Get or set the Moisture Content of Soil (Determines amount of air voids) (0-1.00)
















   ..
       !! processed by numpydoc !!

.. py:property:: pwd1
   :type: Optional[float]


   
   Get or set the Parameter for pore water effects on bulk modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: pwksk
   :type: Optional[float]


   
   Get or set the Skeleton bulk modulus- Pore water parameter n set to zero to eliminate effects.
















   ..
       !! processed by numpydoc !!

.. py:property:: pwd2
   :type: Optional[float]


   
   Get or set the Parameter for pore waterr effects on the effective pressure (confinement).
















   ..
       !! processed by numpydoc !!

.. py:property:: phires
   :type: Optional[float]


   
   Get or set the The minimum internal friction angle, radians (residual shear strength).
















   ..
       !! processed by numpydoc !!

.. py:property:: dint
   :type: Optional[float]


   
   Get or set the Volumetric Strain at Initial damage threshold, EMBED Equation.3.
















   ..
       !! processed by numpydoc !!

.. py:property:: vdfm
   :type: Optional[float]


   
   Get or set the Void formation energy (like fracture energy).
















   ..
       !! processed by numpydoc !!

.. py:property:: damlev
   :type: Optional[float]


   
   Get or set the Level of damage that will cause element deletion (0.0-1.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: epsmax
   :type: Optional[float]


   
   Get or set the Maximum principle failure strain.
















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
   :value: 'MAT'


.. py:attribute:: subkeyword
   :value: 'FHWA_SOIL'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





