





:class:`MatThermalChemicalReactionOrthotropic`
==============================================


.. py:class:: mat_thermal_chemical_reaction_orthotropic.MatThermalChemicalReactionOrthotropic(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_THERMAL_CHEMICAL_REACTION_ORTHOTROPIC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatThermalChemicalReactionOrthotropic

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~tmid`
            - Get or set the Thermal material identification. A unique number or label must be specified.
          * - :py:attr:`~nchsp`
            - Get or set the Number of chemical species (maximum 8)
          * - :py:attr:`~nchrx`
            - Get or set the Number of chemical reactions (maximum 8)
          * - :py:attr:`~icend`
            - Get or set the Species number controlling reaction termination
          * - :py:attr:`~cend`
            - Get or set the Concentration for reaction termination
          * - :py:attr:`~gasc`
            - Get or set the Gas constant: 1.987 cal/(g-mole K), 8314. J/(kg-mole K).
          * - :py:attr:`~fid`
            - Get or set the Function ID for user specified chemical reaction rate equation
          * - :py:attr:`~mf`
            - Get or set the ODE solver method:
          * - :py:attr:`~aopt`
            - Get or set the Material axes definition (see *MAT_OPTIONTROPIC_for a more complete description):
          * - :py:attr:`~xp`
            - Get or set the Coordinates of point p for AOPT = 1 and 4
          * - :py:attr:`~yp`
            - Get or set the Coordinates of point p for AOPT = 1 and 4
          * - :py:attr:`~zp`
            - Get or set the Coordinates of point p for AOPT = 1 and 4
          * - :py:attr:`~a1`
            - Get or set the Components of vector a for AOPT = 2
          * - :py:attr:`~a2`
            - Get or set the Components of vector a for AOPT = 2
          * - :py:attr:`~a3`
            - Get or set the Components of vector a for AOPT = 2
          * - :py:attr:`~d1`
            - Get or set the Components of vector d for AOPT = 2,3 and 4
          * - :py:attr:`~d2`
            - Get or set the Components of vector d for AOPT = 2,3 and 4
          * - :py:attr:`~d3`
            - Get or set the Components of vector d for AOPT = 2,3 and 4
          * - :py:attr:`~rhof`
            - Get or set the Density of the filler material
          * - :py:attr:`~lccf`
            - Get or set the Load curve ID specifying the specific heat as a function of temperature for the filler material.
          * - :py:attr:`~lck1f`
            - Get or set the Load curve ID specifying the thermal conductivity as a function of temperature for the filler material
          * - :py:attr:`~lck2f`
            - Get or set the
          * - :py:attr:`~lck3f`
            - Get or set the
          * - :py:attr:`~vff`
            - Get or set the Volume fraction of the filler material. The remaining volume is occupied by the reacting chemicals
          * - :py:attr:`~rhoi`
            - Get or set the Density of the ith species
          * - :py:attr:`~lcci`
            - Get or set the Load curve ID specifying specific heat vs. temperature for the ith species.
          * - :py:attr:`~lck1i`
            - Get or set the Load curve ID specifying thermal conductivity vs. temperature for the ith species
          * - :py:attr:`~lck2i`
            - Get or set the Load curve ID specifying thermal conductivity vs. temperature for the ith species
          * - :py:attr:`~lck3i`
            - Get or set the Load curve ID specifying thermal conductivity vs. temperature for the ith species
          * - :py:attr:`~vfi`
            - Get or set the Initial fraction of the ith species relative to the other reacting chemicals
          * - :py:attr:`~mwi`
            - Get or set the Molecular weight of the ith species
          * - :py:attr:`~rci1`
            - Get or set the Reaction coefficient for species i in reaction j. Leave blank for undefined reactions
          * - :py:attr:`~rci2`
            - Get or set the Reaction coefficient for species i in reaction j. Leave blank for undefined reactions
          * - :py:attr:`~rci3`
            - Get or set the Reaction coefficient for species i in reaction j. Leave blank for undefined reactions
          * - :py:attr:`~rci4`
            - Get or set the Reaction coefficient for species i in reaction j. Leave blank for undefined reactions
          * - :py:attr:`~rci5`
            - Get or set the Reaction coefficient for species i in reaction j. Leave blank for undefined reactions
          * - :py:attr:`~rci6`
            - Get or set the Reaction coefficient for species i in reaction j. Leave blank for undefined reactions
          * - :py:attr:`~rci7`
            - Get or set the Reaction coefficient for species i in reaction j. Leave blank for undefined reactions
          * - :py:attr:`~rci8`
            - Get or set the Reaction coefficient for species i in reaction j. Leave blank for undefined reactions
          * - :py:attr:`~rxi1`
            - Get or set the Rate exponent for species i in reaction j. Leave blank for undefined reactions.
          * - :py:attr:`~rxi2`
            - Get or set the Rate exponent for species i in reaction j. Leave blank for undefined reactions.
          * - :py:attr:`~rxi3`
            - Get or set the Rate exponent for species i in reaction j. Leave blank for undefined reactions.
          * - :py:attr:`~rxi4`
            - Get or set the Rate exponent for species i in reaction j. Leave blank for undefined reactions.
          * - :py:attr:`~rxi5`
            - Get or set the Rate exponent for species i in reaction j. Leave blank for undefined reactions.
          * - :py:attr:`~rxi6`
            - Get or set the Rate exponent for species i in reaction j. Leave blank for undefined reactions.
          * - :py:attr:`~rxi7`
            - Get or set the Rate exponent for species i in reaction j. Leave blank for undefined reactions.
          * - :py:attr:`~rxi8`
            - Get or set the Rate exponent for species i in reaction j. Leave blank for undefined reactions.
          * - :py:attr:`~lczi1`
            - Get or set the Pre-exponential factor for reaction j. Enter the value as ln(Z). Leave blank for undefined reactions
          * - :py:attr:`~lczi2`
            - Get or set the Pre-exponential factor for reaction j. Enter the value as ln(Z). Leave blank for undefined reactions
          * - :py:attr:`~lczi3`
            - Get or set the Pre-exponential factor for reaction j. Enter the value as ln(Z). Leave blank for undefined reactions
          * - :py:attr:`~lczi4`
            - Get or set the Pre-exponential factor for reaction j. Enter the value as ln(Z). Leave blank for undefined reactions
          * - :py:attr:`~lczi5`
            - Get or set the Pre-exponential factor for reaction j. Enter the value as ln(Z). Leave blank for undefined reactions
          * - :py:attr:`~lczi6`
            - Get or set the Pre-exponential factor for reaction j. Enter the value as ln(Z). Leave blank for undefined reactions
          * - :py:attr:`~lczi7`
            - Get or set the Pre-exponential factor for reaction j. Enter the value as ln(Z). Leave blank for undefined reactions
          * - :py:attr:`~lczi8`
            - Get or set the Pre-exponential factor for reaction j. Enter the value as ln(Z). Leave blank for undefined reactions
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

    from mat_thermal_chemical_reaction_orthotropic import MatThermalChemicalReactionOrthotropic

Property detail
---------------

.. py:property:: tmid
   :type: Optional[int]


   
   Get or set the Thermal material identification. A unique number or label must be specified.
















   ..
       !! processed by numpydoc !!

.. py:property:: nchsp
   :type: Optional[int]


   
   Get or set the Number of chemical species (maximum 8)
















   ..
       !! processed by numpydoc !!

.. py:property:: nchrx
   :type: Optional[int]


   
   Get or set the Number of chemical reactions (maximum 8)
















   ..
       !! processed by numpydoc !!

.. py:property:: icend
   :type: Optional[int]


   
   Get or set the Species number controlling reaction termination
















   ..
       !! processed by numpydoc !!

.. py:property:: cend
   :type: Optional[float]


   
   Get or set the Concentration for reaction termination
















   ..
       !! processed by numpydoc !!

.. py:property:: gasc
   :type: Optional[float]


   
   Get or set the Gas constant: 1.987 cal/(g-mole K), 8314. J/(kg-mole K).
















   ..
       !! processed by numpydoc !!

.. py:property:: fid
   :type: Optional[int]


   
   Get or set the Function ID for user specified chemical reaction rate equation
















   ..
       !! processed by numpydoc !!

.. py:property:: mf
   :type: int


   
   Get or set the ODE solver method:
   EQ.0: default
   EQ.1: an alternative ODE solver
















   ..
       !! processed by numpydoc !!

.. py:property:: aopt
   :type: int


   
   Get or set the Material axes definition (see *MAT_OPTIONTROPIC_for a more complete description):
   EQ.0.0: Locally orthotropic with material axes by element nodes N1, N2 and N4nEQ.1.0:   Locally orthotropic with material axes determined by a point in space and global location of element center
   EQ.2.0: Globally orthotropic with material axes determined by vectors
   EQ.3.0: Locally orthotropic with first material axis orthogonal to element normal (defined by element nodes N1, N2 and N4) and to a vector d- Third material direction corresponds to element normal.
   EQ.4.0: Local orthogonal in cylindrical coordinates with the material axes determined by a vector Image, and an originating point, Image, which define the centerline axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: xp
   :type: Optional[float]


   
   Get or set the Coordinates of point p for AOPT = 1 and 4
















   ..
       !! processed by numpydoc !!

.. py:property:: yp
   :type: Optional[float]


   
   Get or set the Coordinates of point p for AOPT = 1 and 4
















   ..
       !! processed by numpydoc !!

.. py:property:: zp
   :type: Optional[float]


   
   Get or set the Coordinates of point p for AOPT = 1 and 4
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: Optional[float]


   
   Get or set the Components of vector a for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the Components of vector a for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: a3
   :type: Optional[float]


   
   Get or set the Components of vector a for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: d1
   :type: Optional[float]


   
   Get or set the Components of vector d for AOPT = 2,3 and 4
















   ..
       !! processed by numpydoc !!

.. py:property:: d2
   :type: Optional[float]


   
   Get or set the Components of vector d for AOPT = 2,3 and 4
















   ..
       !! processed by numpydoc !!

.. py:property:: d3
   :type: Optional[float]


   
   Get or set the Components of vector d for AOPT = 2,3 and 4
















   ..
       !! processed by numpydoc !!

.. py:property:: rhof
   :type: Optional[float]


   
   Get or set the Density of the filler material
















   ..
       !! processed by numpydoc !!

.. py:property:: lccf
   :type: Optional[int]


   
   Get or set the Load curve ID specifying the specific heat as a function of temperature for the filler material.
















   ..
       !! processed by numpydoc !!

.. py:property:: lck1f
   :type: Optional[int]


   
   Get or set the Load curve ID specifying the thermal conductivity as a function of temperature for the filler material
















   ..
       !! processed by numpydoc !!

.. py:property:: lck2f
   :type: Optional[float]


   
   Get or set the 
















   ..
       !! processed by numpydoc !!

.. py:property:: lck3f
   :type: Optional[float]


   
   Get or set the 
















   ..
       !! processed by numpydoc !!

.. py:property:: vff
   :type: Optional[float]


   
   Get or set the Volume fraction of the filler material. The remaining volume is occupied by the reacting chemicals
















   ..
       !! processed by numpydoc !!

.. py:property:: rhoi
   :type: Optional[float]


   
   Get or set the Density of the ith species
















   ..
       !! processed by numpydoc !!

.. py:property:: lcci
   :type: Optional[int]


   
   Get or set the Load curve ID specifying specific heat vs. temperature for the ith species.
















   ..
       !! processed by numpydoc !!

.. py:property:: lck1i
   :type: Optional[int]


   
   Get or set the Load curve ID specifying thermal conductivity vs. temperature for the ith species
















   ..
       !! processed by numpydoc !!

.. py:property:: lck2i
   :type: Optional[int]


   
   Get or set the Load curve ID specifying thermal conductivity vs. temperature for the ith species
















   ..
       !! processed by numpydoc !!

.. py:property:: lck3i
   :type: Optional[int]


   
   Get or set the Load curve ID specifying thermal conductivity vs. temperature for the ith species
















   ..
       !! processed by numpydoc !!

.. py:property:: vfi
   :type: Optional[float]


   
   Get or set the Initial fraction of the ith species relative to the other reacting chemicals
















   ..
       !! processed by numpydoc !!

.. py:property:: mwi
   :type: Optional[float]


   
   Get or set the Molecular weight of the ith species
















   ..
       !! processed by numpydoc !!

.. py:property:: rci1
   :type: Optional[float]


   
   Get or set the Reaction coefficient for species i in reaction j. Leave blank for undefined reactions
















   ..
       !! processed by numpydoc !!

.. py:property:: rci2
   :type: Optional[float]


   
   Get or set the Reaction coefficient for species i in reaction j. Leave blank for undefined reactions
















   ..
       !! processed by numpydoc !!

.. py:property:: rci3
   :type: Optional[float]


   
   Get or set the Reaction coefficient for species i in reaction j. Leave blank for undefined reactions
















   ..
       !! processed by numpydoc !!

.. py:property:: rci4
   :type: Optional[float]


   
   Get or set the Reaction coefficient for species i in reaction j. Leave blank for undefined reactions
















   ..
       !! processed by numpydoc !!

.. py:property:: rci5
   :type: Optional[float]


   
   Get or set the Reaction coefficient for species i in reaction j. Leave blank for undefined reactions
















   ..
       !! processed by numpydoc !!

.. py:property:: rci6
   :type: Optional[float]


   
   Get or set the Reaction coefficient for species i in reaction j. Leave blank for undefined reactions
















   ..
       !! processed by numpydoc !!

.. py:property:: rci7
   :type: Optional[float]


   
   Get or set the Reaction coefficient for species i in reaction j. Leave blank for undefined reactions
















   ..
       !! processed by numpydoc !!

.. py:property:: rci8
   :type: Optional[float]


   
   Get or set the Reaction coefficient for species i in reaction j. Leave blank for undefined reactions
















   ..
       !! processed by numpydoc !!

.. py:property:: rxi1
   :type: Optional[float]


   
   Get or set the Rate exponent for species i in reaction j. Leave blank for undefined reactions.
















   ..
       !! processed by numpydoc !!

.. py:property:: rxi2
   :type: Optional[float]


   
   Get or set the Rate exponent for species i in reaction j. Leave blank for undefined reactions.
















   ..
       !! processed by numpydoc !!

.. py:property:: rxi3
   :type: Optional[float]


   
   Get or set the Rate exponent for species i in reaction j. Leave blank for undefined reactions.
















   ..
       !! processed by numpydoc !!

.. py:property:: rxi4
   :type: Optional[float]


   
   Get or set the Rate exponent for species i in reaction j. Leave blank for undefined reactions.
















   ..
       !! processed by numpydoc !!

.. py:property:: rxi5
   :type: Optional[float]


   
   Get or set the Rate exponent for species i in reaction j. Leave blank for undefined reactions.
















   ..
       !! processed by numpydoc !!

.. py:property:: rxi6
   :type: Optional[float]


   
   Get or set the Rate exponent for species i in reaction j. Leave blank for undefined reactions.
















   ..
       !! processed by numpydoc !!

.. py:property:: rxi7
   :type: Optional[float]


   
   Get or set the Rate exponent for species i in reaction j. Leave blank for undefined reactions.
















   ..
       !! processed by numpydoc !!

.. py:property:: rxi8
   :type: Optional[float]


   
   Get or set the Rate exponent for species i in reaction j. Leave blank for undefined reactions.
















   ..
       !! processed by numpydoc !!

.. py:property:: lczi1
   :type: Optional[float]


   
   Get or set the Pre-exponential factor for reaction j. Enter the value as ln(Z). Leave blank for undefined reactions
















   ..
       !! processed by numpydoc !!

.. py:property:: lczi2
   :type: Optional[float]


   
   Get or set the Pre-exponential factor for reaction j. Enter the value as ln(Z). Leave blank for undefined reactions
















   ..
       !! processed by numpydoc !!

.. py:property:: lczi3
   :type: Optional[float]


   
   Get or set the Pre-exponential factor for reaction j. Enter the value as ln(Z). Leave blank for undefined reactions
















   ..
       !! processed by numpydoc !!

.. py:property:: lczi4
   :type: Optional[float]


   
   Get or set the Pre-exponential factor for reaction j. Enter the value as ln(Z). Leave blank for undefined reactions
















   ..
       !! processed by numpydoc !!

.. py:property:: lczi5
   :type: Optional[float]


   
   Get or set the Pre-exponential factor for reaction j. Enter the value as ln(Z). Leave blank for undefined reactions
















   ..
       !! processed by numpydoc !!

.. py:property:: lczi6
   :type: Optional[float]


   
   Get or set the Pre-exponential factor for reaction j. Enter the value as ln(Z). Leave blank for undefined reactions
















   ..
       !! processed by numpydoc !!

.. py:property:: lczi7
   :type: Optional[float]


   
   Get or set the Pre-exponential factor for reaction j. Enter the value as ln(Z). Leave blank for undefined reactions
















   ..
       !! processed by numpydoc !!

.. py:property:: lczi8
   :type: Optional[float]


   
   Get or set the Pre-exponential factor for reaction j. Enter the value as ln(Z). Leave blank for undefined reactions
















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
   :value: 'THERMAL_CHEMICAL_REACTION_ORTHOTROPIC'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





