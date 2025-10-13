





:class:`MatT06`
===============


.. py:class:: mat_t06.MatT06(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_T06 keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatT06

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
          * - :py:attr:`~rhof`
            - Get or set the Density of the filler material
          * - :py:attr:`~lccf`
            - Get or set the Load curve ID specifying the specific heat as a function of temperature for the filler material.
          * - :py:attr:`~lckf`
            - Get or set the Load curve ID specifying the thermal conductivity as a function of temperature for the filler material
          * - :py:attr:`~vff`
            - Get or set the Volume fraction of the filler material. The remaining volume is occupied by the reacting chemicals
          * - :py:attr:`~rhoi`
            - Get or set the Density of the ith species
          * - :py:attr:`~lcci`
            - Get or set the Load curve ID specifying specific heat vs. temperature for the ith species.
          * - :py:attr:`~lcki`
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
          * - :py:attr:`~e1`
            - Get or set the Activation energy for reaction j. Leave blank for undefined reactions
          * - :py:attr:`~e2`
            - Get or set the Activation energy for reaction j. Leave blank for undefined reactions
          * - :py:attr:`~e3`
            - Get or set the Activation energy for reaction j. Leave blank for undefined reactions
          * - :py:attr:`~e4`
            - Get or set the Activation energy for reaction j. Leave blank for undefined reactions
          * - :py:attr:`~e5`
            - Get or set the Activation energy for reaction j. Leave blank for undefined reactions
          * - :py:attr:`~e6`
            - Get or set the Activation energy for reaction j. Leave blank for undefined reactions
          * - :py:attr:`~e7`
            - Get or set the Activation energy for reaction j. Leave blank for undefined reactions
          * - :py:attr:`~e8`
            - Get or set the Activation energy for reaction j. Leave blank for undefined reactions
          * - :py:attr:`~q1`
            - Get or set the Heat of reaction for reaction j. Leave blank for undefined reactions
          * - :py:attr:`~q2`
            - Get or set the Heat of reaction for reaction j. Leave blank for undefined reactions
          * - :py:attr:`~q3`
            - Get or set the Heat of reaction for reaction j. Leave blank for undefined reactions
          * - :py:attr:`~q4`
            - Get or set the Heat of reaction for reaction j. Leave blank for undefined reactions
          * - :py:attr:`~q5`
            - Get or set the Heat of reaction for reaction j. Leave blank for undefined reactions
          * - :py:attr:`~q6`
            - Get or set the Heat of reaction for reaction j. Leave blank for undefined reactions
          * - :py:attr:`~q7`
            - Get or set the Heat of reaction for reaction j. Leave blank for undefined reactions
          * - :py:attr:`~q8`
            - Get or set the Heat of reaction for reaction j. Leave blank for undefined reactions
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

    from mat_t06 import MatT06

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

.. py:property:: lckf
   :type: Optional[int]


   
   Get or set the Load curve ID specifying the thermal conductivity as a function of temperature for the filler material
















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

.. py:property:: lcki
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

.. py:property:: e1
   :type: Optional[float]


   
   Get or set the Activation energy for reaction j. Leave blank for undefined reactions
















   ..
       !! processed by numpydoc !!

.. py:property:: e2
   :type: Optional[float]


   
   Get or set the Activation energy for reaction j. Leave blank for undefined reactions
















   ..
       !! processed by numpydoc !!

.. py:property:: e3
   :type: Optional[float]


   
   Get or set the Activation energy for reaction j. Leave blank for undefined reactions
















   ..
       !! processed by numpydoc !!

.. py:property:: e4
   :type: Optional[float]


   
   Get or set the Activation energy for reaction j. Leave blank for undefined reactions
















   ..
       !! processed by numpydoc !!

.. py:property:: e5
   :type: Optional[float]


   
   Get or set the Activation energy for reaction j. Leave blank for undefined reactions
















   ..
       !! processed by numpydoc !!

.. py:property:: e6
   :type: Optional[float]


   
   Get or set the Activation energy for reaction j. Leave blank for undefined reactions
















   ..
       !! processed by numpydoc !!

.. py:property:: e7
   :type: Optional[float]


   
   Get or set the Activation energy for reaction j. Leave blank for undefined reactions
















   ..
       !! processed by numpydoc !!

.. py:property:: e8
   :type: Optional[float]


   
   Get or set the Activation energy for reaction j. Leave blank for undefined reactions
















   ..
       !! processed by numpydoc !!

.. py:property:: q1
   :type: Optional[float]


   
   Get or set the Heat of reaction for reaction j. Leave blank for undefined reactions
















   ..
       !! processed by numpydoc !!

.. py:property:: q2
   :type: Optional[float]


   
   Get or set the Heat of reaction for reaction j. Leave blank for undefined reactions
















   ..
       !! processed by numpydoc !!

.. py:property:: q3
   :type: Optional[float]


   
   Get or set the Heat of reaction for reaction j. Leave blank for undefined reactions
















   ..
       !! processed by numpydoc !!

.. py:property:: q4
   :type: Optional[float]


   
   Get or set the Heat of reaction for reaction j. Leave blank for undefined reactions
















   ..
       !! processed by numpydoc !!

.. py:property:: q5
   :type: Optional[float]


   
   Get or set the Heat of reaction for reaction j. Leave blank for undefined reactions
















   ..
       !! processed by numpydoc !!

.. py:property:: q6
   :type: Optional[float]


   
   Get or set the Heat of reaction for reaction j. Leave blank for undefined reactions
















   ..
       !! processed by numpydoc !!

.. py:property:: q7
   :type: Optional[float]


   
   Get or set the Heat of reaction for reaction j. Leave blank for undefined reactions
















   ..
       !! processed by numpydoc !!

.. py:property:: q8
   :type: Optional[float]


   
   Get or set the Heat of reaction for reaction j. Leave blank for undefined reactions
















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
   :value: 'T06'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





