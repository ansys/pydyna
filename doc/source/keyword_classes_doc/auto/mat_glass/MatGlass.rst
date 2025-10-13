





:class:`MatGlass`
=================


.. py:class:: mat_glass.MatGlass(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_GLASS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatGlass

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number or label must be specified.
          * - :py:attr:`~ro`
            - Get or set the the material density.
          * - :py:attr:`~e`
            - Get or set the Young's modulus.
          * - :py:attr:`~pr`
            - Get or set the Poisson's ratio.
          * - :py:attr:`~imod`
            - Get or set the Flag to choose degradation procedure, when critical stress is reached.
          * - :py:attr:`~ilaw`
            - Get or set the Flag to choose damage evolution law if IMOD=1.0, see Remarks.
          * - :py:attr:`~fmod`
            - Get or set the Flag to choose between failure criteria, see Remarks.
          * - :py:attr:`~ft`
            - Get or set the GT.0.0:  constant value
          * - :py:attr:`~fc`
            - Get or set the Compressive strength.
          * - :py:attr:`~at`
            - Get or set the Tensile damage evolution parameter α_t. Can be interpreted as the residual load carrying capacity ratio for tensile failure ranging from 0 to 1..
          * - :py:attr:`~bt`
            - Get or set the Tensile damage evolution parameter β_t. It controls the softening velocity for tensile failure.
          * - :py:attr:`~ac`
            - Get or set the Compressive damage evolution parameter α_t. Can be interpreted as the residual load carrying capacity ratio for compressive failure ranging from 0 to 1.
          * - :py:attr:`~bc`
            - Get or set the Compressive damage evolution parameter β_t. It controls the softening velocity for compressive failure.
          * - :py:attr:`~ftscl`
            - Get or set the Scale factor for the tensile strength: FTmod = FTSCL * FT.
          * - :py:attr:`~sfsti`
            - Get or set the Scale factor for stiffness in case of failure, e.g. SFSTI = 0.1 means
          * - :py:attr:`~sfstr`
            - Get or set the Scale factor for stress in case of failure, e.g. SFSTR = 0.1 means that
          * - :py:attr:`~crin`
            - Get or set the Flag for crack strain initialization
          * - :py:attr:`~ecrcl`
            - Get or set the Crack strain necessary to reactivate certain stress components after     crack closure..
          * - :py:attr:`~ncycr`
            - Get or set the Number of cycles in which the stress is reduced to SFSTR*failure stress.
          * - :py:attr:`~nipf`
            - Get or set the Number of failed through thickness integration points to fail all through thickness integration points.
          * - :py:attr:`~epscr`
            - Get or set the Effective critical strain to trigger element deletion. This can be useful to get rid of highly distorted elements.
          * - :py:attr:`~engcrt`
            - Get or set the Critical energy for nonlocal failure criterion; see Remark 6.
          * - :py:attr:`~radcrt`
            - Get or set the Critical radius for nonlocal failure criterion; see Remark 6.
          * - :py:attr:`~ratenl`
            - Get or set the Quasi-static strain rate threshold variable which activates a nonlocal, strain rate dependent tensile strength adaption; see Remark 7.
          * - :py:attr:`~rfiltf`
            - Get or set the Smoothing factor on the effective strain rate for the evaluation of the current tensile strength if RATENL > 0.0; see Remark 7.
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

    from mat_glass import MatGlass

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number or label must be specified.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the the material density.
















   ..
       !! processed by numpydoc !!

.. py:property:: e
   :type: Optional[float]


   
   Get or set the Young's modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: Optional[float]


   
   Get or set the Poisson's ratio.
















   ..
       !! processed by numpydoc !!

.. py:property:: imod
   :type: float


   
   Get or set the Flag to choose degradation procedure, when critical stress is reached.
   EQ.0.0: Softening in NCYCR load steps. Define SFSTI, SFSTR, and NCYCR (default).
   EQ.1.0: Damage model for softening. Define ILAW, AT, BT, AC, and BC.
   EQ.2.0 : Drucker - Prager
   EQ.10.0 : Rankine with modified compressive failure
   EQ.11.0 : Mohr - Coulomb with modified compressive failure
   EQ.12.0 : Drucker - Prager with modified compressive failure
















   ..
       !! processed by numpydoc !!

.. py:property:: ilaw
   :type: float


   
   Get or set the Flag to choose damage evolution law if IMOD=1.0, see Remarks.
   EQ.0.0: Same damage evolution for tensile and compressive failure (default).
   EQ.1.0: Different damage evolution for tensile failure and compressive failure.
















   ..
       !! processed by numpydoc !!

.. py:property:: fmod
   :type: float


   
   Get or set the Flag to choose between failure criteria, see Remarks.
   EQ.0.0: Rankine maximum stress (default),
   EQ.1.0: Mohr-Coulomb,
   EQ.2.0: Drucker-Prager.
















   ..
       !! processed by numpydoc !!

.. py:property:: ft
   :type: Optional[float]


   
   Get or set the GT.0.0:  constant value
   LT.0.0: load curve ID = |FT| , which defines tensile strength as a function of effective strain rate(RFILTF is recommended).If used with FTSCL>0, |FT| defines a curve for tensile strength vs. strain rate and FTSCL scales the strength values from that curve as long as the material is intact. If cracked, neighbors get non-scaled values from that curve. RATENL is set to zero in that case.
















   ..
       !! processed by numpydoc !!

.. py:property:: fc
   :type: Optional[float]


   
   Get or set the Compressive strength.
















   ..
       !! processed by numpydoc !!

.. py:property:: at
   :type: Optional[float]


   
   Get or set the Tensile damage evolution parameter α_t. Can be interpreted as the residual load carrying capacity ratio for tensile failure ranging from 0 to 1..
















   ..
       !! processed by numpydoc !!

.. py:property:: bt
   :type: Optional[int]


   
   Get or set the Tensile damage evolution parameter β_t. It controls the softening velocity for tensile failure.
















   ..
       !! processed by numpydoc !!

.. py:property:: ac
   :type: Optional[int]


   
   Get or set the Compressive damage evolution parameter α_t. Can be interpreted as the residual load carrying capacity ratio for compressive failure ranging from 0 to 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: bc
   :type: Optional[float]


   
   Get or set the Compressive damage evolution parameter β_t. It controls the softening velocity for compressive failure.
















   ..
       !! processed by numpydoc !!

.. py:property:: ftscl
   :type: float


   
   Get or set the Scale factor for the tensile strength: FTmod = FTSCL * FT.
   As soon as the first crack happens in the associated part, tensile strength drops to its original value, FT.
   Default value is 1.0, values >1.0 can be helpful to grasp high force peaks in impact events.
















   ..
       !! processed by numpydoc !!

.. py:property:: sfsti
   :type: Optional[float]


   
   Get or set the Scale factor for stiffness in case of failure, e.g. SFSTI = 0.1 means
   that stiffness is reduced to 10% of the stiffness at failure.
















   ..
       !! processed by numpydoc !!

.. py:property:: sfstr
   :type: Optional[float]


   
   Get or set the Scale factor for stress in case of failure, e.g. SFSTR = 0.1 means that
   stress is reduced to 10% of the stress at failure.
















   ..
       !! processed by numpydoc !!

.. py:property:: crin
   :type: float


   
   Get or set the Flag for crack strain initialization
   EQ.0.0: initial crack strain is strain at failure (default),
   EQ.1.0: initial crack strain is zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: ecrcl
   :type: Optional[float]


   
   Get or set the Crack strain necessary to reactivate certain stress components after     crack closure..
















   ..
       !! processed by numpydoc !!

.. py:property:: ncycr
   :type: Optional[float]


   
   Get or set the Number of cycles in which the stress is reduced to SFSTR*failure stress.
















   ..
       !! processed by numpydoc !!

.. py:property:: nipf
   :type: Optional[float]


   
   Get or set the Number of failed through thickness integration points to fail all through thickness integration points.
















   ..
       !! processed by numpydoc !!

.. py:property:: epscr
   :type: Optional[float]


   
   Get or set the Effective critical strain to trigger element deletion. This can be useful to get rid of highly distorted elements.
















   ..
       !! processed by numpydoc !!

.. py:property:: engcrt
   :type: Optional[float]


   
   Get or set the Critical energy for nonlocal failure criterion; see Remark 6.
















   ..
       !! processed by numpydoc !!

.. py:property:: radcrt
   :type: Optional[float]


   
   Get or set the Critical radius for nonlocal failure criterion; see Remark 6.
















   ..
       !! processed by numpydoc !!

.. py:property:: ratenl
   :type: Optional[float]


   
   Get or set the Quasi-static strain rate threshold variable which activates a nonlocal, strain rate dependent tensile strength adaption; see Remark 7.
















   ..
       !! processed by numpydoc !!

.. py:property:: rfiltf
   :type: Optional[float]


   
   Get or set the Smoothing factor on the effective strain rate for the evaluation of the current tensile strength if RATENL > 0.0; see Remark 7.
















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
   :value: 'GLASS'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





