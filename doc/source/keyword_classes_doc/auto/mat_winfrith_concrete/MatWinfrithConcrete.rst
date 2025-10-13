





:class:`MatWinfrithConcrete`
============================


.. py:class:: mat_winfrith_concrete.MatWinfrithConcrete(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_WINFRITH_CONCRETE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatWinfrithConcrete

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number has to be used.
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~tm`
            - Get or set the Tangent modulus (concrete).
          * - :py:attr:`~pr`
            - Get or set the Poisson's ratio.
          * - :py:attr:`~ucs`
            - Get or set the Uniaxial compressive strength.
          * - :py:attr:`~uts`
            - Get or set the Uniaxial tensile strength.
          * - :py:attr:`~fe`
            - Get or set the Depends on value of RATE below:
          * - :py:attr:`~asize`
            - Get or set the Aggregate size (radius).
          * - :py:attr:`~e`
            - Get or set the Young's modulus of rebar.
          * - :py:attr:`~ys`
            - Get or set the Yield stress of rebar.
          * - :py:attr:`~eh`
            - Get or set the Hardening modulus of rebar.
          * - :py:attr:`~uelong`
            - Get or set the Ultimate elongation before rebar fails.
          * - :py:attr:`~rate`
            - Get or set the Rate effects:
          * - :py:attr:`~conm`
            - Get or set the GT.0: Factor to convert model mass units to kg,
          * - :py:attr:`~conl`
            - Get or set the If CONM.GT.0, factor to convert model length units to meters; otherwise CONL is ignored.
          * - :py:attr:`~cont`
            - Get or set the If CONM.GT.0, factor to convert model time units to seconds; otherwise CONT is ignored.
          * - :py:attr:`~eps1`
            - Get or set the First value of volumetric strain-pressure curve (natural logarithmic values).
          * - :py:attr:`~eps2`
            - Get or set the Second value of volumetric strain-pressure curve (natural logarithmic values).
          * - :py:attr:`~eps3`
            - Get or set the Third value of volumetric strain-pressure curve (natural logarithmic values).
          * - :py:attr:`~eps4`
            - Get or set the Fourth value of volumetric strain-pressure curve (natural logarithmic values).
          * - :py:attr:`~eps5`
            - Get or set the Fifth value of volumetric strain-pressure curve (natural logarithmic values).
          * - :py:attr:`~eps6`
            - Get or set the Sixth value of volumetric strain-pressure curve (natural logarithmic values).
          * - :py:attr:`~eps7`
            - Get or set the Seventh value of volumetric strain-pressure curve (natural logarithmic values).
          * - :py:attr:`~eps8`
            - Get or set the Eight value of volumetric strain-pressure curve (natural logarithmic values).
          * - :py:attr:`~p1`
            - Get or set the Pressures corresponding to first volumetric strain value.
          * - :py:attr:`~p2`
            - Get or set the Pressures corresponding to second volumetric strain value.
          * - :py:attr:`~p3`
            - Get or set the Pressures corresponding to third volumetric strain value.
          * - :py:attr:`~p4`
            - Get or set the Pressures corresponding to fourth volumetric strain value.
          * - :py:attr:`~p5`
            - Get or set the Pressures corresponding to fifth volumetric strain value.
          * - :py:attr:`~p6`
            - Get or set the Pressures corresponding to sixth volumetric strain value.
          * - :py:attr:`~p7`
            - Get or set the Pressures corresponding to seventh volumetric strain value.
          * - :py:attr:`~p8`
            - Get or set the Pressures corresponding to eight volumetric strain value.
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

    from mat_winfrith_concrete import MatWinfrithConcrete

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density.
















   ..
       !! processed by numpydoc !!

.. py:property:: tm
   :type: Optional[float]


   
   Get or set the Tangent modulus (concrete).
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: Optional[float]


   
   Get or set the Poisson's ratio.
















   ..
       !! processed by numpydoc !!

.. py:property:: ucs
   :type: Optional[float]


   
   Get or set the Uniaxial compressive strength.
















   ..
       !! processed by numpydoc !!

.. py:property:: uts
   :type: Optional[float]


   
   Get or set the Uniaxial tensile strength.
















   ..
       !! processed by numpydoc !!

.. py:property:: fe
   :type: Optional[float]


   
   Get or set the Depends on value of RATE below:
   RATE.EQ.0.: Fracture energy (energy per unit area dissipated in opening crack).
   RATE.EQ 1.: Crack width at which crack-normal tensile stress goes to zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: asize
   :type: Optional[float]


   
   Get or set the Aggregate size (radius).
















   ..
       !! processed by numpydoc !!

.. py:property:: e
   :type: Optional[float]


   
   Get or set the Young's modulus of rebar.
















   ..
       !! processed by numpydoc !!

.. py:property:: ys
   :type: Optional[float]


   
   Get or set the Yield stress of rebar.
















   ..
       !! processed by numpydoc !!

.. py:property:: eh
   :type: Optional[float]


   
   Get or set the Hardening modulus of rebar.
















   ..
       !! processed by numpydoc !!

.. py:property:: uelong
   :type: Optional[float]


   
   Get or set the Ultimate elongation before rebar fails.
















   ..
       !! processed by numpydoc !!

.. py:property:: rate
   :type: float


   
   Get or set the Rate effects:
   EQ.0.0: Strain rate effects are included (default),
   EQ.1.0: Strain rate effects are turned off.Crack widths are stored as extra history variables 30, 31, 32.
   EQ.2.0:Like RATE=1 but includes improved crack algorithm (recommended).  Crack widths are stored as extra history variables 3, 4, 5.
















   ..
       !! processed by numpydoc !!

.. py:property:: conm
   :type: Optional[float]


   
   Get or set the GT.0: Factor to convert model mass units to kg,
   EQ.-1.: Mass, length, time units in model are lbf*sec 2 /in, inch, sec,
   EQ -2.: Mass, length, time units in model are g, cm, microsec,
   EQ.-3.: Mass, length, time units in model are g, mm, msec,
   EQ.-4.: Mass, length, time units in model are metric ton, mm, sec,
   EQ.-5.: Mass, length, time units in model are kg, mm, msec.
















   ..
       !! processed by numpydoc !!

.. py:property:: conl
   :type: Optional[float]


   
   Get or set the If CONM.GT.0, factor to convert model length units to meters; otherwise CONL is ignored.
















   ..
       !! processed by numpydoc !!

.. py:property:: cont
   :type: Optional[float]


   
   Get or set the If CONM.GT.0, factor to convert model time units to seconds; otherwise CONT is ignored.
















   ..
       !! processed by numpydoc !!

.. py:property:: eps1
   :type: Optional[float]


   
   Get or set the First value of volumetric strain-pressure curve (natural logarithmic values).
   A maximum of 8 values are allowed. The tabulated values must competely cover the expected values in the analysis.
   If the first value is not for a volumetric strain value of zero then the point (0.0,0.0) will be automatically generated and up to a further nine additional values may be defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: eps2
   :type: Optional[float]


   
   Get or set the Second value of volumetric strain-pressure curve (natural logarithmic values).
















   ..
       !! processed by numpydoc !!

.. py:property:: eps3
   :type: Optional[float]


   
   Get or set the Third value of volumetric strain-pressure curve (natural logarithmic values).
















   ..
       !! processed by numpydoc !!

.. py:property:: eps4
   :type: Optional[float]


   
   Get or set the Fourth value of volumetric strain-pressure curve (natural logarithmic values).
















   ..
       !! processed by numpydoc !!

.. py:property:: eps5
   :type: Optional[float]


   
   Get or set the Fifth value of volumetric strain-pressure curve (natural logarithmic values).
















   ..
       !! processed by numpydoc !!

.. py:property:: eps6
   :type: Optional[float]


   
   Get or set the Sixth value of volumetric strain-pressure curve (natural logarithmic values).
















   ..
       !! processed by numpydoc !!

.. py:property:: eps7
   :type: Optional[float]


   
   Get or set the Seventh value of volumetric strain-pressure curve (natural logarithmic values).
















   ..
       !! processed by numpydoc !!

.. py:property:: eps8
   :type: Optional[float]


   
   Get or set the Eight value of volumetric strain-pressure curve (natural logarithmic values).
















   ..
       !! processed by numpydoc !!

.. py:property:: p1
   :type: Optional[float]


   
   Get or set the Pressures corresponding to first volumetric strain value.
















   ..
       !! processed by numpydoc !!

.. py:property:: p2
   :type: Optional[float]


   
   Get or set the Pressures corresponding to second volumetric strain value.
















   ..
       !! processed by numpydoc !!

.. py:property:: p3
   :type: Optional[float]


   
   Get or set the Pressures corresponding to third volumetric strain value.
















   ..
       !! processed by numpydoc !!

.. py:property:: p4
   :type: Optional[float]


   
   Get or set the Pressures corresponding to fourth volumetric strain value.
















   ..
       !! processed by numpydoc !!

.. py:property:: p5
   :type: Optional[float]


   
   Get or set the Pressures corresponding to fifth volumetric strain value.
















   ..
       !! processed by numpydoc !!

.. py:property:: p6
   :type: Optional[float]


   
   Get or set the Pressures corresponding to sixth volumetric strain value.
















   ..
       !! processed by numpydoc !!

.. py:property:: p7
   :type: Optional[float]


   
   Get or set the Pressures corresponding to seventh volumetric strain value.
















   ..
       !! processed by numpydoc !!

.. py:property:: p8
   :type: Optional[float]


   
   Get or set the Pressures corresponding to eight volumetric strain value.
















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
   :value: 'WINFRITH_CONCRETE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





