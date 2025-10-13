





:class:`MatHystereticSoil`
==========================


.. py:class:: mat_hysteretic_soil.MatHystereticSoil(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_HYSTERETIC_SOIL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatHystereticSoil

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
          * - :py:attr:`~k0`
            - Get or set the Bulk modulus at the reference pressure.
          * - :py:attr:`~p0`
            - Get or set the Cut-off/datum pressure (must be 0 <= i.e tensile). Below thie pressure, stiffness and strength disappears; this is also the zero pressure for pressure-varying properties.
          * - :py:attr:`~b`
            - Get or set the Exponent for pressure-sensitive modulis b. b must lie in the range 0 <= b < 1. Values close to 1 are not recommended because the pressure becomes indeterminate.
          * - :py:attr:`~a0`
            - Get or set the Yield function constant ao (default = 1.0), see Mateiral Type 5.
          * - :py:attr:`~a1`
            - Get or set the Yield function constant a1 (default = 0.0), see Material Type 5.
          * - :py:attr:`~a2`
            - Get or set the Yield function constant a2 (default = 0.0), see Material Type 5.
          * - :py:attr:`~df`
            - Get or set the Damping factor. Must be in the range 0 <= df <=1:
          * - :py:attr:`~rp`
            - Get or set the Reference pressure for following input data.
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID defining shear strain verses shear stress. Upto 20 points may be defined in the load curve. See *DEFINE_CURVE.
          * - :py:attr:`~sflc`
            - Get or set the Scale factor to apply to shear stress in LCID.
          * - :py:attr:`~dil_a`
            - Get or set the Dilation parameter A.
          * - :py:attr:`~dil_b`
            - Get or set the Dilation parameter B.
          * - :py:attr:`~dil_c`
            - Get or set the Dilation parameter C.
          * - :py:attr:`~dil_d`
            - Get or set the Dilation parameter D.
          * - :py:attr:`~gam1`
            - Get or set the Shear strain gamma-1 (ignored if LCID is non zero).
          * - :py:attr:`~gam2`
            - Get or set the Shear strain gamma-2 (ignored if LCID is non zero).
          * - :py:attr:`~gam3`
            - Get or set the Shear strain gamma-3 (ignored if LCID is non zero).
          * - :py:attr:`~gam4`
            - Get or set the Shear strain gamma-4 (ignored if LCID is non zero).
          * - :py:attr:`~gam5`
            - Get or set the Shear strain gamma-5 (ignored if LCID is non zero).
          * - :py:attr:`~lcd`
            - Get or set the Optional load curve ID defining damping ratio of hysteresis at different strain amplitudes (overrides Masing rules for unload/reload).  The x-axis is shear strain; the y-axis is the damping ratio (such as 0.05 for 5% damping). The strains (x-axis values) of curve LCD must be identical to those of curve LCID.
          * - :py:attr:`~lcsr`
            - Get or set the Load curve ID defining plastic strain rate scaling effect on yield stress. See *DEFINE_CURVE.  The x-axis is plastic strain rate; the y-axis is the yield enhancement factor.
          * - :py:attr:`~pinit`
            - Get or set the Flag for pressure sensitivity (B and A0, A1, A2 equations):
          * - :py:attr:`~tau1`
            - Get or set the Shear stress at gamma-1 (ignored if LCID is non zero).
          * - :py:attr:`~tau2`
            - Get or set the Shear stress at gamma-2 (ignored if LCID is non zero).
          * - :py:attr:`~tau3`
            - Get or set the Shear stress at gamma-3 (ignored if LCID is non zero).
          * - :py:attr:`~tau4`
            - Get or set the Shear stress at gamma-4 (ignored if LCID is non zero).
          * - :py:attr:`~tau5`
            - Get or set the Shear stress at gamma-5 (ignored if LCID is non zero).
          * - :py:attr:`~flag5_`
            - Get or set the If FLAG5 = 1, optional Card 5 will be read. .
          * - :py:attr:`~sigth`
            - Get or set the Threshold shear stress ratio for cyclic degradation
          * - :py:attr:`~sigr`
            - Get or set the Residual shear stress ratio for cyclic degradation
          * - :py:attr:`~chi`
            - Get or set the Cyclic degradation parameter
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

    from mat_hysteretic_soil import MatHystereticSoil

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

.. py:property:: k0
   :type: Optional[float]


   
   Get or set the Bulk modulus at the reference pressure.
















   ..
       !! processed by numpydoc !!

.. py:property:: p0
   :type: Optional[float]


   
   Get or set the Cut-off/datum pressure (must be 0 <= i.e tensile). Below thie pressure, stiffness and strength disappears; this is also the zero pressure for pressure-varying properties.
















   ..
       !! processed by numpydoc !!

.. py:property:: b
   :type: Optional[float]


   
   Get or set the Exponent for pressure-sensitive modulis b. b must lie in the range 0 <= b < 1. Values close to 1 are not recommended because the pressure becomes indeterminate.
















   ..
       !! processed by numpydoc !!

.. py:property:: a0
   :type: float


   
   Get or set the Yield function constant ao (default = 1.0), see Mateiral Type 5.
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: Optional[float]


   
   Get or set the Yield function constant a1 (default = 0.0), see Material Type 5.
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the Yield function constant a2 (default = 0.0), see Material Type 5.
















   ..
       !! processed by numpydoc !!

.. py:property:: df
   :type: Optional[float]


   
   Get or set the Damping factor. Must be in the range 0 <= df <=1:
   EQ.0: no damping,
   EQ.1: maximum damping.
















   ..
       !! processed by numpydoc !!

.. py:property:: rp
   :type: Optional[float]


   
   Get or set the Reference pressure for following input data.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID defining shear strain verses shear stress. Upto 20 points may be defined in the load curve. See *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: sflc
   :type: float


   
   Get or set the Scale factor to apply to shear stress in LCID.
















   ..
       !! processed by numpydoc !!

.. py:property:: dil_a
   :type: Optional[float]


   
   Get or set the Dilation parameter A.
















   ..
       !! processed by numpydoc !!

.. py:property:: dil_b
   :type: Optional[float]


   
   Get or set the Dilation parameter B.
















   ..
       !! processed by numpydoc !!

.. py:property:: dil_c
   :type: Optional[float]


   
   Get or set the Dilation parameter C.
















   ..
       !! processed by numpydoc !!

.. py:property:: dil_d
   :type: Optional[float]


   
   Get or set the Dilation parameter D.
















   ..
       !! processed by numpydoc !!

.. py:property:: gam1
   :type: Optional[float]


   
   Get or set the Shear strain gamma-1 (ignored if LCID is non zero).
















   ..
       !! processed by numpydoc !!

.. py:property:: gam2
   :type: Optional[float]


   
   Get or set the Shear strain gamma-2 (ignored if LCID is non zero).
















   ..
       !! processed by numpydoc !!

.. py:property:: gam3
   :type: Optional[float]


   
   Get or set the Shear strain gamma-3 (ignored if LCID is non zero).
















   ..
       !! processed by numpydoc !!

.. py:property:: gam4
   :type: Optional[float]


   
   Get or set the Shear strain gamma-4 (ignored if LCID is non zero).
















   ..
       !! processed by numpydoc !!

.. py:property:: gam5
   :type: Optional[float]


   
   Get or set the Shear strain gamma-5 (ignored if LCID is non zero).
















   ..
       !! processed by numpydoc !!

.. py:property:: lcd
   :type: Optional[int]


   
   Get or set the Optional load curve ID defining damping ratio of hysteresis at different strain amplitudes (overrides Masing rules for unload/reload).  The x-axis is shear strain; the y-axis is the damping ratio (such as 0.05 for 5% damping). The strains (x-axis values) of curve LCD must be identical to those of curve LCID.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcsr
   :type: Optional[int]


   
   Get or set the Load curve ID defining plastic strain rate scaling effect on yield stress. See *DEFINE_CURVE.  The x-axis is plastic strain rate; the y-axis is the yield enhancement factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: pinit
   :type: int


   
   Get or set the Flag for pressure sensitivity (B and A0, A1, A2 equations):
   EQ.0: Use current pressure (will vary during the analysis)
   EQ.1: Use pressure from initial stress state
   EQ.2: Use initial "plane stress" pressure
   EQ.3: Use (compressive) initial vertical stres.
















   ..
       !! processed by numpydoc !!

.. py:property:: tau1
   :type: Optional[float]


   
   Get or set the Shear stress at gamma-1 (ignored if LCID is non zero).
















   ..
       !! processed by numpydoc !!

.. py:property:: tau2
   :type: Optional[float]


   
   Get or set the Shear stress at gamma-2 (ignored if LCID is non zero).
















   ..
       !! processed by numpydoc !!

.. py:property:: tau3
   :type: Optional[float]


   
   Get or set the Shear stress at gamma-3 (ignored if LCID is non zero).
















   ..
       !! processed by numpydoc !!

.. py:property:: tau4
   :type: Optional[float]


   
   Get or set the Shear stress at gamma-4 (ignored if LCID is non zero).
















   ..
       !! processed by numpydoc !!

.. py:property:: tau5
   :type: Optional[float]


   
   Get or set the Shear stress at gamma-5 (ignored if LCID is non zero).
















   ..
       !! processed by numpydoc !!

.. py:property:: flag5_
   :type: Optional[int]


   
   Get or set the If FLAG5 = 1, optional Card 5 will be read. .
















   ..
       !! processed by numpydoc !!

.. py:property:: sigth
   :type: Optional[float]


   
   Get or set the Threshold shear stress ratio for cyclic degradation
















   ..
       !! processed by numpydoc !!

.. py:property:: sigr
   :type: Optional[float]


   
   Get or set the Residual shear stress ratio for cyclic degradation
















   ..
       !! processed by numpydoc !!

.. py:property:: chi
   :type: Optional[float]


   
   Get or set the Cyclic degradation parameter
















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
   :value: 'HYSTERETIC_SOIL'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





