





:class:`MatCscm`
================


.. py:class:: mat_cscm.MatCscm(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_CSCM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatCscm

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification, a unique number has to be chosen.
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~nplot`
            - Get or set the Plotting options:
          * - :py:attr:`~incre`
            - Get or set the Maximum strain increment for subincrementation.  If left blank, a default value is set during initialization based upon the shear strength and stiffness
          * - :py:attr:`~irate`
            - Get or set the Rate effects options:
          * - :py:attr:`~erode`
            - Get or set the Elements erode when damage exceeds 0.99 and the maximum principal strain exceeds 1.-ERODE.   For erosion that is independent of strain, set ERODE equal to 1.0.   Erosion does not occur if ERODE is less than 1.0.
          * - :py:attr:`~recov`
            - Get or set the The modulus is recovered in compression when RECOV is equal to 0 (default).  The modulus remains at the brittle damage level when RECOV is equal to 1.  Partial recovery is modeled for values of RECOV between 0 and 1.  Two options are available:
          * - :py:attr:`~itretrc`
            - Get or set the Cap retraction option:
          * - :py:attr:`~pred`
            - Get or set the Pre-existing damage (0   PreD < 1).  If left blank, the default is zero (no pre-existing damage).
          * - :py:attr:`~g`
            - Get or set the Shear modulus. .
          * - :py:attr:`~k`
            - Get or set the Bulk modulus.
          * - :py:attr:`~alpha`
            - Get or set the Tri-axial compression surface constant term,  .
          * - :py:attr:`~theta`
            - Get or set the Tri-axial compression surface linear term,  .
          * - :py:attr:`~lamda`
            - Get or set the Tri-axial compression surface nonlinear term,  .
          * - :py:attr:`~beta`
            - Get or set the Tri-axial compression surface exponent.
          * - :py:attr:`~nh`
            - Get or set the Hardening initiation
          * - :py:attr:`~ch`
            - Get or set the Hardening rate
          * - :py:attr:`~alpha1`
            - Get or set the Torsion surface constant term.
          * - :py:attr:`~theta1`
            - Get or set the Torsion surface linear term.
          * - :py:attr:`~lamda1`
            - Get or set the Torsion surface nonlinear term.
          * - :py:attr:`~beta1`
            - Get or set the Torsion surface exponent
          * - :py:attr:`~alpha2`
            - Get or set the Tri-axial extension surface constant term
          * - :py:attr:`~theta2`
            - Get or set the Tri-axial extension surface linear term.
          * - :py:attr:`~lamda2`
            - Get or set the Tri-axial extension surface nonlinear term
          * - :py:attr:`~beta2`
            - Get or set the Tri-axial extension surface exponent
          * - :py:attr:`~r`
            - Get or set the Cap aspect ratio.
          * - :py:attr:`~xd`
            - Get or set the Cap initial location.
          * - :py:attr:`~w`
            - Get or set the Maximum plastic volume compaction, W.
          * - :py:attr:`~d1`
            - Get or set the Linear shape parameter, D1
          * - :py:attr:`~d2`
            - Get or set the Quadratic shape parameter, D2
          * - :py:attr:`~b`
            - Get or set the Ductile shape softening parameter, B.
          * - :py:attr:`~gfc`
            - Get or set the Fracture energy in uniaxial stress Gfc.
          * - :py:attr:`~d`
            - Get or set the Brittle shape softening parameter, D.
          * - :py:attr:`~gft`
            - Get or set the Fracture energy in uniaxial tension, Gft
          * - :py:attr:`~gfs`
            - Get or set the Fracture energy in pure shear stress, Gfs
          * - :py:attr:`~pwrc`
            - Get or set the Shear-to-compression transition parameter.
          * - :py:attr:`~pwrt`
            - Get or set the Shear-to-tension transition parameter.
          * - :py:attr:`~pmod`
            - Get or set the Modify moderate pressure softening parameter
          * - :py:attr:`~eta0c`
            - Get or set the Rate effects parameter for uniaxial compressive stress,.
          * - :py:attr:`~nc`
            - Get or set the Rate effects power for uniaxial compressive stress.
          * - :py:attr:`~etaot`
            - Get or set the Rate effects parameter for uniaxial tensile stress,  0t.
          * - :py:attr:`~nt`
            - Get or set the Rate effects power for uniaxial tensile stress,  Nt.
          * - :py:attr:`~overc`
            - Get or set the Maximum overstress allowed in compression
          * - :py:attr:`~overt`
            - Get or set the .Maximum overstress allowed in tension
          * - :py:attr:`~srate`
            - Get or set the Ratio of effective shear stress to tensile stress fluidity parameters.
          * - :py:attr:`~rep0w`
            - Get or set the Power which increases fracture energy with rate effects.
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

    from mat_cscm import MatCscm

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification, a unique number has to be chosen.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density.
















   ..
       !! processed by numpydoc !!

.. py:property:: nplot
   :type: int


   
   Get or set the Plotting options:
   EQ. 1: Maximum of brittle and ductile damage (default).
   EQ. 2: Maximum of brittle and ductile damage, with recovery of  brittle damage.
   EQ. 3:  Brittle damage.
   EQ. 4:  Ductile damage.
   EQ. 5:    (intersection of cap with shear surface).
   EQ. 6: X0 (intersection of cap with pressure axis).
   EQ. 7:   (plastic volume strain).
















   ..
       !! processed by numpydoc !!

.. py:property:: incre
   :type: Optional[float]


   
   Get or set the Maximum strain increment for subincrementation.  If left blank, a default value is set during initialization based upon the shear strength and stiffness
















   ..
       !! processed by numpydoc !!

.. py:property:: irate
   :type: int


   
   Get or set the Rate effects options:
   EQ.   0: Rate effects model turned off (default).
   EQ.   1: Rate effects model turned on.
















   ..
       !! processed by numpydoc !!

.. py:property:: erode
   :type: Optional[float]


   
   Get or set the Elements erode when damage exceeds 0.99 and the maximum principal strain exceeds 1.-ERODE.   For erosion that is independent of strain, set ERODE equal to 1.0.   Erosion does not occur if ERODE is less than 1.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: recov
   :type: float


   
   Get or set the The modulus is recovered in compression when RECOV is equal to 0 (default).  The modulus remains at the brittle damage level when RECOV is equal to 1.  Partial recovery is modeled for values of RECOV between 0 and 1.  Two options are available:
   Option 1:  Input a value between 0 and 1.  Recovery is based upon the sign of the pressure invariant only.
   Option 2:  Input a value between 10 and 11.  Recovery is based upon the sign of both the pressure and volumetric strain.    In this case, RECOV=RECOV-10, and a flag is set to request the volumetric strain check.
















   ..
       !! processed by numpydoc !!

.. py:property:: itretrc
   :type: int


   
   Get or set the Cap retraction option:
   EQ.0: Cap does not retract (default).
   EQ.1: Cap retracts.
















   ..
       !! processed by numpydoc !!

.. py:property:: pred
   :type: Optional[float]


   
   Get or set the Pre-existing damage (0   PreD < 1).  If left blank, the default is zero (no pre-existing damage).
















   ..
       !! processed by numpydoc !!

.. py:property:: g
   :type: Optional[float]


   
   Get or set the Shear modulus. .
















   ..
       !! processed by numpydoc !!

.. py:property:: k
   :type: Optional[float]


   
   Get or set the Bulk modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha
   :type: Optional[float]


   
   Get or set the Tri-axial compression surface constant term,  .
















   ..
       !! processed by numpydoc !!

.. py:property:: theta
   :type: Optional[float]


   
   Get or set the Tri-axial compression surface linear term,  .
















   ..
       !! processed by numpydoc !!

.. py:property:: lamda
   :type: Optional[float]


   
   Get or set the Tri-axial compression surface nonlinear term,  .
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Tri-axial compression surface exponent.
















   ..
       !! processed by numpydoc !!

.. py:property:: nh
   :type: Optional[float]


   
   Get or set the Hardening initiation
















   ..
       !! processed by numpydoc !!

.. py:property:: ch
   :type: Optional[float]


   
   Get or set the Hardening rate
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha1
   :type: Optional[float]


   
   Get or set the Torsion surface constant term.
















   ..
       !! processed by numpydoc !!

.. py:property:: theta1
   :type: Optional[float]


   
   Get or set the Torsion surface linear term.
















   ..
       !! processed by numpydoc !!

.. py:property:: lamda1
   :type: Optional[float]


   
   Get or set the Torsion surface nonlinear term.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta1
   :type: Optional[float]


   
   Get or set the Torsion surface exponent
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha2
   :type: Optional[float]


   
   Get or set the Tri-axial extension surface constant term
















   ..
       !! processed by numpydoc !!

.. py:property:: theta2
   :type: Optional[float]


   
   Get or set the Tri-axial extension surface linear term.
















   ..
       !! processed by numpydoc !!

.. py:property:: lamda2
   :type: Optional[float]


   
   Get or set the Tri-axial extension surface nonlinear term
















   ..
       !! processed by numpydoc !!

.. py:property:: beta2
   :type: Optional[float]


   
   Get or set the Tri-axial extension surface exponent
















   ..
       !! processed by numpydoc !!

.. py:property:: r
   :type: Optional[float]


   
   Get or set the Cap aspect ratio.
















   ..
       !! processed by numpydoc !!

.. py:property:: xd
   :type: Optional[float]


   
   Get or set the Cap initial location.
















   ..
       !! processed by numpydoc !!

.. py:property:: w
   :type: Optional[float]


   
   Get or set the Maximum plastic volume compaction, W.
















   ..
       !! processed by numpydoc !!

.. py:property:: d1
   :type: Optional[float]


   
   Get or set the Linear shape parameter, D1
















   ..
       !! processed by numpydoc !!

.. py:property:: d2
   :type: Optional[float]


   
   Get or set the Quadratic shape parameter, D2
















   ..
       !! processed by numpydoc !!

.. py:property:: b
   :type: Optional[float]


   
   Get or set the Ductile shape softening parameter, B.
















   ..
       !! processed by numpydoc !!

.. py:property:: gfc
   :type: Optional[float]


   
   Get or set the Fracture energy in uniaxial stress Gfc.
















   ..
       !! processed by numpydoc !!

.. py:property:: d
   :type: Optional[float]


   
   Get or set the Brittle shape softening parameter, D.
















   ..
       !! processed by numpydoc !!

.. py:property:: gft
   :type: Optional[float]


   
   Get or set the Fracture energy in uniaxial tension, Gft
















   ..
       !! processed by numpydoc !!

.. py:property:: gfs
   :type: Optional[float]


   
   Get or set the Fracture energy in pure shear stress, Gfs
















   ..
       !! processed by numpydoc !!

.. py:property:: pwrc
   :type: Optional[float]


   
   Get or set the Shear-to-compression transition parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: pwrt
   :type: Optional[float]


   
   Get or set the Shear-to-tension transition parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: pmod
   :type: Optional[float]


   
   Get or set the Modify moderate pressure softening parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: eta0c
   :type: Optional[float]


   
   Get or set the Rate effects parameter for uniaxial compressive stress,.
















   ..
       !! processed by numpydoc !!

.. py:property:: nc
   :type: Optional[float]


   
   Get or set the Rate effects power for uniaxial compressive stress.
















   ..
       !! processed by numpydoc !!

.. py:property:: etaot
   :type: Optional[float]


   
   Get or set the Rate effects parameter for uniaxial tensile stress,  0t.
















   ..
       !! processed by numpydoc !!

.. py:property:: nt
   :type: Optional[float]


   
   Get or set the Rate effects power for uniaxial tensile stress,  Nt.
















   ..
       !! processed by numpydoc !!

.. py:property:: overc
   :type: Optional[float]


   
   Get or set the Maximum overstress allowed in compression
















   ..
       !! processed by numpydoc !!

.. py:property:: overt
   :type: Optional[float]


   
   Get or set the .Maximum overstress allowed in tension
















   ..
       !! processed by numpydoc !!

.. py:property:: srate
   :type: Optional[float]


   
   Get or set the Ratio of effective shear stress to tensile stress fluidity parameters.
















   ..
       !! processed by numpydoc !!

.. py:property:: rep0w
   :type: Optional[float]


   
   Get or set the Power which increases fracture energy with rate effects.
















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
   :value: 'CSCM'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





