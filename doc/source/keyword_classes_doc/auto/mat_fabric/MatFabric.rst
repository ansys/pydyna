





:class:`MatFabric`
==================


.. py:class:: mat_fabric.MatFabric(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_FABRIC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatFabric

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
          * - :py:attr:`~ea`
            - Get or set the Young's modulus - longitudinal direction. For an isotopic elastic fabric material only EA and PRBA are defined and are used as the isotropic Young's modulus and Poisson's ratio, respectively. The input for the fiber directions and liner should be input as zero for the isotropic elastic fabric.
          * - :py:attr:`~eb`
            - Get or set the Young's modulus - transverse direction, set to zero for isotropic elastic material.
          * - :py:attr:`~prba`
            - Get or set the Minor Poisson's ratio ba direction.
          * - :py:attr:`~prab`
            - Get or set the Major Poisson's ratio ca direction, set to zero for isotropic elastic material.
          * - :py:attr:`~gab`
            - Get or set the Shear modulus ab direction, set to zero for isotropic elastic material.
          * - :py:attr:`~cse`
            - Get or set the Compressive stress elimination option:
          * - :py:attr:`~el`
            - Get or set the Young's modulus for elastic liner (required if LRATIO>0).
          * - :py:attr:`~prl`
            - Get or set the Poisson's ratio for elastic liner (required if LRATIO>0).
          * - :py:attr:`~lratio`
            - Get or set the A non-zero value activates the elastic liner and defines the ratio of liner thickness to total fabric thickness (optional).
          * - :py:attr:`~damp`
            - Get or set the Rayleigh damping coefficient.  A 0.05 coefficient is recommended corresponding to 5% of critical damping.  Sometimes larger values are necessary
          * - :py:attr:`~aopt`
            - Get or set the Material axes option:
          * - :py:attr:`~flc`
            - Get or set the Fabric leakage coefficient (optional), FLC
          * - :py:attr:`~fac`
            - Get or set the Fabric area coefficient (optional), FAC
          * - :py:attr:`~ela`
            - Get or set the Effective leakage area for blocked fabric, ELA.
          * - :py:attr:`~lnrc`
            - Get or set the Flag to turn off compression in liner until the reference geometry is reached.
          * - :py:attr:`~form`
            - Get or set the Flag to modify membrane formulation for fabric material:
          * - :py:attr:`~fvopt`
            - Get or set the Fabric venting option.
          * - :py:attr:`~tsrfac`
            - Get or set the Tensile stress cutoff reduction factor:
          * - :py:attr:`~rgbrth`
            - Get or set the Material dependent birth time of airbag reference geometry. Nonzero
          * - :py:attr:`~a0ref`
            - Get or set the Calculation option of initial area, A0, used for airbag porosity leakage calculation.
          * - :py:attr:`~a1`
            - Get or set the Component of vector a for AOPT = 2.
          * - :py:attr:`~a2`
            - Get or set the Component of vector a for AOPT = 2.
          * - :py:attr:`~a3`
            - Get or set the Component of vector a for AOPT = 2.
          * - :py:attr:`~x0`
            - Get or set the Coefficients of Anagonye and Wang [1999] porosity equation for the leakage area:
          * - :py:attr:`~x1`
            - Get or set the Coefficients of Anagonye and Wang [1999] porosity equation for the leakage area:
          * - :py:attr:`~v1`
            - Get or set the Component of vector v for AOPT = 3.
          * - :py:attr:`~v2`
            - Get or set the Component of vector v for AOPT = 3.
          * - :py:attr:`~v3`
            - Get or set the Component of vector v for AOPT = 3.
          * - :py:attr:`~beta`
            - Get or set the Material angle in degrees for AOPT=3, may be overridden on the element card, see *ELEMENT_SHELL_BETA.
          * - :py:attr:`~isrefg`
            - Get or set the Initial stress by reference geometry for FORM=12
          * - :py:attr:`~lca`
            - Get or set the Load curve ID for stress versus strain along the a-axis fiber; available when FORM=4 only. If zero, EA is used.
          * - :py:attr:`~lcb`
            - Get or set the Load curve ID for stress versus strain along the b-axis fiber; available when FORM=4 only. If zero, EB is used.
          * - :py:attr:`~lcab`
            - Get or set the Load curve ID for stress versus strain in the ab-plane; available when FORM=4 only. If zero, GAB is used.
          * - :py:attr:`~lcua`
            - Get or set the Unload/reload curve ID for stress versus strain along the a-axis fiber; available when FORM=4 only. If zero, LCA is used.
          * - :py:attr:`~lcub`
            - Get or set the Load curve ID for stress versus strain along the b-axis fiber; available when FORM=4 only. If zero, LCB is used.
          * - :py:attr:`~lcuab`
            - Get or set the Load curve ID for stress versus strain in the ab-plane; available when FORM=4 only. If zero, LCAB is used.
          * - :py:attr:`~rl`
            - Get or set the Optional reloading parameter for FORM=14.  Values between 0.0 (reloading on unloading curve-default) and 1.0 (reloading on a minimum linear slope between unloading curve and loading curve) are possible.
          * - :py:attr:`~lcaa`
            - Get or set the Load curve or table ID. Load curve ID defines the stress along the a-axis fiber versus biaxial strain. Table ID defines for each directional strain rate a load curve representing stress along the a-axis fiber versus biaxial strain. Available for FORM=-14 only, if zero, LCA is used.
          * - :py:attr:`~lcbb`
            - Get or set the Load curve or table ID. Load curve ID defines the stress along the b-axis fiber versus biaxial strain. Table ID defines for each directional strain rate a load curve representing stress along the b-axis fiber versus biaxial strain. Available for FORM=-14 only, if zero, LCB is used.
          * - :py:attr:`~h`
            - Get or set the Normalized hysteresis parameter between 0 and 1.
          * - :py:attr:`~dt`
            - Get or set the Strain rate averaging option.
          * - :py:attr:`~ecoat`
            - Get or set the Young's modulus of coat material, see remark 14.
          * - :py:attr:`~scoat`
            - Get or set the Yield stress of coat material, see remark 14.
          * - :py:attr:`~tcoat`
            - Get or set the Thickness of coat material, see remark 14.
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

    from mat_fabric import MatFabric

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

.. py:property:: ea
   :type: Optional[float]


   
   Get or set the Young's modulus - longitudinal direction. For an isotopic elastic fabric material only EA and PRBA are defined and are used as the isotropic Young's modulus and Poisson's ratio, respectively. The input for the fiber directions and liner should be input as zero for the isotropic elastic fabric.
















   ..
       !! processed by numpydoc !!

.. py:property:: eb
   :type: Optional[float]


   
   Get or set the Young's modulus - transverse direction, set to zero for isotropic elastic material.
















   ..
       !! processed by numpydoc !!

.. py:property:: prba
   :type: Optional[float]


   
   Get or set the Minor Poisson's ratio ba direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: prab
   :type: Optional[float]


   
   Get or set the Major Poisson's ratio ca direction, set to zero for isotropic elastic material.
















   ..
       !! processed by numpydoc !!

.. py:property:: gab
   :type: Optional[float]


   
   Get or set the Shear modulus ab direction, set to zero for isotropic elastic material.
















   ..
       !! processed by numpydoc !!

.. py:property:: cse
   :type: float


   
   Get or set the Compressive stress elimination option:
   EQ.0.0: don't eliminate compressive stresses (default),
   EQ.1.0: eliminate compressive stresses (does not apply to linear).
















   ..
       !! processed by numpydoc !!

.. py:property:: el
   :type: Optional[float]


   
   Get or set the Young's modulus for elastic liner (required if LRATIO>0).
















   ..
       !! processed by numpydoc !!

.. py:property:: prl
   :type: Optional[float]


   
   Get or set the Poisson's ratio for elastic liner (required if LRATIO>0).
















   ..
       !! processed by numpydoc !!

.. py:property:: lratio
   :type: Optional[float]


   
   Get or set the A non-zero value activates the elastic liner and defines the ratio of liner thickness to total fabric thickness (optional).
















   ..
       !! processed by numpydoc !!

.. py:property:: damp
   :type: Optional[float]


   
   Get or set the Rayleigh damping coefficient.  A 0.05 coefficient is recommended corresponding to 5% of critical damping.  Sometimes larger values are necessary
















   ..
       !! processed by numpydoc !!

.. py:property:: aopt
   :type: Optional[float]


   
   Get or set the Material axes option:
   EQ.0.0: locally orthotropic with material axes determined by
   element nodes 1, 2, and 4, as with *DEFINE_COORDINATE_NODES, and then rotated about the shell element normal by the angle BETA.
   EQ.2.0: globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDI_NATE_VECTOR.
   EQ.3.0: locally orthotropic material axes determined by rotating the material axes about the element normal by an angle,
   BETA, from a line in the plane of the element defined by        the cross product of the vector v with the element normal.
   LT.0.0: the absolute value of AOPT is a coordinate system ID number (CID on *DEFINE_COORDINATE_NODES,
   *DEFINE_COORDINATE_SYSTEM or *DEFINE_COOR_DINATE_VECTOR). Available with the R3 release of Version 971 and later.
















   ..
       !! processed by numpydoc !!

.. py:property:: flc
   :type: Optional[float]


   
   Get or set the Fabric leakage coefficient (optional), FLC
   LT.0.0: |FLC| is the load curve ID of the curve defining FLC versus time.
















   ..
       !! processed by numpydoc !!

.. py:property:: fac
   :type: Optional[float]


   
   Get or set the Fabric area coefficient (optional), FAC
   LT.0.0: |FAC| is the load curve ID of the curve defining FAC versus ABSOLUTE pressure.
















   ..
       !! processed by numpydoc !!

.. py:property:: ela
   :type: Optional[float]


   
   Get or set the Effective leakage area for blocked fabric, ELA.
   LT.0.0: |ELA| is the load curve ID of the curve defining ELA versus time. The default value of zero assumes that no leakage occurs. A value of .10 would assume that 10% of the blocked fabric is leaking gas.
















   ..
       !! processed by numpydoc !!

.. py:property:: lnrc
   :type: float


   
   Get or set the Flag to turn off compression in liner until the reference geometry is reached.
   EQ.0.0: off (default),
   EQ.1.0: on.
















   ..
       !! processed by numpydoc !!

.. py:property:: form
   :type: int


   
   Get or set the Flag to modify membrane formulation for fabric material:
   EQ.0: Least costly and very reliable (default).
   EQ.1:invarient local membrane coordinate system
   EQ.2:Green-Lagrange strain formulation
   EQ.3:large strain with nonorthogonal material angles.
   EQ.4:large strain with nonorthogonal material angles and nonlinear stress strain behavior. Define optional load curve IDs on optional card.
   EQ12,13,14 are the updated versions of forms 2,3,4 respectively
   EQ.#14.0: Same as form 14, but invokes reading of card 7
   EQ.24.0: Enhanced version of formulation 14. See Remark 11.
















   ..
       !! processed by numpydoc !!

.. py:property:: fvopt
   :type: int


   
   Get or set the Fabric venting option.
   EQ. 1: Wang-Nefske formulas for venting through an orifice are used. Blockage is not considered.
   EQ. 2: Wang-Nefske formulas for venting through an orifice are used. Blockage of venting area due to contact is considered.
   EQ. 3: Leakage formulas of Graefe, Krummheuer, and Siejak [1990] are used. Blockage is not considered.
   EQ. 4: Leakage formulas of Graefe, Krummheuer, and Siejak [1990] are used. Blockage of venting area due to contact is considered.
   EQ. 5: Leakage formulas based on flow through a porous media are used. Blockage is not considered.
   EQ. 6: Leakage formulas based on flow through a porous media are used. Blockage of venting area due to contact is considered.
   EQ. 7: Leakage is based on gas volume outflow versus pressure load curve. Blockage is not considered. Absolute pressure is used in the porous-velocity-versus-pressure load curve, given as FAC in the *MAT_FABRIC card.
   EQ. 8: Leakage is based on gas volume outflow versus pressure load curve. Blockage of venting or porous area due to contact is considered. Absolute pressure is used in the porous-velocity-versus-pressure load curve, given as FAC in the *MAT_FABRIC card.
   LT.0:   |FVOPT| defines the same fabric venting options as above, but a new formula for the leakage area is used to replace the element area. See Remark 16.
















   ..
       !! processed by numpydoc !!

.. py:property:: tsrfac
   :type: float


   
   Get or set the Tensile stress cutoff reduction factor:
   LT.0: |TSRFAC| is the load curve ID of the curve defining TSRFAC versus time.
   GT.0 and LT.1:  TSRFAC applied from time 0.
   GE.1:   TSRFAC is the ID of a curve that defines TSRFAC versus time using an alternate method (not available for FORM=0 or 1).
















   ..
       !! processed by numpydoc !!

.. py:property:: rgbrth
   :type: Optional[float]


   
   Get or set the Material dependent birth time of airbag reference geometry. Nonzero
   RGBRTH overwrites the birth time defined in the *AIRBAG_REFERENCE_GEOMETRY_BIRTH section. RGBRTH also applies to
   reference geometry defined by *AIRBAG_SHELL_REFERENCE_GEOMETRY
















   ..
       !! processed by numpydoc !!

.. py:property:: a0ref
   :type: int


   
   Get or set the Calculation option of initial area, A0, used for airbag porosity leakage calculation.
   EQ.0.:  default.  Use the initial geometry defined in *NODE.
   EQ.1.:  Use the reference geometry defined in *AIRBAG_REFERENCE_GEOMETRY or *AIRBAG_SHELL_REFERENCE_GEOMETRY.
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: Optional[float]


   
   Get or set the Component of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the Component of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: a3
   :type: Optional[float]


   
   Get or set the Component of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: x0
   :type: Optional[float]


   
   Get or set the Coefficients of Anagonye and Wang [1999] porosity equation for the leakage area:
















   ..
       !! processed by numpydoc !!

.. py:property:: x1
   :type: Optional[float]


   
   Get or set the Coefficients of Anagonye and Wang [1999] porosity equation for the leakage area:
















   ..
       !! processed by numpydoc !!

.. py:property:: v1
   :type: Optional[float]


   
   Get or set the Component of vector v for AOPT = 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: v2
   :type: Optional[float]


   
   Get or set the Component of vector v for AOPT = 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: v3
   :type: Optional[float]


   
   Get or set the Component of vector v for AOPT = 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Material angle in degrees for AOPT=3, may be overridden on the element card, see *ELEMENT_SHELL_BETA.
















   ..
       !! processed by numpydoc !!

.. py:property:: isrefg
   :type: int


   
   Get or set the Initial stress by reference geometry for FORM=12
   EQ.0.0:  default.  Not active.
   EQ.1.0:  active
















   ..
       !! processed by numpydoc !!

.. py:property:: lca
   :type: int


   
   Get or set the Load curve ID for stress versus strain along the a-axis fiber; available when FORM=4 only. If zero, EA is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcb
   :type: int


   
   Get or set the Load curve ID for stress versus strain along the b-axis fiber; available when FORM=4 only. If zero, EB is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcab
   :type: int


   
   Get or set the Load curve ID for stress versus strain in the ab-plane; available when FORM=4 only. If zero, GAB is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcua
   :type: int


   
   Get or set the Unload/reload curve ID for stress versus strain along the a-axis fiber; available when FORM=4 only. If zero, LCA is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcub
   :type: int


   
   Get or set the Load curve ID for stress versus strain along the b-axis fiber; available when FORM=4 only. If zero, LCB is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcuab
   :type: int


   
   Get or set the Load curve ID for stress versus strain in the ab-plane; available when FORM=4 only. If zero, LCAB is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: rl
   :type: Optional[float]


   
   Get or set the Optional reloading parameter for FORM=14.  Values between 0.0 (reloading on unloading curve-default) and 1.0 (reloading on a minimum linear slope between unloading curve and loading curve) are possible.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcaa
   :type: Optional[int]


   
   Get or set the Load curve or table ID. Load curve ID defines the stress along the a-axis fiber versus biaxial strain. Table ID defines for each directional strain rate a load curve representing stress along the a-axis fiber versus biaxial strain. Available for FORM=-14 only, if zero, LCA is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcbb
   :type: Optional[int]


   
   Get or set the Load curve or table ID. Load curve ID defines the stress along the b-axis fiber versus biaxial strain. Table ID defines for each directional strain rate a load curve representing stress along the b-axis fiber versus biaxial strain. Available for FORM=-14 only, if zero, LCB is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: h
   :type: Optional[float]


   
   Get or set the Normalized hysteresis parameter between 0 and 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: dt
   :type: Optional[int]


   
   Get or set the Strain rate averaging option.
   EQ.0.0: Strain rate is evaluated using a running average.
   LT.0.0: Strain rate is evaluated using average of last 11 time steps.
   GT.0.0: Strain rate is averaged over the last DT time units.
















   ..
       !! processed by numpydoc !!

.. py:property:: ecoat
   :type: Optional[float]


   
   Get or set the Young's modulus of coat material, see remark 14.
















   ..
       !! processed by numpydoc !!

.. py:property:: scoat
   :type: Optional[float]


   
   Get or set the Yield stress of coat material, see remark 14.
















   ..
       !! processed by numpydoc !!

.. py:property:: tcoat
   :type: Optional[float]


   
   Get or set the Thickness of coat material, see remark 14.
















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
   :value: 'FABRIC'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





