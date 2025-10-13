





:class:`MatEmmi`
================


.. py:class:: mat_emmi.MatEmmi(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_EMMI keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatEmmi

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number has to be used.
          * - :py:attr:`~rho`
            - Get or set the Material density
          * - :py:attr:`~e`
            - Get or set the Young's modulus
          * - :py:attr:`~pr`
            - Get or set the Poisson's ratio.
          * - :py:attr:`~rgas`
            - Get or set the universal gas constant.
          * - :py:attr:`~bvect`
            - Get or set the Burger's vector
          * - :py:attr:`~d0`
            - Get or set the pre-exponential diffusivity coefficient
          * - :py:attr:`~qd`
            - Get or set the activation energy.
          * - :py:attr:`~cv`
            - Get or set the specific heat at constant volume
          * - :py:attr:`~adrag`
            - Get or set the drag intercept
          * - :py:attr:`~bdrag`
            - Get or set the drag coefficient
          * - :py:attr:`~dmtheta`
            - Get or set the shear modulus temperature coefficient
          * - :py:attr:`~dmphi`
            - Get or set the shear modulus damage coefficient.
          * - :py:attr:`~dntheta`
            - Get or set the bulk modulus temperature coefficient
          * - :py:attr:`~dnphi`
            - Get or set the bulk modulus damage coefficient
          * - :py:attr:`~theta0`
            - Get or set the reference temperature.
          * - :py:attr:`~thetam`
            - Get or set the melt temperature
          * - :py:attr:`~beta0`
            - Get or set the coefficient of thermal expansion at reference temperature
          * - :py:attr:`~btheta`
            - Get or set the thermal expansion temperature coefficient
          * - :py:attr:`~dmr`
            - Get or set the damage rate sensitivity parameter
          * - :py:attr:`~dnuc1`
            - Get or set the Nucleation coefficient .
          * - :py:attr:`~dnuc2`
            - Get or set the Nucleation coefficient
          * - :py:attr:`~dnuc3`
            - Get or set the Nucleation coefficient
          * - :py:attr:`~dnuc4`
            - Get or set the Nucleation coefficient
          * - :py:attr:`~dm1`
            - Get or set the coefficient of yield temperature dependence.
          * - :py:attr:`~dm2`
            - Get or set the coefficient of yield temperature dependence
          * - :py:attr:`~dm3`
            - Get or set the coefficient of yield temperature dependence
          * - :py:attr:`~dm4`
            - Get or set the coefficient of yield temperature dependence.
          * - :py:attr:`~dm5`
            - Get or set the coefficient of yield temperature dependence
          * - :py:attr:`~qind`
            - Get or set the dimensionless activation energy for f
          * - :py:attr:`~q2nd`
            - Get or set the dimensionless activation energy for rd
          * - :py:attr:`~q3nd`
            - Get or set the dimensionless activation energy for Rd
          * - :py:attr:`~q4nd`
            - Get or set the dimensionless activation energy Rs.
          * - :py:attr:`~calpha`
            - Get or set the coefficient for backstress alpha
          * - :py:attr:`~ckappa`
            - Get or set the coefficient for internal stress kappa
          * - :py:attr:`~c1`
            - Get or set the parameter for flow rule exponent n .
          * - :py:attr:`~c2nd`
            - Get or set the parameter for transition rate f
          * - :py:attr:`~c3`
            - Get or set the parameter for alpha dynamic recovery rd
          * - :py:attr:`~c4`
            - Get or set the parameter for alpha hardening h
          * - :py:attr:`~c5`
            - Get or set the parameter for kappa dynamic recovery Rd
          * - :py:attr:`~c6`
            - Get or set the parameter for kappa hardening H
          * - :py:attr:`~c7nd`
            - Get or set the parameter kappa static recovery Rs
          * - :py:attr:`~c8nd`
            - Get or set the parameter for yield
          * - :py:attr:`~c9nd`
            - Get or set the parameter for temperature dependence of flow rule exponent n .
          * - :py:attr:`~c10`
            - Get or set the parameter for static recovery (set=1)
          * - :py:attr:`~a1`
            - Get or set the plastic anisotropy parameter
          * - :py:attr:`~a2`
            - Get or set the plastic anisotropy parameter
          * - :py:attr:`~a3`
            - Get or set the plastic anisotropy parameter
          * - :py:attr:`~a4`
            - Get or set the plastic anisotropy parameter.
          * - :py:attr:`~a_xx`
            - Get or set the initial structure tensor component
          * - :py:attr:`~a_yy`
            - Get or set the initial structure tensor component
          * - :py:attr:`~a_zz`
            - Get or set the initial structure tensor component.
          * - :py:attr:`~a_xy`
            - Get or set the initial structure tensor component
          * - :py:attr:`~a_yz`
            - Get or set the initial structure tensor component
          * - :py:attr:`~a_xz`
            - Get or set the initial structure tensor component
          * - :py:attr:`~alphxx`
            - Get or set the initial backstress component
          * - :py:attr:`~alphyy`
            - Get or set the initial backstress component.
          * - :py:attr:`~alphzz`
            - Get or set the initial backstress component
          * - :py:attr:`~alphxy`
            - Get or set the initial backstress component
          * - :py:attr:`~alphyz`
            - Get or set the initial backstress component.
          * - :py:attr:`~alphxz`
            - Get or set the initial backstress component
          * - :py:attr:`~dkappa`
            - Get or set the initial isotropic internal stress
          * - :py:attr:`~phi0`
            - Get or set the initial isotropic porosity
          * - :py:attr:`~phicr`
            - Get or set the Critical cutoff porosity
          * - :py:attr:`~dlbdag`
            - Get or set the slip system geometry parameter
          * - :py:attr:`~factor`
            - Get or set the fraction of plastic work converted to heat, adiabatic.
          * - :py:attr:`~rswtch`
            - Get or set the Rate sensitivity switch
          * - :py:attr:`~dmgopt`
            - Get or set the damage model option parameter
          * - :py:attr:`~delaso`
            - Get or set the Temperature option:
          * - :py:attr:`~dimplo`
            - Get or set the implementation option flag
          * - :py:attr:`~atol`
            - Get or set the absolute error tolerance for local Newton iteration
          * - :py:attr:`~rtol`
            - Get or set the relative error tolerance for local Newton iteration
          * - :py:attr:`~dniter`
            - Get or set the maximum number of iterations for local Newton iteration
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

    from mat_emmi import MatEmmi

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: rho
   :type: Optional[float]


   
   Get or set the Material density
















   ..
       !! processed by numpydoc !!

.. py:property:: e
   :type: Optional[float]


   
   Get or set the Young's modulus
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: Optional[float]


   
   Get or set the Poisson's ratio.
















   ..
       !! processed by numpydoc !!

.. py:property:: rgas
   :type: Optional[float]


   
   Get or set the universal gas constant.
















   ..
       !! processed by numpydoc !!

.. py:property:: bvect
   :type: Optional[float]


   
   Get or set the Burger's vector
















   ..
       !! processed by numpydoc !!

.. py:property:: d0
   :type: Optional[float]


   
   Get or set the pre-exponential diffusivity coefficient
















   ..
       !! processed by numpydoc !!

.. py:property:: qd
   :type: Optional[float]


   
   Get or set the activation energy.
















   ..
       !! processed by numpydoc !!

.. py:property:: cv
   :type: Optional[float]


   
   Get or set the specific heat at constant volume
















   ..
       !! processed by numpydoc !!

.. py:property:: adrag
   :type: Optional[float]


   
   Get or set the drag intercept
















   ..
       !! processed by numpydoc !!

.. py:property:: bdrag
   :type: Optional[float]


   
   Get or set the drag coefficient
















   ..
       !! processed by numpydoc !!

.. py:property:: dmtheta
   :type: Optional[float]


   
   Get or set the shear modulus temperature coefficient
















   ..
       !! processed by numpydoc !!

.. py:property:: dmphi
   :type: Optional[float]


   
   Get or set the shear modulus damage coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: dntheta
   :type: Optional[float]


   
   Get or set the bulk modulus temperature coefficient
















   ..
       !! processed by numpydoc !!

.. py:property:: dnphi
   :type: Optional[float]


   
   Get or set the bulk modulus damage coefficient
















   ..
       !! processed by numpydoc !!

.. py:property:: theta0
   :type: Optional[float]


   
   Get or set the reference temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: thetam
   :type: Optional[float]


   
   Get or set the melt temperature
















   ..
       !! processed by numpydoc !!

.. py:property:: beta0
   :type: Optional[float]


   
   Get or set the coefficient of thermal expansion at reference temperature
















   ..
       !! processed by numpydoc !!

.. py:property:: btheta
   :type: Optional[float]


   
   Get or set the thermal expansion temperature coefficient
















   ..
       !! processed by numpydoc !!

.. py:property:: dmr
   :type: Optional[float]


   
   Get or set the damage rate sensitivity parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: dnuc1
   :type: Optional[float]


   
   Get or set the Nucleation coefficient .
















   ..
       !! processed by numpydoc !!

.. py:property:: dnuc2
   :type: Optional[float]


   
   Get or set the Nucleation coefficient
















   ..
       !! processed by numpydoc !!

.. py:property:: dnuc3
   :type: Optional[float]


   
   Get or set the Nucleation coefficient
















   ..
       !! processed by numpydoc !!

.. py:property:: dnuc4
   :type: Optional[float]


   
   Get or set the Nucleation coefficient
















   ..
       !! processed by numpydoc !!

.. py:property:: dm1
   :type: Optional[float]


   
   Get or set the coefficient of yield temperature dependence.
















   ..
       !! processed by numpydoc !!

.. py:property:: dm2
   :type: Optional[float]


   
   Get or set the coefficient of yield temperature dependence
















   ..
       !! processed by numpydoc !!

.. py:property:: dm3
   :type: Optional[float]


   
   Get or set the coefficient of yield temperature dependence
















   ..
       !! processed by numpydoc !!

.. py:property:: dm4
   :type: Optional[float]


   
   Get or set the coefficient of yield temperature dependence.
















   ..
       !! processed by numpydoc !!

.. py:property:: dm5
   :type: Optional[float]


   
   Get or set the coefficient of yield temperature dependence
















   ..
       !! processed by numpydoc !!

.. py:property:: qind
   :type: Optional[float]


   
   Get or set the dimensionless activation energy for f
















   ..
       !! processed by numpydoc !!

.. py:property:: q2nd
   :type: Optional[float]


   
   Get or set the dimensionless activation energy for rd
















   ..
       !! processed by numpydoc !!

.. py:property:: q3nd
   :type: Optional[float]


   
   Get or set the dimensionless activation energy for Rd
















   ..
       !! processed by numpydoc !!

.. py:property:: q4nd
   :type: Optional[float]


   
   Get or set the dimensionless activation energy Rs.
















   ..
       !! processed by numpydoc !!

.. py:property:: calpha
   :type: Optional[float]


   
   Get or set the coefficient for backstress alpha
















   ..
       !! processed by numpydoc !!

.. py:property:: ckappa
   :type: Optional[float]


   
   Get or set the coefficient for internal stress kappa
















   ..
       !! processed by numpydoc !!

.. py:property:: c1
   :type: Optional[float]


   
   Get or set the parameter for flow rule exponent n .
















   ..
       !! processed by numpydoc !!

.. py:property:: c2nd
   :type: Optional[float]


   
   Get or set the parameter for transition rate f
















   ..
       !! processed by numpydoc !!

.. py:property:: c3
   :type: Optional[float]


   
   Get or set the parameter for alpha dynamic recovery rd
















   ..
       !! processed by numpydoc !!

.. py:property:: c4
   :type: Optional[float]


   
   Get or set the parameter for alpha hardening h
















   ..
       !! processed by numpydoc !!

.. py:property:: c5
   :type: Optional[float]


   
   Get or set the parameter for kappa dynamic recovery Rd
















   ..
       !! processed by numpydoc !!

.. py:property:: c6
   :type: Optional[float]


   
   Get or set the parameter for kappa hardening H
















   ..
       !! processed by numpydoc !!

.. py:property:: c7nd
   :type: Optional[float]


   
   Get or set the parameter kappa static recovery Rs
















   ..
       !! processed by numpydoc !!

.. py:property:: c8nd
   :type: Optional[float]


   
   Get or set the parameter for yield
















   ..
       !! processed by numpydoc !!

.. py:property:: c9nd
   :type: Optional[float]


   
   Get or set the parameter for temperature dependence of flow rule exponent n .
















   ..
       !! processed by numpydoc !!

.. py:property:: c10
   :type: Optional[float]


   
   Get or set the parameter for static recovery (set=1)
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: Optional[float]


   
   Get or set the plastic anisotropy parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the plastic anisotropy parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: a3
   :type: Optional[float]


   
   Get or set the plastic anisotropy parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: a4
   :type: Optional[float]


   
   Get or set the plastic anisotropy parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: a_xx
   :type: Optional[float]


   
   Get or set the initial structure tensor component
















   ..
       !! processed by numpydoc !!

.. py:property:: a_yy
   :type: Optional[float]


   
   Get or set the initial structure tensor component
















   ..
       !! processed by numpydoc !!

.. py:property:: a_zz
   :type: Optional[float]


   
   Get or set the initial structure tensor component.
















   ..
       !! processed by numpydoc !!

.. py:property:: a_xy
   :type: Optional[float]


   
   Get or set the initial structure tensor component
















   ..
       !! processed by numpydoc !!

.. py:property:: a_yz
   :type: Optional[float]


   
   Get or set the initial structure tensor component
















   ..
       !! processed by numpydoc !!

.. py:property:: a_xz
   :type: Optional[float]


   
   Get or set the initial structure tensor component
















   ..
       !! processed by numpydoc !!

.. py:property:: alphxx
   :type: Optional[float]


   
   Get or set the initial backstress component
















   ..
       !! processed by numpydoc !!

.. py:property:: alphyy
   :type: Optional[float]


   
   Get or set the initial backstress component.
















   ..
       !! processed by numpydoc !!

.. py:property:: alphzz
   :type: Optional[float]


   
   Get or set the initial backstress component
















   ..
       !! processed by numpydoc !!

.. py:property:: alphxy
   :type: Optional[float]


   
   Get or set the initial backstress component
















   ..
       !! processed by numpydoc !!

.. py:property:: alphyz
   :type: Optional[float]


   
   Get or set the initial backstress component.
















   ..
       !! processed by numpydoc !!

.. py:property:: alphxz
   :type: Optional[float]


   
   Get or set the initial backstress component
















   ..
       !! processed by numpydoc !!

.. py:property:: dkappa
   :type: Optional[float]


   
   Get or set the initial isotropic internal stress
















   ..
       !! processed by numpydoc !!

.. py:property:: phi0
   :type: Optional[float]


   
   Get or set the initial isotropic porosity
















   ..
       !! processed by numpydoc !!

.. py:property:: phicr
   :type: Optional[float]


   
   Get or set the Critical cutoff porosity
















   ..
       !! processed by numpydoc !!

.. py:property:: dlbdag
   :type: Optional[float]


   
   Get or set the slip system geometry parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: factor
   :type: Optional[float]


   
   Get or set the fraction of plastic work converted to heat, adiabatic.
















   ..
       !! processed by numpydoc !!

.. py:property:: rswtch
   :type: Optional[float]


   
   Get or set the Rate sensitivity switch
















   ..
       !! processed by numpydoc !!

.. py:property:: dmgopt
   :type: Optional[float]


   
   Get or set the damage model option parameter
   1.0     pressure independent Cocks/Ashby 1980
   2.0     pressure dependent Cocks/Ashby 1980
   3.0     pressure dependent Cocks 1989
















   ..
       !! processed by numpydoc !!

.. py:property:: delaso
   :type: Optional[float]


   
   Get or set the Temperature option:
   EQ.0.0: Driven externally
   EQ.1.0 : Adiabatic
















   ..
       !! processed by numpydoc !!

.. py:property:: dimplo
   :type: Optional[float]


   
   Get or set the implementation option flag
   1.0     combined viscous drag and thermally activated dislocation motion
   2.0     separate viscous drag and thermally activated dislocation motion.
















   ..
       !! processed by numpydoc !!

.. py:property:: atol
   :type: Optional[float]


   
   Get or set the absolute error tolerance for local Newton iteration
















   ..
       !! processed by numpydoc !!

.. py:property:: rtol
   :type: Optional[float]


   
   Get or set the relative error tolerance for local Newton iteration
















   ..
       !! processed by numpydoc !!

.. py:property:: dniter
   :type: Optional[float]


   
   Get or set the maximum number of iterations for local Newton iteration
















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
   :value: 'EMMI'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





