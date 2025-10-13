





:class:`MatRht`
===============


.. py:class:: mat_rht.MatRht(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_RHT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatRht

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
            - Get or set the Mass density.
          * - :py:attr:`~shear`
            - Get or set the Elastic shear modulus.
          * - :py:attr:`~onempa`
            - Get or set the Unit conversion factor defining 1 Mpa in the pressure units used. It can also be used for automatic generation of material parameters for a given
          * - :py:attr:`~epsf`
            - Get or set the Eroding plastic strain.
          * - :py:attr:`~b0`
            - Get or set the Parameter for polynomial EOS.
          * - :py:attr:`~b1`
            - Get or set the Parameter for polynomial EOS.
          * - :py:attr:`~t1`
            - Get or set the Parameter for polynomial EOS.
          * - :py:attr:`~a`
            - Get or set the Failure surface parameter A.
          * - :py:attr:`~n`
            - Get or set the Failure surface parameter N.
          * - :py:attr:`~fc`
            - Get or set the Compressive strength.
          * - :py:attr:`~fs_`
            - Get or set the Relative shear strength.
          * - :py:attr:`~ft_`
            - Get or set the Relative tensile strength.
          * - :py:attr:`~q0`
            - Get or set the Lode angle dependence factor.
          * - :py:attr:`~b`
            - Get or set the Lode angle dependence factor.
          * - :py:attr:`~t2`
            - Get or set the Parameter for polynomial EOS.
          * - :py:attr:`~e0c`
            - Get or set the Reference compressive strain rate.
          * - :py:attr:`~e0t`
            - Get or set the Reference tensile strain rate.
          * - :py:attr:`~ec`
            - Get or set the Break compressive strain rate.
          * - :py:attr:`~et`
            - Get or set the Break tensile strain rate.
          * - :py:attr:`~betac`
            - Get or set the Compressive strain rate dependence exponent (optional).
          * - :py:attr:`~betat`
            - Get or set the Tensile strain rate dependence exponent (optional).
          * - :py:attr:`~ptf`
            - Get or set the Pressure influence on plastic flow in tension.
          * - :py:attr:`~gc_`
            - Get or set the Compressive yield surface parameter.
          * - :py:attr:`~gt_`
            - Get or set the Tensile yield surface parameter.
          * - :py:attr:`~xi`
            - Get or set the Shear modulus reduction factor.
          * - :py:attr:`~d1`
            - Get or set the Damage parameter.
          * - :py:attr:`~d2`
            - Get or set the Damage parameter.
          * - :py:attr:`~epm`
            - Get or set the Minimum damaged residual strain.
          * - :py:attr:`~af`
            - Get or set the Residual surface parameter.
          * - :py:attr:`~nf`
            - Get or set the Residual surface parameter.
          * - :py:attr:`~gamma`
            - Get or set the Gruneisen gamma.
          * - :py:attr:`~a1`
            - Get or set the Hugoniot polynomial coefficient.
          * - :py:attr:`~a2`
            - Get or set the Hugoniot polynomial coefficient.
          * - :py:attr:`~a3`
            - Get or set the Hugoniot polynomial coefficient.
          * - :py:attr:`~pel`
            - Get or set the Crush pressure.
          * - :py:attr:`~pco`
            - Get or set the Compaction pressure.
          * - :py:attr:`~np`
            - Get or set the Porosity exponent.
          * - :py:attr:`~alpha`
            - Get or set the Initial porosity.
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

    from mat_rht import MatRht

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number or label must be specified.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density.
















   ..
       !! processed by numpydoc !!

.. py:property:: shear
   :type: Optional[float]


   
   Get or set the Elastic shear modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: onempa
   :type: float


   
   Get or set the Unit conversion factor defining 1 Mpa in the pressure units used. It can also be used for automatic generation of material parameters for a given
   compressive strength. (See remarks)
   EQ.0: Defaults to 1.0
   EQ.-1: Parameters generated in m, s and kg (Pa)
   EQ.-2: Parameters generated in mm, s and tonne (MPa)
   EQ.-3: Parameters generated in mm, ms and kg (GPa)
   EQ.-4: Parameters generated in in, s and dozens of slugs (psi).
















   ..
       !! processed by numpydoc !!

.. py:property:: epsf
   :type: float


   
   Get or set the Eroding plastic strain.
















   ..
       !! processed by numpydoc !!

.. py:property:: b0
   :type: Optional[float]


   
   Get or set the Parameter for polynomial EOS.
















   ..
       !! processed by numpydoc !!

.. py:property:: b1
   :type: Optional[float]


   
   Get or set the Parameter for polynomial EOS.
















   ..
       !! processed by numpydoc !!

.. py:property:: t1
   :type: Optional[float]


   
   Get or set the Parameter for polynomial EOS.
















   ..
       !! processed by numpydoc !!

.. py:property:: a
   :type: Optional[float]


   
   Get or set the Failure surface parameter A.
















   ..
       !! processed by numpydoc !!

.. py:property:: n
   :type: Optional[float]


   
   Get or set the Failure surface parameter N.
















   ..
       !! processed by numpydoc !!

.. py:property:: fc
   :type: Optional[float]


   
   Get or set the Compressive strength.
















   ..
       !! processed by numpydoc !!

.. py:property:: fs_
   :type: Optional[float]


   
   Get or set the Relative shear strength.
















   ..
       !! processed by numpydoc !!

.. py:property:: ft_
   :type: Optional[float]


   
   Get or set the Relative tensile strength.
















   ..
       !! processed by numpydoc !!

.. py:property:: q0
   :type: Optional[float]


   
   Get or set the Lode angle dependence factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: b
   :type: Optional[float]


   
   Get or set the Lode angle dependence factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: t2
   :type: Optional[float]


   
   Get or set the Parameter for polynomial EOS.
















   ..
       !! processed by numpydoc !!

.. py:property:: e0c
   :type: Optional[float]


   
   Get or set the Reference compressive strain rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: e0t
   :type: Optional[float]


   
   Get or set the Reference tensile strain rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: ec
   :type: Optional[float]


   
   Get or set the Break compressive strain rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: et
   :type: Optional[float]


   
   Get or set the Break tensile strain rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: betac
   :type: Optional[float]


   
   Get or set the Compressive strain rate dependence exponent (optional).
















   ..
       !! processed by numpydoc !!

.. py:property:: betat
   :type: Optional[float]


   
   Get or set the Tensile strain rate dependence exponent (optional).
















   ..
       !! processed by numpydoc !!

.. py:property:: ptf
   :type: float


   
   Get or set the Pressure influence on plastic flow in tension.
















   ..
       !! processed by numpydoc !!

.. py:property:: gc_
   :type: Optional[float]


   
   Get or set the Compressive yield surface parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: gt_
   :type: Optional[float]


   
   Get or set the Tensile yield surface parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: xi
   :type: Optional[float]


   
   Get or set the Shear modulus reduction factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: d1
   :type: Optional[float]


   
   Get or set the Damage parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: d2
   :type: Optional[float]


   
   Get or set the Damage parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: epm
   :type: Optional[float]


   
   Get or set the Minimum damaged residual strain.
















   ..
       !! processed by numpydoc !!

.. py:property:: af
   :type: Optional[float]


   
   Get or set the Residual surface parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: nf
   :type: Optional[float]


   
   Get or set the Residual surface parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: gamma
   :type: Optional[float]


   
   Get or set the Gruneisen gamma.
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: Optional[float]


   
   Get or set the Hugoniot polynomial coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the Hugoniot polynomial coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: a3
   :type: Optional[float]


   
   Get or set the Hugoniot polynomial coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: pel
   :type: Optional[float]


   
   Get or set the Crush pressure.
















   ..
       !! processed by numpydoc !!

.. py:property:: pco
   :type: Optional[float]


   
   Get or set the Compaction pressure.
















   ..
       !! processed by numpydoc !!

.. py:property:: np
   :type: Optional[float]


   
   Get or set the Porosity exponent.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha
   :type: Optional[float]


   
   Get or set the Initial porosity.
















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
   :value: 'RHT'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





