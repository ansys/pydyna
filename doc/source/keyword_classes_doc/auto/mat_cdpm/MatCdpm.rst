





:class:`MatCdpm`
================


.. py:class:: mat_cdpm.MatCdpm(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_CDPM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatCdpm

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
          * - :py:attr:`~e`
            - Get or set the Young's modulus. The sign determines if an anisotropic (E positive, referred to as ISOFLAG=0 in the remarks) or an isotropic (E negative, referred to as ISOFLAG=1 in the remarks) damage formulation is used. The Young's modulus is taken as the absolute value of this parameter.
          * - :py:attr:`~pr`
            - Get or set the Poisson's ratio.
          * - :py:attr:`~ecc`
            - Get or set the Eccentricity parameter.
          * - :py:attr:`~qh0`
            - Get or set the Initial hardening defined as FC0/FC where FC0 is the compressive stress at which the initial yield surface is reached. Default = 0.3.
          * - :py:attr:`~ft`
            - Get or set the Uniaxial tensile strength (stress).
          * - :py:attr:`~fc`
            - Get or set the Uniaxial compression strength (stress)
          * - :py:attr:`~hp`
            - Get or set the Hardening parameter. Default is HP=0.5 which is the value used in Grassl et al. (2011) for strain rate dependent material response (STRFLG = 1). For applications without strain rate effect  (STRFLG = 0) a value of HP = 0.01 is recommended, which has been used in Grassl et al. (2013)
          * - :py:attr:`~ah`
            - Get or set the Hardening ductility parameter 1.
          * - :py:attr:`~bh`
            - Get or set the Hardening ductility parameter 2.
          * - :py:attr:`~ch`
            - Get or set the Hardening ductility parameter 3.
          * - :py:attr:`~dh`
            - Get or set the Hardening ductility parameter 4.
          * - :py:attr:`~as_`
            - Get or set the Ductility parameter during damage.
          * - :py:attr:`~df`
            - Get or set the Flow rule parameter.
          * - :py:attr:`~fc0`
            - Get or set the Rate dependent parameter.Only needed if STRFLG = 1. Recommended value is 10 MPa, which has to be entered consistently with the system of units used.
          * - :py:attr:`~type`
            - Get or set the Flag for damage type.
          * - :py:attr:`~bs`
            - Get or set the Damage ductility exponent during damage. Default = 1.0.
          * - :py:attr:`~wf`
            - Get or set the Tensile threshold value for linear damage formulation.Parameter controlling tensile softening branch for exponential tensile damage formulation.
          * - :py:attr:`~wf1`
            - Get or set the Tensile threshold value for the second part of the bi-linear damage formulation. Default = 0.15*WF.
          * - :py:attr:`~ft1`
            - Get or set the Tensile strength threshold value for bi-linear damage formulation. Default = 0.3*FT.
          * - :py:attr:`~strflg`
            - Get or set the Strain rate flag.
          * - :py:attr:`~failflg`
            - Get or set the Failure flag.
          * - :py:attr:`~efc`
            - Get or set the Parameter controlling compressive damage softening branch in the exponential compressive damage formulation.Default = 1.0E-4
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

    from mat_cdpm import MatCdpm

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

.. py:property:: e
   :type: Optional[float]


   
   Get or set the Young's modulus. The sign determines if an anisotropic (E positive, referred to as ISOFLAG=0 in the remarks) or an isotropic (E negative, referred to as ISOFLAG=1 in the remarks) damage formulation is used. The Young's modulus is taken as the absolute value of this parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: float


   
   Get or set the Poisson's ratio.
















   ..
       !! processed by numpydoc !!

.. py:property:: ecc
   :type: Optional[float]


   
   Get or set the Eccentricity parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: qh0
   :type: float


   
   Get or set the Initial hardening defined as FC0/FC where FC0 is the compressive stress at which the initial yield surface is reached. Default = 0.3.
















   ..
       !! processed by numpydoc !!

.. py:property:: ft
   :type: Optional[float]


   
   Get or set the Uniaxial tensile strength (stress).
















   ..
       !! processed by numpydoc !!

.. py:property:: fc
   :type: Optional[float]


   
   Get or set the Uniaxial compression strength (stress)
















   ..
       !! processed by numpydoc !!

.. py:property:: hp
   :type: float


   
   Get or set the Hardening parameter. Default is HP=0.5 which is the value used in Grassl et al. (2011) for strain rate dependent material response (STRFLG = 1). For applications without strain rate effect  (STRFLG = 0) a value of HP = 0.01 is recommended, which has been used in Grassl et al. (2013)
















   ..
       !! processed by numpydoc !!

.. py:property:: ah
   :type: float


   
   Get or set the Hardening ductility parameter 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: bh
   :type: float


   
   Get or set the Hardening ductility parameter 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: ch
   :type: float


   
   Get or set the Hardening ductility parameter 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: dh
   :type: float


   
   Get or set the Hardening ductility parameter 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: as_
   :type: float


   
   Get or set the Ductility parameter during damage.
















   ..
       !! processed by numpydoc !!

.. py:property:: df
   :type: float


   
   Get or set the Flow rule parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: fc0
   :type: Optional[float]


   
   Get or set the Rate dependent parameter.Only needed if STRFLG = 1. Recommended value is 10 MPa, which has to be entered consistently with the system of units used.
















   ..
       !! processed by numpydoc !!

.. py:property:: type
   :type: float


   
   Get or set the Flag for damage type.
   EQ.0.0: Linear damage formulation (Default)
   EQ.1.0: Bi-linear damage formulation
   EQ.2.0: Exponential damage formulation
   EQ.3.0: No damage The best results are obtained with the bi-linear formulation.
















   ..
       !! processed by numpydoc !!

.. py:property:: bs
   :type: float


   
   Get or set the Damage ductility exponent during damage. Default = 1.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: wf
   :type: Optional[float]


   
   Get or set the Tensile threshold value for linear damage formulation.Parameter controlling tensile softening branch for exponential tensile damage formulation.
















   ..
       !! processed by numpydoc !!

.. py:property:: wf1
   :type: Optional[float]


   
   Get or set the Tensile threshold value for the second part of the bi-linear damage formulation. Default = 0.15*WF.
















   ..
       !! processed by numpydoc !!

.. py:property:: ft1
   :type: Optional[float]


   
   Get or set the Tensile strength threshold value for bi-linear damage formulation. Default = 0.3*FT.
















   ..
       !! processed by numpydoc !!

.. py:property:: strflg
   :type: float


   
   Get or set the Strain rate flag.
   EQ.1.0: Strain rate dependent
   EQ.0.0: No strain rate dependency.
















   ..
       !! processed by numpydoc !!

.. py:property:: failflg
   :type: Optional[float]


   
   Get or set the Failure flag.
   EQ.0.0: Not active. No erosion.
   EQ.X > 0.0: Active and element will erode if wt and wc is equal to 1 in
   X percent of the integration points. If X=0.60, 60% of all integration
   points must fail before erosion..
















   ..
       !! processed by numpydoc !!

.. py:property:: efc
   :type: float


   
   Get or set the Parameter controlling compressive damage softening branch in the exponential compressive damage formulation.Default = 1.0E-4
















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
   :value: 'CDPM'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





