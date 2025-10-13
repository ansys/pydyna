





:class:`MatSeatbelt2D`
======================


.. py:class:: mat_seatbelt_2d.MatSeatbelt2D(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_SEATBELT_2D keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatSeatbelt2D

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Belt material number. A unique number has to be used.
          * - :py:attr:`~mpul`
            - Get or set the Mass per unit length.
          * - :py:attr:`~llcid`
            - Get or set the Load curve identification for loading (strain/force with engineering strain).
          * - :py:attr:`~ulcid`
            - Get or set the Load curve identification for unloading (strain/force with engineering strain).
          * - :py:attr:`~lmin`
            - Get or set the Minimum length (for elements connected to slip rings and retractors).
          * - :py:attr:`~cse`
            - Get or set the Optional compressive stress elimination option which applies to shell elements only (default 0.0):
          * - :py:attr:`~damp`
            - Get or set the Optional Rayleigh damping coefficient, which applies to shell elements only.  A coefficient value of 0.10 is the default corresponding to 10% of critical damping.  Sometimes smaller or larger values work better.
          * - :py:attr:`~e`
            - Get or set the Young's modulus for bending/compression stiffness, when positive the optional card is invoked.
          * - :py:attr:`~a`
            - Get or set the Cross sectional area for bending/compression stiffness
          * - :py:attr:`~i`
            - Get or set the Area moment of inertia for bending/compression stiffness
          * - :py:attr:`~j`
            - Get or set the Torsional constant for bending/compression stiffness
          * - :py:attr:`~as_`
            - Get or set the Shear area for bending/compression stiffness
          * - :py:attr:`~f`
            - Get or set the Maximum force in compression/tension.
          * - :py:attr:`~m`
            - Get or set the Maximum torque
          * - :py:attr:`~r`
            - Get or set the Rotational mass scaling factor
          * - :py:attr:`~p1doff`
            - Get or set the Part ID offset for internally created 1D, bar-type, belt parts for 2D seatbelt of this material, i.e., the IDs of newly created 1d belt parts will be P1DOFF+1, P1DOFF+2, ....  If zero, the maximum ID of user-defined parts is used as the part ID offset.
          * - :py:attr:`~form`
            - Get or set the Formulation of the translated fabric material, see FORM of *MAT_FABRIC for details.  FORM=0 was used since R8 and non-zero FORM is available since r137418/dev
          * - :py:attr:`~ecoat`
            - Get or set the Young's modulus of coat material for FORM=-14, see *MAT_FABRIC for details.
          * - :py:attr:`~tcoat`
            - Get or set the Thickness of coat material for FORM=-14, see *MAT_FABRIC for details.
          * - :py:attr:`~scoat`
            - Get or set the Yield stress of coat material for FORM=-14, see *MAT_FABRIC for details.
          * - :py:attr:`~eb`
            - Get or set the Young’s modulus along transverse direction, see *MAT_FABRIC for details.
          * - :py:attr:`~prba`
            - Get or set the Minor (Major) Poisson's ratioba (ab) direction
          * - :py:attr:`~gab`
            - Get or set the Shear modulus in the ab direction. Set to a very small value for an
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

    from mat_seatbelt_2d import MatSeatbelt2D

Property detail
---------------

.. py:property:: mid
   :type: int


   
   Get or set the Belt material number. A unique number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: mpul
   :type: Optional[float]


   
   Get or set the Mass per unit length.
















   ..
       !! processed by numpydoc !!

.. py:property:: llcid
   :type: int


   
   Get or set the Load curve identification for loading (strain/force with engineering strain).
















   ..
       !! processed by numpydoc !!

.. py:property:: ulcid
   :type: int


   
   Get or set the Load curve identification for unloading (strain/force with engineering strain).
















   ..
       !! processed by numpydoc !!

.. py:property:: lmin
   :type: Optional[float]


   
   Get or set the Minimum length (for elements connected to slip rings and retractors).
















   ..
       !! processed by numpydoc !!

.. py:property:: cse
   :type: float


   
   Get or set the Optional compressive stress elimination option which applies to shell elements only (default 0.0):
   EQ.0.0: eliminate compressive stresses in shell fabric
   EQ.1.0: don't eliminate compressive stresses.  This option should not be used if retractors and sliprings are present in the model.
   EQ.2.0: whether or not compressive stress is eliminated is decided by ls-dyna automatically, recommended for shell belt.
















   ..
       !! processed by numpydoc !!

.. py:property:: damp
   :type: Optional[float]


   
   Get or set the Optional Rayleigh damping coefficient, which applies to shell elements only.  A coefficient value of 0.10 is the default corresponding to 10% of critical damping.  Sometimes smaller or larger values work better.
















   ..
       !! processed by numpydoc !!

.. py:property:: e
   :type: Optional[float]


   
   Get or set the Young's modulus for bending/compression stiffness, when positive the optional card is invoked.
















   ..
       !! processed by numpydoc !!

.. py:property:: a
   :type: Optional[float]


   
   Get or set the Cross sectional area for bending/compression stiffness
















   ..
       !! processed by numpydoc !!

.. py:property:: i
   :type: Optional[float]


   
   Get or set the Area moment of inertia for bending/compression stiffness
















   ..
       !! processed by numpydoc !!

.. py:property:: j
   :type: Optional[float]


   
   Get or set the Torsional constant for bending/compression stiffness
















   ..
       !! processed by numpydoc !!

.. py:property:: as_
   :type: Optional[float]


   
   Get or set the Shear area for bending/compression stiffness
















   ..
       !! processed by numpydoc !!

.. py:property:: f
   :type: float


   
   Get or set the Maximum force in compression/tension.
















   ..
       !! processed by numpydoc !!

.. py:property:: m
   :type: float


   
   Get or set the Maximum torque
















   ..
       !! processed by numpydoc !!

.. py:property:: r
   :type: float


   
   Get or set the Rotational mass scaling factor
















   ..
       !! processed by numpydoc !!

.. py:property:: p1doff
   :type: Optional[int]


   
   Get or set the Part ID offset for internally created 1D, bar-type, belt parts for 2D seatbelt of this material, i.e., the IDs of newly created 1d belt parts will be P1DOFF+1, P1DOFF+2, ....  If zero, the maximum ID of user-defined parts is used as the part ID offset.
















   ..
       !! processed by numpydoc !!

.. py:property:: form
   :type: int


   
   Get or set the Formulation of the translated fabric material, see FORM of *MAT_FABRIC for details.  FORM=0 was used since R8 and non-zero FORM is available since r137418/dev
















   ..
       !! processed by numpydoc !!

.. py:property:: ecoat
   :type: Optional[float]


   
   Get or set the Young's modulus of coat material for FORM=-14, see *MAT_FABRIC for details.
   The coat material is assumed to be elastic, therefore there is no need to define its yield stress, see SCOAT of *MAT_FABRIC.
   EQ.0.0: ECOAT is the Young's modulus determined by LS-DYNA.
   GT.0.0: ECOAT is the Young's modulus to be used for coat material.
   LT.0.0: |ECOAT| is the ratio of coat material's Young's modulus to that of the fabric shell which is determined by LS-DYNA.
















   ..
       !! processed by numpydoc !!

.. py:property:: tcoat
   :type: Optional[float]


   
   Get or set the Thickness of coat material for FORM=-14, see *MAT_FABRIC for details.
   GT.0.0: TCOAT is the thickness of the coat material.
   LT.0.0: |TCOAT| is the ratio of coat material's thickness to that of the fabric shell defined in the related *SECTION_SHELL.
















   ..
       !! processed by numpydoc !!

.. py:property:: scoat
   :type: Optional[float]


   
   Get or set the Yield stress of coat material for FORM=-14, see *MAT_FABRIC for details.
   If not defined, the coat material is assumed to be elastic.
















   ..
       !! processed by numpydoc !!

.. py:property:: eb
   :type: float


   
   Get or set the Young’s modulus along transverse direction, see *MAT_FABRIC for details.
   EQ.0.0: The Young’s modulus along transverse direction is 10 % of
   the Young’s determined by LS - DYNA based on the loading curve, LLCID.
   LT.0.0 : | EB | is the ratio of Young’s modulus along transverse
   direction to the Young’s determined by LS - DYNA based on the loading curve, LLCID.
   GT.0.0 : EB is the Young's modulus along the transverse direction
















   ..
       !! processed by numpydoc !!

.. py:property:: prba
   :type: float


   
   Get or set the Minor (Major) Poisson's ratioba (ab) direction
















   ..
       !! processed by numpydoc !!

.. py:property:: gab
   :type: Optional[float]


   
   Get or set the Shear modulus in the ab direction. Set to a very small value for an
   isotropic elastic material, see * MAT_FABRIC.If defined to be zero, a
   default value of EA / (2 * (1 + PRBA)) will be used where EA is the
   Young's modulus along the longitudinal direction and is set to 1 % of the
   Young's modulus determined by LS - DYNA according to the loading curve, LLCID
















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
   :value: 'SEATBELT_2D'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





