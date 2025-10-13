





:class:`MatAddDamageDiem`
=========================


.. py:class:: mat_add_damage_diem.MatAddDamageDiem(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_ADD_DAMAGE_DIEM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatAddDamageDiem

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification for which this erosion definition applies. A unique number or label must be specified.
          * - :py:attr:`~ndiemc`
            - Get or set the Number of damage initiation and evolution model (DIEM) criteria to be applied.
          * - :py:attr:`~dinit`
            - Get or set the Damage initialization option.
          * - :py:attr:`~deps`
            - Get or set the Plastic strain increment between evaluation of damage instability and evolution criteria. See remarks, the default is zero.
          * - :py:attr:`~numfip`
            - Get or set the Number or percentage of failed integration points prior to element deletion (default value is 1).
          * - :py:attr:`~dityp`
            - Get or set the Damage initiation type
          * - :py:attr:`~p1`
            - Get or set the Damage initiation parameter
          * - :py:attr:`~p2`
            - Get or set the Damage initiation parameter
          * - :py:attr:`~p3`
            - Get or set the Damage initiation parameter
          * - :py:attr:`~p4`
            - Get or set the Plane stress option for shell elements:
          * - :py:attr:`~p5`
            - Get or set the Load curve or table ID representing regularization factor as a function of the characteristic element size (curve) or regularization factor as a function of the characteristic element size and abscissa value of the criterion used (table).. This factor scales the plastic strain at the onset of damage defined with P1.
          * - :py:attr:`~detyp`
            - Get or set the Damage evolution type
          * - :py:attr:`~dctyp`
            - Get or set the Damage composition option for multiple criteria
          * - :py:attr:`~q1`
            - Get or set the Damage evolution parameter
          * - :py:attr:`~q2`
            - Get or set the Set to 1.0 to output information to log files (messag and d3hsp) when an integration point fails.
          * - :py:attr:`~q3`
            - Get or set the Damage evolution parameter:
          * - :py:attr:`~q4`
            - Get or set the Load curve or table ID representing regularization factor as a function of the characteristic element size (curve) or regularization factor as a function of the characteristic element size and plastic strain rate (table). This factor scales the damage evolution parameter Q1
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

    from mat_add_damage_diem import MatAddDamageDiem

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification for which this erosion definition applies. A unique number or label must be specified.
















   ..
       !! processed by numpydoc !!

.. py:property:: ndiemc
   :type: float


   
   Get or set the Number of damage initiation and evolution model (DIEM) criteria to be applied.
















   ..
       !! processed by numpydoc !!

.. py:property:: dinit
   :type: int


   
   Get or set the Damage initialization option.
   EQ.0:   No action is taken
   EQ.1:   Damage history is initiated based on values of initial plastic strains and initial strain tensor, this is to be used in multistage analyses.
















   ..
       !! processed by numpydoc !!

.. py:property:: deps
   :type: float


   
   Get or set the Plastic strain increment between evaluation of damage instability and evolution criteria. See remarks, the default is zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: numfip
   :type: float


   
   Get or set the Number or percentage of failed integration points prior to element deletion (default value is 1).
   GT.0.0: Number of integration points which must fail before element is deleted.
   LT.0.0: Applies only to shells. |NUMFIP| is the percentage of layers which must fail before element fails.
   For shell formulations with 4 integration points per layer, the layer is considered failed if any of the integration points in the layer fails
















   ..
       !! processed by numpydoc !!

.. py:property:: dityp
   :type: float


   
   Get or set the Damage initiation type
   EQ.0.0: Ductile based on stress triaxiality
   EQ.1.0: Shear
   EQ.2.0: MSFLD
   EQ.3.0: FLD
   EQ.4.0: Ductile based on normalized principal stress.
















   ..
       !! processed by numpydoc !!

.. py:property:: p1
   :type: Optional[float]


   
   Get or set the Damage initiation parameter
   DITYP.EQ.0.0:   Load curve/table ID representing plastic strain at onset of damage as function of stress triaxiality and optionally plastic strain rate.
   DITYP.EQ.1.0:   Load curve/table ID representing plastic strain at onset of damage as function of shear influence and optionally plastic strain rate.
   DITYP.EQ.2.0:   Load curve/table ID representing plastic strain at onset of damage as function of ratio of principal plastic strain rates and optionally plastic strain rate.
   DITYP.EQ.3.0:   Load curve/table ID representing plastic strain at onset of damage as function of ratio of principal plastic strain rates and optionally plastic strain rate.
   DITYP.EQ.4.0:   Load curve/table ID representing plastic strain at onset of damage as function of stress state parameter and optionally plastic strain rate..
















   ..
       !! processed by numpydoc !!

.. py:property:: p2
   :type: Optional[float]


   
   Get or set the Damage initiation parameter
   DITYP.EQ.0.0:   Not used
   DITYP.EQ.1.0:   Pressure influence parameter k_s
   DITYP.EQ.2.0:   Layer specification
   EQ.0:   Mid layer
   EQ.1:   Outer layer
   DITYP.EQ.3.0:   Layer specification
   EQ.0:   Mid layer
   EQ.1:   Outer layer
   DITYP.EQ.4.0:   Triaxiality influence parameter k_d.
















   ..
       !! processed by numpydoc !!

.. py:property:: p3
   :type: Optional[float]


   
   Get or set the Damage initiation parameter
   DITYP.EQ.0.0:   Not used
   DITYP.EQ.1.0:   Not used
   DITYP.EQ.2.0:   Initiation formulation
   EQ.0: Direct
   EQ.1: Incremental
   DITYP.EQ.3.0:   Initiation formulation
   EQ.0:   Direct
   EQ.1:   Incremental
   DITYP.EQ.4.0:   Not used.
















   ..
       !! processed by numpydoc !!

.. py:property:: p4
   :type: Optional[float]


   
   Get or set the Plane stress option for shell elements:
   EQ.0.0: transverse shear stresses σ_yz and σ_zx are included in the computation of stress invariants, such as the triaxiality.
   EQ.1.0 : transverse shear stresses σ_yz and σ_zx are not included in the computation of stress invariants, such as the triaxiality.Useful in combination with “plane stress” material models, where the transverse shear stresses are also excluded from the yield condition, e.g.,* MAT_024_2D or *MAT_036.
















   ..
       !! processed by numpydoc !!

.. py:property:: p5
   :type: Optional[float]


   
   Get or set the Load curve or table ID representing regularization factor as a function of the characteristic element size (curve) or regularization factor as a function of the characteristic element size and abscissa value of the criterion used (table).. This factor scales the plastic strain at the onset of damage defined with P1.
















   ..
       !! processed by numpydoc !!

.. py:property:: detyp
   :type: float


   
   Get or set the Damage evolution type
   EQ.0.0: Linear softening, evolution of damage is a function of the plastic displacement after the initiation of damage.
   EQ.1.0: Linear softening, evolution of damage is a function of the fracture energy after the initiation of damage.
















   ..
       !! processed by numpydoc !!

.. py:property:: dctyp
   :type: float


   
   Get or set the Damage composition option for multiple criteria
   EQ.-1.0:        Damage not coupled to stress
   EQ.0.0: Maximum
   EQ.1.0: Multiplicative.
















   ..
       !! processed by numpydoc !!

.. py:property:: q1
   :type: Optional[float]


   
   Get or set the Damage evolution parameter
   DETYP.EQ.0.0:   Plastic displacement at failure,u_f^p, a negative value corresponds to a table ID for u_f^p as a function of triaxiality and damage.
   DETYP.EQ.1.0:   Fracture energy at failure,G_f.
















   ..
       !! processed by numpydoc !!

.. py:property:: q2
   :type: Optional[float]


   
   Get or set the Set to 1.0 to output information to log files (messag and d3hsp) when an integration point fails.
















   ..
       !! processed by numpydoc !!

.. py:property:: q3
   :type: Optional[float]


   
   Get or set the Damage evolution parameter:
   DETYP.EQ.0.0:   Exponent, α, in nonlinear damage evolution law, activated when u_f^ p > 0 and α > 0.
   DETYP.EQ.1.0:   Not used.
















   ..
       !! processed by numpydoc !!

.. py:property:: q4
   :type: Optional[float]


   
   Get or set the Load curve or table ID representing regularization factor as a function of the characteristic element size (curve) or regularization factor as a function of the characteristic element size and plastic strain rate (table). This factor scales the damage evolution parameter Q1
















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
   :value: 'ADD_DAMAGE_DIEM'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





