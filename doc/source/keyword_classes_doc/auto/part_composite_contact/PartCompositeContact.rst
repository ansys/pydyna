





:class:`PartCompositeContact`
=============================


.. py:class:: part_composite_contact.PartCompositeContact(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA PART_COMPOSITE_CONTACT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: PartCompositeContact

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~title`
            - Get or set the Heading for the part.
          * - :py:attr:`~pid`
            - Get or set the Part ID.
          * - :py:attr:`~elform`
            - Get or set the Element formulation options, see Remarks 1 and 2 below:
          * - :py:attr:`~shrf`
            - Get or set the Shear correction factor which scales the transverse shear stress.  The shell formulations in LS-DYNA, with the exception of the BCIZ and DK elements, are based on a first order shear deformation theory that yields constant transverse shear strains which violates the condition of zero traction on the top and bottom surfaces of the shell.  The shear correction factor is attempt to compensate for this error.
          * - :py:attr:`~nloc`
            - Get or set the Location of reference surface for three dimensional shell elements.  If nonzero, the mid-surface of the shell is offset by a value equal to  .  Alternatively, the offset can be specified by using the OFFSET option in the *ELEMENT_SHELL input section.
          * - :py:attr:`~marea`
            - Get or set the Non-structural mass per unit area.  This is additional mass which comes from materials such as carpeting.  This mass is not directly included in the time step calculation.
          * - :py:attr:`~hgid`
            - Get or set the Hourglass/bulk viscosity identification defined in the *HOURGLASS Section:
          * - :py:attr:`~adpopt`
            - Get or set the Indicate if this part is adapted or not. See also *CONTROL_ADAPTIVITY.
          * - :py:attr:`~thshel`
            - Get or set the Thermal shell formulation:
          * - :py:attr:`~fs`
            - Get or set the Static coefficient of friction.  The functional coefficient is assumed to be dependent on the relative velocity vrel of the surfaces in contact
          * - :py:attr:`~fd`
            - Get or set the Dynamic coefficient of friction.  The functional coefficient is assumed to be dependent on the relative velocity vrel of the surfaces in contact
          * - :py:attr:`~dc`
            - Get or set the Exponential decay coefficient.  The functional coefficient is assumed to be dependent on the relative velocity vrel of the surfaces in contact .
          * - :py:attr:`~vc`
            - Get or set the Coefficient for viscous friction.  This is necessary to limit the friction force to a maximum.  A limiting force is computed  .  Acont being the area of the segment contacted by the node in contact.  The suggested value for VC is to use the yield stress in shear   where  o is the yield stress of the contacted material.
          * - :py:attr:`~optt`
            - Get or set the Optional contact thickness. For SOFT = 2, it applies to solids, shells and beams. For SOFT = 0 and 1 and for Mortar contacts, it applies to shells and beams only. For SOFT = 0 and 1 with the MPP version, OPTT has a different meaning for solid elements. In this case, OPTT overrides the thickness of solid elements used for the calculation of the contact penetration release (see Table Error! Reference source not found.), but it does not affect the contact thickness
          * - :py:attr:`~sft`
            - Get or set the Optional thickness scale factor for PART ID in automatic contact (scales true thickness).  This option applies only to contact with shell elements.  True thickness is the element thickness of the shell elements.
          * - :py:attr:`~ssf`
            - Get or set the Scale factor on default slave penalty stiffness for this PART ID whenever it appears in the contact definition.  If zero, SSF is taken as unity.
          * - :py:attr:`~cparm8`
            - Get or set the Flag to exclude beam-to-beam contact from the same PID for CONTACT_‌AUTOMATIC_‌GENERAL.  This applies only to MPP.  Global default may be set using CPARM8 on *CONTACT_‌…_MPP Optional Card.
          * - :py:attr:`~mid1`
            - Get or set the Material ID of integration point i, see *MAT_? Section
          * - :py:attr:`~thick1`
            - Get or set the Thickness of integration point .
          * - :py:attr:`~b1`
            - Get or set the Material angle of integration point i.
          * - :py:attr:`~tmid1`
            - Get or set the Thermal ID
          * - :py:attr:`~mid2`
            - Get or set the Material ID of integration point i, see *MAT_? Section
          * - :py:attr:`~thick2`
            - Get or set the Thickness of integration point
          * - :py:attr:`~b2`
            - Get or set the Material angle of integration point i
          * - :py:attr:`~tmid2`
            - Get or set the Thermal ID


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~keyword`
            - 
          * - :py:attr:`~subkeyword`
            - 






Import detail
-------------

.. code-block:: python

    from part_composite_contact import PartCompositeContact

Property detail
---------------

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Heading for the part.
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: elform
   :type: int


   
   Get or set the Element formulation options, see Remarks 1 and 2 below:
   EQ.1:  Hughes-Liu,
   EQ.2:  Belytschko-Tsay,
   EQ.3:  BCIZ triangular shell,
   EQ.4:  C0 triangular shell,
   EQ.6:  S/R Hughes-Liu,
   EQ.7:   S/R co-rotational Hughes-Liu,
   EQ.8:   Belytschko-Leviathan shell,
   EQ.9:   Fully integrated Belytschko-Tsay membrane,
   EQ.10: Belytschko-Wong-Chiang,
   EQ.11: Fast (co-rotational) Hughes-Liu,
   EQ.16:  Fully integrated shell element (very fast)
   EQ.-16: Fully integrated shell element modified for higher accuracy
















   ..
       !! processed by numpydoc !!

.. py:property:: shrf
   :type: Optional[float]


   
   Get or set the Shear correction factor which scales the transverse shear stress.  The shell formulations in LS-DYNA, with the exception of the BCIZ and DK elements, are based on a first order shear deformation theory that yields constant transverse shear strains which violates the condition of zero traction on the top and bottom surfaces of the shell.  The shear correction factor is attempt to compensate for this error.
















   ..
       !! processed by numpydoc !!

.. py:property:: nloc
   :type: float


   
   Get or set the Location of reference surface for three dimensional shell elements.  If nonzero, the mid-surface of the shell is offset by a value equal to  .  Alternatively, the offset can be specified by using the OFFSET option in the *ELEMENT_SHELL input section.
   EQ. 1.0:  top surface,
   EQ. 0.0:  mid-surface (default),
   EQ.-1.0:  bottom surface..
















   ..
       !! processed by numpydoc !!

.. py:property:: marea
   :type: float


   
   Get or set the Non-structural mass per unit area.  This is additional mass which comes from materials such as carpeting.  This mass is not directly included in the time step calculation.
















   ..
       !! processed by numpydoc !!

.. py:property:: hgid
   :type: int


   
   Get or set the Hourglass/bulk viscosity identification defined in the *HOURGLASS Section:
   EQ.0:  default values are used..
















   ..
       !! processed by numpydoc !!

.. py:property:: adpopt
   :type: int


   
   Get or set the Indicate if this part is adapted or not. See also *CONTROL_ADAPTIVITY.
   EQ.0: no adaptivity (default),
   EQ.1: H-adaptive for 3D shells,
   EQ.2: R-adaptive remeshing for 2D shells.
















   ..
       !! processed by numpydoc !!

.. py:property:: thshel
   :type: int


   
   Get or set the Thermal shell formulation:
           EQ.0 Default
   EQ.1 Thick thermal shell
           EQ. 2 Thin thermal shell
















   ..
       !! processed by numpydoc !!

.. py:property:: fs
   :type: Optional[float]


   
   Get or set the Static coefficient of friction.  The functional coefficient is assumed to be dependent on the relative velocity vrel of the surfaces in contact
















   ..
       !! processed by numpydoc !!

.. py:property:: fd
   :type: Optional[float]


   
   Get or set the Dynamic coefficient of friction.  The functional coefficient is assumed to be dependent on the relative velocity vrel of the surfaces in contact
















   ..
       !! processed by numpydoc !!

.. py:property:: dc
   :type: Optional[float]


   
   Get or set the Exponential decay coefficient.  The functional coefficient is assumed to be dependent on the relative velocity vrel of the surfaces in contact .
















   ..
       !! processed by numpydoc !!

.. py:property:: vc
   :type: Optional[float]


   
   Get or set the Coefficient for viscous friction.  This is necessary to limit the friction force to a maximum.  A limiting force is computed  .  Acont being the area of the segment contacted by the node in contact.  The suggested value for VC is to use the yield stress in shear   where  o is the yield stress of the contacted material.
















   ..
       !! processed by numpydoc !!

.. py:property:: optt
   :type: Optional[float]


   
   Get or set the Optional contact thickness. For SOFT = 2, it applies to solids, shells and beams. For SOFT = 0 and 1 and for Mortar contacts, it applies to shells and beams only. For SOFT = 0 and 1 with the MPP version, OPTT has a different meaning for solid elements. In this case, OPTT overrides the thickness of solid elements used for the calculation of the contact penetration release (see Table Error! Reference source not found.), but it does not affect the contact thickness
















   ..
       !! processed by numpydoc !!

.. py:property:: sft
   :type: Optional[float]


   
   Get or set the Optional thickness scale factor for PART ID in automatic contact (scales true thickness).  This option applies only to contact with shell elements.  True thickness is the element thickness of the shell elements.
















   ..
       !! processed by numpydoc !!

.. py:property:: ssf
   :type: Optional[float]


   
   Get or set the Scale factor on default slave penalty stiffness for this PART ID whenever it appears in the contact definition.  If zero, SSF is taken as unity.
















   ..
       !! processed by numpydoc !!

.. py:property:: cparm8
   :type: Optional[float]


   
   Get or set the Flag to exclude beam-to-beam contact from the same PID for CONTACT_‌AUTOMATIC_‌GENERAL.  This applies only to MPP.  Global default may be set using CPARM8 on *CONTACT_‌…_MPP Optional Card.
   EQ.0:   Flag is not set(default).
   EQ.1 : Flag is set.
   EQ.2 : Flag is set.CPARM8 = 2 has the additional effect of permitting contact treatment of spot weld(type 9) beams in AUTOMATIC_‌GENERAL contacts; spot weld beams are otherwise disregarded entirely by AUTOMATIC_‌GENERAL contacts.
















   ..
       !! processed by numpydoc !!

.. py:property:: mid1
   :type: Optional[int]


   
   Get or set the Material ID of integration point i, see *MAT_? Section
















   ..
       !! processed by numpydoc !!

.. py:property:: thick1
   :type: Optional[float]


   
   Get or set the Thickness of integration point .
















   ..
       !! processed by numpydoc !!

.. py:property:: b1
   :type: Optional[float]


   
   Get or set the Material angle of integration point i.
















   ..
       !! processed by numpydoc !!

.. py:property:: tmid1
   :type: Optional[int]


   
   Get or set the Thermal ID
















   ..
       !! processed by numpydoc !!

.. py:property:: mid2
   :type: Optional[int]


   
   Get or set the Material ID of integration point i, see *MAT_? Section
















   ..
       !! processed by numpydoc !!

.. py:property:: thick2
   :type: Optional[float]


   
   Get or set the Thickness of integration point
















   ..
       !! processed by numpydoc !!

.. py:property:: b2
   :type: Optional[float]


   
   Get or set the Material angle of integration point i
















   ..
       !! processed by numpydoc !!

.. py:property:: tmid2
   :type: Optional[int]


   
   Get or set the Thermal ID
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'PART'


.. py:attribute:: subkeyword
   :value: 'COMPOSITE_CONTACT'






