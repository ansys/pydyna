





:class:`PartComposite`
======================


.. py:class:: part_composite.PartComposite(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA PART_COMPOSITE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: PartComposite

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

    from part_composite import PartComposite

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
   :value: 'COMPOSITE'






