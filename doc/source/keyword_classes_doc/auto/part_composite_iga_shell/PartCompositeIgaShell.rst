





:class:`PartCompositeIgaShell`
==============================


.. py:class:: part_composite_iga_shell.PartCompositeIgaShell(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA PART_COMPOSITE_IGA_SHELL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: PartCompositeIgaShell

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
            - Get or set the Element formulation options for IGA shells:
          * - :py:attr:`~shrf`
            - Get or set the Shear correction factor which scales the transverse shear stress.  The shell formulations in LS-DYNA, with the exception of the BCIZ and DK elements, are based on a first order shear deformation theory that yields constant transverse shear strains which violates the condition of zero traction on the top and bottom surfaces of the shell.  The shear correction factor is attempt to compensate for this error.
          * - :py:attr:`~nloc`
            - Get or set the Location of reference surface; see the definition of NLOC in *SECTION_IGA_SHELL for more detail
          * - :py:attr:`~irl`
            - Get or set the Lamina integration rule:
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

    from part_composite_iga_shell import PartCompositeIgaShell

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


   
   Get or set the Element formulation options for IGA shells:
   EQ.0: Reissner - Mindlin with fibers at the control points
   EQ.1 : Kirchhoff - Love with fibers at the control points
   EQ.2 : Kirchhoff - Love with fibers at the integration point
   EQ.3 : Reissner - Mindlin with fibers at the integration poin
















   ..
       !! processed by numpydoc !!

.. py:property:: shrf
   :type: Optional[float]


   
   Get or set the Shear correction factor which scales the transverse shear stress.  The shell formulations in LS-DYNA, with the exception of the BCIZ and DK elements, are based on a first order shear deformation theory that yields constant transverse shear strains which violates the condition of zero traction on the top and bottom surfaces of the shell.  The shear correction factor is attempt to compensate for this error.
















   ..
       !! processed by numpydoc !!

.. py:property:: nloc
   :type: float


   
   Get or set the Location of reference surface; see the definition of NLOC in *SECTION_IGA_SHELL for more detail
















   ..
       !! processed by numpydoc !!

.. py:property:: irl
   :type: int


   
   Get or set the Lamina integration rule:
   EQ.0: Reduced Gauss - Legendre
   EQ.1 : Gauss - Legendre
   EQ.2 : Patchwise reduced Gauss - Legendre(for biquadraticNURBS only)
















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
   :value: 'COMPOSITE_IGA_SHELL'






