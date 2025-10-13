





:class:`SectionIgaShell`
========================


.. py:class:: section_iga_shell.SectionIgaShell(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SECTION_IGA_SHELL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SectionIgaShell

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~secid`
            - Get or set the Section ID. SECID is referenced on the *PART card and must be unique.
          * - :py:attr:`~elform`
            - Get or set the Element formulation
          * - :py:attr:`~shrf`
            - Get or set the Shear correction factor which scales the transverse shear stress, see Remark 1.
          * - :py:attr:`~nip`
            - Get or set the Number of through thickness integration points, see Remark 2.
          * - :py:attr:`~irl`
            - Get or set the Lamina integration rule
          * - :py:attr:`~qr_irid`
            - Get or set the Fiber quadrature rule or fiber integration rule ID, see *INTEGRATION_SHELL.
          * - :py:attr:`~icomp`
            - Get or set the Flag for anisotropic layered composite material model, see Remark 3.
          * - :py:attr:`~t`
            - Get or set the Shell thickness.
          * - :py:attr:`~nloc`
            - Get or set the Location of reference surface, see Remark 4.
          * - :py:attr:`~b1`
            - Get or set the Material angle at the ith fiber integration point.
          * - :py:attr:`~b2`
            - Get or set the Material angle at the ith fiber integration point.
          * - :py:attr:`~b3`
            - Get or set the Material angle at the ith fiber integration point.
          * - :py:attr:`~b4`
            - Get or set the Material angle at the ith fiber integration point.
          * - :py:attr:`~b5`
            - Get or set the Material angle at the ith fiber integration point.
          * - :py:attr:`~b6`
            - Get or set the Material angle at the ith fiber integration point.
          * - :py:attr:`~b7`
            - Get or set the Material angle at the ith fiber integration point.
          * - :py:attr:`~b8`
            - Get or set the Material angle at the ith fiber integration point.
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

    from section_iga_shell import SectionIgaShell

Property detail
---------------

.. py:property:: secid
   :type: Optional[int]


   
   Get or set the Section ID. SECID is referenced on the *PART card and must be unique.
















   ..
       !! processed by numpydoc !!

.. py:property:: elform
   :type: int


   
   Get or set the Element formulation
   EQ.0: Reissner - Mindlin with fibers at the control points
   EQ.1 : Kirchhoff - Love with fibers at the control points
   EQ.2 : Kirchhoff - Love with fibers at the integration points
   EQ.3 : Reissner - Mindlin with fibers at the integration points.
   EQ.5:   Thick shell with thickness stretch based on the ELFORM = 0. See Remark 1.
   EQ.6:   Thick shell with thickness stretch based on ELFORM = 3. See Remark 1
















   ..
       !! processed by numpydoc !!

.. py:property:: shrf
   :type: float


   
   Get or set the Shear correction factor which scales the transverse shear stress, see Remark 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: nip
   :type: int


   
   Get or set the Number of through thickness integration points, see Remark 2.
   GT.0.0: Number of quadrature points(up to 10 points).
















   ..
       !! processed by numpydoc !!

.. py:property:: irl
   :type: int


   
   Get or set the Lamina integration rule
   EQ.0: Reduced Gauss - Legendre
   EQ.1 : Gauss - Legendre
   EQ.2 : Patchwise reduced Gauss - Legendre(for biquadratic NURBS only).
















   ..
       !! processed by numpydoc !!

.. py:property:: qr_irid
   :type: float


   
   Get or set the Fiber quadrature rule or fiber integration rule ID, see *INTEGRATION_SHELL.
   LT.0.0: Absolute value is specified rule number.
   EQ.0.0 : Gauss - Legendre / Gauss - Lobatto(up to 10 points)
   EQ.1.0 : Trapezoidal, not recommended for accuracy reasons.
















   ..
       !! processed by numpydoc !!

.. py:property:: icomp
   :type: int


   
   Get or set the Flag for anisotropic layered composite material model, see Remark 3.
   EQ.1: A material angle in degrees is defined for each through
   thickness integration point.Thus, each layer has one integration point.
















   ..
       !! processed by numpydoc !!

.. py:property:: t
   :type: float


   
   Get or set the Shell thickness.
















   ..
       !! processed by numpydoc !!

.. py:property:: nloc
   :type: float


   
   Get or set the Location of reference surface, see Remark 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: b1
   :type: Optional[float]


   
   Get or set the Material angle at the ith fiber integration point.
















   ..
       !! processed by numpydoc !!

.. py:property:: b2
   :type: Optional[float]


   
   Get or set the Material angle at the ith fiber integration point.
















   ..
       !! processed by numpydoc !!

.. py:property:: b3
   :type: Optional[float]


   
   Get or set the Material angle at the ith fiber integration point.
















   ..
       !! processed by numpydoc !!

.. py:property:: b4
   :type: Optional[float]


   
   Get or set the Material angle at the ith fiber integration point.
















   ..
       !! processed by numpydoc !!

.. py:property:: b5
   :type: Optional[float]


   
   Get or set the Material angle at the ith fiber integration point.
















   ..
       !! processed by numpydoc !!

.. py:property:: b6
   :type: Optional[float]


   
   Get or set the Material angle at the ith fiber integration point.
















   ..
       !! processed by numpydoc !!

.. py:property:: b7
   :type: Optional[float]


   
   Get or set the Material angle at the ith fiber integration point.
















   ..
       !! processed by numpydoc !!

.. py:property:: b8
   :type: Optional[float]


   
   Get or set the Material angle at the ith fiber integration point.
















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
   :value: 'SECTION'


.. py:attribute:: subkeyword
   :value: 'IGA_SHELL'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





