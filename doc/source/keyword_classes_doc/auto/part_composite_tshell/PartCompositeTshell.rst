





:class:`PartCompositeTshell`
============================


.. py:class:: part_composite_tshell.PartCompositeTshell(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA PART_COMPOSITE_TSHELL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: PartCompositeTshell

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
            - Get or set the Element formulation:
          * - :py:attr:`~shrf`
            - Get or set the Shear correction factor which scales the transverse shear stress.  The shell formulations in LS-DYNA, with the exception of the BCIZ and DK elements, are based on a first order shear deformation theory that yields constant transverse shear strains which violates the condition of zero traction on the top and bottom surfaces of the shell.  The shear correction factor is attempt to compensate for this error.
          * - :py:attr:`~hgid`
            - Get or set the Hourglass/bulk viscosity identification defined in the *HOURGLASS Section:
          * - :py:attr:`~tshear`
            - Get or set the Flag for transverse shear strain distribution (see remarks 3 and 4):
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

    from part_composite_tshell import PartCompositeTshell

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


   
   Get or set the Element formulation:
   EQ.1: one point reduced integration (default),
   EQ.2: selective reduced 2x2 in plane integration.
   EQ.3: assumed strain 2x2 in plane integration.
   EQ.5:  assumed strain reduced integration.
















   ..
       !! processed by numpydoc !!

.. py:property:: shrf
   :type: float


   
   Get or set the Shear correction factor which scales the transverse shear stress.  The shell formulations in LS-DYNA, with the exception of the BCIZ and DK elements, are based on a first order shear deformation theory that yields constant transverse shear strains which violates the condition of zero traction on the top and bottom surfaces of the shell.  The shear correction factor is attempt to compensate for this error.
















   ..
       !! processed by numpydoc !!

.. py:property:: hgid
   :type: int


   
   Get or set the Hourglass/bulk viscosity identification defined in the *HOURGLASS Section:
   EQ.0:  default values are used..
















   ..
       !! processed by numpydoc !!

.. py:property:: tshear
   :type: int


   
   Get or set the Flag for transverse shear strain distribution (see remarks 3 and 4):
   EQ.0: Parabolic,
   EQ.1: Constant through thickness
















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
   :value: 'COMPOSITE_TSHELL'






