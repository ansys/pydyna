





:class:`ElementShellBextPatch`
==============================


.. py:class:: element_shell_bext_patch.ElementShellBextPatch(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ELEMENT_SHELL_BEXT_PATCH keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ElementShellBextPatch

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~epid`
            - Get or set the Extraction patch element ID. A unique number has to be chosen.
          * - :py:attr:`~pid`
            - Get or set the Part ID. See *PART.
          * - :py:attr:`~nel`
            - Get or set the Number of Bezier elements in the patch.
          * - :py:attr:`~wfl`
            - Get or set the Flag for weighting factors of the control points.
          * - :py:attr:`~form`
            - Get or set the Shell formulation to be used.
          * - :py:attr:`~int_`
            - Get or set the In-plane numerical integration rule.
          * - :py:attr:`~nisr`
            - Get or set the Number of (automatically created) interpolation shell elements in the local r-direction of each Bezier element for visualization (post processing) and contact.
          * - :py:attr:`~niss`
            - Get or set the Number of (automatically created) interpolation shell elements in the local s-direction of each Bezier element for visualization (post processing) and contact.
          * - :py:attr:`~imass`
            - Get or set the Option for lumping of mass matrix.
          * - :py:attr:`~nl`
            - Get or set the Number of trimming loops
          * - :py:attr:`~shpe`
            - Get or set the The Bezier element shape
          * - :py:attr:`~pr`
            - Get or set the The Bezier element degree in the local r-direction.
          * - :py:attr:`~ps`
            - Get or set the The Bezier element degree in the local s-direction.
          * - :py:attr:`~bdry`
            - Get or set the A boolean indicating if the Bezier is on the patch boundary.
          * - :py:attr:`~trm`
            - Get or set the A boolean indicating if the Bezier element is trimmed.
          * - :py:attr:`~smth`
            - Get or set the A boolean indicating whether smoothness information will be specifed for the element.
          * - :py:attr:`~b1`
            - Get or set the The Bezier element (format A) borders which touch the patch boundary.
          * - :py:attr:`~b2`
            - Get or set the The Bezier element (format A) borders which touch the patch boundary..
          * - :py:attr:`~b3`
            - Get or set the The Bezier element (format A) borders which touch the patch boundary.
          * - :py:attr:`~b4`
            - Get or set the The Bezier element (format A) borders which touch the patch boundary..
          * - :py:attr:`~b5`
            - Get or set the The Bezier element (format A) borders which touch the patch boundary..
          * - :py:attr:`~b6`
            - Get or set the The Bezier element (format A) borders which touch the patch boundary.
          * - :py:attr:`~b7`
            - Get or set the The Bezier element (format A) borders which touch the patch boundary.
          * - :py:attr:`~b8`
            - Get or set the The Bezier element (format A) borders which touch the patch boundary.
          * - :py:attr:`~s1`
            - Get or set the The smoothness levels on each side of a Bezier element (format A)..
          * - :py:attr:`~s2`
            - Get or set the The smoothness levels on each side of a Bezier element (format A)..
          * - :py:attr:`~s3`
            - Get or set the The smoothness levels on each side of a Bezier element (format A).
          * - :py:attr:`~s4`
            - Get or set the The smoothness levels on each side of a Bezier element (format A)..
          * - :py:attr:`~s5`
            - Get or set the The smoothness levels on each side of a Bezier element (format A)..
          * - :py:attr:`~s6`
            - Get or set the The smoothness levels on each side of a Bezier element (format A).
          * - :py:attr:`~s7`
            - Get or set the The smoothness levels on each side of a Bezier element (format A).
          * - :py:attr:`~s8`
            - Get or set the The smoothness levels on each side of a Bezier element (format A).
          * - :py:attr:`~n1`
            - Get or set the Control points i (de
          * - :py:attr:`~n2`
            - Get or set the Control points i (de
          * - :py:attr:`~n3`
            - Get or set the Control points i (de
          * - :py:attr:`~n4`
            - Get or set the Control points i (de
          * - :py:attr:`~n5`
            - Get or set the Control points i (de
          * - :py:attr:`~n6`
            - Get or set the Control points i (de
          * - :py:attr:`~n7`
            - Get or set the Control points i (de
          * - :py:attr:`~n8`
            - Get or set the Control points i (de
          * - :py:attr:`~w1`
            - Get or set the Weighting factor of control point i.
          * - :py:attr:`~w2`
            - Get or set the Weighting factor of control point i.
          * - :py:attr:`~w3`
            - Get or set the Weighting factor of control point i
          * - :py:attr:`~w4`
            - Get or set the Weighting factor of control point i.
          * - :py:attr:`~w5`
            - Get or set the Weighting factor of control point i.
          * - :py:attr:`~w6`
            - Get or set the Weighting factor of control point i
          * - :py:attr:`~w7`
            - Get or set the Weighting factor of control point i
          * - :py:attr:`~w8`
            - Get or set the Weighting factor of control point i
          * - :py:attr:`~o1`
            - Get or set the The extraction operator values for a Bezier element.
          * - :py:attr:`~o2`
            - Get or set the The extraction operator values for a Bezier element.
          * - :py:attr:`~o3`
            - Get or set the The extraction operator values for a Bezier element
          * - :py:attr:`~o4`
            - Get or set the The extraction operator values for a Bezier element.
          * - :py:attr:`~o5`
            - Get or set the The extraction operator values for a Bezier element.
          * - :py:attr:`~o6`
            - Get or set the The extraction operator values for a Bezier element
          * - :py:attr:`~o7`
            - Get or set the The extraction operator values for a Bezier element
          * - :py:attr:`~o8`
            - Get or set the The extraction operator values for a Bezier element


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

    from element_shell_bext_patch import ElementShellBextPatch

Property detail
---------------

.. py:property:: epid
   :type: Optional[int]


   
   Get or set the Extraction patch element ID. A unique number has to be chosen.
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID. See *PART.
















   ..
       !! processed by numpydoc !!

.. py:property:: nel
   :type: Optional[int]


   
   Get or set the Number of Bezier elements in the patch.
















   ..
       !! processed by numpydoc !!

.. py:property:: wfl
   :type: Optional[int]


   
   Get or set the Flag for weighting factors of the control points.
   WFL = 0 : All weights at the control points are set to 1 and no optional        cards D are allowed.
   WFL != 0 : The weights at the control points are de
   ned in optional cards
   D which must be de
   ned after cards C..
















   ..
       !! processed by numpydoc !!

.. py:property:: form
   :type: int


   
   Get or set the Shell formulation to be used.
   FORM = 0 : Shear deformable shell theory with rotational DOFs.
   FORM = 1 : Shear deformable shell theory without rotational DOFs.
   FORM = 2 : Thin shell theory without rotational DOFs.
   FORM = 4 : Combination of FORM = 0 and FORM = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: int_
   :type: int


   
   Get or set the In-plane numerical integration rule.
   INT = 0 : Uniformly reduced Gauss integration, NIP = PR  PS. Note
   that the number of integration points may change from element
   to element depending on local element degree.
   INT = 1 : Full Gauss integration, NIP = (PR + 1)  (PS + 1). Note that
   the number of integration points may change from element to
   element depending on local element degree.
















   ..
       !! processed by numpydoc !!

.. py:property:: nisr
   :type: Optional[int]


   
   Get or set the Number of (automatically created) interpolation shell elements in the local r-direction of each Bezier element for visualization (post processing) and contact.
















   ..
       !! processed by numpydoc !!

.. py:property:: niss
   :type: Optional[int]


   
   Get or set the Number of (automatically created) interpolation shell elements in the local s-direction of each Bezier element for visualization (post processing) and contact.
















   ..
       !! processed by numpydoc !!

.. py:property:: imass
   :type: int


   
   Get or set the Option for lumping of mass matrix.
   IMASS = 0 : Row sum.
   IMASS = 1 : Diagonal weighting.
















   ..
       !! processed by numpydoc !!

.. py:property:: nl
   :type: Optional[int]


   
   Get or set the Number of trimming loops
   NL = 0 : No trimming loops (i.e., untrimmed U-spline).
   NL > 0 : Trimmed U-spline with NL trimming loops.
















   ..
       !! processed by numpydoc !!

.. py:property:: shpe
   :type: int


   
   Get or set the The Bezier element shape
   SHPE = 0 : Quadrilateral Bezier element.
   SHPE = 1 : Triangular Bezier element.
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: Optional[int]


   
   Get or set the The Bezier element degree in the local r-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: ps
   :type: Optional[int]


   
   Get or set the The Bezier element degree in the local s-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: bdry
   :type: int


   
   Get or set the A boolean indicating if the Bezier is on the patch boundary.
















   ..
       !! processed by numpydoc !!

.. py:property:: trm
   :type: int


   
   Get or set the A boolean indicating if the Bezier element is trimmed.
















   ..
       !! processed by numpydoc !!

.. py:property:: smth
   :type: int


   
   Get or set the A boolean indicating whether smoothness information will be specifed for the element.
















   ..
       !! processed by numpydoc !!

.. py:property:: b1
   :type: int


   
   Get or set the The Bezier element (format A) borders which touch the patch boundary.
















   ..
       !! processed by numpydoc !!

.. py:property:: b2
   :type: int


   
   Get or set the The Bezier element (format A) borders which touch the patch boundary..
















   ..
       !! processed by numpydoc !!

.. py:property:: b3
   :type: int


   
   Get or set the The Bezier element (format A) borders which touch the patch boundary.
















   ..
       !! processed by numpydoc !!

.. py:property:: b4
   :type: int


   
   Get or set the The Bezier element (format A) borders which touch the patch boundary..
















   ..
       !! processed by numpydoc !!

.. py:property:: b5
   :type: int


   
   Get or set the The Bezier element (format A) borders which touch the patch boundary..
















   ..
       !! processed by numpydoc !!

.. py:property:: b6
   :type: int


   
   Get or set the The Bezier element (format A) borders which touch the patch boundary.
















   ..
       !! processed by numpydoc !!

.. py:property:: b7
   :type: int


   
   Get or set the The Bezier element (format A) borders which touch the patch boundary.
















   ..
       !! processed by numpydoc !!

.. py:property:: b8
   :type: int


   
   Get or set the The Bezier element (format A) borders which touch the patch boundary.
















   ..
       !! processed by numpydoc !!

.. py:property:: s1
   :type: int


   
   Get or set the The smoothness levels on each side of a Bezier element (format A)..
















   ..
       !! processed by numpydoc !!

.. py:property:: s2
   :type: int


   
   Get or set the The smoothness levels on each side of a Bezier element (format A)..
















   ..
       !! processed by numpydoc !!

.. py:property:: s3
   :type: int


   
   Get or set the The smoothness levels on each side of a Bezier element (format A).
















   ..
       !! processed by numpydoc !!

.. py:property:: s4
   :type: int


   
   Get or set the The smoothness levels on each side of a Bezier element (format A)..
















   ..
       !! processed by numpydoc !!

.. py:property:: s5
   :type: int


   
   Get or set the The smoothness levels on each side of a Bezier element (format A)..
















   ..
       !! processed by numpydoc !!

.. py:property:: s6
   :type: int


   
   Get or set the The smoothness levels on each side of a Bezier element (format A).
















   ..
       !! processed by numpydoc !!

.. py:property:: s7
   :type: int


   
   Get or set the The smoothness levels on each side of a Bezier element (format A).
















   ..
       !! processed by numpydoc !!

.. py:property:: s8
   :type: int


   
   Get or set the The smoothness levels on each side of a Bezier element (format A).
















   ..
       !! processed by numpydoc !!

.. py:property:: n1
   :type: int


   
   Get or set the Control points i (de
   ned via *NODE) which de
   ne the Bezier element (format  A)..
















   ..
       !! processed by numpydoc !!

.. py:property:: n2
   :type: int


   
   Get or set the Control points i (de
   ned via *NODE) which de
   ne the Bezier element (format  A).
















   ..
       !! processed by numpydoc !!

.. py:property:: n3
   :type: int


   
   Get or set the Control points i (de
   ned via *NODE) which de
   ne the Bezier element (format  A)
















   ..
       !! processed by numpydoc !!

.. py:property:: n4
   :type: int


   
   Get or set the Control points i (de
   ned via *NODE) which de
   ne the Bezier element (format  A).
















   ..
       !! processed by numpydoc !!

.. py:property:: n5
   :type: int


   
   Get or set the Control points i (de
   ned via *NODE) which de
   ne the Bezier element (format  A).
















   ..
       !! processed by numpydoc !!

.. py:property:: n6
   :type: int


   
   Get or set the Control points i (de
   ned via *NODE) which de
   ne the Bezier element (format  A)
















   ..
       !! processed by numpydoc !!

.. py:property:: n7
   :type: int


   
   Get or set the Control points i (de
   ned via *NODE) which de
   ne the Bezier element (format  A)
















   ..
       !! processed by numpydoc !!

.. py:property:: n8
   :type: int


   
   Get or set the Control points i (de
   ned via *NODE) which de
   ne the Bezier element (format  A)
















   ..
       !! processed by numpydoc !!

.. py:property:: w1
   :type: float


   
   Get or set the Weighting factor of control point i.
















   ..
       !! processed by numpydoc !!

.. py:property:: w2
   :type: float


   
   Get or set the Weighting factor of control point i.
















   ..
       !! processed by numpydoc !!

.. py:property:: w3
   :type: float


   
   Get or set the Weighting factor of control point i
















   ..
       !! processed by numpydoc !!

.. py:property:: w4
   :type: float


   
   Get or set the Weighting factor of control point i.
















   ..
       !! processed by numpydoc !!

.. py:property:: w5
   :type: float


   
   Get or set the Weighting factor of control point i.
















   ..
       !! processed by numpydoc !!

.. py:property:: w6
   :type: float


   
   Get or set the Weighting factor of control point i
















   ..
       !! processed by numpydoc !!

.. py:property:: w7
   :type: float


   
   Get or set the Weighting factor of control point i
















   ..
       !! processed by numpydoc !!

.. py:property:: w8
   :type: float


   
   Get or set the Weighting factor of control point i
















   ..
       !! processed by numpydoc !!

.. py:property:: o1
   :type: float


   
   Get or set the The extraction operator values for a Bezier element.
















   ..
       !! processed by numpydoc !!

.. py:property:: o2
   :type: float


   
   Get or set the The extraction operator values for a Bezier element.
















   ..
       !! processed by numpydoc !!

.. py:property:: o3
   :type: float


   
   Get or set the The extraction operator values for a Bezier element
















   ..
       !! processed by numpydoc !!

.. py:property:: o4
   :type: float


   
   Get or set the The extraction operator values for a Bezier element.
















   ..
       !! processed by numpydoc !!

.. py:property:: o5
   :type: float


   
   Get or set the The extraction operator values for a Bezier element.
















   ..
       !! processed by numpydoc !!

.. py:property:: o6
   :type: float


   
   Get or set the The extraction operator values for a Bezier element
















   ..
       !! processed by numpydoc !!

.. py:property:: o7
   :type: float


   
   Get or set the The extraction operator values for a Bezier element
















   ..
       !! processed by numpydoc !!

.. py:property:: o8
   :type: float


   
   Get or set the The extraction operator values for a Bezier element
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ELEMENT'


.. py:attribute:: subkeyword
   :value: 'SHELL_BEXT_PATCH'






