





:class:`SectionTShell`
======================


.. py:class:: section_tshell.SectionTShell(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SECTION_TSHELL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SectionTShell

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
            - Get or set the Element formulation:
          * - :py:attr:`~shrf`
            - Get or set the Shear factor (default =1.0). A value of 5/6 is recommended.
          * - :py:attr:`~nip`
            - Get or set the Number of through shell thickness integration points (default = 2).
          * - :py:attr:`~propt`
            - Get or set the Printout option:
          * - :py:attr:`~qr`
            - Get or set the Quadrature rule:
          * - :py:attr:`~icomp`
            - Get or set the Flag for layered composite material mode:
          * - :py:attr:`~tshear`
            - Get or set the Flag for transverse shear strain or stress distribution (see Remarks 4 and 5):
          * - :py:attr:`~bi`
            - dynamic array of beta-i: material angle at ith-integration point..
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

    from section_tshell import SectionTShell

Property detail
---------------

.. py:property:: secid
   :type: Optional[int]


   
   Get or set the Section ID. SECID is referenced on the *PART card and must be unique.
















   ..
       !! processed by numpydoc !!

.. py:property:: elform
   :type: int


   
   Get or set the Element formulation:
   EQ.1: one point reduced integration (default),
   EQ.2: selective reduced 2x2 in plane integration.
   EQ.3: assumed strain 2x2 in plane integration.
   EQ.5:  assumed strain reduced integration.
   EQ.6: assumed strain reduced integration with shell materials
   EQ.7:   assumed strain  2×2 in plane integration
















   ..
       !! processed by numpydoc !!

.. py:property:: shrf
   :type: float


   
   Get or set the Shear factor (default =1.0). A value of 5/6 is recommended.
















   ..
       !! processed by numpydoc !!

.. py:property:: nip
   :type: int


   
   Get or set the Number of through shell thickness integration points (default = 2).
















   ..
       !! processed by numpydoc !!

.. py:property:: propt
   :type: float


   
   Get or set the Printout option:
   EQ.1.0: average resultants and fiber lengths (default),
   EQ.2.0: resultants at plan points and fiber lengths,
   EQ.3.0: resultants, stresses at all points, fiber lengths.
















   ..
       !! processed by numpydoc !!

.. py:property:: qr
   :type: int


   
   Get or set the Quadrature rule:
   LT.0: absolute value is specified rule number,
   EQ.0: Gauss (up to five points are permitted),
   EQ.1: trapezoidal, not recommended for accuracy reasons.
















   ..
       !! processed by numpydoc !!

.. py:property:: icomp
   :type: int


   
   Get or set the Flag for layered composite material mode:
   EQ.0: Flag turned off (default),
   EQ.1: a material angle is defined for each through thickness integration point . For each layer one integration point is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: tshear
   :type: int


   
   Get or set the Flag for transverse shear strain or stress distribution (see Remarks 4 and 5):
   EQ.0.0: Parabolic,
   EQ.1.0: Constant through thickness.
















   ..
       !! processed by numpydoc !!

.. py:property:: bi
   :type: ansys.dyna.core.lib.series_card.SeriesCard


   
   dynamic array of beta-i: material angle at ith-integration point..
















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
   :value: 'TSHELL'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





