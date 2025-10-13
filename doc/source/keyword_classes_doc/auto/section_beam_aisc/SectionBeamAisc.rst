





:class:`SectionBeamAisc`
========================


.. py:class:: section_beam_aisc.SectionBeamAisc(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SECTION_BEAM_AISC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SectionBeamAisc

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~secid`
            - Get or set the Section ID.  SECID is referenced on the *PART card.  A unique number or label not exceeding 8 characters must be specified.
          * - :py:attr:`~label`
            - Get or set the AISC section label.
          * - :py:attr:`~elform`
            - Get or set the Element formulation.
          * - :py:attr:`~shrf`
            - Get or set the Shear factor.
          * - :py:attr:`~nsm`
            - Get or set the Non-structural mass per unit length.
          * - :py:attr:`~lfac`
            - Get or set the GT.0.0: Length scale factor to convert dimensions from standard units
          * - :py:attr:`~nsloc`
            - Get or set the Location of reference surface (see *SECTION_BEAM).
          * - :py:attr:`~ntloc`
            - Get or set the Location of reference surface (see *SECTION_BEAM).
          * - :py:attr:`~k`
            - Get or set the Integration refinement parameter (see *INTEGRATION_BEAM).
          * - :py:attr:`~rampt`
            - Get or set the Optional ramp-up time (see *SECTION_BEAM).
          * - :py:attr:`~stress`
            - Get or set the Optional initial stress (see *SECTION_BEAM).
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

    from section_beam_aisc import SectionBeamAisc

Property detail
---------------

.. py:property:: secid
   :type: Optional[int]


   
   Get or set the Section ID.  SECID is referenced on the *PART card.  A unique number or label not exceeding 8 characters must be specified.
















   ..
       !! processed by numpydoc !!

.. py:property:: label
   :type: Optional[str]


   
   Get or set the AISC section label.
















   ..
       !! processed by numpydoc !!

.. py:property:: elform
   :type: int


   
   Get or set the Element formulation.
















   ..
       !! processed by numpydoc !!

.. py:property:: shrf
   :type: float


   
   Get or set the Shear factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsm
   :type: float


   
   Get or set the Non-structural mass per unit length.
















   ..
       !! processed by numpydoc !!

.. py:property:: lfac
   :type: float


   
   Get or set the GT.0.0: Length scale factor to convert dimensions from standard units
   If LFAC < 0, then a predefined length factor for specific model units is used:
   EQ.-1.0: ft
   EQ.-2.0: m
   EQ.-3.0: in
   EQ.-4.0: mm
   EQ.-5.0: cm.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsloc
   :type: float


   
   Get or set the Location of reference surface (see *SECTION_BEAM).
















   ..
       !! processed by numpydoc !!

.. py:property:: ntloc
   :type: float


   
   Get or set the Location of reference surface (see *SECTION_BEAM).
















   ..
       !! processed by numpydoc !!

.. py:property:: k
   :type: int


   
   Get or set the Integration refinement parameter (see *INTEGRATION_BEAM).
















   ..
       !! processed by numpydoc !!

.. py:property:: rampt
   :type: float


   
   Get or set the Optional ramp-up time (see *SECTION_BEAM).
















   ..
       !! processed by numpydoc !!

.. py:property:: stress
   :type: float


   
   Get or set the Optional initial stress (see *SECTION_BEAM).
















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
   :value: 'BEAM_AISC'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





