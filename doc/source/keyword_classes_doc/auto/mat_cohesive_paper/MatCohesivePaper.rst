





:class:`MatCohesivePaper`
=========================


.. py:class:: mat_cohesive_paper.MatCohesivePaper(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_COHESIVE_PAPER keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatCohesivePaper

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number or label must be specified.
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~roflg`
            - Get or set the Flag for whether density is specified per unit area or volume:
          * - :py:attr:`~intfail`
            - Get or set the The number of integration points required for the cohesive element to be deleted. The value of INTFAIL may range from 1 to 4 with 1 the recommended value.
          * - :py:attr:`~en0`
            - Get or set the The initial tensile stiffness (units of stress / length) normal to the
          * - :py:attr:`~et0`
            - Get or set the The initial stiffness (units of stress / length) tangential to the plane
          * - :py:attr:`~en1`
            - Get or set the The final tensile stiffness (units of stress / length) normal to the
          * - :py:attr:`~et1`
            - Get or set the The final stiffness (units of stress / length) tangential to the plane of
          * - :py:attr:`~t0n`
            - Get or set the Peak tensile traction in normal direction.
          * - :py:attr:`~dn`
            - Get or set the Scale factor (unit of length).
          * - :py:attr:`~t1n`
            - Get or set the Final tensile traction in normal direction.
          * - :py:attr:`~t0t`
            - Get or set the Peak tensile traction in tangential direction. If negative, the absolute
          * - :py:attr:`~dt`
            - Get or set the Scale factor (unit of length). If negative, the absolute value indicates
          * - :py:attr:`~t1t`
            - Get or set the Final traction in tangential direction. If negative, the absolute value
          * - :py:attr:`~e3c`
            - Get or set the Elastic parameter in normal compression.
          * - :py:attr:`~cc`
            - Get or set the Elastic parameter in normal compression.
          * - :py:attr:`~asig`
            - Get or set the Plasticity hardening parameter in normal compression.
          * - :py:attr:`~bsig`
            - Get or set the Plasticity hardening parameter in normal compression.
          * - :py:attr:`~csig`
            - Get or set the Plasticity hardening parameter in normal compression.
          * - :py:attr:`~failn`
            - Get or set the Maximum effective separation distance in normal direction. Beyond
          * - :py:attr:`~failt`
            - Get or set the Maximum effective separation distance in tangential direction.
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

    from mat_cohesive_paper import MatCohesivePaper

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number or label must be specified.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density.
















   ..
       !! processed by numpydoc !!

.. py:property:: roflg
   :type: int


   
   Get or set the Flag for whether density is specified per unit area or volume:
   EQ.0:   Specified density is per unit volume(default).
   EQ.1 : Specified density is per unit area for controlling the mass of cohesive elements with an initial volume of zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: intfail
   :type: Optional[float]


   
   Get or set the The number of integration points required for the cohesive element to be deleted. The value of INTFAIL may range from 1 to 4 with 1 the recommended value.
   LT.0.0: Employs a Newton - Cotes integration scheme. The element will be deleted when |INTFAIL| integration points have failed.
   EQ.0.0 : Employs a Newton - Cotes integration scheme. The element will not be deleted even if it satisfies the failure criterion.
   GT.0.0 : Employs a Gauss integration scheme. The element will be deleted when INTFAIL integration points have failed.
















   ..
       !! processed by numpydoc !!

.. py:property:: en0
   :type: Optional[float]


   
   Get or set the The initial tensile stiffness (units of stress / length) normal to the
   plane of the cohesive element.
















   ..
       !! processed by numpydoc !!

.. py:property:: et0
   :type: Optional[float]


   
   Get or set the The initial stiffness (units of stress / length) tangential to the plane
   of the cohesive element.
















   ..
       !! processed by numpydoc !!

.. py:property:: en1
   :type: Optional[float]


   
   Get or set the The final tensile stiffness (units of stress / length) normal to the
   plane of the cohesive element.
















   ..
       !! processed by numpydoc !!

.. py:property:: et1
   :type: Optional[float]


   
   Get or set the The final stiffness (units of stress / length) tangential to the plane of
   the cohesive element.
















   ..
       !! processed by numpydoc !!

.. py:property:: t0n
   :type: Optional[float]


   
   Get or set the Peak tensile traction in normal direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: dn
   :type: Optional[float]


   
   Get or set the Scale factor (unit of length).
















   ..
       !! processed by numpydoc !!

.. py:property:: t1n
   :type: Optional[float]


   
   Get or set the Final tensile traction in normal direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: t0t
   :type: Optional[float]


   
   Get or set the Peak tensile traction in tangential direction. If negative, the absolute
   value indicates a curve with respect to the normal traction.
















   ..
       !! processed by numpydoc !!

.. py:property:: dt
   :type: Optional[float]


   
   Get or set the Scale factor (unit of length). If negative, the absolute value indicates
   a curve with respect to the normal stress.
















   ..
       !! processed by numpydoc !!

.. py:property:: t1t
   :type: Optional[float]


   
   Get or set the Final traction in tangential direction. If negative, the absolute value
   indicates a curve with respect to the normal traction.
















   ..
       !! processed by numpydoc !!

.. py:property:: e3c
   :type: Optional[float]


   
   Get or set the Elastic parameter in normal compression.
















   ..
       !! processed by numpydoc !!

.. py:property:: cc
   :type: Optional[float]


   
   Get or set the Elastic parameter in normal compression.
















   ..
       !! processed by numpydoc !!

.. py:property:: asig
   :type: Optional[float]


   
   Get or set the Plasticity hardening parameter in normal compression.
















   ..
       !! processed by numpydoc !!

.. py:property:: bsig
   :type: Optional[float]


   
   Get or set the Plasticity hardening parameter in normal compression.
















   ..
       !! processed by numpydoc !!

.. py:property:: csig
   :type: Optional[float]


   
   Get or set the Plasticity hardening parameter in normal compression.
















   ..
       !! processed by numpydoc !!

.. py:property:: failn
   :type: Optional[float]


   
   Get or set the Maximum effective separation distance in normal direction. Beyond
   this distance failure occurs.
















   ..
       !! processed by numpydoc !!

.. py:property:: failt
   :type: Optional[float]


   
   Get or set the Maximum effective separation distance in tangential direction.
   Beyond this distance failure occurs.
















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
   :value: 'COHESIVE_PAPER'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





