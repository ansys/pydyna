





:class:`MatAddCohesive`
=======================


.. py:class:: mat_add_cohesive.MatAddCohesive(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_ADD_COHESIVE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatAddCohesive

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID for which the cohesive property applies.
          * - :py:attr:`~roflg`
            - Get or set the Flag for whether density is specified per unit area or volume:
          * - :py:attr:`~intfail`
            - Get or set the The number of integration points required for the cohesive element to be deleted. The value of INTFAIL may range from 1 to 4 with 1 the recommended value.
          * - :py:attr:`~thick`
            - Get or set the Thickness of the adhesive layer.
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

    from mat_add_cohesive import MatAddCohesive

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID for which the cohesive property applies.
















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
   LT.0.0: Employs a Newton - Cotes integration scheme and the element will be deleted when | INTFAIL | integration points have failed.
   EQ.0.0 : Employs a Newton - Cotes integration scheme and the element will not be deleted even if it satisfies the failure criterion.
   GT.0.0 : Employs a Gauss integration scheme and the element will be deleted when INTFAIL integration points have failed.
















   ..
       !! processed by numpydoc !!

.. py:property:: thick
   :type: Optional[float]


   
   Get or set the Thickness of the adhesive layer.
   EQ.0.0: The actual thickness of the cohesive element is used.
   GT.0.0: User specified thickness.
















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
   :value: 'ADD_COHESIVE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





