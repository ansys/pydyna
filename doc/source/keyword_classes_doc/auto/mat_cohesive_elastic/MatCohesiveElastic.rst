





:class:`MatCohesiveElastic`
===========================


.. py:class:: mat_cohesive_elastic.MatCohesiveElastic(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_COHESIVE_ELASTIC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatCohesiveElastic

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
            - Get or set the Flag stating whether density is specified per unit area or volume:
          * - :py:attr:`~intfail`
            - Get or set the The number of integration points required for the cohesive element to be deleted. The value of |INTFAIL| may range from 1 to 4 with 1 the recommended value.
          * - :py:attr:`~et`
            - Get or set the The stiffness in the plane of the cohesive element.
          * - :py:attr:`~en`
            - Get or set the The stiffness normal to the plane of the cohesive element.
          * - :py:attr:`~fn_fail`
            - Get or set the The force in the normal direction for tensile failure.
          * - :py:attr:`~ft_fail`
            - Get or set the The traction in the tangential direction for shear failure.
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

    from mat_cohesive_elastic import MatCohesiveElastic

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


   
   Get or set the Flag stating whether density is specified per unit area or volume:
   EQ.0:   Specified density is per unit volume(default).
   EQ.1 : Specified density is per unit area for controlling the mass of cohesive elements with an initial volume of zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: intfail
   :type: Optional[float]


   
   Get or set the The number of integration points required for the cohesive element to be deleted. The value of |INTFAIL| may range from 1 to 4 with 1 the recommended value.
   LT.0.0: Employs a Newton - Cotes integration scheme and the element will be deleted when | INTFAIL | integration points have failed.
   EQ.0.0 : Employs a Newton - Cotes integration scheme and the element will not be deleted even if it satisfies the failure criterion.
   GT.0.0 : Employs a Gauss integration scheme and the element will be deleted when INTFAIL integration points have failed.
















   ..
       !! processed by numpydoc !!

.. py:property:: et
   :type: Optional[float]


   
   Get or set the The stiffness in the plane of the cohesive element.
















   ..
       !! processed by numpydoc !!

.. py:property:: en
   :type: Optional[float]


   
   Get or set the The stiffness normal to the plane of the cohesive element.
















   ..
       !! processed by numpydoc !!

.. py:property:: fn_fail
   :type: Optional[float]


   
   Get or set the The force in the normal direction for tensile failure.
















   ..
       !! processed by numpydoc !!

.. py:property:: ft_fail
   :type: Optional[float]


   
   Get or set the The traction in the tangential direction for shear failure.
















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
   :value: 'COHESIVE_ELASTIC'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





