





:class:`MatElasticPhaseChange`
==============================


.. py:class:: mat_elastic_phase_change.MatElasticPhaseChange(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_ELASTIC_PHASE_CHANGE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatElasticPhaseChange

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number or label not exceeding 8        characters must be specified..
          * - :py:attr:`~ro1`
            - Get or set the Mass density for phase i.
          * - :py:attr:`~e1`
            - Get or set the Young's modulus for phase i
          * - :py:attr:`~pr1`
            - Get or set the Poisson's ratio for phase i.
          * - :py:attr:`~ro2`
            - Get or set the Mass density for phase i.
          * - :py:attr:`~e2`
            - Get or set the Young's modulus for phase i
          * - :py:attr:`~pr2`
            - Get or set the Poisson's ratio for phase i.
          * - :py:attr:`~x1`
            - Get or set the Coordinates of a point on the phase transition plane
          * - :py:attr:`~y1`
            - Get or set the Coordinates of a point on the phase transition plane
          * - :py:attr:`~z1`
            - Get or set the Coordinates of a point on the phase transition plane
          * - :py:attr:`~x2`
            - Get or set the Coordinates of a point that defines the exterior normal with the first point.
          * - :py:attr:`~y2`
            - Get or set the Coordinates of a point that defines the exterior normal with the first point
          * - :py:attr:`~z2`
            - Get or set the Coordinates of a point that defines the exterior normal with the first point
          * - :py:attr:`~thkfac`
            - Get or set the Scale factor applied to the shell thickness after the phase transformation.
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

    from mat_elastic_phase_change import MatElasticPhaseChange

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number or label not exceeding 8        characters must be specified..
















   ..
       !! processed by numpydoc !!

.. py:property:: ro1
   :type: Optional[float]


   
   Get or set the Mass density for phase i.
















   ..
       !! processed by numpydoc !!

.. py:property:: e1
   :type: Optional[float]


   
   Get or set the Young's modulus for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: pr1
   :type: Optional[float]


   
   Get or set the Poisson's ratio for phase i.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro2
   :type: Optional[float]


   
   Get or set the Mass density for phase i.
















   ..
       !! processed by numpydoc !!

.. py:property:: e2
   :type: Optional[float]


   
   Get or set the Young's modulus for phase i
















   ..
       !! processed by numpydoc !!

.. py:property:: pr2
   :type: Optional[float]


   
   Get or set the Poisson's ratio for phase i.
















   ..
       !! processed by numpydoc !!

.. py:property:: x1
   :type: Optional[float]


   
   Get or set the Coordinates of a point on the phase transition plane
















   ..
       !! processed by numpydoc !!

.. py:property:: y1
   :type: Optional[float]


   
   Get or set the Coordinates of a point on the phase transition plane
















   ..
       !! processed by numpydoc !!

.. py:property:: z1
   :type: Optional[float]


   
   Get or set the Coordinates of a point on the phase transition plane
















   ..
       !! processed by numpydoc !!

.. py:property:: x2
   :type: Optional[float]


   
   Get or set the Coordinates of a point that defines the exterior normal with the first point.
















   ..
       !! processed by numpydoc !!

.. py:property:: y2
   :type: Optional[float]


   
   Get or set the Coordinates of a point that defines the exterior normal with the first point
















   ..
       !! processed by numpydoc !!

.. py:property:: z2
   :type: Optional[float]


   
   Get or set the Coordinates of a point that defines the exterior normal with the first point
















   ..
       !! processed by numpydoc !!

.. py:property:: thkfac
   :type: float


   
   Get or set the Scale factor applied to the shell thickness after the phase transformation.
















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
   :value: 'ELASTIC_PHASE_CHANGE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





