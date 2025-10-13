





:class:`MatDeshpandeFleckFoam`
==============================


.. py:class:: mat_deshpande_fleck_foam.MatDeshpandeFleckFoam(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_DESHPANDE_FLECK_FOAM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatDeshpandeFleckFoam

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification, a unique number has to be chosen.
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~e`
            - Get or set the Young's modulus.
          * - :py:attr:`~pr`
            - Get or set the Poisson's ratio.
          * - :py:attr:`~alpha`
            - Get or set the Controls shape of yield surface.
          * - :py:attr:`~gamma`
            - Get or set the See remarks.
          * - :py:attr:`~epsd`
            - Get or set the Densification strain.
          * - :py:attr:`~alpha2`
            - Get or set the See remarks.
          * - :py:attr:`~beta`
            - Get or set the See remarks.
          * - :py:attr:`~sigp`
            - Get or set the See remarks.
          * - :py:attr:`~derfi`
            - Get or set the Type of derivation used in material subroutine
          * - :py:attr:`~cfail`
            - Get or set the Tensile volumetric strain at failure.  Default is no failure due to tensile volumetric strain..
          * - :py:attr:`~pfail`
            - Get or set the Maximum principal stress at failure.  Must be sustained NUM ( > 0) timesteps to fail element.  Default is no failure due to maximum principal stress
          * - :py:attr:`~num`
            - Get or set the Number of timesteps at or above PFAIL to trigger element failure
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

    from mat_deshpande_fleck_foam import MatDeshpandeFleckFoam

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification, a unique number has to be chosen.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density.
















   ..
       !! processed by numpydoc !!

.. py:property:: e
   :type: Optional[float]


   
   Get or set the Young's modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: Optional[float]


   
   Get or set the Poisson's ratio.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha
   :type: Optional[float]


   
   Get or set the Controls shape of yield surface.
















   ..
       !! processed by numpydoc !!

.. py:property:: gamma
   :type: Optional[float]


   
   Get or set the See remarks.
















   ..
       !! processed by numpydoc !!

.. py:property:: epsd
   :type: Optional[float]


   
   Get or set the Densification strain.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha2
   :type: Optional[float]


   
   Get or set the See remarks.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the See remarks.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigp
   :type: Optional[float]


   
   Get or set the See remarks.
















   ..
       !! processed by numpydoc !!

.. py:property:: derfi
   :type: float


   
   Get or set the Type of derivation used in material subroutine
   EQ.0: Numerical derivation.
   EQ.1: Analytical derivation.
















   ..
       !! processed by numpydoc !!

.. py:property:: cfail
   :type: Optional[float]


   
   Get or set the Tensile volumetric strain at failure.  Default is no failure due to tensile volumetric strain..
















   ..
       !! processed by numpydoc !!

.. py:property:: pfail
   :type: Optional[float]


   
   Get or set the Maximum principal stress at failure.  Must be sustained NUM ( > 0) timesteps to fail element.  Default is no failure due to maximum principal stress
















   ..
       !! processed by numpydoc !!

.. py:property:: num
   :type: int


   
   Get or set the Number of timesteps at or above PFAIL to trigger element failure
















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
   :value: 'DESHPANDE_FLECK_FOAM'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





