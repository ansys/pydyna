





:class:`MatPlasticityPolymer`
=============================


.. py:class:: mat_plasticity_polymer.MatPlasticityPolymer(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_PLASTICITY_POLYMER keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatPlasticityPolymer

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number has to be used.
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~e`
            - Get or set the Young's modulus.
          * - :py:attr:`~pr`
            - Get or set the Poisson's ratio.
          * - :py:attr:`~c`
            - Get or set the Strain rate parameter, C.
          * - :py:attr:`~p`
            - Get or set the Strain rate parameter, P.
          * - :py:attr:`~lcss`
            - Get or set the Load curve ID defining effective stress versus total effective strain. The stress versus effective plastic strain curve for the lowest value of strain rate is used if the strain rate falls below the minimum value. Likewise, the stress versus effective plastic strain curve for the highest value of strain rate is used if the strain rate exceeds the maximum value.
          * - :py:attr:`~lcsr`
            - Get or set the Load curve ID defining strain rate scaling effect on yield stress.
          * - :py:attr:`~eftx`
            - Get or set the Failure flag:
          * - :py:attr:`~damp`
            - Get or set the Stiffness-propotional damping ratio. Typical values are 1e-3 or 1e-4. If set too high instabilites can result.
          * - :py:attr:`~ratefac`
            - Get or set the Filtering factor for strain rate effects. Must be between 0 (no filtering) and 1 (infinite filtering) The filter is a simple low pass filter to remove high frequency oscillation from the strain rates before they are used in rate effect calculations. The cut off frequency of the filter is [(1 - RATEFAC)/ timestep] rad/sec.
          * - :py:attr:`~lcfail`
            - Get or set the Load curve ID giving variation of failure strain with strain rate. The points on the x-axis should be natural log of strain rate, the y-axis should be the true strain to failure. Typically this is measured by uniaxial tensile test, and the strain values converted to true strain.
          * - :py:attr:`~numint`
            - Get or set the Number of integration points which must fail before the element is deleted. This option is available for shells only.
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

    from mat_plasticity_polymer import MatPlasticityPolymer

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number has to be used.
















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

.. py:property:: c
   :type: Optional[float]


   
   Get or set the Strain rate parameter, C.
















   ..
       !! processed by numpydoc !!

.. py:property:: p
   :type: Optional[float]


   
   Get or set the Strain rate parameter, P.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcss
   :type: int


   
   Get or set the Load curve ID defining effective stress versus total effective strain. The stress versus effective plastic strain curve for the lowest value of strain rate is used if the strain rate falls below the minimum value. Likewise, the stress versus effective plastic strain curve for the highest value of strain rate is used if the strain rate exceeds the maximum value.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcsr
   :type: int


   
   Get or set the Load curve ID defining strain rate scaling effect on yield stress.
















   ..
       !! processed by numpydoc !!

.. py:property:: eftx
   :type: float


   
   Get or set the Failure flag:
   EQ.0.0: failure determined by maximum tensile strain (default),
   EQ.1.0: failure determined only by tensile strain in local x direction,
   EQ.2.0: failure determined only by tensile strain in local y direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: damp
   :type: Optional[float]


   
   Get or set the Stiffness-propotional damping ratio. Typical values are 1e-3 or 1e-4. If set too high instabilites can result.
















   ..
       !! processed by numpydoc !!

.. py:property:: ratefac
   :type: Optional[float]


   
   Get or set the Filtering factor for strain rate effects. Must be between 0 (no filtering) and 1 (infinite filtering) The filter is a simple low pass filter to remove high frequency oscillation from the strain rates before they are used in rate effect calculations. The cut off frequency of the filter is [(1 - RATEFAC)/ timestep] rad/sec.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcfail
   :type: int


   
   Get or set the Load curve ID giving variation of failure strain with strain rate. The points on the x-axis should be natural log of strain rate, the y-axis should be the true strain to failure. Typically this is measured by uniaxial tensile test, and the strain values converted to true strain.
















   ..
       !! processed by numpydoc !!

.. py:property:: numint
   :type: float


   
   Get or set the Number of integration points which must fail before the element is deleted. This option is available for shells only.
   LT.0.0: |NUMINT| is percentage of integration points/layers which must fail before shell element fails.
















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
   :value: 'PLASTICITY_POLYMER'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





