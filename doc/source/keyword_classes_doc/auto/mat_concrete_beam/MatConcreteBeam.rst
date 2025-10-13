





:class:`MatConcreteBeam`
========================


.. py:class:: mat_concrete_beam.MatConcreteBeam(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_CONCRETE_BEAM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatConcreteBeam

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
          * - :py:attr:`~sigy`
            - Get or set the Yield stress.
          * - :py:attr:`~etan`
            - Get or set the Tangent modulus.
          * - :py:attr:`~fail`
            - Get or set the Failure flag:
          * - :py:attr:`~tdel`
            - Get or set the Minimum time step size for automatic element deletion.
          * - :py:attr:`~c`
            - Get or set the Strain rate parameter.
          * - :py:attr:`~p`
            - Get or set the Strain rate parameter.
          * - :py:attr:`~lcss`
            - Get or set the Load curve ID or Table ID. Load curve ID defining effective stress versus effective plastic strain. The table ID defines for each strain rate value a load curve ID giving the stress versus effective plastic strain for that rate. The stress versus effective plastic strain curve for the lowest value of strain rate is used if the strain rate falls below the minimum value. Likewise, the stress versus effective plastic strain curve for the highest value of strain rate is used if the strain rate exceeds the maximum value.
          * - :py:attr:`~lcsr`
            - Get or set the Load curve ID defining strain rate scaling effect on yield stress.
          * - :py:attr:`~noten`
            - Get or set the No-tension flag:
          * - :py:attr:`~tencut`
            - Get or set the Tension cutoff value.
          * - :py:attr:`~sdr`
            - Get or set the Stiffness degradation factor.
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

    from mat_concrete_beam import MatConcreteBeam

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

.. py:property:: sigy
   :type: Optional[float]


   
   Get or set the Yield stress.
















   ..
       !! processed by numpydoc !!

.. py:property:: etan
   :type: Optional[float]


   
   Get or set the Tangent modulus.
   Ignored if LCSS.GT.0 is defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: fail
   :type: float


   
   Get or set the Failure flag:
   LT.0.0: user defined failure subroutine is called to determine failure
   EQ.0.0: Failure is not considered. This option is recommended if failure is not of interest since many caluculations will be saved (default),
   GT.0.0: Plastic strain to failure. When the plastic strain reaches this value, the element is deleted from the calculation.
















   ..
       !! processed by numpydoc !!

.. py:property:: tdel
   :type: float


   
   Get or set the Minimum time step size for automatic element deletion.
   Default is set to 10.0E+20
















   ..
       !! processed by numpydoc !!

.. py:property:: c
   :type: Optional[float]


   
   Get or set the Strain rate parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: p
   :type: Optional[float]


   
   Get or set the Strain rate parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcss
   :type: int


   
   Get or set the Load curve ID or Table ID. Load curve ID defining effective stress versus effective plastic strain. The table ID defines for each strain rate value a load curve ID giving the stress versus effective plastic strain for that rate. The stress versus effective plastic strain curve for the lowest value of strain rate is used if the strain rate falls below the minimum value. Likewise, the stress versus effective plastic strain curve for the highest value of strain rate is used if the strain rate exceeds the maximum value.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcsr
   :type: int


   
   Get or set the Load curve ID defining strain rate scaling effect on yield stress.
















   ..
       !! processed by numpydoc !!

.. py:property:: noten
   :type: int


   
   Get or set the No-tension flag:
   EQ:0 beam takes tension,
   EQ:1 beam takes no tension,
   EQ:2 beam takes tension up to value given by TENCUT.
















   ..
       !! processed by numpydoc !!

.. py:property:: tencut
   :type: float


   
   Get or set the Tension cutoff value.
















   ..
       !! processed by numpydoc !!

.. py:property:: sdr
   :type: Optional[float]


   
   Get or set the Stiffness degradation factor.
   Default is set to 0.0
















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
   :value: 'CONCRETE_BEAM'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





