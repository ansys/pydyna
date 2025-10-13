





:class:`MatSoilAndFoamFailure`
==============================


.. py:class:: mat_soil_and_foam_failure.MatSoilAndFoamFailure(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_SOIL_AND_FOAM_FAILURE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatSoilAndFoamFailure

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
          * - :py:attr:`~g`
            - Get or set the Shear modulus.
          * - :py:attr:`~bulk`
            - Get or set the Bulk modulus for unloading used for VCR=0.0.
          * - :py:attr:`~a0`
            - Get or set the Yield function constant for plastic yield function.
          * - :py:attr:`~a1`
            - Get or set the Yield function constant for plastic yield function.
          * - :py:attr:`~a2`
            - Get or set the Yield function constant for plastic yield function.
          * - :py:attr:`~pc`
            - Get or set the Pressure cutoff for tensile fracture.
          * - :py:attr:`~vcr`
            - Get or set the Volumetric crushing option:
          * - :py:attr:`~ref`
            - Get or set the Use reference geometry to initialize the pressure, see *INITIAL_FOAM_REFERENCE_GEOMETRY.
          * - :py:attr:`~eps1`
            - Get or set the Volumetric strain value at point 1. A maximum of 10 values are allowed and a minimum of 2 values are necessary.
          * - :py:attr:`~eps2`
            - Get or set the Volumetric strain value at point 2.
          * - :py:attr:`~eps3`
            - Get or set the Volumetric strain value at point 3.
          * - :py:attr:`~eps4`
            - Get or set the Volumetric strain value at point 4.
          * - :py:attr:`~eps5`
            - Get or set the Volumetric strain value at point 5.
          * - :py:attr:`~eps6`
            - Get or set the Volumetric strain value at point 6.
          * - :py:attr:`~eps7`
            - Get or set the Volumetric strain value at point 7.
          * - :py:attr:`~eps8`
            - Get or set the Volumetric strain value at point 8.
          * - :py:attr:`~eps9`
            - Get or set the Volumetric strain value at point 9.
          * - :py:attr:`~eps10`
            - Get or set the Volumetric strain value at point 10.
          * - :py:attr:`~p1`
            - Get or set the Pressure corresponding to volumetric strain at point 1.
          * - :py:attr:`~p2`
            - Get or set the Pressure corresponding to volumetric strain at point 2.
          * - :py:attr:`~p3`
            - Get or set the Pressure corresponding to volumetric strain at point 3.
          * - :py:attr:`~p4`
            - Get or set the Pressure corresponding to volumetric strain at point 4.
          * - :py:attr:`~p5`
            - Get or set the Pressure corresponding to volumetric strain at point 5.
          * - :py:attr:`~p6`
            - Get or set the Pressure corresponding to volumetric strain at point 6.
          * - :py:attr:`~p7`
            - Get or set the Pressure corresponding to volumetric strain at point 7.
          * - :py:attr:`~p8`
            - Get or set the Pressure corresponding to volumetric strain at point 8.
          * - :py:attr:`~p9`
            - Get or set the Pressure corresponding to volumetric strain at point 9.
          * - :py:attr:`~p10`
            - Get or set the Pressure corresponding to volumetric strain at point 10.
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

    from mat_soil_and_foam_failure import MatSoilAndFoamFailure

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

.. py:property:: g
   :type: Optional[float]


   
   Get or set the Shear modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: bulk
   :type: Optional[float]


   
   Get or set the Bulk modulus for unloading used for VCR=0.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: a0
   :type: Optional[float]


   
   Get or set the Yield function constant for plastic yield function.
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: Optional[float]


   
   Get or set the Yield function constant for plastic yield function.
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the Yield function constant for plastic yield function.
















   ..
       !! processed by numpydoc !!

.. py:property:: pc
   :type: Optional[float]


   
   Get or set the Pressure cutoff for tensile fracture.
















   ..
       !! processed by numpydoc !!

.. py:property:: vcr
   :type: float


   
   Get or set the Volumetric crushing option:
   EQ.0.0: on (default),
   EQ.1.0: loading and unloading paths are the same.
















   ..
       !! processed by numpydoc !!

.. py:property:: ref
   :type: float


   
   Get or set the Use reference geometry to initialize the pressure, see *INITIAL_FOAM_REFERENCE_GEOMETRY.
   EQ.0.0: off (default),
   EQ.1.0: on.
















   ..
       !! processed by numpydoc !!

.. py:property:: eps1
   :type: Optional[float]


   
   Get or set the Volumetric strain value at point 1. A maximum of 10 values are allowed and a minimum of 2 values are necessary.
















   ..
       !! processed by numpydoc !!

.. py:property:: eps2
   :type: Optional[float]


   
   Get or set the Volumetric strain value at point 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: eps3
   :type: Optional[float]


   
   Get or set the Volumetric strain value at point 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: eps4
   :type: Optional[float]


   
   Get or set the Volumetric strain value at point 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: eps5
   :type: Optional[float]


   
   Get or set the Volumetric strain value at point 5.
















   ..
       !! processed by numpydoc !!

.. py:property:: eps6
   :type: Optional[float]


   
   Get or set the Volumetric strain value at point 6.
















   ..
       !! processed by numpydoc !!

.. py:property:: eps7
   :type: Optional[float]


   
   Get or set the Volumetric strain value at point 7.
















   ..
       !! processed by numpydoc !!

.. py:property:: eps8
   :type: Optional[float]


   
   Get or set the Volumetric strain value at point 8.
















   ..
       !! processed by numpydoc !!

.. py:property:: eps9
   :type: Optional[float]


   
   Get or set the Volumetric strain value at point 9.
















   ..
       !! processed by numpydoc !!

.. py:property:: eps10
   :type: Optional[float]


   
   Get or set the Volumetric strain value at point 10.
















   ..
       !! processed by numpydoc !!

.. py:property:: p1
   :type: Optional[float]


   
   Get or set the Pressure corresponding to volumetric strain at point 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: p2
   :type: Optional[float]


   
   Get or set the Pressure corresponding to volumetric strain at point 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: p3
   :type: Optional[float]


   
   Get or set the Pressure corresponding to volumetric strain at point 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: p4
   :type: Optional[float]


   
   Get or set the Pressure corresponding to volumetric strain at point 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: p5
   :type: Optional[float]


   
   Get or set the Pressure corresponding to volumetric strain at point 5.
















   ..
       !! processed by numpydoc !!

.. py:property:: p6
   :type: Optional[float]


   
   Get or set the Pressure corresponding to volumetric strain at point 6.
















   ..
       !! processed by numpydoc !!

.. py:property:: p7
   :type: Optional[float]


   
   Get or set the Pressure corresponding to volumetric strain at point 7.
















   ..
       !! processed by numpydoc !!

.. py:property:: p8
   :type: Optional[float]


   
   Get or set the Pressure corresponding to volumetric strain at point 8.
















   ..
       !! processed by numpydoc !!

.. py:property:: p9
   :type: Optional[float]


   
   Get or set the Pressure corresponding to volumetric strain at point 9.
















   ..
       !! processed by numpydoc !!

.. py:property:: p10
   :type: Optional[float]


   
   Get or set the Pressure corresponding to volumetric strain at point 10.
















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
   :value: 'SOIL_AND_FOAM_FAILURE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





