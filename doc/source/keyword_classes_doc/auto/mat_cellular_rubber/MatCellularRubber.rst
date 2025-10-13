





:class:`MatCellularRubber`
==========================


.. py:class:: mat_cellular_rubber.MatCellularRubber(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_CELLULAR_RUBBER keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatCellularRubber

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
          * - :py:attr:`~pr`
            - Get or set the Poisson's ratio, typical values are between 0.0 to 0.2. Due to the large compressibility of air, large values of Poisson's ratio generates physically meaningless results.
          * - :py:attr:`~n`
            - Get or set the Order of fit (currently < 3). If n>0 then a least square fit is computed with uniaxial data. The parameters given on card 2 should be specified. Also see *MAT_MOONEY_RIVLIN_RUBBER (material model 27). A Poisson's ratio of .5 is assumed for the void free rubber during the fit. The Poisson's ratio defined on Card 1 is for the cellular rubber. A void fraction formulation is used.
          * - :py:attr:`~sgl`
            - Get or set the Specimen gauge length l0.
          * - :py:attr:`~sw`
            - Get or set the Specimen width.
          * - :py:attr:`~st`
            - Get or set the Specimen thickness.
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID giving the force versus actual change dL in the gauge length.
          * - :py:attr:`~c10`
            - Get or set the Coefficient, C10.
          * - :py:attr:`~c01`
            - Get or set the Coefficient, C01.
          * - :py:attr:`~c11`
            - Get or set the Coefficient, C11.
          * - :py:attr:`~c20`
            - Get or set the Coefficient, C20.
          * - :py:attr:`~c02`
            - Get or set the Coefficient, C02.
          * - :py:attr:`~p0`
            - Get or set the Initial air pressure, P0.
          * - :py:attr:`~phi`
            - Get or set the Ratio of cellular rubber to rubber density.
          * - :py:attr:`~ivs`
            - Get or set the Initial volumetric strain.
          * - :py:attr:`~g`
            - Get or set the Optional shear relaxation modulus, G, for rate effects (viscosity).
          * - :py:attr:`~beta`
            - Get or set the Optional decay constant.
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

    from mat_cellular_rubber import MatCellularRubber

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

.. py:property:: pr
   :type: Optional[float]


   
   Get or set the Poisson's ratio, typical values are between 0.0 to 0.2. Due to the large compressibility of air, large values of Poisson's ratio generates physically meaningless results.
















   ..
       !! processed by numpydoc !!

.. py:property:: n
   :type: Optional[int]


   
   Get or set the Order of fit (currently < 3). If n>0 then a least square fit is computed with uniaxial data. The parameters given on card 2 should be specified. Also see *MAT_MOONEY_RIVLIN_RUBBER (material model 27). A Poisson's ratio of .5 is assumed for the void free rubber during the fit. The Poisson's ratio defined on Card 1 is for the cellular rubber. A void fraction formulation is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: sgl
   :type: Optional[float]


   
   Get or set the Specimen gauge length l0.
















   ..
       !! processed by numpydoc !!

.. py:property:: sw
   :type: Optional[float]


   
   Get or set the Specimen width.
















   ..
       !! processed by numpydoc !!

.. py:property:: st
   :type: Optional[float]


   
   Get or set the Specimen thickness.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID giving the force versus actual change dL in the gauge length.
















   ..
       !! processed by numpydoc !!

.. py:property:: c10
   :type: Optional[float]


   
   Get or set the Coefficient, C10.
















   ..
       !! processed by numpydoc !!

.. py:property:: c01
   :type: Optional[float]


   
   Get or set the Coefficient, C01.
















   ..
       !! processed by numpydoc !!

.. py:property:: c11
   :type: Optional[float]


   
   Get or set the Coefficient, C11.
















   ..
       !! processed by numpydoc !!

.. py:property:: c20
   :type: Optional[float]


   
   Get or set the Coefficient, C20.
















   ..
       !! processed by numpydoc !!

.. py:property:: c02
   :type: Optional[float]


   
   Get or set the Coefficient, C02.
















   ..
       !! processed by numpydoc !!

.. py:property:: p0
   :type: Optional[float]


   
   Get or set the Initial air pressure, P0.
















   ..
       !! processed by numpydoc !!

.. py:property:: phi
   :type: Optional[float]


   
   Get or set the Ratio of cellular rubber to rubber density.
















   ..
       !! processed by numpydoc !!

.. py:property:: ivs
   :type: Optional[float]


   
   Get or set the Initial volumetric strain.
















   ..
       !! processed by numpydoc !!

.. py:property:: g
   :type: Optional[float]


   
   Get or set the Optional shear relaxation modulus, G, for rate effects (viscosity).
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Optional decay constant.
















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
   :value: 'CELLULAR_RUBBER'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





