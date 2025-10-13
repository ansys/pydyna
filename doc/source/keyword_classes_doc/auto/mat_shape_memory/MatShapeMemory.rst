





:class:`MatShapeMemory`
=======================


.. py:class:: mat_shape_memory.MatShapeMemory(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_SHAPE_MEMORY keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatShapeMemory

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique has to be used.
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~e`
            - Get or set the Young's modulus.
          * - :py:attr:`~pr`
            - Get or set the Poisson's ratio.
          * - :py:attr:`~lcss`
            - Get or set the The absolute value of LCSS is a load curve ID for effective stress as a function of effective plastic strain. The first data point, at zero plastic strain, indicates the initial yield stress.
          * - :py:attr:`~sig_ass`
            - Get or set the Starting value for the forward phase transformation(conversion of austenite into martensite) in the case of a uniaxial tensile state of stress.
          * - :py:attr:`~sig_asf`
            - Get or set the Final value for the forward phase transformation (conversion of austenite into martensite) in the case of a uniaxial tensile state of stress.
          * - :py:attr:`~sig_sas`
            - Get or set the Starting value for the reverse phase transformation (conversion of martensite into austenite) in the case of a uniaxial tensile state of stress.
          * - :py:attr:`~sig_saf`
            - Get or set the Final value for the reverse phase transformation (conversion of martensite into austenite) in the case of a uniaxial tensile state of stress.
          * - :py:attr:`~epsl`
            - Get or set the Recoverable strain or maximum residual strain. It is a measure of the maximum deformation obtainable for all the martensite in one direction
          * - :py:attr:`~alpha`
            - Get or set the Parameter measuring the difference between material responses in tension and compression (set alpha = 0 for no difference).  Also, see the following remarks.
          * - :py:attr:`~ymrt`
            - Get or set the Young’s modulus for the martensite if it is different from the modulus for the austenite.  Defaults to the austenite modulus if it is set to zero.
          * - :py:attr:`~lcid_as`
            - Get or set the Load curve ID or table ID for the forward phase change (conversion of austenite into martensite).
          * - :py:attr:`~lcid_sa`
            - Get or set the Load curve ID or table ID for reverse phase change (conversion of martensite into austenite).
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

    from mat_shape_memory import MatShapeMemory

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique has to be used.
















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

.. py:property:: lcss
   :type: float


   
   Get or set the The absolute value of LCSS is a load curve ID for effective stress as a function of effective plastic strain. The first data point, at zero plastic strain, indicates the initial yield stress.
   For a negative value of LCSS, negative values of SIG_ASS, SIG_ASF, SIG_SAS, SIG_SAF will indicate dependence on plastic strain, see below.
















   ..
       !! processed by numpydoc !!

.. py:property:: sig_ass
   :type: Optional[float]


   
   Get or set the Starting value for the forward phase transformation(conversion of austenite into martensite) in the case of a uniaxial tensile state of stress.
   LT.0.0: -SIG_ASS is a load curve ID defining the starting value as a function of temperature.If LCSS is also negative, then - SIG_‌ASS is either a load curve specifying the starting value as a function of effective plastic strain or a table of such load curves for different temperatures
















   ..
       !! processed by numpydoc !!

.. py:property:: sig_asf
   :type: Optional[float]


   
   Get or set the Final value for the forward phase transformation (conversion of austenite into martensite) in the case of a uniaxial tensile state of stress.
   LT.0.0: -SIG_ASF is a load curve ID defining the final value as a function of temperature is specified.If LCSS is also negative, -SIG_‌ASF is either a load curve specifying the final value as a function of effective plastic strain or a table of such load curves for different temperatures
















   ..
       !! processed by numpydoc !!

.. py:property:: sig_sas
   :type: Optional[float]


   
   Get or set the Starting value for the reverse phase transformation (conversion of martensite into austenite) in the case of a uniaxial tensile state of stress.
   LT.0.0: -SIG_SAS is a load curve ID defining the starting value as a function of temperature.If LCSS is also negative, -SIG_SAS is either a load curve specifying the starting value as a function of effective plastic strain or a table of such load curves for different temperatures
















   ..
       !! processed by numpydoc !!

.. py:property:: sig_saf
   :type: Optional[float]


   
   Get or set the Final value for the reverse phase transformation (conversion of martensite into austenite) in the case of a uniaxial tensile state of stress.
   LT.0.0: -SIG_SAF is a load curve ID specifying the reverse value as a function of temperature.If LCSS is also negative, -SIG_SAF is either a load curve specifying the final value as a function of effective plastic strain or a table of such load curves for different temperatures.
















   ..
       !! processed by numpydoc !!

.. py:property:: epsl
   :type: Optional[float]


   
   Get or set the Recoverable strain or maximum residual strain. It is a measure of the maximum deformation obtainable for all the martensite in one direction
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha
   :type: Optional[float]


   
   Get or set the Parameter measuring the difference between material responses in tension and compression (set alpha = 0 for no difference).  Also, see the following remarks.
















   ..
       !! processed by numpydoc !!

.. py:property:: ymrt
   :type: Optional[float]


   
   Get or set the Young’s modulus for the martensite if it is different from the modulus for the austenite.  Defaults to the austenite modulus if it is set to zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid_as
   :type: Optional[int]


   
   Get or set the Load curve ID or table ID for the forward phase change (conversion of austenite into martensite).
   1.      When LCID_AS is a load curve ID the curve is taken to be effective stress as a function of martensite fraction(ranging from 0 to 1).
   2.      When LCID_AS is a table ID the table defines for each phase transition rate(derivative of martensite fraction) a load curve ID specifying the stress as a function of martensite fraction for that phase transition rate.
   The stress as a function of martensite fraction curve for the lowest value of the phase transition rate is used if the phase transition rate falls below the minimum value.Likewise, the stress as a function of martensite fraction curve for the highest value of phase transition rate is used if the phase transition rate exceeds the maximum value..
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid_sa
   :type: Optional[int]


   
   Get or set the Load curve ID or table ID for reverse phase change (conversion of martensite into austenite).
   1.      When LCID_SA is a load curve ID, the curve is taken to be effective stress as a function of martensite fraction(ranging from 0 to 1).
   2.      When LCID_SA is a table ID, the table defines for each phase transition rate(derivative of martensite fraction) a load curve ID specifying the stress as a function of martensite fraction for that phase transition rate.
   The stress as a function of martensite fraction curve for the lowest value of the phase transition rate is used if the phase transition rate falls below the minimum value.Likewise, the stress as a function of martensite fraction curve for the highest value of phase transition rate is used if phase transition rate exceeds the maximum value.
   3.      The values of SIG_ASS and SIG_ASF are overwritten when this option is used..
















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
   :value: 'SHAPE_MEMORY'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





