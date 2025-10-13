





:class:`MatAddFatigue`
======================


.. py:class:: mat_add_fatigue.MatAddFatigue(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_ADD_FATIGUE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatAddFatigue

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification for which the fatigue property applies.
          * - :py:attr:`~lcid`
            - Get or set the S-N fatigue curve ID.
          * - :py:attr:`~ltype`
            - Get or set the Type of S-N curve.
          * - :py:attr:`~a`
            - Get or set the Material parameter a in S-N fatigue equation.
          * - :py:attr:`~b`
            - Get or set the Material parameter a in S-N fatigue equation.
          * - :py:attr:`~sthres`
            - Get or set the Fatigue threshold if the S-N curve is defined by equation (LCID<0).
          * - :py:attr:`~snlimt`
            - Get or set the If LCID > 0
          * - :py:attr:`~sntype`
            - Get or set the Stress type of S-N curve.
          * - :py:attr:`~ai`
            - Get or set the Material parameter a in S-N fatigue equation for the i-th segment.
          * - :py:attr:`~bi`
            - Get or set the Material parameter b in S-N fatigue equation for the i-th segment.
          * - :py:attr:`~sthresi`
            - Get or set the Fatigue threshold stress for the i-th segment.
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

    from mat_add_fatigue import MatAddFatigue

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification for which the fatigue property applies.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: int


   
   Get or set the S-N fatigue curve ID.
   GT.0: S-N fatigue curve ID.
   EQ.-1: S-N fatigue curve uses equation N*S**b=a.
   EQ.-2: S-N fatigue curve uses equation log(S)=a-b*log(N).
   EQ.-3: S-N fatigue curve uses equation S=a*N**b
















   ..
       !! processed by numpydoc !!

.. py:property:: ltype
   :type: int


   
   Get or set the Type of S-N curve.
   EQ.0: Semi-log interpolation (default).
   EQ.1: Log-Log interpolation.
   EQ.2: Linear-Linear interpolation.
















   ..
       !! processed by numpydoc !!

.. py:property:: a
   :type: Optional[float]


   
   Get or set the Material parameter a in S-N fatigue equation.
















   ..
       !! processed by numpydoc !!

.. py:property:: b
   :type: Optional[float]


   
   Get or set the Material parameter a in S-N fatigue equation.
















   ..
       !! processed by numpydoc !!

.. py:property:: sthres
   :type: Optional[float]


   
   Get or set the Fatigue threshold if the S-N curve is defined by equation (LCID<0).
















   ..
       !! processed by numpydoc !!

.. py:property:: snlimt
   :type: int


   
   Get or set the If LCID > 0
   Flag setting algorithm used when stress is lower than the lowest stress on S-N curve.
   EQ.0: use the life at the last point on S-N curve.
   EQ.1: extrapolation from the last two points on S-N curve.
   EQ.2: infinity.
   If LCID < 0
   Flag setting algorithm used when stress is lower than STHRES.
   EQ.0: use the life at STHRES.
   EQ.1: Ignored. only applicable for LCID > 0.
   EQ.2: infinity.
















   ..
       !! processed by numpydoc !!

.. py:property:: sntype
   :type: int


   
   Get or set the Stress type of S-N curve.
   EQ.0: stress range (default)
   EQ.1: stress amplitude.
















   ..
       !! processed by numpydoc !!

.. py:property:: ai
   :type: Optional[float]


   
   Get or set the Material parameter a in S-N fatigue equation for the i-th segment.
















   ..
       !! processed by numpydoc !!

.. py:property:: bi
   :type: Optional[float]


   
   Get or set the Material parameter b in S-N fatigue equation for the i-th segment.
















   ..
       !! processed by numpydoc !!

.. py:property:: sthresi
   :type: Optional[float]


   
   Get or set the Fatigue threshold stress for the i-th segment.
















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
   :value: 'ADD_FATIGUE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





