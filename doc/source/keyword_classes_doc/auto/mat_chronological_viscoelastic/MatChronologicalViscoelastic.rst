





:class:`MatChronologicalViscoelastic`
=====================================


.. py:class:: mat_chronological_viscoelastic.MatChronologicalViscoelastic(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_CHRONOLOGICAL_VISCOELASTIC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatChronologicalViscoelastic

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
          * - :py:attr:`~bulk`
            - Get or set the Elastic bulk modulus.
          * - :py:attr:`~pcf`
            - Get or set the Tensile pressure elimination flag for solid elements only. If set to unity tensile pressures are set to zero.
          * - :py:attr:`~ef`
            - Get or set the Elastic flag (if equal 1, the layer is elastic. If 0 the layer is viscoelastic).
          * - :py:attr:`~tref`
            - Get or set the Reference temperature for shift function (must be greater than zero).
          * - :py:attr:`~a`
            - Get or set the Chronological coefficient.
          * - :py:attr:`~b`
            - Get or set the Chronological coefficient.
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID for deviatoric behavior if constants, Gi, and Bi are determined via a least squares fit. This relaxation curve is shown below.
          * - :py:attr:`~nt`
            - Get or set the Number of terms in shear fit. If zero the default is 6. Fewer than NT terms will be used if the fit produces one or more negative shear moduli.Currently, the maximum number is set to 6.
          * - :py:attr:`~bstart`
            - Get or set the In the fit, B1 is set to zero, B2 is set to BSTART, B3 is 10 times B2, B4 is 100 times greater than B3 , and so on. If zero, BSTART is determined by an iterative trial and error scheme.
          * - :py:attr:`~tramp`
            - Get or set the Optional ramp time for loading
          * - :py:attr:`~lcidk`
            - Get or set the Load curve ID for bulk behavior if constants, Ki, and BKi are determined via a least squares fit. This relaxation curve is shown below.
          * - :py:attr:`~ntk`
            - Get or set the Number of terms desired in bulk fit. If zero the default is 6. Currently, the maximum number is set to 6.
          * - :py:attr:`~bstartk`
            - Get or set the In the fit, BK1 is set to zero, BK2 is set to BSTARTK, BK3 is 10 times BK2, BK4 is 100 times greater than BK3 , and so on. If zero, BSTARTK is determined by an iterative trial and error scheme.
          * - :py:attr:`~trampk`
            - Get or set the Optional ramp time for bulk loading.
          * - :py:attr:`~gi`
            - Get or set the Optional shear relaxation modulus for the ith term.
          * - :py:attr:`~betai`
            - Get or set the Optional shear decay constant for the ith term.
          * - :py:attr:`~ki`
            - Get or set the Optional bulk relaxation modulus for the ith term.
          * - :py:attr:`~betaki`
            - Get or set the Optional bulk decay constant for the ith term
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

    from mat_chronological_viscoelastic import MatChronologicalViscoelastic

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

.. py:property:: bulk
   :type: Optional[float]


   
   Get or set the Elastic bulk modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: pcf
   :type: Optional[float]


   
   Get or set the Tensile pressure elimination flag for solid elements only. If set to unity tensile pressures are set to zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: ef
   :type: Optional[float]


   
   Get or set the Elastic flag (if equal 1, the layer is elastic. If 0 the layer is viscoelastic).
















   ..
       !! processed by numpydoc !!

.. py:property:: tref
   :type: Optional[float]


   
   Get or set the Reference temperature for shift function (must be greater than zero).
















   ..
       !! processed by numpydoc !!

.. py:property:: a
   :type: Optional[float]


   
   Get or set the Chronological coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: b
   :type: Optional[float]


   
   Get or set the Chronological coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID for deviatoric behavior if constants, Gi, and Bi are determined via a least squares fit. This relaxation curve is shown below.
















   ..
       !! processed by numpydoc !!

.. py:property:: nt
   :type: Optional[int]


   
   Get or set the Number of terms in shear fit. If zero the default is 6. Fewer than NT terms will be used if the fit produces one or more negative shear moduli.Currently, the maximum number is set to 6.
















   ..
       !! processed by numpydoc !!

.. py:property:: bstart
   :type: Optional[float]


   
   Get or set the In the fit, B1 is set to zero, B2 is set to BSTART, B3 is 10 times B2, B4 is 100 times greater than B3 , and so on. If zero, BSTART is determined by an iterative trial and error scheme.
















   ..
       !! processed by numpydoc !!

.. py:property:: tramp
   :type: Optional[float]


   
   Get or set the Optional ramp time for loading
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidk
   :type: Optional[int]


   
   Get or set the Load curve ID for bulk behavior if constants, Ki, and BKi are determined via a least squares fit. This relaxation curve is shown below.
















   ..
       !! processed by numpydoc !!

.. py:property:: ntk
   :type: Optional[int]


   
   Get or set the Number of terms desired in bulk fit. If zero the default is 6. Currently, the maximum number is set to 6.
















   ..
       !! processed by numpydoc !!

.. py:property:: bstartk
   :type: Optional[float]


   
   Get or set the In the fit, BK1 is set to zero, BK2 is set to BSTARTK, BK3 is 10 times BK2, BK4 is 100 times greater than BK3 , and so on. If zero, BSTARTK is determined by an iterative trial and error scheme.
















   ..
       !! processed by numpydoc !!

.. py:property:: trampk
   :type: Optional[float]


   
   Get or set the Optional ramp time for bulk loading.
















   ..
       !! processed by numpydoc !!

.. py:property:: gi
   :type: Optional[float]


   
   Get or set the Optional shear relaxation modulus for the ith term.
















   ..
       !! processed by numpydoc !!

.. py:property:: betai
   :type: Optional[float]


   
   Get or set the Optional shear decay constant for the ith term.
















   ..
       !! processed by numpydoc !!

.. py:property:: ki
   :type: Optional[float]


   
   Get or set the Optional bulk relaxation modulus for the ith term.
















   ..
       !! processed by numpydoc !!

.. py:property:: betaki
   :type: Optional[float]


   
   Get or set the Optional bulk decay constant for the ith term
















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
   :value: 'CHRONOLOGICAL_VISCOELASTIC'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





