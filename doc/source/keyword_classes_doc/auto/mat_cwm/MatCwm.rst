





:class:`MatCwm`
===============


.. py:class:: mat_cwm.MatCwm(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_CWM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatCwm

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification.
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~lcem`
            - Get or set the Load curve for Young's modulus as function of temperature.
          * - :py:attr:`~lcpr`
            - Get or set the Load curve for Poisson's ratio as function of temperature.
          * - :py:attr:`~lcsy`
            - Get or set the Load curve for yield stress as function of temperature.
          * - :py:attr:`~lchr`
            - Get or set the Load curve for hardening modulus as function of temperature.
          * - :py:attr:`~lcat`
            - Get or set the Load curve for thermal expansion coefficient as function of temperature.
          * - :py:attr:`~beta`
            - Get or set the Fraction isotropic hardening between 0 and 1
          * - :py:attr:`~tastart`
            - Get or set the Annealing temperature start.
          * - :py:attr:`~taend`
            - Get or set the Annealing temperature end.
          * - :py:attr:`~tlstart`
            - Get or set the Birth temperature start.
          * - :py:attr:`~tlend`
            - Get or set the Birth temperature end.
          * - :py:attr:`~eghost`
            - Get or set the Young's modulus for ghost (quiet) material.
          * - :py:attr:`~pghost`
            - Get or set the Poisson's ratio for ghost (quiet) material.
          * - :py:attr:`~aghost`
            - Get or set the Thermal expansion coefficient for ghost (quiet) material.
          * - :py:attr:`~t2phase`
            - Get or set the Temperature at which phase change commences.
          * - :py:attr:`~t1phase`
            - Get or set the Temperature at which phase change ends.
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

    from mat_cwm import MatCwm

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcem
   :type: Optional[int]


   
   Get or set the Load curve for Young's modulus as function of temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcpr
   :type: Optional[int]


   
   Get or set the Load curve for Poisson's ratio as function of temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcsy
   :type: Optional[int]


   
   Get or set the Load curve for yield stress as function of temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: lchr
   :type: Optional[int]


   
   Get or set the Load curve for hardening modulus as function of temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcat
   :type: Optional[int]


   
   Get or set the Load curve for thermal expansion coefficient as function of temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Fraction isotropic hardening between 0 and 1
   EQ.0: Kinematic hardening
   EQ.1: Isotropic hardening.
















   ..
       !! processed by numpydoc !!

.. py:property:: tastart
   :type: Optional[float]


   
   Get or set the Annealing temperature start.
















   ..
       !! processed by numpydoc !!

.. py:property:: taend
   :type: Optional[float]


   
   Get or set the Annealing temperature end.
















   ..
       !! processed by numpydoc !!

.. py:property:: tlstart
   :type: Optional[float]


   
   Get or set the Birth temperature start.
















   ..
       !! processed by numpydoc !!

.. py:property:: tlend
   :type: Optional[float]


   
   Get or set the Birth temperature end.
















   ..
       !! processed by numpydoc !!

.. py:property:: eghost
   :type: Optional[float]


   
   Get or set the Young's modulus for ghost (quiet) material.
















   ..
       !! processed by numpydoc !!

.. py:property:: pghost
   :type: Optional[float]


   
   Get or set the Poisson's ratio for ghost (quiet) material.
















   ..
       !! processed by numpydoc !!

.. py:property:: aghost
   :type: Optional[float]


   
   Get or set the Thermal expansion coefficient for ghost (quiet) material.
















   ..
       !! processed by numpydoc !!

.. py:property:: t2phase
   :type: Optional[float]


   
   Get or set the Temperature at which phase change commences.
















   ..
       !! processed by numpydoc !!

.. py:property:: t1phase
   :type: Optional[float]


   
   Get or set the Temperature at which phase change ends.
















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
   :value: 'CWM'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





