





:class:`MatAleMixingLength`
===========================


.. py:class:: mat_ale_mixing_length.MatAleMixingLength(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_ALE_MIXING_LENGTH keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatAleMixingLength

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
          * - :py:attr:`~pc`
            - Get or set the Pressure cutoff (<=0.0).
          * - :py:attr:`~mulo`
            - Get or set the There are 3 possible cases: (1) If MULO > 0.0, and MUHI = 0.0 or is
          * - :py:attr:`~muhi`
            - Get or set the Upper dynamic viscosity limit (default = 0.0). This is defined only if RK and RN are defined for the variable viscosity case
          * - :py:attr:`~rk`
            - Get or set the Variable dynamic viscosity multiplie
          * - :py:attr:`~rn`
            - Get or set the Variable dynamic viscosity exponent
          * - :py:attr:`~lci`
            - Get or set the Characteristic length, lci , of the internal turbulent domain.
          * - :py:attr:`~c1`
            - Get or set the Internal flow mixing length polynomial coefficients.
          * - :py:attr:`~c2`
            - Get or set the Internal flow mixing length polynomial coefficients.
          * - :py:attr:`~c3`
            - Get or set the Internal flow mixing length polynomial coefficients.
          * - :py:attr:`~c4`
            - Get or set the Internal flow mixing length polynomial coefficients
          * - :py:attr:`~c5`
            - Get or set the Internal flow mixing length polynomial coefficients
          * - :py:attr:`~c6`
            - Get or set the Internal flow mixing length polynomial coefficients
          * - :py:attr:`~c7`
            - Get or set the Internal flow mixing length polynomial coefficients
          * - :py:attr:`~lcx`
            - Get or set the Characteristic length, lcx , of the external turbulent domain.
          * - :py:attr:`~d0`
            - Get or set the External flow mixing length polynomial coefficients.
          * - :py:attr:`~d1`
            - Get or set the External flow mixing length polynomial coefficients.
          * - :py:attr:`~d2`
            - Get or set the External flow mixing length polynomial coefficients.
          * - :py:attr:`~e0`
            - Get or set the External flow mixing length polynomial coefficients
          * - :py:attr:`~e1`
            - Get or set the External flow mixing length polynomial coefficients
          * - :py:attr:`~e2`
            - Get or set the External flow mixing length polynomial coefficients
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

    from mat_ale_mixing_length import MatAleMixingLength

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

.. py:property:: pc
   :type: Optional[float]


   
   Get or set the Pressure cutoff (<=0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: mulo
   :type: Optional[float]


   
   Get or set the There are 3 possible cases: (1) If MULO > 0.0, and MUHI = 0.0 or is
   not defined, then this is the traditional constant dynamic viscosity coefficientμ. (2) If MULO > 0.0, and MUHI > 0.0, then MULO and
   MUHI are lower and upper viscosity limit values. (3) If MULO is negative (for example, MULO = -1), then a user-input data load
   curve (with LCID = 1) defining dynamic viscosity as a function of equivalent strain rate is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: muhi
   :type: Optional[float]


   
   Get or set the Upper dynamic viscosity limit (default = 0.0). This is defined only if RK and RN are defined for the variable viscosity case
















   ..
       !! processed by numpydoc !!

.. py:property:: rk
   :type: Optional[float]


   
   Get or set the Variable dynamic viscosity multiplie
















   ..
       !! processed by numpydoc !!

.. py:property:: rn
   :type: Optional[float]


   
   Get or set the Variable dynamic viscosity exponent
















   ..
       !! processed by numpydoc !!

.. py:property:: lci
   :type: Optional[float]


   
   Get or set the Characteristic length, lci , of the internal turbulent domain.
















   ..
       !! processed by numpydoc !!

.. py:property:: c1
   :type: Optional[float]


   
   Get or set the Internal flow mixing length polynomial coefficients.
















   ..
       !! processed by numpydoc !!

.. py:property:: c2
   :type: Optional[float]


   
   Get or set the Internal flow mixing length polynomial coefficients.
















   ..
       !! processed by numpydoc !!

.. py:property:: c3
   :type: Optional[float]


   
   Get or set the Internal flow mixing length polynomial coefficients.
















   ..
       !! processed by numpydoc !!

.. py:property:: c4
   :type: Optional[float]


   
   Get or set the Internal flow mixing length polynomial coefficients
















   ..
       !! processed by numpydoc !!

.. py:property:: c5
   :type: Optional[float]


   
   Get or set the Internal flow mixing length polynomial coefficients
















   ..
       !! processed by numpydoc !!

.. py:property:: c6
   :type: Optional[float]


   
   Get or set the Internal flow mixing length polynomial coefficients
















   ..
       !! processed by numpydoc !!

.. py:property:: c7
   :type: Optional[float]


   
   Get or set the Internal flow mixing length polynomial coefficients
















   ..
       !! processed by numpydoc !!

.. py:property:: lcx
   :type: Optional[float]


   
   Get or set the Characteristic length, lcx , of the external turbulent domain.
















   ..
       !! processed by numpydoc !!

.. py:property:: d0
   :type: Optional[float]


   
   Get or set the External flow mixing length polynomial coefficients.
















   ..
       !! processed by numpydoc !!

.. py:property:: d1
   :type: Optional[float]


   
   Get or set the External flow mixing length polynomial coefficients.
















   ..
       !! processed by numpydoc !!

.. py:property:: d2
   :type: Optional[float]


   
   Get or set the External flow mixing length polynomial coefficients.
















   ..
       !! processed by numpydoc !!

.. py:property:: e0
   :type: Optional[float]


   
   Get or set the External flow mixing length polynomial coefficients
















   ..
       !! processed by numpydoc !!

.. py:property:: e1
   :type: Optional[float]


   
   Get or set the External flow mixing length polynomial coefficients
















   ..
       !! processed by numpydoc !!

.. py:property:: e2
   :type: Optional[float]


   
   Get or set the External flow mixing length polynomial coefficients
















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
   :value: 'ALE_MIXING_LENGTH'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





