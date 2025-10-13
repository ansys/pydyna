





:class:`DualceseEosInflator2`
=============================


.. py:class:: dualcese_eos_inflator2.DualceseEosInflator2(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DUALCESE_EOS_INFLATOR2 keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DualceseEosInflator2

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~eosid`
            - Get or set the Equation of state identifier
          * - :py:attr:`~cp10`
            - Get or set the Coefficients of temperature-dependent specific heat at constant pressureCp1_(T) = Cp1_0 + Cp1_1 T + Cp1_2 T2 + Cp1_3 T3 + Cp1_4 T4
          * - :py:attr:`~cp11`
            - Get or set the Coefficients of temperature-dependent specific heat at constant pressureCp1_(T) = Cp1_0 + Cp1_1 T + Cp1_2 T2 + Cp1_3 T3 + Cp1_4 T4
          * - :py:attr:`~cp12`
            - Get or set the Coefficients of temperature-dependent specific heat at constant pressureCp1_(T) = Cp1_0 + Cp1_1 T + Cp1_2 T2 + Cp1_3 T3 + Cp1_4 T4
          * - :py:attr:`~cp13`
            - Get or set the Coefficients of temperature-dependent specific heat at constant pressureCp1_(T) = Cp1_0 + Cp1_1 T + Cp1_2 T2 + Cp1_3 T3 + Cp1_4 T4
          * - :py:attr:`~cp14`
            - Get or set the Coefficients of temperature - dependent specific heat at constant pressureCp1_(T) = Cp1_0 + Cp1_1 T + Cp1_2 T2 + Cp1_3 T3 + Cp1_4 T4
          * - :py:attr:`~cp20`
            - Get or set the Coefficients of temperature-dependent specific heat at constant volumeCp2_(T) = Cp2_0 + Cp2_1 T + Cp2_2 T2 + Cp2_3 T3 + Cp2_4 T4
          * - :py:attr:`~cp21`
            - Get or set the Coefficients of temperature-dependent specific heat at constant volumeCp2_(T) = Cp2_0 + Cp2_1 T + Cp2_2 T2 + Cp2_3 T3 + Cp2_4 T4
          * - :py:attr:`~cp22`
            - Get or set the Coefficients of temperature-dependent specific heat at constant volumeCp2_(T) = Cp2_0 + Cp2_1 T + Cp2_2 T2 + Cp2_3 T3 + Cp2_4 T4
          * - :py:attr:`~cp23`
            - Get or set the Coefficients of temperature-dependent specific heat at constant volumeCp2_(T) = Cp2_0 + Cp2_1 T + Cp2_2 T2 + Cp2_3 T3 + Cp2_4 T4
          * - :py:attr:`~cp24`
            - Get or set the Coefficients of temperature-dependent specific heat at constant volumeCp2_(T) = Cp2_0 + Cp2_1 T + Cp2_2 T2 + Cp2_3 T3 + Cp2_4 T4
          * - :py:attr:`~cv10`
            - Get or set the Coefficients of temperature-dependent specific heat at constant volumeCv1_(T) = Cv1_0 + Cv1_1 T + Cv1_2 T2 + Cv1_3 T3 + Cv1_4 T4
          * - :py:attr:`~cv11`
            - Get or set the Coefficients of temperature-dependent specific heat at constant volumeCv1_(T) = Cv1_0 + Cv1_1 T + Cv1_2 T2 + Cv1_3 T3 + Cv1_4 T4
          * - :py:attr:`~cv12`
            - Get or set the Coefficients of temperature-dependent specific heat at constant volumeCv1_(T) = Cv1_0 + Cv1_1 T + Cv1_2 T2 + Cv1_3 T3 + Cv1_4 T4
          * - :py:attr:`~cv13`
            - Get or set the Coefficients of temperature-dependent specific heat at constant volumeCv1_(T) = Cv1_0 + Cv1_1 T + Cv1_2 T2 + Cv1_3 T3 + Cv1_4 T4
          * - :py:attr:`~cv14`
            - Get or set the Coefficients of temperature-dependent specific heat at constant volumeCv1_(T) = Cv1_0 + Cv1_1 T + Cv1_2 T2 + Cv1_3 T3 + Cv1_4 T4
          * - :py:attr:`~cv20`
            - Get or set the Coefficients of temperature-dependent specific heat at constant volumeCv2_(T) = Cv2_0 + Cv2_1 T + Cv2_2 T2 + Cv2_3 T3 + Cv2_4 T4
          * - :py:attr:`~cv21`
            - Get or set the Coefficients of temperature-dependent specific heat at constant volumeCv2_(T) = Cv2_0 + Cv2_1 T + Cv2_2 T2 + Cv2_3 T3 + Cv2_4 T4
          * - :py:attr:`~cv22`
            - Get or set the Coefficients of temperature-dependent specific heat at constant volumeCv2_(T) = Cv2_0 + Cv2_1 T + Cv2_2 T2 + Cv2_3 T3 + Cv2_4 T4
          * - :py:attr:`~cv23`
            - Get or set the Coefficients of temperature-dependent specific heat at constant volumeCv2_(T) = Cv2_0 + Cv2_1 T + Cv2_2 T2 + Cv2_3 T3 + Cv2_4 T4
          * - :py:attr:`~cv24`
            - Get or set the Coefficients of temperature-dependent specific heat at constant volumeCv2_(T) = Cv2_0 + Cv2_1 T + Cv2_2 T2 + Cv2_3 T3 + Cv2_4 T4


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~keyword`
            - 
          * - :py:attr:`~subkeyword`
            - 






Import detail
-------------

.. code-block:: python

    from dualcese_eos_inflator2 import DualceseEosInflator2

Property detail
---------------

.. py:property:: eosid
   :type: Optional[int]


   
   Get or set the Equation of state identifier
















   ..
       !! processed by numpydoc !!

.. py:property:: cp10
   :type: float


   
   Get or set the Coefficients of temperature-dependent specific heat at constant pressureCp1_(T) = Cp1_0 + Cp1_1 T + Cp1_2 T2 + Cp1_3 T3 + Cp1_4 T4
















   ..
       !! processed by numpydoc !!

.. py:property:: cp11
   :type: float


   
   Get or set the Coefficients of temperature-dependent specific heat at constant pressureCp1_(T) = Cp1_0 + Cp1_1 T + Cp1_2 T2 + Cp1_3 T3 + Cp1_4 T4
















   ..
       !! processed by numpydoc !!

.. py:property:: cp12
   :type: float


   
   Get or set the Coefficients of temperature-dependent specific heat at constant pressureCp1_(T) = Cp1_0 + Cp1_1 T + Cp1_2 T2 + Cp1_3 T3 + Cp1_4 T4
















   ..
       !! processed by numpydoc !!

.. py:property:: cp13
   :type: float


   
   Get or set the Coefficients of temperature-dependent specific heat at constant pressureCp1_(T) = Cp1_0 + Cp1_1 T + Cp1_2 T2 + Cp1_3 T3 + Cp1_4 T4
















   ..
       !! processed by numpydoc !!

.. py:property:: cp14
   :type: float


   
   Get or set the Coefficients of temperature - dependent specific heat at constant pressureCp1_(T) = Cp1_0 + Cp1_1 T + Cp1_2 T2 + Cp1_3 T3 + Cp1_4 T4
















   ..
       !! processed by numpydoc !!

.. py:property:: cp20
   :type: float


   
   Get or set the Coefficients of temperature-dependent specific heat at constant volumeCp2_(T) = Cp2_0 + Cp2_1 T + Cp2_2 T2 + Cp2_3 T3 + Cp2_4 T4
















   ..
       !! processed by numpydoc !!

.. py:property:: cp21
   :type: float


   
   Get or set the Coefficients of temperature-dependent specific heat at constant volumeCp2_(T) = Cp2_0 + Cp2_1 T + Cp2_2 T2 + Cp2_3 T3 + Cp2_4 T4
















   ..
       !! processed by numpydoc !!

.. py:property:: cp22
   :type: float


   
   Get or set the Coefficients of temperature-dependent specific heat at constant volumeCp2_(T) = Cp2_0 + Cp2_1 T + Cp2_2 T2 + Cp2_3 T3 + Cp2_4 T4
















   ..
       !! processed by numpydoc !!

.. py:property:: cp23
   :type: float


   
   Get or set the Coefficients of temperature-dependent specific heat at constant volumeCp2_(T) = Cp2_0 + Cp2_1 T + Cp2_2 T2 + Cp2_3 T3 + Cp2_4 T4
















   ..
       !! processed by numpydoc !!

.. py:property:: cp24
   :type: float


   
   Get or set the Coefficients of temperature-dependent specific heat at constant volumeCp2_(T) = Cp2_0 + Cp2_1 T + Cp2_2 T2 + Cp2_3 T3 + Cp2_4 T4
















   ..
       !! processed by numpydoc !!

.. py:property:: cv10
   :type: float


   
   Get or set the Coefficients of temperature-dependent specific heat at constant volumeCv1_(T) = Cv1_0 + Cv1_1 T + Cv1_2 T2 + Cv1_3 T3 + Cv1_4 T4
















   ..
       !! processed by numpydoc !!

.. py:property:: cv11
   :type: float


   
   Get or set the Coefficients of temperature-dependent specific heat at constant volumeCv1_(T) = Cv1_0 + Cv1_1 T + Cv1_2 T2 + Cv1_3 T3 + Cv1_4 T4
















   ..
       !! processed by numpydoc !!

.. py:property:: cv12
   :type: float


   
   Get or set the Coefficients of temperature-dependent specific heat at constant volumeCv1_(T) = Cv1_0 + Cv1_1 T + Cv1_2 T2 + Cv1_3 T3 + Cv1_4 T4
















   ..
       !! processed by numpydoc !!

.. py:property:: cv13
   :type: float


   
   Get or set the Coefficients of temperature-dependent specific heat at constant volumeCv1_(T) = Cv1_0 + Cv1_1 T + Cv1_2 T2 + Cv1_3 T3 + Cv1_4 T4
















   ..
       !! processed by numpydoc !!

.. py:property:: cv14
   :type: float


   
   Get or set the Coefficients of temperature-dependent specific heat at constant volumeCv1_(T) = Cv1_0 + Cv1_1 T + Cv1_2 T2 + Cv1_3 T3 + Cv1_4 T4
















   ..
       !! processed by numpydoc !!

.. py:property:: cv20
   :type: float


   
   Get or set the Coefficients of temperature-dependent specific heat at constant volumeCv2_(T) = Cv2_0 + Cv2_1 T + Cv2_2 T2 + Cv2_3 T3 + Cv2_4 T4
















   ..
       !! processed by numpydoc !!

.. py:property:: cv21
   :type: float


   
   Get or set the Coefficients of temperature-dependent specific heat at constant volumeCv2_(T) = Cv2_0 + Cv2_1 T + Cv2_2 T2 + Cv2_3 T3 + Cv2_4 T4
















   ..
       !! processed by numpydoc !!

.. py:property:: cv22
   :type: float


   
   Get or set the Coefficients of temperature-dependent specific heat at constant volumeCv2_(T) = Cv2_0 + Cv2_1 T + Cv2_2 T2 + Cv2_3 T3 + Cv2_4 T4
















   ..
       !! processed by numpydoc !!

.. py:property:: cv23
   :type: float


   
   Get or set the Coefficients of temperature-dependent specific heat at constant volumeCv2_(T) = Cv2_0 + Cv2_1 T + Cv2_2 T2 + Cv2_3 T3 + Cv2_4 T4
















   ..
       !! processed by numpydoc !!

.. py:property:: cv24
   :type: float


   
   Get or set the Coefficients of temperature-dependent specific heat at constant volumeCv2_(T) = Cv2_0 + Cv2_1 T + Cv2_2 T2 + Cv2_3 T3 + Cv2_4 T4
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DUALCESE'


.. py:attribute:: subkeyword
   :value: 'EOS_INFLATOR2'






