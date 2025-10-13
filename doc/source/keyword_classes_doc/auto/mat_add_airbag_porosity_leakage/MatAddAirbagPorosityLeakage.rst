





:class:`MatAddAirbagPorosityLeakage`
====================================


.. py:class:: mat_add_airbag_porosity_leakage.MatAddAirbagPorosityLeakage(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_ADD_AIRBAG_POROSITY_LEAKAGE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatAddAirbagPorosityLeakage

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material ID for which the porosity leakage property applies
          * - :py:attr:`~x2_flc`
            - Get or set the X2 is one of the coefficients of the porosity in the equation of Anagonye  and Wang [1999].  (Defined below in description for X0/X1)
          * - :py:attr:`~x3_fac`
            - Get or set the X3 is one of the coefficients of the porosity in the equation of Anagonye and Wang [1999].  (Defined below in description for X0/X1)
          * - :py:attr:`~ela`
            - Get or set the Effective leakage area for blocked fabric, ELA.
          * - :py:attr:`~fvopt`
            - Get or set the Fabric venting option.
          * - :py:attr:`~x0`
            - Get or set the Coefficients of Anagonye and Wang [1999] porosity equation for the leakage area:
          * - :py:attr:`~x1`
            - Get or set the Coefficients of Anagonye and Wang [1999] porosity equation for the leakage area:
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

    from mat_add_airbag_porosity_leakage import MatAddAirbagPorosityLeakage

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material ID for which the porosity leakage property applies
















   ..
       !! processed by numpydoc !!

.. py:property:: x2_flc
   :type: Optional[float]


   
   Get or set the X2 is one of the coefficients of the porosity in the equation of Anagonye  and Wang [1999].  (Defined below in description for X0/X1)
   Optional fabric porous leakage flow coefficient:
   GE.0.0: fabric porous leakage flow coefficient
   LT.0.0 : | FLC | is the load curve ID of the curve defining FLC as a function of time.
















   ..
       !! processed by numpydoc !!

.. py:property:: x3_fac
   :type: float


   
   Get or set the X3 is one of the coefficients of the porosity in the equation of Anagonye and Wang [1999].  (Defined below in description for X0/X1)
   Optional fabric characteristic parameter:
   GE.0.0: optional fabric characteristic parameter
   LT.0.0 : | FAC | is the load curve ID of the curve defining FAC as a function of absolute pressure.
















   ..
       !! processed by numpydoc !!

.. py:property:: ela
   :type: Optional[float]


   
   Get or set the Effective leakage area for blocked fabric, ELA.
   LT.0.0: | ELA | is the load curve ID of the curve defining ELA as a function of time.The default value of zero assumes that no leakage occurs.A value of .10 would assume that 10 % of the blocked fabric is leaking gas.
















   ..
       !! processed by numpydoc !!

.. py:property:: fvopt
   :type: Optional[float]


   
   Get or set the Fabric venting option.
   EQ.1:   Wang - Nefske formulas for venting through an orifice are used.Blockage is not considered.
   EQ.2 : Wang - Nefske formulas for venting through an orifice are used.Blockage of venting area due to contact is considered.
   EQ.3 : Leakage formulas of Graefe, Krummheuer,and Siejak[1990] are used.Blockage is not considered.
   EQ.4 : Leakage formulas of Graefe, Krummheuer,and Siejak[1990] are used.Blockage of venting area due to contact is considered.
   EQ.5 : Leakage formulas based on flow through a porous media are used.Blockage is not considered.
   EQ.6 : Leakage formulas based on flow through a porous media are used.Blockage of venting area due to contact is considered.
   EQ.7 : Leakage is based on gas volume outflow as a function of pressure load curve[Lian, 2000].Blockage is not considered.Absolute pressure is used in the porous - velocity - versus - pressure load curve, given as FAC.
   EQ.8 : Leakage is based on gas volume outflow as a function of pressure load curve[Lian 2000].Blockage of venting or porous area due to contact is considered.Absolute pressure is used in the porous - velocity - versus - pressure load curve, given as FAC.
















   ..
       !! processed by numpydoc !!

.. py:property:: x0
   :type: Optional[float]


   
   Get or set the Coefficients of Anagonye and Wang [1999] porosity equation for the leakage area:
















   ..
       !! processed by numpydoc !!

.. py:property:: x1
   :type: Optional[float]


   
   Get or set the Coefficients of Anagonye and Wang [1999] porosity equation for the leakage area:
















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
   :value: 'ADD_AIRBAG_POROSITY_LEAKAGE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





