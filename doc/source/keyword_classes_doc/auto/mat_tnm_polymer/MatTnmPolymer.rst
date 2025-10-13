





:class:`MatTnmPolymer`
======================


.. py:class:: mat_tnm_polymer.MatTnmPolymer(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_TNM_POLYMER keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatTnmPolymer

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number or label be specified (see *PART).
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~roflg`
            - Get or set the Flag for whether density is specified per unit area or volume:
          * - :py:attr:`~mua`
            - Get or set the Shear modulus for network A
          * - :py:attr:`~thetah`
            - Get or set the Temperature factor
          * - :py:attr:`~lambl`
            - Get or set the Locking stretch
          * - :py:attr:`~kappa`
            - Get or set the Bulk modulus
          * - :py:attr:`~tauha`
            - Get or set the Flow resistance of network A
          * - :py:attr:`~a`
            - Get or set the Pressure dependence of flow
          * - :py:attr:`~ma`
            - Get or set the Stress exponential of network A
          * - :py:attr:`~n`
            - Get or set the Temperature exponential
          * - :py:attr:`~mubi`
            - Get or set the Initial shear modulus for network B
          * - :py:attr:`~mubf`
            - Get or set the Final shear modulus for network B
          * - :py:attr:`~beta`
            - Get or set the Evolution rate of shear modulus for network B
          * - :py:attr:`~tauhb`
            - Get or set the Flow resistance of network B
          * - :py:attr:`~mb`
            - Get or set the Stress exponential of network B
          * - :py:attr:`~muc`
            - Get or set the Shear modulus for network C
          * - :py:attr:`~q`
            - Get or set the Relative contribution of I2 on network C
          * - :py:attr:`~alpha`
            - Get or set the Thermal expansion coefficient
          * - :py:attr:`~theata0`
            - Get or set the Reference temperature
          * - :py:attr:`~ibulk`
            - Get or set the Internal bulk modulus
          * - :py:attr:`~ig`
            - Get or set the Internal shear modulus
          * - :py:attr:`~tsstif`
            - Get or set the Transversal stiffness for shells
          * - :py:attr:`~gamma0`
            - Get or set the Reference strain rate
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

    from mat_tnm_polymer import MatTnmPolymer

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number or label be specified (see *PART).
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density.
















   ..
       !! processed by numpydoc !!

.. py:property:: roflg
   :type: Optional[int]


   
   Get or set the Flag for whether density is specified per unit area or volume:
   EQ.0:   Density is per unit volume(default).
   EQ.1 : Density is per unit area for controlling the mass of cohesive elements with an initial volume of zero
















   ..
       !! processed by numpydoc !!

.. py:property:: mua
   :type: Optional[float]


   
   Get or set the Shear modulus for network A
















   ..
       !! processed by numpydoc !!

.. py:property:: thetah
   :type: Optional[float]


   
   Get or set the Temperature factor
















   ..
       !! processed by numpydoc !!

.. py:property:: lambl
   :type: Optional[float]


   
   Get or set the Locking stretch
















   ..
       !! processed by numpydoc !!

.. py:property:: kappa
   :type: Optional[float]


   
   Get or set the Bulk modulus
















   ..
       !! processed by numpydoc !!

.. py:property:: tauha
   :type: Optional[float]


   
   Get or set the Flow resistance of network A
















   ..
       !! processed by numpydoc !!

.. py:property:: a
   :type: Optional[float]


   
   Get or set the Pressure dependence of flow
















   ..
       !! processed by numpydoc !!

.. py:property:: ma
   :type: Optional[float]


   
   Get or set the Stress exponential of network A
















   ..
       !! processed by numpydoc !!

.. py:property:: n
   :type: Optional[float]


   
   Get or set the Temperature exponential
















   ..
       !! processed by numpydoc !!

.. py:property:: mubi
   :type: Optional[float]


   
   Get or set the Initial shear modulus for network B
















   ..
       !! processed by numpydoc !!

.. py:property:: mubf
   :type: Optional[float]


   
   Get or set the Final shear modulus for network B
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Evolution rate of shear modulus for network B
















   ..
       !! processed by numpydoc !!

.. py:property:: tauhb
   :type: Optional[float]


   
   Get or set the Flow resistance of network B
















   ..
       !! processed by numpydoc !!

.. py:property:: mb
   :type: Optional[float]


   
   Get or set the Stress exponential of network B
















   ..
       !! processed by numpydoc !!

.. py:property:: muc
   :type: Optional[float]


   
   Get or set the Shear modulus for network C
















   ..
       !! processed by numpydoc !!

.. py:property:: q
   :type: Optional[float]


   
   Get or set the Relative contribution of I2 on network C
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha
   :type: Optional[float]


   
   Get or set the Thermal expansion coefficient
















   ..
       !! processed by numpydoc !!

.. py:property:: theata0
   :type: Optional[float]


   
   Get or set the Reference temperature
















   ..
       !! processed by numpydoc !!

.. py:property:: ibulk
   :type: Optional[float]


   
   Get or set the Internal bulk modulus
















   ..
       !! processed by numpydoc !!

.. py:property:: ig
   :type: Optional[float]


   
   Get or set the Internal shear modulus
















   ..
       !! processed by numpydoc !!

.. py:property:: tsstif
   :type: Optional[float]


   
   Get or set the Transversal stiffness for shells
















   ..
       !! processed by numpydoc !!

.. py:property:: gamma0
   :type: Optional[float]


   
   Get or set the Reference strain rate
















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
   :value: 'TNM_POLYMER'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





