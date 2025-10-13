





:class:`MatJohnsonCookStochastic`
=================================


.. py:class:: mat_johnson_cook_stochastic.MatJohnsonCookStochastic(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_JOHNSON_COOK_STOCHASTIC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatJohnsonCookStochastic

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
          * - :py:attr:`~e`
            - Get or set the Young's Modulus (shell elements only).
          * - :py:attr:`~pr`
            - Get or set the Poisson's ratio (shell elements only).
          * - :py:attr:`~dtf`
            - Get or set the Minimum time step size for automatic element deletion (shell elements).
          * - :py:attr:`~vp`
            - Get or set the Formulation for rate effects:
          * - :py:attr:`~rateop`
            - Get or set the Optional forms of strain-rate term:
          * - :py:attr:`~a`
            - Get or set the See equations in the keyword manual page 62 (volume two).
          * - :py:attr:`~b`
            - Get or set the See equations in the keyword manual page 62 (volume two).
          * - :py:attr:`~n`
            - Get or set the See equations in the keyword manual page 62 (volume two).
          * - :py:attr:`~c`
            - Get or set the See equations in the keyword manual page 62 (volume two).
          * - :py:attr:`~m`
            - Get or set the See equations in the keyword manual page 62 (volume two).
          * - :py:attr:`~tm`
            - Get or set the Melt temperature.
          * - :py:attr:`~tr`
            - Get or set the Room temperature.
          * - :py:attr:`~epso`
            - Get or set the Effective plastic strain rate. This value depends on the time units.  Typically, input 1 for units of seconds, 0.001 for units of milliseconds, 0.000001 for microseconds, etc.
          * - :py:attr:`~cp`
            - Get or set the Specific heat.
          * - :py:attr:`~pc`
            - Get or set the Failure stress or pressure cutoff (pmin < 0.0).
          * - :py:attr:`~spall`
            - Get or set the Spall type:
          * - :py:attr:`~it`
            - Get or set the Plastic strain iteration options. This input applies to solid elements only since it is always necessary to iterate for the shell element plane stress condition.
          * - :py:attr:`~d1`
            - Get or set the Failure parameter. See equations in the keyword manual page 62 (volume two).
          * - :py:attr:`~d2`
            - Get or set the Failure parameter. See equations in the keyword manual page 62 (volume two).
          * - :py:attr:`~d3`
            - Get or set the Failure parameter. See equations in the keyword manual page 62 (volume two).
          * - :py:attr:`~d4`
            - Get or set the Failure parameter. See equations in the keyword manual page 62 (volume two).
          * - :py:attr:`~d5`
            - Get or set the Failure parameter. Please see equations in the keyword manual page 62 (volume two).
          * - :py:attr:`~c2_p_xnp_d`
            - Get or set the Optional strain-rate parameter for Huh-Kang (C2) or Cowper-Symonds (P) forms; see equations below
          * - :py:attr:`~erod`
            - Get or set the Erosion flag:
          * - :py:attr:`~efmin`
            - Get or set the The lower bound for calculated strain at fracture
          * - :py:attr:`~numint`
            - Get or set the Number of through thickness integration points which must fail before the shell element is deleted. (If zero, all points must fail.)
          * - :py:attr:`~k`
            - Get or set the Optional strain-rate parameter for Couque term
          * - :py:attr:`~eps1`
            - Get or set the Optional reference strain rate for Couque term, characterizing the transition between the thermally activated regime and the viscous regime. Input in units of [time ]^(-1)
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

    from mat_johnson_cook_stochastic import MatJohnsonCookStochastic

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

.. py:property:: e
   :type: Optional[float]


   
   Get or set the Young's Modulus (shell elements only).
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: Optional[float]


   
   Get or set the Poisson's ratio (shell elements only).
















   ..
       !! processed by numpydoc !!

.. py:property:: dtf
   :type: Optional[float]


   
   Get or set the Minimum time step size for automatic element deletion (shell elements).
















   ..
       !! processed by numpydoc !!

.. py:property:: vp
   :type: float


   
   Get or set the Formulation for rate effects:
   EQ.0.0: Scale yield stress (default),
   EQ.1.0: Viscoplastic formulation.
















   ..
       !! processed by numpydoc !!

.. py:property:: rateop
   :type: float


   
   Get or set the Optional forms of strain-rate term:
   EQ.0.0:  Log-Linear Johnson-Cook (default),
   EQ.1.0:  Log-Quadratic Huh-Kang (2 parameters),
   EQ.2.0:  Exponential Allen-Rule-Jones,
   EQ.3.0:  Exponential Cowper-Symonds (2 parameters).
   EQ.4.0: nonlinear rate coefficient (2 parameters)
   EQ.5.0: log - exponential Couque(4 parameters)
















   ..
       !! processed by numpydoc !!

.. py:property:: a
   :type: Optional[float]


   
   Get or set the See equations in the keyword manual page 62 (volume two).
















   ..
       !! processed by numpydoc !!

.. py:property:: b
   :type: Optional[float]


   
   Get or set the See equations in the keyword manual page 62 (volume two).
















   ..
       !! processed by numpydoc !!

.. py:property:: n
   :type: Optional[float]


   
   Get or set the See equations in the keyword manual page 62 (volume two).
















   ..
       !! processed by numpydoc !!

.. py:property:: c
   :type: Optional[float]


   
   Get or set the See equations in the keyword manual page 62 (volume two).
















   ..
       !! processed by numpydoc !!

.. py:property:: m
   :type: Optional[float]


   
   Get or set the See equations in the keyword manual page 62 (volume two).
















   ..
       !! processed by numpydoc !!

.. py:property:: tm
   :type: Optional[float]


   
   Get or set the Melt temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: tr
   :type: Optional[float]


   
   Get or set the Room temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: epso
   :type: Optional[float]


   
   Get or set the Effective plastic strain rate. This value depends on the time units.  Typically, input 1 for units of seconds, 0.001 for units of milliseconds, 0.000001 for microseconds, etc.
















   ..
       !! processed by numpydoc !!

.. py:property:: cp
   :type: Optional[float]


   
   Get or set the Specific heat.
















   ..
       !! processed by numpydoc !!

.. py:property:: pc
   :type: Optional[float]


   
   Get or set the Failure stress or pressure cutoff (pmin < 0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: spall
   :type: float


   
   Get or set the Spall type:
   EQ.0.0: default is set to 2.0,
   EQ. 1.0: p => pmin ,
   EQ. 2.0: if sigma-max  => -pmin element spalls and tension, p < 0, is never allowed (default),
   EQ. 3.0: p < -pmin element spalls and tension, p < 0, is never allowed.
















   ..
       !! processed by numpydoc !!

.. py:property:: it
   :type: float


   
   Get or set the Plastic strain iteration options. This input applies to solid elements only since it is always necessary to iterate for the shell element plane stress condition.
   EQ. 0.0: no iterations (default),
   EQ. 1.0: accurate iterative solution for plastic strain. Much more expensive than default.
















   ..
       !! processed by numpydoc !!

.. py:property:: d1
   :type: Optional[float]


   
   Get or set the Failure parameter. See equations in the keyword manual page 62 (volume two).
















   ..
       !! processed by numpydoc !!

.. py:property:: d2
   :type: Optional[float]


   
   Get or set the Failure parameter. See equations in the keyword manual page 62 (volume two).
















   ..
       !! processed by numpydoc !!

.. py:property:: d3
   :type: Optional[float]


   
   Get or set the Failure parameter. See equations in the keyword manual page 62 (volume two).
















   ..
       !! processed by numpydoc !!

.. py:property:: d4
   :type: Optional[float]


   
   Get or set the Failure parameter. See equations in the keyword manual page 62 (volume two).
















   ..
       !! processed by numpydoc !!

.. py:property:: d5
   :type: Optional[float]


   
   Get or set the Failure parameter. Please see equations in the keyword manual page 62 (volume two).
















   ..
       !! processed by numpydoc !!

.. py:property:: c2_p_xnp_d
   :type: Optional[float]


   
   Get or set the Optional strain-rate parameter for Huh-Kang (C2) or Cowper-Symonds (P) forms; see equations below
















   ..
       !! processed by numpydoc !!

.. py:property:: erod
   :type: Optional[float]


   
   Get or set the Erosion flag:
   EQ.0.0: element erosion allowed(default).
   NE.0.0 : element does not erode; deviatoric stresses set to zero when element fails.
















   ..
       !! processed by numpydoc !!

.. py:property:: efmin
   :type: float


   
   Get or set the The lower bound for calculated strain at fracture
















   ..
       !! processed by numpydoc !!

.. py:property:: numint
   :type: Optional[float]


   
   Get or set the Number of through thickness integration points which must fail before the shell element is deleted. (If zero, all points must fail.)
   Since nodal fiber rotations limit strains at active integration points, the default, which is to require that all integration points fail, is not recommended, because elements undergoing large strain are often not deleted using this criterion.Better results may be obtained when NUMINT is set to 1 or a number less than one half of the number of through thickness points.
















   ..
       !! processed by numpydoc !!

.. py:property:: k
   :type: Optional[float]


   
   Get or set the Optional strain-rate parameter for Couque term
















   ..
       !! processed by numpydoc !!

.. py:property:: eps1
   :type: Optional[float]


   
   Get or set the Optional reference strain rate for Couque term, characterizing the transition between the thermally activated regime and the viscous regime. Input in units of [time ]^(-1)
















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
   :value: 'JOHNSON_COOK_STOCHASTIC'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





