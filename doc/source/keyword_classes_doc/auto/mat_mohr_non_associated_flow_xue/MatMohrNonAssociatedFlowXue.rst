





:class:`MatMohrNonAssociatedFlowXue`
====================================


.. py:class:: mat_mohr_non_associated_flow_xue.MatMohrNonAssociatedFlowXue(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_MOHR_NON_ASSOCIATED_FLOW_XUE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatMohrNonAssociatedFlowXue

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
          * - :py:attr:`~e`
            - Get or set the Young's Modulus.
          * - :py:attr:`~pr`
            - Get or set the Poisson's ratio.
          * - :py:attr:`~p12`
            - Get or set the Yield function parameters, defined by Lankford parameters in
          * - :py:attr:`~p22`
            - Get or set the Yield function parameters, defined by Lankford parameters in
          * - :py:attr:`~p33`
            - Get or set the Yield function parameters, defined by Lankford parameters in
          * - :py:attr:`~g12`
            - Get or set the Plastic flow potential parameters, defined by Lankford parameters
          * - :py:attr:`~g22`
            - Get or set the Plastic flow potential parameters, defined by Lankford parameters
          * - :py:attr:`~g33`
            - Get or set the Plastic flow potential parameters, defined by Lankford parameters
          * - :py:attr:`~lcids`
            - Get or set the Load curve ID defining stress vs. strain hardening behavior from a
          * - :py:attr:`~lcidv`
            - Get or set the Load curve ID defining stress scale factors vs. strain rates (Figure
          * - :py:attr:`~lcidt`
            - Get or set the Load curve ID defining stress scale factors vs. temperature in
          * - :py:attr:`~lfld`
            - Get or set the Load curve ID defining traditional Forming Limit Diagram for linear strain paths.
          * - :py:attr:`~lfrac`
            - Get or set the Load curve ID defining a fracture limit curve. Leave this field
          * - :py:attr:`~w0`
            - Get or set the Neck (FLD failure) width, typically is the blank thickness.
          * - :py:attr:`~a`
            - Get or set the Material parameters for the rate-dependent Hosford-Coulomb
          * - :py:attr:`~b0`
            - Get or set the Material parameters for the rate-dependent Hosford-Coulomb
          * - :py:attr:`~gamma`
            - Get or set the Material parameters for the rate-dependent Hosford-Coulomb
          * - :py:attr:`~c`
            - Get or set the Material parameters for the rate-dependent Hosford-Coulomb
          * - :py:attr:`~n`
            - Get or set the Material parameters for the rate-dependent Hosford-Coulomb
          * - :py:attr:`~scale`
            - Get or set the This variable can be used to speed up the simulation while
          * - :py:attr:`~size0`
            - Get or set the Fracture gage length used in an experimental measurement,        typically between 0.2~0.5mm.
          * - :py:attr:`~tref`
            - Get or set the Material parameters for strain softening effect due to temperature.
          * - :py:attr:`~tmelt`
            - Get or set the Material parameters for strain softening effect due to temperature.
          * - :py:attr:`~m`
            - Get or set the Material parameters for strain softening effect due to temperature.
          * - :py:attr:`~eta`
            - Get or set the Material parameters for strain softening effect due to temperature.
          * - :py:attr:`~cp`
            - Get or set the Material parameters for strain softening effect due to temperature.
          * - :py:attr:`~tini`
            - Get or set the Material parameters for strain softening effect due to temperature.
          * - :py:attr:`~depso`
            - Get or set the Material parameters for strain softening effect due to temperature.
          * - :py:attr:`~depsad`
            - Get or set the Material parameters for strain softening effect due to temperature.
          * - :py:attr:`~ef0`
            - Get or set the Material parameters for the option XUE. The parameter k in the
          * - :py:attr:`~plim`
            - Get or set the Material parameters for the option XUE. The parameter k in the
          * - :py:attr:`~q`
            - Get or set the Material parameters for the option XUE. The parameter k in the
          * - :py:attr:`~gama`
            - Get or set the Material parameters for the option XUE. The parameter k in the
          * - :py:attr:`~aopt`
            - Get or set the Material axes option (see MAT_OPTION TROPIC_ELASTIC for a more complete description):
          * - :py:attr:`~a1`
            - Get or set the Components of vector a for AOPT = 2.
          * - :py:attr:`~a2`
            - Get or set the Components of vector a for AOPT = 2.
          * - :py:attr:`~a3`
            - Get or set the Components of vector a for AOPT = 2.
          * - :py:attr:`~v1`
            - Get or set the Components of vector v for AOPT = 3.
          * - :py:attr:`~v2`
            - Get or set the Components of vector v for AOPT = 3.
          * - :py:attr:`~v3`
            - Get or set the Components of vector v for AOPT = 3.
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

    from mat_mohr_non_associated_flow_xue import MatMohrNonAssociatedFlowXue

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

.. py:property:: e
   :type: Optional[float]


   
   Get or set the Young's Modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: Optional[float]


   
   Get or set the Poisson's ratio.
















   ..
       !! processed by numpydoc !!

.. py:property:: p12
   :type: float


   
   Get or set the Yield function parameters, defined by Lankford parameters in
   rolling (0 degree), diagonal (45 degree) and transverse (90 degree) directions,
   respectively; see Non-associated flow rule.
















   ..
       !! processed by numpydoc !!

.. py:property:: p22
   :type: float


   
   Get or set the Yield function parameters, defined by Lankford parameters in
   rolling (0 degree), diagonal (45 degree) and transverse (90 degree) directions,
   respectively; see Non-associated flow rule.
















   ..
       !! processed by numpydoc !!

.. py:property:: p33
   :type: float


   
   Get or set the Yield function parameters, defined by Lankford parameters in
   rolling (0 degree), diagonal (45 degree) and transverse (90 degree) directions,
   respectively; see Non-associated flow rule.
















   ..
       !! processed by numpydoc !!

.. py:property:: g12
   :type: float


   
   Get or set the Plastic flow potential parameters, defined by Lankford parameters
   in rolling (0 degree), diagonal (45 degree) and transverse (90 degree) directions,
   respectively; see Non-associated flow rule.
















   ..
       !! processed by numpydoc !!

.. py:property:: g22
   :type: float


   
   Get or set the Plastic flow potential parameters, defined by Lankford parameters
   in rolling (0 degree), diagonal (45 degree) and transverse (90 degree) directions,
   respectively; see Non-associated flow rule.
















   ..
       !! processed by numpydoc !!

.. py:property:: g33
   :type: float


   
   Get or set the Plastic flow potential parameters, defined by Lankford parameters
   in rolling (0 degree), diagonal (45 degree) and transverse (90 degree) directions,
   respectively; see Non-associated flow rule.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcids
   :type: Optional[int]


   
   Get or set the Load curve ID defining stress vs. strain hardening behavior from a
   uniaxial tension test; must be along the rolling direction. Also see A
   modified Johnson-Cook.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidv
   :type: Optional[int]


   
   Get or set the Load curve ID defining stress scale factors vs. strain rates (Figure
   M260B-1 middle); determined from experiments. Strain rates are
   stored in history variable #5. Strain rate scale factors are stored in
   history variable #6. To turn on the variables for viewing in LSPrePost,
   set NEIPS to at least "6" in *DATABASE_EXTENT_BINARY.
   It is very useful to know what levels of strain rates, and strain
   rate scale factors in a particular simulation. Once d3plot files are
   opened in LS-PrePost, individual element time history can be plotted
   via menu option Post → History, or a color contour of the entire part
   can be viewed with the menu option Post → FriComp → Misc. Also
   see A modified Johnson-Cook.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidt
   :type: Optional[int]


   
   Get or set the Load curve ID defining stress scale factors vs. temperature in
   Kelvin (Figure M260B-1 bottom), determined from experiments.
   Temperatures are stored in history variable #4. Temperature scale
   factors are stored in history variable #7. To turn on this variable for
   viewing in LS-PrePost, set NEIPS to at least "7" in
   *DATABASE_EXTENT_BINARY. It is very useful to know what
   levels of temperatures and temperature scale factors in a particular
   simulation. Once d3plot files are opened in LS-PrePost, individual
   element time history can be plotted via menu option Post → History,
   or a color contour of the entire part can be viewed with the menu
   option Post → FriComp → Misc. Also see A modified Johnson-Cook..
















   ..
       !! processed by numpydoc !!

.. py:property:: lfld
   :type: int


   
   Get or set the Load curve ID defining traditional Forming Limit Diagram for linear strain paths.
















   ..
       !! processed by numpydoc !!

.. py:property:: lfrac
   :type: Optional[int]


   
   Get or set the Load curve ID defining a fracture limit curve. Leave this field
   empty if parameters A, B0, GAMMA, C, N are defined. However, if
   this field is defined, parameters A, B0, GAMMA, C, N will be
   ignored even if they are defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: w0
   :type: Optional[float]


   
   Get or set the Neck (FLD failure) width, typically is the blank thickness.
















   ..
       !! processed by numpydoc !!

.. py:property:: a
   :type: Optional[float]


   
   Get or set the Material parameters for the rate-dependent Hosford-Coulomb
   fracture initiation model, see Rate-dependent Hosford-Coulomb.
   Ignored if LFRAC is defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: b0
   :type: Optional[float]


   
   Get or set the Material parameters for the rate-dependent Hosford-Coulomb
   fracture initiation model, see Rate-dependent Hosford-Coulomb.
   Ignored if LFRAC is defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: gamma
   :type: Optional[float]


   
   Get or set the Material parameters for the rate-dependent Hosford-Coulomb
   fracture initiation model, see Rate-dependent Hosford-Coulomb.
   Ignored if LFRAC is defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: c
   :type: Optional[float]


   
   Get or set the Material parameters for the rate-dependent Hosford-Coulomb
   fracture initiation model, see Rate-dependent Hosford-Coulomb.
   Ignored if LFRAC is defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: n
   :type: Optional[float]


   
   Get or set the Material parameters for the rate-dependent Hosford-Coulomb
   fracture initiation model, see Rate-dependent Hosford-Coulomb.
   Ignored if LFRAC is defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: scale
   :type: float


   
   Get or set the This variable can be used to speed up the simulation while
   equalizing the strain rate effect, useful especially in cases where the
   pulling speed or punch speed is slow. For example, if the pulling
   speed is at 15 mm/s but running the simulation at this speed will
   take a long time, the pulling speed can be increased to 500 mm/s
   while "SCALE" can be set to 0.03, giving the same results as those
   from 15 mm/s, but with the benefit of greatly reduced computational
   time, see examples and Figures in *MAT_260A for details.
   Furthermore, the increased absolute value (within a reasonable
   range) of mass scaling -1.0*dt2ms frequently used in forming
   simulation does not affect the strain rates, as shown in the examples
   and Figures in *MAT_260A.
















   ..
       !! processed by numpydoc !!

.. py:property:: size0
   :type: Optional[float]


   
   Get or set the Fracture gage length used in an experimental measurement,        typically between 0.2~0.5mm.
















   ..
       !! processed by numpydoc !!

.. py:property:: tref
   :type: Optional[float]


   
   Get or set the Material parameters for strain softening effect due to temperature.
   TINI is the initial temperature. See A modified Johnson-Cook for
   other parameters' definitions..
















   ..
       !! processed by numpydoc !!

.. py:property:: tmelt
   :type: Optional[float]


   
   Get or set the Material parameters for strain softening effect due to temperature.
   TINI is the initial temperature. See A modified Johnson-Cook for
   other parameters' definitions.
















   ..
       !! processed by numpydoc !!

.. py:property:: m
   :type: Optional[float]


   
   Get or set the Material parameters for strain softening effect due to temperature.
   TINI is the initial temperature. See A modified Johnson-Cook for
   other parameters' definitions.
















   ..
       !! processed by numpydoc !!

.. py:property:: eta
   :type: Optional[float]


   
   Get or set the Material parameters for strain softening effect due to temperature.
   TINI is the initial temperature. See A modified Johnson-Cook for
   other parameters' definitions.
















   ..
       !! processed by numpydoc !!

.. py:property:: cp
   :type: Optional[float]


   
   Get or set the Material parameters for strain softening effect due to temperature.
   TINI is the initial temperature. See A modified Johnson-Cook for
   other parameters' definitions.
















   ..
       !! processed by numpydoc !!

.. py:property:: tini
   :type: Optional[float]


   
   Get or set the Material parameters for strain softening effect due to temperature.
   TINI is the initial temperature. See A modified Johnson-Cook for
   other parameters' definitions.
















   ..
       !! processed by numpydoc !!

.. py:property:: depso
   :type: Optional[float]


   
   Get or set the Material parameters for strain softening effect due to temperature.
   TINI is the initial temperature. See A modified Johnson-Cook for
   other parameters' definitions.
















   ..
       !! processed by numpydoc !!

.. py:property:: depsad
   :type: Optional[float]


   
   Get or set the Material parameters for strain softening effect due to temperature.
   TINI is the initial temperature. See A modified Johnson-Cook for
   other parameters' definitions.
















   ..
       !! processed by numpydoc !!

.. py:property:: ef0
   :type: Optional[float]


   
   Get or set the Material parameters for the option XUE. The parameter k in the
   original paper is assumed to be 1.0. For details, refer to Xue, L.,
   Wierzbicki, T.'s 2009 paper "Numerical simulation of fracture mode
   transition in ductile plates" in the International Journal of Solids and Structures
















   ..
       !! processed by numpydoc !!

.. py:property:: plim
   :type: Optional[float]


   
   Get or set the Material parameters for the option XUE. The parameter k in the
   original paper is assumed to be 1.0. For details, refer to Xue, L.,
   Wierzbicki, T.'s 2009 paper "Numerical simulation of fracture mode
   transition in ductile plates" in the International Journal of Solids and Structures
















   ..
       !! processed by numpydoc !!

.. py:property:: q
   :type: Optional[float]


   
   Get or set the Material parameters for the option XUE. The parameter k in the
   original paper is assumed to be 1.0. For details, refer to Xue, L.,
   Wierzbicki, T.'s 2009 paper "Numerical simulation of fracture mode
   transition in ductile plates" in the International Journal of Solids and Structures
















   ..
       !! processed by numpydoc !!

.. py:property:: gama
   :type: Optional[float]


   
   Get or set the Material parameters for the option XUE. The parameter k in the
   original paper is assumed to be 1.0. For details, refer to Xue, L.,
   Wierzbicki, T.'s 2009 paper "Numerical simulation of fracture mode
   transition in ductile plates" in the International Journal of Solids and Structures.
















   ..
       !! processed by numpydoc !!

.. py:property:: aopt
   :type: Optional[float]


   
   Get or set the Material axes option (see MAT_OPTION TROPIC_ELASTIC for a more complete description):
   EQ.0.0: locally orthotropic with material axes determined by element nodes 1, 2, and 4, as with *DEFINE_COORDINATE_NODES, and then rotated about the shell element normal by theangle BETA.
   EQ.2.0: globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDINATE_VECTOR:
   EQ.3.0: locally orthotropic material axes determined by rotating the material axes about the element normal by an angle, BETA, from a line in the plane of the element defined by the cross product of the vector v with the element normal:
   LT.0.0: the absolute value of AOPT is a coordinate system ID number (CID on *DEFINE_COORDINATE_NODES, *DEFINE_COORDINATE_SYSTEM or *DEFINE__COORDINATE_VECTOR)..
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: Optional[float]


   
   Get or set the Components of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the Components of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: a3
   :type: Optional[float]


   
   Get or set the Components of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: v1
   :type: Optional[float]


   
   Get or set the Components of vector v for AOPT = 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: v2
   :type: Optional[float]


   
   Get or set the Components of vector v for AOPT = 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: v3
   :type: Optional[float]


   
   Get or set the Components of vector v for AOPT = 3.
















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
   :value: 'MOHR_NON_ASSOCIATED_FLOW_XUE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





