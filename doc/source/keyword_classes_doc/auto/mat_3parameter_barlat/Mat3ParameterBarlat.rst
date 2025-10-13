





:class:`Mat3ParameterBarlat`
============================


.. py:class:: mat_3parameter_barlat.Mat3ParameterBarlat(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_3PARAMETER_BARLAT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: Mat3ParameterBarlat

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
          * - :py:attr:`~e`
            - Get or set the Young's modulus.
          * - :py:attr:`~pr`
            - Get or set the Poisson's ratio.
          * - :py:attr:`~hr`
            - Get or set the Hardening rule:
          * - :py:attr:`~p1`
            - Get or set the Material parameter: HR.
          * - :py:attr:`~p2`
            - Get or set the Material parameter: HR.
          * - :py:attr:`~iter`
            - Get or set the Iteration flag for speed:
          * - :py:attr:`~m`
            - Get or set the m, exponent in Barlat's yield surface.
          * - :py:attr:`~r00`
            - Get or set the R00 , Lankford parmeter determined from experiments.
          * - :py:attr:`~r45`
            - Get or set the R45 , Lankford parmeter determined from experiments.
          * - :py:attr:`~r90`
            - Get or set the R90 , Lankford parmeter determined from experiments.
          * - :py:attr:`~lcid`
            - Get or set the Load curve/table ID for hardening in the 0 degree direction.
          * - :py:attr:`~e0`
            - Get or set the epsilon-0 for determining initial yield stress for exponential hardening (default = 0.0).
          * - :py:attr:`~spi`
            - Get or set the spi, if epsilon-0 is zero above (default = 0.0).
          * - :py:attr:`~p3`
            - Get or set the Material parameter:
          * - :py:attr:`~crc1`
            - Get or set the Chaboche-Roussilier kinematic hardening parameter1
          * - :py:attr:`~cra1`
            - Get or set the Chaboche-Roussilier kinematic hardening parameter
          * - :py:attr:`~crc2`
            - Get or set the Chaboche-Roussilier kinematic hardening parameter
          * - :py:attr:`~cra2`
            - Get or set the Chaboche-Roussilier kinematic hardening parameter
          * - :py:attr:`~crc3`
            - Get or set the Chaboche-Roussilier kinematic hardening parameter
          * - :py:attr:`~cra3`
            - Get or set the Chaboche-Roussilier kinematic hardening parameter
          * - :py:attr:`~crc4`
            - Get or set the Chaboche-Roussilier kinematic hardening parameter
          * - :py:attr:`~cra4`
            - Get or set the Chaboche-Roussilier kinematic hardening parameter
          * - :py:attr:`~aopt`
            - Get or set the Material axes option:
          * - :py:attr:`~c`
            - Get or set the C in Cowper-Symonds strain rate model
          * - :py:attr:`~p`
            - Get or set the p in Cowper-Symonds strain rate model, p=0.0 for no strain rate effects
          * - :py:attr:`~vlcid`
            - Get or set the Volume correction curve ID defining the relative volume change (change in volume relative to the initial volume) as a function of the effective plastic strain.  This is only used when nonzero.
          * - :py:attr:`~pb`
            - Get or set the Barlat89 parameter, p. If PB > 0, parameters AB, CB, and HB are read instead of R00, R45, and R90.
          * - :py:attr:`~hta`
            - Get or set the Load curve/Table ID for postforming parameter A in heat treatment
          * - :py:attr:`~htb`
            - Get or set the Load curve/Table ID for postforming parameter B in heat treatment
          * - :py:attr:`~xp`
            - Get or set the x-coordinates of point p for AOPT = 1.
          * - :py:attr:`~yp`
            - Get or set the y-coordinates of point p for AOPT = 1.
          * - :py:attr:`~zp`
            - Get or set the z-coordinates of point p for AOPT = 1.
          * - :py:attr:`~a1`
            - Get or set the Component of vector a for AOPT = 2.
          * - :py:attr:`~a2`
            - Get or set the Component of vector a for AOPT = 2.
          * - :py:attr:`~a3`
            - Get or set the Component of vector a for AOPT = 2.
          * - :py:attr:`~htc`
            - Get or set the Load curve/Table ID for postforming parameter C in heat treatment
          * - :py:attr:`~htd`
            - Get or set the Load curve/Table ID for postforming parameter D in heat treatment
          * - :py:attr:`~v1`
            - Get or set the Component of vector v for AOPT = 3.
          * - :py:attr:`~v2`
            - Get or set the Component of vector v for AOPT = 3.
          * - :py:attr:`~v3`
            - Get or set the Component of vector v for AOPT = 3.
          * - :py:attr:`~d1`
            - Get or set the Component of vector d for AOPT = 2.
          * - :py:attr:`~d2`
            - Get or set the Component of vector d for AOPT = 2.
          * - :py:attr:`~d3`
            - Get or set the Component of vector d for AOPT = 2.
          * - :py:attr:`~beta`
            - Get or set the Material angle in degrees for AOPT = 3, which may be overridden on the element card, see *ELEMENT_SHELL.
          * - :py:attr:`~htflag`
            - Get or set the Heat treatment flag:
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

    from mat_3parameter_barlat import Mat3ParameterBarlat

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

.. py:property:: hr
   :type: float


   
   Get or set the Hardening rule:
   EQ.1.0: linear (default),
   EQ.2.0: exponential.
   EQ.3.0: load curve.
   EQ.4.0: exponential (Voce)
   EQ.5.0: exponential (Gosh)
   EQ.6.0: exponential (Hocket-Sherby)
   EQ.7.0 load curve in three directions
   EQ.8.0: table with temperature dependence
   EQ.9.0: 3d table with temperature and strain rate dependence
















   ..
       !! processed by numpydoc !!

.. py:property:: p1
   :type: float


   
   Get or set the Material parameter: HR.
   EQ.1.0: Tangent modulus, HR.
   EQ.2.0: k, strength coefficient for exponential hardening.
















   ..
       !! processed by numpydoc !!

.. py:property:: p2
   :type: float


   
   Get or set the Material parameter: HR.
   EQ.1.0: Yield stress HR.
   EQ.2.0: n, exponent.
















   ..
       !! processed by numpydoc !!

.. py:property:: iter
   :type: float


   
   Get or set the Iteration flag for speed:
   ITER.EQ.0.0: fully iterative
   ITER.EQ.1.0: fixed at three iterations
   Generally, ITER=0 is recommended. However, ITER=1 is somewhat faster and may give acceptable results in most problems.
















   ..
       !! processed by numpydoc !!

.. py:property:: m
   :type: Optional[float]


   
   Get or set the m, exponent in Barlat's yield surface.
















   ..
       !! processed by numpydoc !!

.. py:property:: r00
   :type: Optional[float]


   
   Get or set the R00 , Lankford parmeter determined from experiments.
















   ..
       !! processed by numpydoc !!

.. py:property:: r45
   :type: Optional[float]


   
   Get or set the R45 , Lankford parmeter determined from experiments.
















   ..
       !! processed by numpydoc !!

.. py:property:: r90
   :type: Optional[float]


   
   Get or set the R90 , Lankford parmeter determined from experiments.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve/table ID for hardening in the 0 degree direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: e0
   :type: Optional[float]


   
   Get or set the epsilon-0 for determining initial yield stress for exponential hardening (default = 0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: spi
   :type: Optional[float]


   
   Get or set the spi, if epsilon-0 is zero above (default = 0.0).
   EQ.0.0: e0 = (E/k )**[1/(n -1)]
   LT..02: e0 = spi
   GT..02: e0 = (spi/k)**[1/n].
















   ..
       !! processed by numpydoc !!

.. py:property:: p3
   :type: Optional[float]


   
   Get or set the Material parameter:
   HR EQ.5.0: p,parameter for Gosh exponential hardening
   HR EQ.6.0: n,exponent for Hocket-Sherby exponential hardening
















   ..
       !! processed by numpydoc !!

.. py:property:: crc1
   :type: Optional[float]


   
   Get or set the Chaboche-Roussilier kinematic hardening parameter1
















   ..
       !! processed by numpydoc !!

.. py:property:: cra1
   :type: Optional[float]


   
   Get or set the Chaboche-Roussilier kinematic hardening parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: crc2
   :type: Optional[float]


   
   Get or set the Chaboche-Roussilier kinematic hardening parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: cra2
   :type: Optional[float]


   
   Get or set the Chaboche-Roussilier kinematic hardening parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: crc3
   :type: Optional[float]


   
   Get or set the Chaboche-Roussilier kinematic hardening parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: cra3
   :type: Optional[float]


   
   Get or set the Chaboche-Roussilier kinematic hardening parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: crc4
   :type: Optional[float]


   
   Get or set the Chaboche-Roussilier kinematic hardening parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: cra4
   :type: Optional[float]


   
   Get or set the Chaboche-Roussilier kinematic hardening parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: aopt
   :type: Optional[float]


   
   Get or set the Material axes option:
   EQ.0.0: locally orthotropic with material axes determined by
   element nodes 1, 2, and 4, as with *DEFINE_COORDINATE_NODES, and then rotated about the shell element normal by the angle BETA.
   EQ.2.0: globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDI_NATE_VECTOR.
   EQ.3.0: locally orthotropic material axes determined by rotating the material axes about the element normal by an angle,
   BETA, from a line in the plane of the element defined by        the cross product of the vector v with the element normal.
   LT.0.0: the absolute value of AOPT is a coordinate system ID number (CID on *DEFINE_COORDINATE_NODES,
   *DEFINE_COORDINATE_SYSTEM or *DEFINE_COOR_DINATE_VECTOR). Available with the R3 release of Version 971 and later.
















   ..
       !! processed by numpydoc !!

.. py:property:: c
   :type: Optional[float]


   
   Get or set the C in Cowper-Symonds strain rate model
















   ..
       !! processed by numpydoc !!

.. py:property:: p
   :type: Optional[float]


   
   Get or set the p in Cowper-Symonds strain rate model, p=0.0 for no strain rate effects
















   ..
       !! processed by numpydoc !!

.. py:property:: vlcid
   :type: Optional[int]


   
   Get or set the Volume correction curve ID defining the relative volume change (change in volume relative to the initial volume) as a function of the effective plastic strain.  This is only used when nonzero.
















   ..
       !! processed by numpydoc !!

.. py:property:: pb
   :type: Optional[float]


   
   Get or set the Barlat89 parameter, p. If PB > 0, parameters AB, CB, and HB are read instead of R00, R45, and R90.
















   ..
       !! processed by numpydoc !!

.. py:property:: hta
   :type: Optional[int]


   
   Get or set the Load curve/Table ID for postforming parameter A in heat treatment
















   ..
       !! processed by numpydoc !!

.. py:property:: htb
   :type: Optional[float]


   
   Get or set the Load curve/Table ID for postforming parameter B in heat treatment
















   ..
       !! processed by numpydoc !!

.. py:property:: xp
   :type: Optional[float]


   
   Get or set the x-coordinates of point p for AOPT = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: yp
   :type: Optional[float]


   
   Get or set the y-coordinates of point p for AOPT = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: zp
   :type: Optional[float]


   
   Get or set the z-coordinates of point p for AOPT = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: Optional[float]


   
   Get or set the Component of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the Component of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: a3
   :type: Optional[float]


   
   Get or set the Component of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: htc
   :type: Optional[int]


   
   Get or set the Load curve/Table ID for postforming parameter C in heat treatment
















   ..
       !! processed by numpydoc !!

.. py:property:: htd
   :type: Optional[int]


   
   Get or set the Load curve/Table ID for postforming parameter D in heat treatment
















   ..
       !! processed by numpydoc !!

.. py:property:: v1
   :type: Optional[float]


   
   Get or set the Component of vector v for AOPT = 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: v2
   :type: Optional[float]


   
   Get or set the Component of vector v for AOPT = 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: v3
   :type: Optional[float]


   
   Get or set the Component of vector v for AOPT = 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: d1
   :type: Optional[float]


   
   Get or set the Component of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: d2
   :type: Optional[float]


   
   Get or set the Component of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: d3
   :type: Optional[float]


   
   Get or set the Component of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Material angle in degrees for AOPT = 3, which may be overridden on the element card, see *ELEMENT_SHELL.
















   ..
       !! processed by numpydoc !!

.. py:property:: htflag
   :type: Optional[float]


   
   Get or set the Heat treatment flag:
   EQ.0: Preforming stage
   EQ.1: Heat treatment stage
   EQ.2: Postforming stage
















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
   :value: '3PARAMETER_BARLAT'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





