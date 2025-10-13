





:class:`MatVegter`
==================


.. py:class:: mat_vegter.MatVegter(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_VEGTER keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatVegter

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification.  A unique number or label must be specified
          * - :py:attr:`~ro`
            - Get or set the Material density
          * - :py:attr:`~e`
            - Get or set the Elastic Young's modulus
          * - :py:attr:`~pr`
            - Get or set the Poisson's ratio
          * - :py:attr:`~n`
            - Get or set the Order of Fourier series (i.e., number of test groups minus one).  The minimum number for N is 2, and the maximum is 12
          * - :py:attr:`~fbi`
            - Get or set the Normalized yield stress for equibiaxial test
          * - :py:attr:`~rbi0`
            - Get or set the Initial strain ratio for equibiaxial test
          * - :py:attr:`~lcid`
            - Get or set the Stress-strain curve ID.  If defined, SYS, SIP, SHS, and SHL are ignored
          * - :py:attr:`~sys`
            - Get or set the Static yield stress
          * - :py:attr:`~sip`
            - Get or set the Stress increment parameter
          * - :py:attr:`~shs`
            - Get or set the Strain hardening parameter for small strain
          * - :py:attr:`~shl`
            - Get or set the Strain hardening parameter for larger strain
          * - :py:attr:`~esh`
            - Get or set the Exponent for strain hardening
          * - :py:attr:`~e0`
            - Get or set the Initial plastic strain
          * - :py:attr:`~alpha`
            - Get or set the distribution of hardening used in the curve-fitting.    pure kinematic hardening and   provides pure isotropic hardening
          * - :py:attr:`~lcid2`
            - Get or set the Curve ID.  The curve defines Young's modulus change with respect to the plastic strain.  By default it is assumed that Young's modulus remains constant.  Effective value is between 0-1
          * - :py:attr:`~aopt`
            - Get or set the Material axes option:
          * - :py:attr:`~xp`
            - Get or set the Coordinates of point p for AOPT = 1
          * - :py:attr:`~yp`
            - Get or set the Coordinates of point p for AOPT = 1
          * - :py:attr:`~zp`
            - Get or set the Coordinates of point p for AOPT = 1
          * - :py:attr:`~a1`
            - Get or set the Components of vector a for AOPT = 2
          * - :py:attr:`~a2`
            - Get or set the Components of vector a for AOPT = 2
          * - :py:attr:`~a3`
            - Get or set the Components of vector a for AOPT = 2
          * - :py:attr:`~v1`
            - Get or set the Components of vector v for AOPT = 3
          * - :py:attr:`~v2`
            - Get or set the Components of vector v for AOPT = 3
          * - :py:attr:`~v3`
            - Get or set the Components of vector v for AOPT = 3
          * - :py:attr:`~d1`
            - Get or set the Components of vector d for AOPT = 4
          * - :py:attr:`~d2`
            - Get or set the Components of vector d for AOPT = 4
          * - :py:attr:`~d3`
            - Get or set the Components of vector d for AOPT = 4
          * - :py:attr:`~beta`
            - Get or set the Material angle in degrees for AOPT=3, may be overwritten on the element card
          * - :py:attr:`~fun_i`
            - Get or set the Normalized yield stress for uniaxial test for the ith direction
          * - :py:attr:`~run_i`
            - Get or set the Strain ratio for uniaxial test for the ith direction
          * - :py:attr:`~fps1_i`
            - Get or set the First normalized yield stress for plain strain test for the ith direction
          * - :py:attr:`~fps2_i`
            - Get or set the Second normalized yield stress for plain strain test for the ith direction
          * - :py:attr:`~fsh_i`
            - Get or set the First normalized yield stress for pure shear test for the ith direction
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

    from mat_vegter import MatVegter

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification.  A unique number or label must be specified
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Material density
















   ..
       !! processed by numpydoc !!

.. py:property:: e
   :type: Optional[float]


   
   Get or set the Elastic Young's modulus
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: Optional[float]


   
   Get or set the Poisson's ratio
















   ..
       !! processed by numpydoc !!

.. py:property:: n
   :type: Optional[int]


   
   Get or set the Order of Fourier series (i.e., number of test groups minus one).  The minimum number for N is 2, and the maximum is 12
















   ..
       !! processed by numpydoc !!

.. py:property:: fbi
   :type: Optional[float]


   
   Get or set the Normalized yield stress for equibiaxial test
















   ..
       !! processed by numpydoc !!

.. py:property:: rbi0
   :type: Optional[float]


   
   Get or set the Initial strain ratio for equibiaxial test
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[float]


   
   Get or set the Stress-strain curve ID.  If defined, SYS, SIP, SHS, and SHL are ignored
















   ..
       !! processed by numpydoc !!

.. py:property:: sys
   :type: Optional[float]


   
   Get or set the Static yield stress
















   ..
       !! processed by numpydoc !!

.. py:property:: sip
   :type: Optional[float]


   
   Get or set the Stress increment parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: shs
   :type: Optional[float]


   
   Get or set the Strain hardening parameter for small strain
















   ..
       !! processed by numpydoc !!

.. py:property:: shl
   :type: Optional[float]


   
   Get or set the Strain hardening parameter for larger strain
















   ..
       !! processed by numpydoc !!

.. py:property:: esh
   :type: Optional[float]


   
   Get or set the Exponent for strain hardening
















   ..
       !! processed by numpydoc !!

.. py:property:: e0
   :type: Optional[float]


   
   Get or set the Initial plastic strain
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha
   :type: Optional[float]


   
   Get or set the distribution of hardening used in the curve-fitting.    pure kinematic hardening and   provides pure isotropic hardening
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid2
   :type: Optional[float]


   
   Get or set the Curve ID.  The curve defines Young's modulus change with respect to the plastic strain.  By default it is assumed that Young's modulus remains constant.  Effective value is between 0-1
















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

.. py:property:: xp
   :type: Optional[float]


   
   Get or set the Coordinates of point p for AOPT = 1
















   ..
       !! processed by numpydoc !!

.. py:property:: yp
   :type: Optional[float]


   
   Get or set the Coordinates of point p for AOPT = 1
















   ..
       !! processed by numpydoc !!

.. py:property:: zp
   :type: Optional[float]


   
   Get or set the Coordinates of point p for AOPT = 1
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: Optional[float]


   
   Get or set the Components of vector a for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the Components of vector a for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: a3
   :type: Optional[float]


   
   Get or set the Components of vector a for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: v1
   :type: Optional[float]


   
   Get or set the Components of vector v for AOPT = 3
















   ..
       !! processed by numpydoc !!

.. py:property:: v2
   :type: Optional[float]


   
   Get or set the Components of vector v for AOPT = 3
















   ..
       !! processed by numpydoc !!

.. py:property:: v3
   :type: Optional[float]


   
   Get or set the Components of vector v for AOPT = 3
















   ..
       !! processed by numpydoc !!

.. py:property:: d1
   :type: Optional[float]


   
   Get or set the Components of vector d for AOPT = 4
















   ..
       !! processed by numpydoc !!

.. py:property:: d2
   :type: Optional[float]


   
   Get or set the Components of vector d for AOPT = 4
















   ..
       !! processed by numpydoc !!

.. py:property:: d3
   :type: Optional[float]


   
   Get or set the Components of vector d for AOPT = 4
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Material angle in degrees for AOPT=3, may be overwritten on the element card
















   ..
       !! processed by numpydoc !!

.. py:property:: fun_i
   :type: Optional[float]


   
   Get or set the Normalized yield stress for uniaxial test for the ith direction
















   ..
       !! processed by numpydoc !!

.. py:property:: run_i
   :type: Optional[float]


   
   Get or set the Strain ratio for uniaxial test for the ith direction
















   ..
       !! processed by numpydoc !!

.. py:property:: fps1_i
   :type: Optional[float]


   
   Get or set the First normalized yield stress for plain strain test for the ith direction
















   ..
       !! processed by numpydoc !!

.. py:property:: fps2_i
   :type: Optional[float]


   
   Get or set the Second normalized yield stress for plain strain test for the ith direction
















   ..
       !! processed by numpydoc !!

.. py:property:: fsh_i
   :type: Optional[float]


   
   Get or set the First normalized yield stress for pure shear test for the ith direction
















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
   :value: 'VEGTER'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





