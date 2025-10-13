





:class:`MatWtmStmPlc`
=====================


.. py:class:: mat_wtm_stm_plc.MatWtmStmPlc(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_WTM_STM_PLC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatWtmStmPlc

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number has to be chosen.
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~e`
            - Get or set the Young's modulus
          * - :py:attr:`~pr`
            - Get or set the Poisson's ratio
          * - :py:attr:`~numfi`
            - Get or set the Number of through thickness integration points that must fail before the element is deleted (remember to change this number if switching between full and reduced integration type of elements).
          * - :py:attr:`~npsc`
            - Get or set the Critical value   of the plastic thickness strain (used in the CTS fracture criterion).
          * - :py:attr:`~wc`
            - Get or set the Critical value   for the Cockcroft-Latham fracture criterion
          * - :py:attr:`~tauc`
            - Get or set the Critical value   for the Bressan-Williams shear fracture criterion
          * - :py:attr:`~sigma0`
            - Get or set the Initial mean value of yield stress  .
          * - :py:attr:`~qr1`
            - Get or set the Isotropic hardening parameter .
          * - :py:attr:`~cr1`
            - Get or set the Isotropic hardening parameter
          * - :py:attr:`~qr2`
            - Get or set the Isotropic hardening parameter
          * - :py:attr:`~cr2`
            - Get or set the Isotropic hardening parameter
          * - :py:attr:`~k`
            - Get or set the equals half YLD2003 exponent  .  Recommended value for FCC materials is  , i.e.  .
          * - :py:attr:`~a1`
            - Get or set the Yld2003 parameter  .
          * - :py:attr:`~a2`
            - Get or set the Yld2003 parameter .
          * - :py:attr:`~a3`
            - Get or set the Yld2003 parameter
          * - :py:attr:`~a4`
            - Get or set the Yld2003 parameter
          * - :py:attr:`~a5`
            - Get or set the Yld2003 parameter
          * - :py:attr:`~a6`
            - Get or set the Yld2003 parameter
          * - :py:attr:`~a7`
            - Get or set the Yld2003 parameter
          * - :py:attr:`~a8`
            - Get or set the Yld2003 parameter
          * - :py:attr:`~s`
            - Get or set the Dynamic strain aging parameter, S.
          * - :py:attr:`~h`
            - Get or set the Dynamic strain aging parameter, H.
          * - :py:attr:`~omega`
            - Get or set the Dynamic strain aging parameter,  .
          * - :py:attr:`~td`
            - Get or set the Dynamic strain aging parameter,  .
          * - :py:attr:`~alpha`
            - Get or set the Dynamic strain aging parameter,  .
          * - :py:attr:`~eps0`
            - Get or set the Dynamic strain aging parameter,  .
          * - :py:attr:`~aopt`
            - Get or set the Material axes option:
          * - :py:attr:`~beta`
            - Get or set the Material angle in degrees for AOPT=3, may be overwritten on the element card, see *ELEMENT_SHELL_BETA or *ELEMENT_ SOLID_ORTHO..
          * - :py:attr:`~xp`
            - Get or set the Coordinates of point p for AOPT = 1..
          * - :py:attr:`~yp`
            - Get or set the Coordinates of point p for AOPT = 1..
          * - :py:attr:`~zp`
            - Get or set the Coordinates of point p for AOPT = 1..
          * - :py:attr:`~v1`
            - Get or set the Components of vector v for AOPT = 3.
          * - :py:attr:`~v2`
            - Get or set the Components of vector v for AOPT = 3.
          * - :py:attr:`~v3`
            - Get or set the Components of vector v for AOPT = 3
          * - :py:attr:`~d1`
            - Get or set the Components of vector d for AOPT = 2.
          * - :py:attr:`~d2`
            - Get or set the Components of vector d for AOPT = 2.
          * - :py:attr:`~d3`
            - Get or set the Components of vector d for AOPT = 2.
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

    from mat_wtm_stm_plc import MatWtmStmPlc

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number has to be chosen.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density.
















   ..
       !! processed by numpydoc !!

.. py:property:: e
   :type: Optional[float]


   
   Get or set the Young's modulus
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: Optional[float]


   
   Get or set the Poisson's ratio
















   ..
       !! processed by numpydoc !!

.. py:property:: numfi
   :type: Optional[float]


   
   Get or set the Number of through thickness integration points that must fail before the element is deleted (remember to change this number if switching between full and reduced integration type of elements).
















   ..
       !! processed by numpydoc !!

.. py:property:: npsc
   :type: Optional[float]


   
   Get or set the Critical value   of the plastic thickness strain (used in the CTS fracture criterion).
















   ..
       !! processed by numpydoc !!

.. py:property:: wc
   :type: Optional[float]


   
   Get or set the Critical value   for the Cockcroft-Latham fracture criterion
















   ..
       !! processed by numpydoc !!

.. py:property:: tauc
   :type: Optional[float]


   
   Get or set the Critical value   for the Bressan-Williams shear fracture criterion
















   ..
       !! processed by numpydoc !!

.. py:property:: sigma0
   :type: Optional[float]


   
   Get or set the Initial mean value of yield stress  .
















   ..
       !! processed by numpydoc !!

.. py:property:: qr1
   :type: Optional[float]


   
   Get or set the Isotropic hardening parameter .
















   ..
       !! processed by numpydoc !!

.. py:property:: cr1
   :type: Optional[float]


   
   Get or set the Isotropic hardening parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: qr2
   :type: Optional[float]


   
   Get or set the Isotropic hardening parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: cr2
   :type: Optional[float]


   
   Get or set the Isotropic hardening parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: k
   :type: Optional[float]


   
   Get or set the equals half YLD2003 exponent  .  Recommended value for FCC materials is  , i.e.  .
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: Optional[float]


   
   Get or set the Yld2003 parameter  .
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the Yld2003 parameter .
















   ..
       !! processed by numpydoc !!

.. py:property:: a3
   :type: Optional[float]


   
   Get or set the Yld2003 parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: a4
   :type: Optional[float]


   
   Get or set the Yld2003 parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: a5
   :type: Optional[float]


   
   Get or set the Yld2003 parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: a6
   :type: Optional[float]


   
   Get or set the Yld2003 parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: a7
   :type: Optional[float]


   
   Get or set the Yld2003 parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: a8
   :type: Optional[float]


   
   Get or set the Yld2003 parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: s
   :type: Optional[float]


   
   Get or set the Dynamic strain aging parameter, S.
















   ..
       !! processed by numpydoc !!

.. py:property:: h
   :type: Optional[float]


   
   Get or set the Dynamic strain aging parameter, H.
















   ..
       !! processed by numpydoc !!

.. py:property:: omega
   :type: Optional[float]


   
   Get or set the Dynamic strain aging parameter,  .
















   ..
       !! processed by numpydoc !!

.. py:property:: td
   :type: Optional[float]


   
   Get or set the Dynamic strain aging parameter,  .
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha
   :type: Optional[float]


   
   Get or set the Dynamic strain aging parameter,  .
















   ..
       !! processed by numpydoc !!

.. py:property:: eps0
   :type: Optional[float]


   
   Get or set the Dynamic strain aging parameter,  .
















   ..
       !! processed by numpydoc !!

.. py:property:: aopt
   :type: Optional[float]


   
   Get or set the Material axes option:
   EQ.0.0:  Locally orthotropic with material axes determined by element nodes as shown in Figure M2-1, and then rotated
   about the shell element normal by the angle BETA.       Nodes 1, 2 and 4 of an element are identical to the nodes
   used for the definition of a coordinate system as by *DEFINE_COORDINATE_NODES..
   EQ.2.0: globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDI_NATE_VECTOR.
   EQ.3.0: locally orthotropic material axes determined by rotating the material axes about the element normal by an angle,
   BETA, from a line in the plane of the element defined by        the cross product of the vector v with the element normal.
   LT.0.0: the absolute value of AOPT is a coordinate system ID number (CID on *DEFINE_COORDINATE_NODES,
   *DEFINE_COORDINATE_SYSTEM or *DEFINE_COOR_DINATE_VECTOR). Available with the R3 release of Version 971 and later.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Material angle in degrees for AOPT=3, may be overwritten on the element card, see *ELEMENT_SHELL_BETA or *ELEMENT_ SOLID_ORTHO..
















   ..
       !! processed by numpydoc !!

.. py:property:: xp
   :type: Optional[float]


   
   Get or set the Coordinates of point p for AOPT = 1..
















   ..
       !! processed by numpydoc !!

.. py:property:: yp
   :type: Optional[float]


   
   Get or set the Coordinates of point p for AOPT = 1..
















   ..
       !! processed by numpydoc !!

.. py:property:: zp
   :type: Optional[float]


   
   Get or set the Coordinates of point p for AOPT = 1..
















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


   
   Get or set the Components of vector v for AOPT = 3
















   ..
       !! processed by numpydoc !!

.. py:property:: d1
   :type: Optional[float]


   
   Get or set the Components of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: d2
   :type: Optional[float]


   
   Get or set the Components of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: d3
   :type: Optional[float]


   
   Get or set the Components of vector d for AOPT = 2.
















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
   :value: 'WTM_STM_PLC'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





