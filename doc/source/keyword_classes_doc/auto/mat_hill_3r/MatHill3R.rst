





:class:`MatHill3R`
==================


.. py:class:: mat_hill_3r.MatHill3R(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_HILL_3R keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatHill3R

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
            - Get or set the Young's modulus, E.
          * - :py:attr:`~pr`
            - Get or set the Poisson's ratio, v.
          * - :py:attr:`~hr`
            - Get or set the Hardening rule:
          * - :py:attr:`~p1`
            - Get or set the Material parameter:
          * - :py:attr:`~p2`
            - Get or set the Material parameter:
          * - :py:attr:`~r00`
            - Get or set the R00, Lankford parameter determined from experiments.
          * - :py:attr:`~r45`
            - Get or set the R45, Lankford parameter determined from experiments.
          * - :py:attr:`~r90`
            - Get or set the R90, Lankford parameter determined from experiments.
          * - :py:attr:`~lcid`
            - Get or set the load curve ID for the load curve hardening rule
          * - :py:attr:`~e0`
            - Get or set the E0 for determining initial yield stress for exponential hardening. (Default=0.0).
          * - :py:attr:`~aopt`
            - Get or set the Material axes option:
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
          * - :py:attr:`~d1`
            - Get or set the Components of vector d for AOPT = 2.
          * - :py:attr:`~d2`
            - Get or set the Components of vector d for AOPT = 2.
          * - :py:attr:`~d3`
            - Get or set the Components of vector d for AOPT = 2.
          * - :py:attr:`~beta`
            - Get or set the Material angle in degrees for AOPT = 3, may be overridden on the element card, see *ELEMENT_SHELL_BETA.
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

    from mat_hill_3r import MatHill3R

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


   
   Get or set the Young's modulus, E.
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: Optional[float]


   
   Get or set the Poisson's ratio, v.
















   ..
       !! processed by numpydoc !!

.. py:property:: hr
   :type: float


   
   Get or set the Hardening rule:
   EQ.1.0: linear (default),
   EQ.2.0: exponential.
   EQ3.0: load curve.
















   ..
       !! processed by numpydoc !!

.. py:property:: p1
   :type: Optional[float]


   
   Get or set the Material parameter:
   HR.EQ.1.0: Tangent modulus,
   HR.EQ.2.0: k, strength coefficient for exponential harding.
















   ..
       !! processed by numpydoc !!

.. py:property:: p2
   :type: Optional[float]


   
   Get or set the Material parameter:
   HR.EQ.1.0: Yield stress
   HR.EQ.2.0: n, exponent.
















   ..
       !! processed by numpydoc !!

.. py:property:: r00
   :type: Optional[float]


   
   Get or set the R00, Lankford parameter determined from experiments.
















   ..
       !! processed by numpydoc !!

.. py:property:: r45
   :type: Optional[float]


   
   Get or set the R45, Lankford parameter determined from experiments.
















   ..
       !! processed by numpydoc !!

.. py:property:: r90
   :type: Optional[float]


   
   Get or set the R90, Lankford parameter determined from experiments.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the load curve ID for the load curve hardening rule
















   ..
       !! processed by numpydoc !!

.. py:property:: e0
   :type: Optional[float]


   
   Get or set the E0 for determining initial yield stress for exponential hardening. (Default=0.0).
















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

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Material angle in degrees for AOPT = 3, may be overridden on the element card, see *ELEMENT_SHELL_BETA.
















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
   :value: 'HILL_3R'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





