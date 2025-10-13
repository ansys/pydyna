





:class:`MatThermalOrthotropic`
==============================


.. py:class:: mat_thermal_orthotropic.MatThermalOrthotropic(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_THERMAL_ORTHOTROPIC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatThermalOrthotropic

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~tmid`
            - Get or set the Thermal material identification, a unique number has to be used.
          * - :py:attr:`~tro`
            - Get or set the Thermal density:
          * - :py:attr:`~tgrlc`
            - Get or set the Thermal generation rate (see *DEFINE_CURVE):
          * - :py:attr:`~tgmult`
            - Get or set the Thermal generation rate multiplier:
          * - :py:attr:`~aopt`
            - Get or set the Material axes definition:
          * - :py:attr:`~tlat`
            - Get or set the Phase change temperature.
          * - :py:attr:`~hlat`
            - Get or set the Latent heat.
          * - :py:attr:`~hc`
            - Get or set the Heat capacity.
          * - :py:attr:`~k1`
            - Get or set the Thermal conductivity K1 in local x-direction.
          * - :py:attr:`~k2`
            - Get or set the Thermal conductivity K2 in local y-direction.
          * - :py:attr:`~k3`
            - Get or set the Thermal conductivity K3 in local z-direction.
          * - :py:attr:`~xp`
            - Get or set the x-coordinate of point p for AOPT = 1 and 4.
          * - :py:attr:`~yp`
            - Get or set the y-coordinate of point p for AOPT = 1 and 4.
          * - :py:attr:`~zp`
            - Get or set the z-coordinate of point p for AOPT = 1 and 4.
          * - :py:attr:`~a1`
            - Get or set the Component of vector a for AOPT = 2.
          * - :py:attr:`~a2`
            - Get or set the Component of vector a for AOPT = 2.
          * - :py:attr:`~a3`
            - Get or set the Component of vector a for AOPT = 2.
          * - :py:attr:`~d1`
            - Get or set the Component of vector d for AOPT = 2,3 and 4.
          * - :py:attr:`~d2`
            - Get or set the Component of vector d for AOPT = 2,3 and 4.
          * - :py:attr:`~d3`
            - Get or set the Component of vector d for AOPT = 2,3 and 4.
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

    from mat_thermal_orthotropic import MatThermalOrthotropic

Property detail
---------------

.. py:property:: tmid
   :type: Optional[int]


   
   Get or set the Thermal material identification, a unique number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: tro
   :type: Optional[float]


   
   Get or set the Thermal density:
   EQ 0.0 structural density(default).
















   ..
       !! processed by numpydoc !!

.. py:property:: tgrlc
   :type: Optional[int]


   
   Get or set the Thermal generation rate (see *DEFINE_CURVE):
   GT.0:   Load curve ID defining thermal generation rate as a function of time
   EQ.0 : Thermal generation rate is the constant multiplier, TGMULT.
   LT.0 : | TGRLC | is a load curve ID giving thermal generation rate as a function of temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: tgmult
   :type: Optional[float]


   
   Get or set the Thermal generation rate multiplier:
   EQ.0.0: no heat generation.
















   ..
       !! processed by numpydoc !!

.. py:property:: aopt
   :type: float


   
   Get or set the Material axes definition:
   EQ.0.0: locally orthotropic with material axes by element nodes N1, N2 and N4,
   EQ.1.0: locally orthotropic with material axes determined by a point in space and global location of element center,
   EQ.2.0: globally orthotropic with material axes determined by vectors.
   EQ.3.0: Locally orthotropic with first material axis orthogonal to element normal (defined by element nodes N1, N2 and N4) and to a vector d- Third material direction corresponds to element normal.
   EQ.4.0: Local orthogonal in cylindrical coordinates with the material axes determined by a vector d,and an originating point, P, which define the centerline axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: tlat
   :type: Optional[float]


   
   Get or set the Phase change temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: hlat
   :type: Optional[float]


   
   Get or set the Latent heat.
















   ..
       !! processed by numpydoc !!

.. py:property:: hc
   :type: Optional[float]


   
   Get or set the Heat capacity.
















   ..
       !! processed by numpydoc !!

.. py:property:: k1
   :type: Optional[float]


   
   Get or set the Thermal conductivity K1 in local x-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: k2
   :type: Optional[float]


   
   Get or set the Thermal conductivity K2 in local y-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: k3
   :type: Optional[float]


   
   Get or set the Thermal conductivity K3 in local z-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: xp
   :type: Optional[float]


   
   Get or set the x-coordinate of point p for AOPT = 1 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: yp
   :type: Optional[float]


   
   Get or set the y-coordinate of point p for AOPT = 1 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: zp
   :type: Optional[float]


   
   Get or set the z-coordinate of point p for AOPT = 1 and 4.
















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

.. py:property:: d1
   :type: Optional[float]


   
   Get or set the Component of vector d for AOPT = 2,3 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: d2
   :type: Optional[float]


   
   Get or set the Component of vector d for AOPT = 2,3 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: d3
   :type: Optional[float]


   
   Get or set the Component of vector d for AOPT = 2,3 and 4.
















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
   :value: 'THERMAL_ORTHOTROPIC'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





