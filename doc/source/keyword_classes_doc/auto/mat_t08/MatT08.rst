





:class:`MatT08`
===============


.. py:class:: mat_t08.MatT08(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_T08 keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatT08

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~tmid`
            - Get or set the Thermal material identification. A unique number or label must be specified.
          * - :py:attr:`~tro`
            - Get or set the Thermal density:
          * - :py:attr:`~tgrlc`
            - Get or set the Thermal generation rate curve / table ID(see * DEFINE_‌CURVE) :
          * - :py:attr:`~tgmult`
            - Get or set the Thermal generation rate multiplier.Defines a volumetric heat rate (W/m^3 in SI units system).:
          * - :py:attr:`~aopt`
            - Get or set the Material axes definition:
          * - :py:attr:`~tlat`
            - Get or set the Phase change temperature
          * - :py:attr:`~hlat`
            - Get or set the Latent heat
          * - :py:attr:`~lcc`
            - Get or set the Load curve ID defining specific heat as a function of temperature, or if |ILCCHSV| > 0:
          * - :py:attr:`~lck1`
            - Get or set the Load curve ID defining thermal conductivity, K_i(i = 1,2,3), in the local(x,y,z) - direction as a function of temperature, or if |ILCKHSV > 0:
          * - :py:attr:`~lck2`
            - Get or set the Load curve ID defining thermal conductivity, K_i(i = 1,2,3), in the local(x,y,z) - direction as a function of temperature, or if |ILCKHSV > 0:
          * - :py:attr:`~lck3`
            - Get or set the Load curve ID defining thermal conductivity, K_i(i = 1,2,3), in the local(x,y,z) - direction as a function of temperature, or if |ILCKHSV > 0:
          * - :py:attr:`~ilcchsv`
            - Get or set the Optional:
          * - :py:attr:`~ilckhsv`
            - Get or set the Optional:
          * - :py:attr:`~itghsv`
            - Get or set the Optional:
          * - :py:attr:`~xp`
            - Get or set the Define coordinate of point p for AOPT = 1 and 4
          * - :py:attr:`~yp`
            - Get or set the Define coordinate of point p for AOPT = 1 and 4
          * - :py:attr:`~zp`
            - Get or set the Define coordinate of point p for AOPT = 1 and 4
          * - :py:attr:`~a1`
            - Get or set the Define components of vector a for AOPT = 2
          * - :py:attr:`~a2`
            - Get or set the Define components of vector a for AOPT = 2
          * - :py:attr:`~a3`
            - Get or set the Define components of vector a for AOPT = 2
          * - :py:attr:`~d1`
            - Get or set the Define components of vector d for AOPT = 2,3 and 4
          * - :py:attr:`~d2`
            - Get or set the Define components of vector d for AOPT = 2,3 nd 4
          * - :py:attr:`~d3`
            - Get or set the Define components of vector d for AOPT = 2,3 and 4
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

    from mat_t08 import MatT08

Property detail
---------------

.. py:property:: tmid
   :type: Optional[int]


   
   Get or set the Thermal material identification. A unique number or label must be specified.
















   ..
       !! processed by numpydoc !!

.. py:property:: tro
   :type: Optional[float]


   
   Get or set the Thermal density:
   EQ.0.0: default to structural density
















   ..
       !! processed by numpydoc !!

.. py:property:: tgrlc
   :type: Optional[int]


   
   Get or set the Thermal generation rate curve / table ID(see * DEFINE_‌CURVE) :
   GT.0 : Load curve giving thermal generation rate as a function of the mechanical history variable specified by ITGHSV.
   EQ.0 : Use mechanical history variable specified by ITGHSV times constant multiplier value TGMULT.
   LT.0 : Table of load curves for different temperatures.Each curve gives the thermal generation rate as a function of the mechanical history variable specified by ITGHSV
















   ..
       !! processed by numpydoc !!

.. py:property:: tgmult
   :type: Optional[float]


   
   Get or set the Thermal generation rate multiplier.Defines a volumetric heat rate (W/m^3 in SI units system).:
   EQ.0.0: no heat generation
















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


   
   Get or set the Phase change temperature
















   ..
       !! processed by numpydoc !!

.. py:property:: hlat
   :type: Optional[float]


   
   Get or set the Latent heat
















   ..
       !! processed by numpydoc !!

.. py:property:: lcc
   :type: Optional[int]


   
   Get or set the Load curve ID defining specific heat as a function of temperature, or if |ILCCHSV| > 0:
   GT.0:   Load curve as function of mechanical history variable specified by ILCCHSV.
   LT.0 : Table of load curves for different temperatures.Each curve is a function of the mechanical history variable specified by ILCCHSV.
















   ..
       !! processed by numpydoc !!

.. py:property:: lck1
   :type: Optional[int]


   
   Get or set the Load curve ID defining thermal conductivity, K_i(i = 1,2,3), in the local(x,y,z) - direction as a function of temperature, or if |ILCKHSV > 0:
   GT.0 : Load curve giving thermal conductivity in the local direction as a function of the mechanical history variable specified by ILCKHSV.
   LT.0 : Table of load curves for different temperatures.Each curve giving thermal conductivity in the local direction as a function of the mechanical history variable specified by ILCKHSV.
















   ..
       !! processed by numpydoc !!

.. py:property:: lck2
   :type: Optional[int]


   
   Get or set the Load curve ID defining thermal conductivity, K_i(i = 1,2,3), in the local(x,y,z) - direction as a function of temperature, or if |ILCKHSV > 0:
   GT.0 : Load curve giving thermal conductivity in the local direction as a function of the mechanical history variable specified by ILCKHSV.
   LT.0 : Table of load curves for different temperatures.Each curve giving thermal conductivity in the local direction as a function of the mechanical history variable specified by ILCKHSV.
















   ..
       !! processed by numpydoc !!

.. py:property:: lck3
   :type: Optional[int]


   
   Get or set the Load curve ID defining thermal conductivity, K_i(i = 1,2,3), in the local(x,y,z) - direction as a function of temperature, or if |ILCKHSV > 0:
   GT.0 : Load curve giving thermal conductivity in the local direction as a function of the mechanical history variable specified by ILCKHSV.
   LT.0 : Table of load curves for different temperatures.Each curve giving thermal conductivity in the local direction as a function of the mechanical history variable specified by ILCKHSV.
















   ..
       !! processed by numpydoc !!

.. py:property:: ilcchsv
   :type: Optional[int]


   
   Get or set the Optional:
   GT.0.0: Mechanical history variable # used by LCC
   LT.0.0: As above but | ILCCHSV |= 1 - 6 points to the six stress components, | ILCCHSV |= 7 to plastic strain,and | ILCCHSV |= 7 + k points to history variable k
















   ..
       !! processed by numpydoc !!

.. py:property:: ilckhsv
   :type: Optional[float]


   
   Get or set the Optional:
   GT.0.0: Mechanical history variable # used by LCK1, LCK2, LCK3
   LT.0.0: As above but | ILCKHSV |= 1 - 6 points to the six stress components, | ILCKHSV |= 7 to plastic strain,and | ILCKHSV |= 7 + k points to history variable k
















   ..
       !! processed by numpydoc !!

.. py:property:: itghsv
   :type: Optional[int]


   
   Get or set the Optional:
   GT.0.0: Mechanical history variable # used by TGRLC
   LT.0.0: As above but | ITGHSV |= 1 - 6 points to the six stress components, | ITGHSV |= 7 to plastic strain,and | ITGHSV |= 7 + k points to history variable k
















   ..
       !! processed by numpydoc !!

.. py:property:: xp
   :type: Optional[float]


   
   Get or set the Define coordinate of point p for AOPT = 1 and 4
















   ..
       !! processed by numpydoc !!

.. py:property:: yp
   :type: Optional[float]


   
   Get or set the Define coordinate of point p for AOPT = 1 and 4
















   ..
       !! processed by numpydoc !!

.. py:property:: zp
   :type: Optional[float]


   
   Get or set the Define coordinate of point p for AOPT = 1 and 4
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: Optional[float]


   
   Get or set the Define components of vector a for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the Define components of vector a for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: a3
   :type: Optional[float]


   
   Get or set the Define components of vector a for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: d1
   :type: Optional[float]


   
   Get or set the Define components of vector d for AOPT = 2,3 and 4
















   ..
       !! processed by numpydoc !!

.. py:property:: d2
   :type: Optional[float]


   
   Get or set the Define components of vector d for AOPT = 2,3 nd 4
















   ..
       !! processed by numpydoc !!

.. py:property:: d3
   :type: Optional[float]


   
   Get or set the Define components of vector d for AOPT = 2,3 and 4
















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
   :value: 'T08'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





