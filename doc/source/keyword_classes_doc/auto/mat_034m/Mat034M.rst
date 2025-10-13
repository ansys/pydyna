





:class:`Mat034M`
================


.. py:class:: mat_034m.Mat034M(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_034M keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: Mat034M

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
          * - :py:attr:`~pxx`
            - Get or set the Table giving engineering local XX-stress as function of engineering local XX-strain and YY-strain.
          * - :py:attr:`~pyy`
            - Get or set the Table giving engineering local YY-stress as function of engineering local YY-strain and XX-strain.
          * - :py:attr:`~sxy`
            - Get or set the Curve giving local 2nd Piola-Kirchhoff XY-stress as function of local Green XY-strain.
          * - :py:attr:`~damp`
            - Get or set the Damping coefficient for numerical stability.
          * - :py:attr:`~th`
            - Get or set the Table giving hysteresis factor 0 <= H < 1 as function of engineering local XX-strain and YY-strain.
          * - :py:attr:`~fvopt`
            - Get or set the Fabric venting option, see *MAT_FABRIC.
          * - :py:attr:`~x0`
            - Get or set the Fabric venting option parameters, see *MAT_FABRIC.
          * - :py:attr:`~x1`
            - Get or set the Fabric venting option parameters, see *MAT_FABRIC.
          * - :py:attr:`~flc_x2`
            - Get or set the Fabric venting option parameters, see *MAT_FABRIC.
          * - :py:attr:`~fac_x3`
            - Get or set the Fabric venting option parameters, see *MAT_FABRIC.
          * - :py:attr:`~isrefg`
            - Get or set the Initial stress by reference geometry.
          * - :py:attr:`~cse`
            - Get or set the Compressive stress elimination option.
          * - :py:attr:`~srfac`
            - Get or set the Load curve ID for smooth stress initialization when using a reference geometry.
          * - :py:attr:`~bulkc`
            - Get or set the Bulk modulus for fabric compaction.
          * - :py:attr:`~jacc`
            - Get or set the Jacobian for the onset of fabric compaction.
          * - :py:attr:`~fxx`
            - Get or set the Load curve giving scale factor of uniaxial stress in first material direction as function of engineering strain rate.
          * - :py:attr:`~fyy`
            - Get or set the Load curve giving scale factor of uniaxial stress in second material direction as function of engineering strain rate.
          * - :py:attr:`~dt`
            - Get or set the Time window for smoothing strain rates used for FXX and FYY.
          * - :py:attr:`~aopt`
            - Get or set the Material axes option, see *MAT_FABRIC.
          * - :py:attr:`~ecoat`
            - Get or set the Young's modulus of coat material to include bending properties. This together with the following two parameters (SCOAT and TCOAT) encompass the same coating/bending feature as in *MAT_FABRIC. Please refer to these manual pages and associated remarks..
          * - :py:attr:`~scoat`
            - Get or set the Yield stress of coat material, see *MAT_FABRIC.
          * - :py:attr:`~tcoat`
            - Get or set the Thickness of coat material, may be positive or negative, see *MAT_FABRIC.
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
            - Get or set the Material angle in degrees for AOPT = 0 and 3, may be overridden on the element card, see *ELEMENT_SHELL_BETA.
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

    from mat_034m import Mat034M

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

.. py:property:: pxx
   :type: Optional[float]


   
   Get or set the Table giving engineering local XX-stress as function of engineering local XX-strain and YY-strain.
















   ..
       !! processed by numpydoc !!

.. py:property:: pyy
   :type: Optional[float]


   
   Get or set the Table giving engineering local YY-stress as function of engineering local YY-strain and XX-strain.
















   ..
       !! processed by numpydoc !!

.. py:property:: sxy
   :type: Optional[float]


   
   Get or set the Curve giving local 2nd Piola-Kirchhoff XY-stress as function of local Green XY-strain.
















   ..
       !! processed by numpydoc !!

.. py:property:: damp
   :type: Optional[float]


   
   Get or set the Damping coefficient for numerical stability.
















   ..
       !! processed by numpydoc !!

.. py:property:: th
   :type: Optional[float]


   
   Get or set the Table giving hysteresis factor 0 <= H < 1 as function of engineering local XX-strain and YY-strain.
   GT.0.0: TH is table ID
   LE.0.0: -TH is used as constant value for hysteresis factor
















   ..
       !! processed by numpydoc !!

.. py:property:: fvopt
   :type: Optional[float]


   
   Get or set the Fabric venting option, see *MAT_FABRIC.
















   ..
       !! processed by numpydoc !!

.. py:property:: x0
   :type: Optional[float]


   
   Get or set the Fabric venting option parameters, see *MAT_FABRIC.
















   ..
       !! processed by numpydoc !!

.. py:property:: x1
   :type: Optional[float]


   
   Get or set the Fabric venting option parameters, see *MAT_FABRIC.
















   ..
       !! processed by numpydoc !!

.. py:property:: flc_x2
   :type: Optional[float]


   
   Get or set the Fabric venting option parameters, see *MAT_FABRIC.
















   ..
       !! processed by numpydoc !!

.. py:property:: fac_x3
   :type: Optional[float]


   
   Get or set the Fabric venting option parameters, see *MAT_FABRIC.
















   ..
       !! processed by numpydoc !!

.. py:property:: isrefg
   :type: float


   
   Get or set the Initial stress by reference geometry.
   EQ.0.0: Not active.
   EQ.1.0: Active
















   ..
       !! processed by numpydoc !!

.. py:property:: cse
   :type: float


   
   Get or set the Compressive stress elimination option.
   EQ.0.0: Don't eliminate compressive stresses,
   EQ.1.0: Eliminate compressive stresses.
















   ..
       !! processed by numpydoc !!

.. py:property:: srfac
   :type: Optional[int]


   
   Get or set the Load curve ID for smooth stress initialization when using a reference geometry.
















   ..
       !! processed by numpydoc !!

.. py:property:: bulkc
   :type: Optional[float]


   
   Get or set the Bulk modulus for fabric compaction.
















   ..
       !! processed by numpydoc !!

.. py:property:: jacc
   :type: Optional[float]


   
   Get or set the Jacobian for the onset of fabric compaction.
















   ..
       !! processed by numpydoc !!

.. py:property:: fxx
   :type: Optional[int]


   
   Get or set the Load curve giving scale factor of uniaxial stress in first material direction as function of engineering strain rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: fyy
   :type: Optional[int]


   
   Get or set the Load curve giving scale factor of uniaxial stress in second material direction as function of engineering strain rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: dt
   :type: Optional[float]


   
   Get or set the Time window for smoothing strain rates used for FXX and FYY.
















   ..
       !! processed by numpydoc !!

.. py:property:: aopt
   :type: Optional[float]


   
   Get or set the Material axes option, see *MAT_FABRIC.
















   ..
       !! processed by numpydoc !!

.. py:property:: ecoat
   :type: Optional[float]


   
   Get or set the Young's modulus of coat material to include bending properties. This together with the following two parameters (SCOAT and TCOAT) encompass the same coating/bending feature as in *MAT_FABRIC. Please refer to these manual pages and associated remarks..
















   ..
       !! processed by numpydoc !!

.. py:property:: scoat
   :type: Optional[float]


   
   Get or set the Yield stress of coat material, see *MAT_FABRIC.
















   ..
       !! processed by numpydoc !!

.. py:property:: tcoat
   :type: Optional[float]


   
   Get or set the Thickness of coat material, may be positive or negative, see *MAT_FABRIC.
















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


   
   Get or set the Material angle in degrees for AOPT = 0 and 3, may be overridden on the element card, see *ELEMENT_SHELL_BETA.
















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
   :value: '034M'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





