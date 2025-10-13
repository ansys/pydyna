





:class:`MatJointedRock`
=======================


.. py:class:: mat_jointed_rock.MatJointedRock(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_JOINTED_ROCK keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatJointedRock

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification number, must be unique.
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~gmod`
            - Get or set the Elastic shear modulus.
          * - :py:attr:`~rnu`
            - Get or set the Poisson's ratio.
          * - :py:attr:`~rkf`
            - Get or set the Failture surface shape parameter.
          * - :py:attr:`~phi`
            - Get or set the Angle of friction (radians).
          * - :py:attr:`~cval`
            - Get or set the Cohesion value.
          * - :py:attr:`~psi`
            - Get or set the Dilation angle (radians).
          * - :py:attr:`~str_lim`
            - Get or set the Minimum shear strength of material is given by STR_LIM*CVAL.
          * - :py:attr:`~nplanes`
            - Get or set the Number of jointed planes (maximum 3).
          * - :py:attr:`~elastic`
            - Get or set the Flag = 1 for elastic behaviour only.
          * - :py:attr:`~lccpdr`
            - Get or set the Loadcurve for extra cohesion for parent material (dynamic relaxation).
          * - :py:attr:`~lccpt`
            - Get or set the Loadcurve for extra cohesion for parent material (transient).
          * - :py:attr:`~lccjdr`
            - Get or set the Loadcurve for extra cohesion for joints (dynamic relaxation).
          * - :py:attr:`~lccjt`
            - Get or set the Loadcurve for extra cohesion for joints (transient).
          * - :py:attr:`~lcsfac`
            - Get or set the Loadcurve giving factor on strength vs time.
          * - :py:attr:`~gmoddp`
            - Get or set the Depth at which shear modulus (GMOD) is correct.
          * - :py:attr:`~phidp`
            - Get or set the Depth at which angle of friction (PHI) is correct.
          * - :py:attr:`~cvaldp`
            - Get or set the Depth at which cohesion value (CVAL) is correct.
          * - :py:attr:`~psidp`
            - Get or set the Depth at which dilation angle (PSI) is correct.
          * - :py:attr:`~gmodgr`
            - Get or set the Gradient at which shear modulus (GMOD) increases with depth.
          * - :py:attr:`~phigr`
            - Get or set the Gradient at which friction angle (PHI) increases with depth.
          * - :py:attr:`~cvalgr`
            - Get or set the Gradient at which cohesion value (CVAL) increases with depth.
          * - :py:attr:`~psigr`
            - Get or set the Gradient at which dilation angle (PSI) increases with depth.
          * - :py:attr:`~dip`
            - Get or set the Angle of the plane in degrees below the horizontal.
          * - :py:attr:`~strike`
            - Get or set the Plan view angle (degrees) of downhill vector drawn on the plane.
          * - :py:attr:`~cplane`
            - Get or set the Cohesion for shear behaviour on plane.
          * - :py:attr:`~frplane`
            - Get or set the Friction angle for shear behaviour on plane (degrees).
          * - :py:attr:`~tplane`
            - Get or set the Tensile strength across plane (generally zero or very small).
          * - :py:attr:`~shrmax`
            - Get or set the Max shear stress on plane (upper limit, independent of compression).
          * - :py:attr:`~local`
            - Get or set the EQ=0: DIP and DIPANG are with respect to the global axes.
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

    from mat_jointed_rock import MatJointedRock

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification number, must be unique.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density.
















   ..
       !! processed by numpydoc !!

.. py:property:: gmod
   :type: Optional[float]


   
   Get or set the Elastic shear modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: rnu
   :type: Optional[float]


   
   Get or set the Poisson's ratio.
















   ..
       !! processed by numpydoc !!

.. py:property:: rkf
   :type: float


   
   Get or set the Failture surface shape parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: phi
   :type: Optional[float]


   
   Get or set the Angle of friction (radians).
















   ..
       !! processed by numpydoc !!

.. py:property:: cval
   :type: Optional[float]


   
   Get or set the Cohesion value.
















   ..
       !! processed by numpydoc !!

.. py:property:: psi
   :type: Optional[float]


   
   Get or set the Dilation angle (radians).
















   ..
       !! processed by numpydoc !!

.. py:property:: str_lim
   :type: float


   
   Get or set the Minimum shear strength of material is given by STR_LIM*CVAL.
















   ..
       !! processed by numpydoc !!

.. py:property:: nplanes
   :type: int


   
   Get or set the Number of jointed planes (maximum 3).
















   ..
       !! processed by numpydoc !!

.. py:property:: elastic
   :type: int


   
   Get or set the Flag = 1 for elastic behaviour only.
















   ..
       !! processed by numpydoc !!

.. py:property:: lccpdr
   :type: int


   
   Get or set the Loadcurve for extra cohesion for parent material (dynamic relaxation).
















   ..
       !! processed by numpydoc !!

.. py:property:: lccpt
   :type: int


   
   Get or set the Loadcurve for extra cohesion for parent material (transient).
















   ..
       !! processed by numpydoc !!

.. py:property:: lccjdr
   :type: int


   
   Get or set the Loadcurve for extra cohesion for joints (dynamic relaxation).
















   ..
       !! processed by numpydoc !!

.. py:property:: lccjt
   :type: int


   
   Get or set the Loadcurve for extra cohesion for joints (transient).
















   ..
       !! processed by numpydoc !!

.. py:property:: lcsfac
   :type: int


   
   Get or set the Loadcurve giving factor on strength vs time.
















   ..
       !! processed by numpydoc !!

.. py:property:: gmoddp
   :type: Optional[float]


   
   Get or set the Depth at which shear modulus (GMOD) is correct.
















   ..
       !! processed by numpydoc !!

.. py:property:: phidp
   :type: Optional[float]


   
   Get or set the Depth at which angle of friction (PHI) is correct.
















   ..
       !! processed by numpydoc !!

.. py:property:: cvaldp
   :type: Optional[float]


   
   Get or set the Depth at which cohesion value (CVAL) is correct.
















   ..
       !! processed by numpydoc !!

.. py:property:: psidp
   :type: Optional[float]


   
   Get or set the Depth at which dilation angle (PSI) is correct.
















   ..
       !! processed by numpydoc !!

.. py:property:: gmodgr
   :type: Optional[float]


   
   Get or set the Gradient at which shear modulus (GMOD) increases with depth.
















   ..
       !! processed by numpydoc !!

.. py:property:: phigr
   :type: Optional[float]


   
   Get or set the Gradient at which friction angle (PHI) increases with depth.
















   ..
       !! processed by numpydoc !!

.. py:property:: cvalgr
   :type: Optional[float]


   
   Get or set the Gradient at which cohesion value (CVAL) increases with depth.
















   ..
       !! processed by numpydoc !!

.. py:property:: psigr
   :type: Optional[float]


   
   Get or set the Gradient at which dilation angle (PSI) increases with depth.
















   ..
       !! processed by numpydoc !!

.. py:property:: dip
   :type: Optional[float]


   
   Get or set the Angle of the plane in degrees below the horizontal.
















   ..
       !! processed by numpydoc !!

.. py:property:: strike
   :type: Optional[float]


   
   Get or set the Plan view angle (degrees) of downhill vector drawn on the plane.
















   ..
       !! processed by numpydoc !!

.. py:property:: cplane
   :type: Optional[float]


   
   Get or set the Cohesion for shear behaviour on plane.
















   ..
       !! processed by numpydoc !!

.. py:property:: frplane
   :type: Optional[float]


   
   Get or set the Friction angle for shear behaviour on plane (degrees).
















   ..
       !! processed by numpydoc !!

.. py:property:: tplane
   :type: Optional[float]


   
   Get or set the Tensile strength across plane (generally zero or very small).
















   ..
       !! processed by numpydoc !!

.. py:property:: shrmax
   :type: float


   
   Get or set the Max shear stress on plane (upper limit, independent of compression).
















   ..
       !! processed by numpydoc !!

.. py:property:: local
   :type: Optional[float]


   
   Get or set the EQ=0: DIP and DIPANG are with respect to the global axes.
   EQ=1: DIP and DIPANG are with respect to the local element axes.
















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
   :value: 'JOINTED_ROCK'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





