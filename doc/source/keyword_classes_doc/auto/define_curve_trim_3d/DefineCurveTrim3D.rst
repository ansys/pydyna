





:class:`DefineCurveTrim3D`
==========================


.. py:class:: define_curve_trim_3d.DefineCurveTrim3D(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_CURVE_TRIM_3D keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineCurveTrim3D

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~tcid`
            - Get or set the ID number for trim curve. A unique number has to be defined.
          * - :py:attr:`~tctype`
            - Get or set the Trim curve type:
          * - :py:attr:`~tdir`
            - Get or set the ID of vector (*DEFINE_VECTOR) giving direction of projection for trim curve.
          * - :py:attr:`~tctol`
            - Get or set the Tolerance limiting size of small elements created during trimming (default = 0.25)
          * - :py:attr:`~toln`
            - Get or set the The maximum gap between the trimming curve and the mesh. If the gap is bigger than this value, this section in the curve will not be used. Used only option 3D is chosen, If option 3D is not used, then
          * - :py:attr:`~nseed1`
            - Get or set the A node ID on the blank in the area that remains after trimming, applicable to both options _3D or _NEW.
          * - :py:attr:`~nseed2`
            - Get or set the A node ID on the blank in the area that remains after trimming, applicable to both options _3D or _NEW.
          * - :py:attr:`~cx`
            - Get or set the x-coordinate of trim curve Defined if and only if TCTYPE=1.
          * - :py:attr:`~cy`
            - Get or set the y-coordinate of trim curve Defined if and only if TCTYPE=1.
          * - :py:attr:`~cz`
            - Get or set the z-coordinate of trim curve Defined if and only if TCTYPE=1.
          * - :py:attr:`~filename`
            - Get or set the Name of IGES database containing trim curve(s). Defined if and only if TCTYPE=2.
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

    from define_curve_trim_3d import DefineCurveTrim3D

Property detail
---------------

.. py:property:: tcid
   :type: Optional[int]


   
   Get or set the ID number for trim curve. A unique number has to be defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: tctype
   :type: int


   
   Get or set the Trim curve type:
   EQ.1: digitized curve provided,
   EQ.2: IGES trim curve.
















   ..
       !! processed by numpydoc !!

.. py:property:: tdir
   :type: Optional[int]


   
   Get or set the ID of vector (*DEFINE_VECTOR) giving direction of projection for trim curve.
   EQ. 0: default vector (0,0,1) is used. Curve is defined in the global xy plane, and projected onto mesh in global z-direction to define trim line.
















   ..
       !! processed by numpydoc !!

.. py:property:: tctol
   :type: float


   
   Get or set the Tolerance limiting size of small elements created during trimming (default = 0.25)
















   ..
       !! processed by numpydoc !!

.. py:property:: toln
   :type: Optional[float]


   
   Get or set the The maximum gap between the trimming curve and the mesh. If the gap is bigger than this value, this section in the curve will not be used. Used only option 3D is chosen, If option 3D is not used, then
   IGB.EQ.0: trimming curv is defined in local coordinate system
   IGB.EQ.1: trimming curve is defined in global coordinate system
















   ..
       !! processed by numpydoc !!

.. py:property:: nseed1
   :type: Optional[int]


   
   Get or set the A node ID on the blank in the area that remains after trimming, applicable to both options _3D or _NEW.
   LT.0: positive number is a node ID, which may not necessarily be from the blank
















   ..
       !! processed by numpydoc !!

.. py:property:: nseed2
   :type: Optional[int]


   
   Get or set the A node ID on the blank in the area that remains after trimming, applicable to both options _3D or _NEW.
   LT.0: positive number is a node ID, which may not necessarily be from the blank
















   ..
       !! processed by numpydoc !!

.. py:property:: cx
   :type: float


   
   Get or set the x-coordinate of trim curve Defined if and only if TCTYPE=1.
















   ..
       !! processed by numpydoc !!

.. py:property:: cy
   :type: float


   
   Get or set the y-coordinate of trim curve Defined if and only if TCTYPE=1.
















   ..
       !! processed by numpydoc !!

.. py:property:: cz
   :type: float


   
   Get or set the z-coordinate of trim curve Defined if and only if TCTYPE=1.
















   ..
       !! processed by numpydoc !!

.. py:property:: filename
   :type: Optional[str]


   
   Get or set the Name of IGES database containing trim curve(s). Defined if and only if TCTYPE=2.
















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
   :value: 'DEFINE'


.. py:attribute:: subkeyword
   :value: 'CURVE_TRIM_3D'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





