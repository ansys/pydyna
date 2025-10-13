





:class:`LoadNurbsShell`
=======================


.. py:class:: load_nurbs_shell.LoadNurbsShell(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_NURBS_SHELL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadNurbsShell

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the loading ID
          * - :py:attr:`~heading`
            - Get or set the A description of the loading.
          * - :py:attr:`~ssid`
            - Get or set the NURBS shell patch ID.
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID.
          * - :py:attr:`~sf`
            - Get or set the Load curve scale factor.
          * - :py:attr:`~at`
            - Get or set the Arrival/birth time for load.
          * - :py:attr:`~dt`
            - Get or set the Death time for load.
          * - :py:attr:`~ltype`
            - Get or set the Load type.
          * - :py:attr:`~regdef`
            - Get or set the The method of defining the region of a NURBS shell patch on which the loading is applied.
          * - :py:attr:`~cid`
            - Get or set the Coordinate system ID.
          * - :py:attr:`~v1`
            - Get or set the Vector direction cosines defining the direction of the traction loading.
          * - :py:attr:`~v2`
            - Get or set the Vector direction cosines defining the direction of the traction loading.
          * - :py:attr:`~v3`
            - Get or set the Vector direction cosines defining the direction of the traction loading.
          * - :py:attr:`~rmin`
            - Get or set the The minimum and maximum values of the univariate knot vector in local r and s-directions of the area on which the pressure loading is applied.
          * - :py:attr:`~smin`
            - Get or set the The minimum and maximum values of the univariate knot vector in local r and s-directions of the area on which the pressure loading is applied.
          * - :py:attr:`~rmax`
            - Get or set the The minimum and maximum values of the univariate knot vector in local r and s-directions of the area on which the pressure loading is applied.
          * - :py:attr:`~smax`
            - Get or set the The minimum and maximum values of the univariate knot vector in local r and s-directions of the area on which the pressure loading is applied.
          * - :py:attr:`~ne1`
            - Get or set the An interior node on the surface of the NURBS element on which the traction loading is applied.
          * - :py:attr:`~ne2`
            - Get or set the An interior node on the surface of the NURBS element on which the traction loading is applied.
          * - :py:attr:`~ne3`
            - Get or set the An interior node on the surface of the NURBS element on which the traction loading is applied.
          * - :py:attr:`~ne4`
            - Get or set the An interior node on the surface of the NURBS element on which the traction loading is applied.
          * - :py:attr:`~ne5`
            - Get or set the An interior node on the surface of the NURBS element on which the traction loading is applied.
          * - :py:attr:`~ne6`
            - Get or set the An interior node on the surface of the NURBS element on which the traction loading is applied.
          * - :py:attr:`~ne7`
            - Get or set the An interior node on the surface of the NURBS element on which the traction loading is applied.
          * - :py:attr:`~ne8`
            - Get or set the An interior node on the surface of the NURBS element on which the traction loading is applied.
          * - :py:attr:`~nt1`
            - Get or set the Nodes defining an area on the surface of a NURBS element where loading is applied.
          * - :py:attr:`~nt2`
            - Get or set the Nodes defining an area on the surface of a NURBS element where loading is applied.
          * - :py:attr:`~nt3`
            - Get or set the Nodes defining an area on the surface of a NURBS element where loading is applied.
          * - :py:attr:`~nt4`
            - Get or set the Nodes defining an area on the surface of a NURBS element where loading is applied.
          * - :py:attr:`~nte`
            - Get or set the Optional node used to identify the NURBS element on which the load application area defined by the NTi's is located.
          * - :py:attr:`~r1`
            - Get or set the The univariate knot vector in local r and s-directions of the starting and ending points of the curve on which the curve loading is applied.
          * - :py:attr:`~s1`
            - Get or set the The univariate knot vector in local r and s-directions of the starting and ending points of the curve on which the curve loading is applied.
          * - :py:attr:`~r2`
            - Get or set the The univariate knot vector in local r and s-directions of the starting and ending points of the curve on which the curve loading is applied.
          * - :py:attr:`~s2`
            - Get or set the The univariate knot vector in local r and s-directions of the starting and ending points of the curve on which the curve loading is applied.
          * - :py:attr:`~nc1`
            - Get or set the Nodes defining a curve on the surface of a NURBS element where loading is applied.
          * - :py:attr:`~nc2`
            - Get or set the Nodes defining a curve on the surface of a NURBS element where loading is applied.
          * - :py:attr:`~nce`
            - Get or set the Optional node used to identify the NURBS element on which the load application curve defined by the NTi's is located.


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~keyword`
            - 
          * - :py:attr:`~subkeyword`
            - 






Import detail
-------------

.. code-block:: python

    from load_nurbs_shell import LoadNurbsShell

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the loading ID
















   ..
       !! processed by numpydoc !!

.. py:property:: heading
   :type: Optional[str]


   
   Get or set the A description of the loading.
















   ..
       !! processed by numpydoc !!

.. py:property:: ssid
   :type: Optional[int]


   
   Get or set the NURBS shell patch ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: sf
   :type: float


   
   Get or set the Load curve scale factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: at
   :type: float


   
   Get or set the Arrival/birth time for load.
















   ..
       !! processed by numpydoc !!

.. py:property:: dt
   :type: float


   
   Get or set the Death time for load.
















   ..
       !! processed by numpydoc !!

.. py:property:: ltype
   :type: str


   
   Get or set the Load type.
   EQ.PRESS:Surface traction is applied on a region on a NURBS patch, along the opposite direction to the NURBS patch surface normal.
   EQ.CRV:Loading is applied on a curve on a NURBS patch,along (V1,V2,V3) of CID coordinate system.The loading dimension is force per unit length along the curve.
   EQ.CRVS:Loading, force per unit length, is applied on a curve on the surface of a NURBS patch,along the local shear direction, CS direction
   EQ.CRVT:Loading, force per unit length, is applied on a curve on the surface of a NURBS patch, along the local transverse direction, CT direction.EQ.CRVN:Loading, force per unit length, is applied on a curve on the surface of a NURBS patch, along the local normal direction, CN direction.
   EQ.TRACT:Surface traction, force per unit area, is applied on a region on a NURBS patch, along (V1,V2,V3) of the coordinate system CID.
















   ..
       !! processed by numpydoc !!

.. py:property:: regdef
   :type: str


   
   Get or set the The method of defining the region of a NURBS shell patch on which the loading is applied.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: int


   
   Get or set the Coordinate system ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: v1
   :type: Optional[float]


   
   Get or set the Vector direction cosines defining the direction of the traction loading.
















   ..
       !! processed by numpydoc !!

.. py:property:: v2
   :type: Optional[float]


   
   Get or set the Vector direction cosines defining the direction of the traction loading.
















   ..
       !! processed by numpydoc !!

.. py:property:: v3
   :type: Optional[float]


   
   Get or set the Vector direction cosines defining the direction of the traction loading.
















   ..
       !! processed by numpydoc !!

.. py:property:: rmin
   :type: Optional[float]


   
   Get or set the The minimum and maximum values of the univariate knot vector in local r and s-directions of the area on which the pressure loading is applied.
















   ..
       !! processed by numpydoc !!

.. py:property:: smin
   :type: Optional[float]


   
   Get or set the The minimum and maximum values of the univariate knot vector in local r and s-directions of the area on which the pressure loading is applied.
















   ..
       !! processed by numpydoc !!

.. py:property:: rmax
   :type: Optional[float]


   
   Get or set the The minimum and maximum values of the univariate knot vector in local r and s-directions of the area on which the pressure loading is applied.
















   ..
       !! processed by numpydoc !!

.. py:property:: smax
   :type: Optional[float]


   
   Get or set the The minimum and maximum values of the univariate knot vector in local r and s-directions of the area on which the pressure loading is applied.
















   ..
       !! processed by numpydoc !!

.. py:property:: ne1
   :type: Optional[int]


   
   Get or set the An interior node on the surface of the NURBS element on which the traction loading is applied.
















   ..
       !! processed by numpydoc !!

.. py:property:: ne2
   :type: Optional[int]


   
   Get or set the An interior node on the surface of the NURBS element on which the traction loading is applied.
















   ..
       !! processed by numpydoc !!

.. py:property:: ne3
   :type: Optional[int]


   
   Get or set the An interior node on the surface of the NURBS element on which the traction loading is applied.
















   ..
       !! processed by numpydoc !!

.. py:property:: ne4
   :type: Optional[int]


   
   Get or set the An interior node on the surface of the NURBS element on which the traction loading is applied.
















   ..
       !! processed by numpydoc !!

.. py:property:: ne5
   :type: Optional[int]


   
   Get or set the An interior node on the surface of the NURBS element on which the traction loading is applied.
















   ..
       !! processed by numpydoc !!

.. py:property:: ne6
   :type: Optional[int]


   
   Get or set the An interior node on the surface of the NURBS element on which the traction loading is applied.
















   ..
       !! processed by numpydoc !!

.. py:property:: ne7
   :type: Optional[int]


   
   Get or set the An interior node on the surface of the NURBS element on which the traction loading is applied.
















   ..
       !! processed by numpydoc !!

.. py:property:: ne8
   :type: Optional[int]


   
   Get or set the An interior node on the surface of the NURBS element on which the traction loading is applied.
















   ..
       !! processed by numpydoc !!

.. py:property:: nt1
   :type: Optional[int]


   
   Get or set the Nodes defining an area on the surface of a NURBS element where loading is applied.
















   ..
       !! processed by numpydoc !!

.. py:property:: nt2
   :type: Optional[int]


   
   Get or set the Nodes defining an area on the surface of a NURBS element where loading is applied.
















   ..
       !! processed by numpydoc !!

.. py:property:: nt3
   :type: Optional[int]


   
   Get or set the Nodes defining an area on the surface of a NURBS element where loading is applied.
















   ..
       !! processed by numpydoc !!

.. py:property:: nt4
   :type: Optional[int]


   
   Get or set the Nodes defining an area on the surface of a NURBS element where loading is applied.
















   ..
       !! processed by numpydoc !!

.. py:property:: nte
   :type: Optional[int]


   
   Get or set the Optional node used to identify the NURBS element on which the load application area defined by the NTi's is located.
















   ..
       !! processed by numpydoc !!

.. py:property:: r1
   :type: Optional[float]


   
   Get or set the The univariate knot vector in local r and s-directions of the starting and ending points of the curve on which the curve loading is applied.
















   ..
       !! processed by numpydoc !!

.. py:property:: s1
   :type: Optional[float]


   
   Get or set the The univariate knot vector in local r and s-directions of the starting and ending points of the curve on which the curve loading is applied.
















   ..
       !! processed by numpydoc !!

.. py:property:: r2
   :type: Optional[float]


   
   Get or set the The univariate knot vector in local r and s-directions of the starting and ending points of the curve on which the curve loading is applied.
















   ..
       !! processed by numpydoc !!

.. py:property:: s2
   :type: Optional[float]


   
   Get or set the The univariate knot vector in local r and s-directions of the starting and ending points of the curve on which the curve loading is applied.
















   ..
       !! processed by numpydoc !!

.. py:property:: nc1
   :type: Optional[int]


   
   Get or set the Nodes defining a curve on the surface of a NURBS element where loading is applied.
















   ..
       !! processed by numpydoc !!

.. py:property:: nc2
   :type: Optional[int]


   
   Get or set the Nodes defining a curve on the surface of a NURBS element where loading is applied.
















   ..
       !! processed by numpydoc !!

.. py:property:: nce
   :type: Optional[int]


   
   Get or set the Optional node used to identify the NURBS element on which the load application curve defined by the NTi's is located.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'NURBS_SHELL'






