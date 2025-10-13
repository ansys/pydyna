





:class:`IcfdDefineHeatsource`
=============================


.. py:class:: icfd_define_heatsource.IcfdDefineHeatsource(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_DEFINE_HEATSOURCE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdDefineHeatsource

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~hsid`
            - Get or set the Heat source ID
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID specifying the evolution of the heat source term function of time for the X, Y and Z dofs, see *DEFINE_CURVE,*DEFINE_CURVE_FUNCTION or *DEFINE_FUNCTION. If a DEFINE_FUNCTION is used, the following parameters are allowed:  f(x,y,z,vx,vy,vz,temp,pres,time).
          * - :py:attr:`~shape`
            - Get or set the Shape of the volumetric heat source:
          * - :py:attr:`~r`
            - Get or set the Radius of the sphere is SHAPE=3
          * - :py:attr:`~ptid1`
            - Get or set the ID of point (See ICFD_DEFINE_POINT) of minimum coordinates if SHAPE=1, tail point if SHAPE=2, origin if SHAPE=3
          * - :py:attr:`~ptid2`
            - Get or set the ID of point of maximum coordinates if SHAPE=2, head point if SHAPE=2.


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

    from icfd_define_heatsource import IcfdDefineHeatsource

Property detail
---------------

.. py:property:: hsid
   :type: Optional[int]


   
   Get or set the Heat source ID
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID specifying the evolution of the heat source term function of time for the X, Y and Z dofs, see *DEFINE_CURVE,*DEFINE_CURVE_FUNCTION or *DEFINE_FUNCTION. If a DEFINE_FUNCTION is used, the following parameters are allowed:  f(x,y,z,vx,vy,vz,temp,pres,time).
















   ..
       !! processed by numpydoc !!

.. py:property:: shape
   :type: Optional[int]


   
   Get or set the Shape of the volumetric heat source:
   EQ.1 :  Box shape
   EQ.2 :  Cylinder shape
   EQ.3 :  Sphere shape
















   ..
       !! processed by numpydoc !!

.. py:property:: r
   :type: Optional[float]


   
   Get or set the Radius of the sphere is SHAPE=3
















   ..
       !! processed by numpydoc !!

.. py:property:: ptid1
   :type: Optional[int]


   
   Get or set the ID of point (See ICFD_DEFINE_POINT) of minimum coordinates if SHAPE=1, tail point if SHAPE=2, origin if SHAPE=3
















   ..
       !! processed by numpydoc !!

.. py:property:: ptid2
   :type: Optional[int]


   
   Get or set the ID of point of maximum coordinates if SHAPE=2, head point if SHAPE=2.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'DEFINE_HEATSOURCE'






