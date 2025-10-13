





:class:`IcfdDefineTurbsource`
=============================


.. py:class:: icfd_define_turbsource.IcfdDefineTurbsource(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_DEFINE_TURBSOURCE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdDefineTurbsource

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the source ID
          * - :py:attr:`~lcidx`
            - Get or set the Load curve ID specifying the evolution of the external source term function of time for the turbulent kinetic energy k equation, see *DEFINE_CURVE,*DEFINE_CURVE_FUNCTION or *DEFINE_FUNCTION. If a DEFINE_FUNCTION is used, the following parameters are allowed:  f(x,y,z,vx,vy,vz,temp,pres,time
          * - :py:attr:`~lcidy`
            - Get or set the Load curve ID specifying the evolution of the external source term function of time for the turbulent diffusion ε or specific rate of dissipation w equation, see *DEFINE_CURVE,*DEFINE_CURVE_FUNCTION or *DEFINE_FUNCTION. If a DEFINE_FUNCTION is used, the following parameters are allowed:  f(x,y,z,vx,vy,vz,temp,pres,time).
          * - :py:attr:`~lcidz`
            - Get or set the Load curve ID specifying the evolution of the external source term function of time for the kinematic eddy turbulent viscosity equation used in the Spalart-Allmaras model, see *DEFINE_CURVE,*DEFINE_CURVE_FUNCTION or *DEFINE_FUNCTION. If a DEFINE_FUNCTION is used, the following parameters are allowed:  f(x,y,z,vx,vy,vz,temp,pres,time).
          * - :py:attr:`~shape`
            - Get or set the Shape of the external source:
          * - :py:attr:`~r`
            - Get or set the Radius of the sphere is SHAPE=3
          * - :py:attr:`~ptid1`
            - Get or set the ID of point (See ICFD_DEFINE_POINT) of minimum coordinates if SHAPE=1, tail point if SHAPE=2, origin if SHAPE=3.
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

    from icfd_define_turbsource import IcfdDefineTurbsource

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the source ID
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidx
   :type: Optional[int]


   
   Get or set the Load curve ID specifying the evolution of the external source term function of time for the turbulent kinetic energy k equation, see *DEFINE_CURVE,*DEFINE_CURVE_FUNCTION or *DEFINE_FUNCTION. If a DEFINE_FUNCTION is used, the following parameters are allowed:  f(x,y,z,vx,vy,vz,temp,pres,time
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidy
   :type: Optional[int]


   
   Get or set the Load curve ID specifying the evolution of the external source term function of time for the turbulent diffusion ε or specific rate of dissipation w equation, see *DEFINE_CURVE,*DEFINE_CURVE_FUNCTION or *DEFINE_FUNCTION. If a DEFINE_FUNCTION is used, the following parameters are allowed:  f(x,y,z,vx,vy,vz,temp,pres,time).
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidz
   :type: Optional[int]


   
   Get or set the Load curve ID specifying the evolution of the external source term function of time for the kinematic eddy turbulent viscosity equation used in the Spalart-Allmaras model, see *DEFINE_CURVE,*DEFINE_CURVE_FUNCTION or *DEFINE_FUNCTION. If a DEFINE_FUNCTION is used, the following parameters are allowed:  f(x,y,z,vx,vy,vz,temp,pres,time).
















   ..
       !! processed by numpydoc !!

.. py:property:: shape
   :type: int


   
   Get or set the Shape of the external source:
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


   
   Get or set the ID of point (See ICFD_DEFINE_POINT) of minimum coordinates if SHAPE=1, tail point if SHAPE=2, origin if SHAPE=3.
















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
   :value: 'DEFINE_TURBSOURCE'






