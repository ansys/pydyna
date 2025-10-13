





:class:`IcfdBoundaryConvectionTemp`
===================================


.. py:class:: icfd_boundary_convection_temp.IcfdBoundaryConvectionTemp(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_BOUNDARY_CONVECTION_TEMP keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdBoundaryConvectionTemp

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the PID for a fluid surface.
          * - :py:attr:`~hlcid`
            - Get or set the Load curve ID to describe the heat transfer coefficient value versus time, see *DEFINE_CURVE,*DEFINE_CURVE_FUNCTION or *DEFINE_FUNCTION. If a DEFINE_FUNCTION is used, the following parameters are allowed:  f(x,y,z,vx,vy,vz,temp,pres,time).
          * - :py:attr:`~hsf`
            - Get or set the Load curve scale factor applied on the heat transfer coefficient value.  (default=1.0)
          * - :py:attr:`~tblcid`
            - Get or set the Load curve ID to describe the environment (i.e bulk) temperature value versus time, see *DEFINE_CURVE,*DEFINE_CURVE_FUNCTION or *DEFINE_FUNCTION. If a DEFINE_FUNCTION is used, the following parameters are allowed:  f(x,y,z,vx,vy,vz,temp,pres,time).
          * - :py:attr:`~tbsf`
            - Get or set the Load curve scale factor applied on the environment value.  (default=1.0)


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

    from icfd_boundary_convection_temp import IcfdBoundaryConvectionTemp

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the PID for a fluid surface.
















   ..
       !! processed by numpydoc !!

.. py:property:: hlcid
   :type: Optional[int]


   
   Get or set the Load curve ID to describe the heat transfer coefficient value versus time, see *DEFINE_CURVE,*DEFINE_CURVE_FUNCTION or *DEFINE_FUNCTION. If a DEFINE_FUNCTION is used, the following parameters are allowed:  f(x,y,z,vx,vy,vz,temp,pres,time).
















   ..
       !! processed by numpydoc !!

.. py:property:: hsf
   :type: float


   
   Get or set the Load curve scale factor applied on the heat transfer coefficient value.  (default=1.0)
















   ..
       !! processed by numpydoc !!

.. py:property:: tblcid
   :type: Optional[int]


   
   Get or set the Load curve ID to describe the environment (i.e bulk) temperature value versus time, see *DEFINE_CURVE,*DEFINE_CURVE_FUNCTION or *DEFINE_FUNCTION. If a DEFINE_FUNCTION is used, the following parameters are allowed:  f(x,y,z,vx,vy,vz,temp,pres,time).
















   ..
       !! processed by numpydoc !!

.. py:property:: tbsf
   :type: float


   
   Get or set the Load curve scale factor applied on the environment value.  (default=1.0)
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'BOUNDARY_CONVECTION_TEMP'






