





:class:`IcfdBoundaryNavierslip`
===============================


.. py:class:: icfd_boundary_navierslip.IcfdBoundaryNavierslip(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_BOUNDARY_NAVIERSLIP keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdBoundaryNavierslip

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the PID of the fluid surface where non-slip boundary condition is applied.
          * - :py:attr:`~fric`
            - Get or set the Friction coefficient. If a negative value is entered, it will refer to a Load curve ID used to describe the friction coefficient  value versus time, see *DEFINE_CURVE, *DEFINE_CURVE_FUNCTION, or *DEFINE_FUNCTION.  If a DEFINE_FUNCTION is used, the following parameters are allowed: f(x,y,z,vx,vy,vz,temp,pres,time).


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

    from icfd_boundary_navierslip import IcfdBoundaryNavierslip

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the PID of the fluid surface where non-slip boundary condition is applied.
















   ..
       !! processed by numpydoc !!

.. py:property:: fric
   :type: Optional[float]


   
   Get or set the Friction coefficient. If a negative value is entered, it will refer to a Load curve ID used to describe the friction coefficient  value versus time, see *DEFINE_CURVE, *DEFINE_CURVE_FUNCTION, or *DEFINE_FUNCTION.  If a DEFINE_FUNCTION is used, the following parameters are allowed: f(x,y,z,vx,vy,vz,temp,pres,time).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'BOUNDARY_NAVIERSLIP'






