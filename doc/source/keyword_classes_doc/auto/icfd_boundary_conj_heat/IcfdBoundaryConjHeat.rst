





:class:`IcfdBoundaryConjHeat`
=============================


.. py:class:: icfd_boundary_conj_heat.IcfdBoundaryConjHeat(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_BOUNDARY_CONJ_HEAT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdBoundaryConjHeat

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the PID of the fluid surface in contact with the solid
          * - :py:attr:`~ctype`
            - Get or set the Contact type:
          * - :py:attr:`~val`
            - Get or set the Optional Temperature drop if CTYPE=0 or Interface Heat Transfer Coefficient if CTYPE=1 (high value by default to insure perfect contact)
          * - :py:attr:`~sflcid`
            - Get or set the Load curve ID used to describe scale factor on VAL value versus time, see *DEFINE_CURVE, *DEFINE_CURVE_FUNCTION, or *DEFINE_FUNCTION.  If a DEFINE_FUNCTION is used, the following parameters are allowed: f(x,y,z,vx,vy,vz,temp,pres,time).


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

    from icfd_boundary_conj_heat import IcfdBoundaryConjHeat

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the PID of the fluid surface in contact with the solid
















   ..
       !! processed by numpydoc !!

.. py:property:: ctype
   :type: int


   
   Get or set the Contact type:
   EQ.0:   Constraint approach.
   EQ.1 : Mortar contact
















   ..
       !! processed by numpydoc !!

.. py:property:: val
   :type: float


   
   Get or set the Optional Temperature drop if CTYPE=0 or Interface Heat Transfer Coefficient if CTYPE=1 (high value by default to insure perfect contact)
















   ..
       !! processed by numpydoc !!

.. py:property:: sflcid
   :type: Optional[float]


   
   Get or set the Load curve ID used to describe scale factor on VAL value versus time, see *DEFINE_CURVE, *DEFINE_CURVE_FUNCTION, or *DEFINE_FUNCTION.  If a DEFINE_FUNCTION is used, the following parameters are allowed: f(x,y,z,vx,vy,vz,temp,pres,time).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'BOUNDARY_CONJ_HEAT'






