





:class:`BoundaryPap`
====================


.. py:class:: boundary_pap.BoundaryPap(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_PAP keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryPap

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~segid`
            - Get or set the Segment set ID.
          * - :py:attr:`~lcid`
            - Get or set the Load curve giving pore air pressure vs. time. EQ.0: constant pressure assumed equal to CMULT
          * - :py:attr:`~cmult`
            - Get or set the Factor on curve or constant pressure head if LCID=0
          * - :py:attr:`~cvmass`
            - Get or set the Initial mass of a control volume next to the segment set SETID.
          * - :py:attr:`~block`
            - Get or set the Contact blockage effect,
          * - :py:attr:`~tbirth`
            - Get or set the Time at which boundary condition becomes active
          * - :py:attr:`~tdeath`
            - Get or set the Time at which boundary condition becomes inactive
          * - :py:attr:`~cvrper`
            - Get or set the Permeability factor of cover material, where cover refers to a shell layer coating the surface of the solid. Default value is 1.0 when it is not defined. See Remark 3 below. 0.0 <= CVRPER <= 1.0


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

    from boundary_pap import BoundaryPap

Property detail
---------------

.. py:property:: segid
   :type: Optional[int]


   
   Get or set the Segment set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve giving pore air pressure vs. time. EQ.0: constant pressure assumed equal to CMULT
















   ..
       !! processed by numpydoc !!

.. py:property:: cmult
   :type: Optional[float]


   
   Get or set the Factor on curve or constant pressure head if LCID=0
















   ..
       !! processed by numpydoc !!

.. py:property:: cvmass
   :type: Optional[float]


   
   Get or set the Initial mass of a control volume next to the segment set SETID.
















   ..
       !! processed by numpydoc !!

.. py:property:: block
   :type: float


   
   Get or set the Contact blockage effect,
   EQ.0: When all segments in SEGID are subject to the pressure
   defined by LCID and CMULT;
   EQ.-1: When only elements in SEGID not involved in contact are
   subject to the pressure defined by LCID and CMULT
















   ..
       !! processed by numpydoc !!

.. py:property:: tbirth
   :type: float


   
   Get or set the Time at which boundary condition becomes active
















   ..
       !! processed by numpydoc !!

.. py:property:: tdeath
   :type: float


   
   Get or set the Time at which boundary condition becomes inactive
















   ..
       !! processed by numpydoc !!

.. py:property:: cvrper
   :type: float


   
   Get or set the Permeability factor of cover material, where cover refers to a shell layer coating the surface of the solid. Default value is 1.0 when it is not defined. See Remark 3 below. 0.0 <= CVRPER <= 1.0
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'PAP'






