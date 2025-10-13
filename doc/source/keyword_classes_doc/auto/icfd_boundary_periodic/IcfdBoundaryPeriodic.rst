





:class:`IcfdBoundaryPeriodic`
=============================


.. py:class:: icfd_boundary_periodic.IcfdBoundaryPeriodic(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_BOUNDARY_PERIODIC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdBoundaryPeriodic

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the PID for a fluid surface
          * - :py:attr:`~ptype`
            - Get or set the Boundary type:
          * - :py:attr:`~pid2`
            - Get or set the PID for the second surface mesh. The boundary condition defined in PTYPE will applied between PID and PID2. See Remark 1.
          * - :py:attr:`~pdlcid`
            - Get or set the Optional load curve ID to describe the pressure drop value versus time between PID and PID2.
          * - :py:attr:`~axe`
            - Get or set the If PTYPE=1 :
          * - :py:attr:`~ptid`
            - Get or set the Origin point ID for PTYPE=1 and PTYPE=2 (See *ICFD_DEFINE_POINT).
          * - :py:attr:`~angle`
            - Get or set the Rotation angle for PTYPE=1. Characterizes contact distance for PTYPE=3 and axe different then 0.


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

    from icfd_boundary_periodic import IcfdBoundaryPeriodic

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the PID for a fluid surface
















   ..
       !! processed by numpydoc !!

.. py:property:: ptype
   :type: int


   
   Get or set the Boundary type:
   EQ.1:   Periodic rotation boundary condition.
   EQ.2 : Periodic reflective boundary condition.
   EQ.3 : Sliding mesh boundary condition
   .
















   ..
       !! processed by numpydoc !!

.. py:property:: pid2
   :type: Optional[int]


   
   Get or set the PID for the second surface mesh. The boundary condition defined in PTYPE will applied between PID and PID2. See Remark 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: pdlcid
   :type: Optional[int]


   
   Get or set the Optional load curve ID to describe the pressure drop value versus time between PID and PID2.
















   ..
       !! processed by numpydoc !!

.. py:property:: axe
   :type: Optional[int]


   
   Get or set the If PTYPE=1 :
   EQ.1:   Rotation around X - Axis.
   EQ.2 : Rotation around Y - Axis.
   EQ.3 : Rotation around Z - Axis.
   If PTYPE = 3 :
   EQ.0 : The contact distance between two faces of PID and PID2 is based on the characteristic local element size.
   EQ.1 : The contact distance between two faces of PID and PID2 is based on the characteristic local element size scaled by a factor given by ANGLE.
   EQ.2 : The contact distance between two faces of PID and PID2 is based on the length given by ANGLE.
















   ..
       !! processed by numpydoc !!

.. py:property:: ptid
   :type: Optional[int]


   
   Get or set the Origin point ID for PTYPE=1 and PTYPE=2 (See *ICFD_DEFINE_POINT).
















   ..
       !! processed by numpydoc !!

.. py:property:: angle
   :type: Optional[int]


   
   Get or set the Rotation angle for PTYPE=1. Characterizes contact distance for PTYPE=3 and axe different then 0.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'BOUNDARY_PERIODIC'






