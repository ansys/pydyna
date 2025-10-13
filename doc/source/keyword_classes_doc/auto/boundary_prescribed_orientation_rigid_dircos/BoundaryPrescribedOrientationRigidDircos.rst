





:class:`BoundaryPrescribedOrientationRigidDircos`
=================================================


.. py:class:: boundary_prescribed_orientation_rigid_dircos.BoundaryPrescribedOrientationRigidDircos(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_PRESCRIBED_ORIENTATION_RIGID_DIRCOS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryPrescribedOrientationRigidDircos

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pidb`
            - Get or set the Part ID for rigid body B whose orientation is prescribed
          * - :py:attr:`~pida`
            - Get or set the Part ID for rigid body A.  The orientation of PIDB is measured with respect to the coordinate system of PIDA, as defined by LCO on *MAT_RIGID.  If zero then orientation of PIDB is measured with respect to the global reference frame except for BODY=1 in the ANGLES option
          * - :py:attr:`~intrp`
            - Get or set the Interpolation method used on time history curves:
          * - :py:attr:`~birth`
            - Get or set the Prior to this time the body moves freely under the action of other agents.
          * - :py:attr:`~death`
            - Get or set the The body is freed at this time and subsequently allowed to move under the action of other agents
          * - :py:attr:`~toffset`
            - Get or set the Time offset flag:
          * - :py:attr:`~lcidc11`
            - Get or set the Load curve ID specifying direction cosine Cij as a function of time.  Cij is defined as:where ai (i=1,2,3) are mutually perpendicular unit vectors fixed in PIDA and bj (j=1,2,3) are mutually perpendicular unit vectors fixed in PIDB.  If PIDA=0 then aj (j=1,2,3) are unit vectors aligned, respectively, with the global axes X, Y, and Z.
          * - :py:attr:`~lcidc12`
            - Get or set the Load curve ID specifying direction cosine Cij as a function of time.  Cij is defined as:where ai (i=1,2,3) are mutually perpendicular unit vectors fixed in PIDA and bj (j=1,2,3) are mutually perpendicular unit vectors fixed in PIDB.  If PIDA=0 then aj (j=1,2,3) are unit vectors aligned, respectively, with the global axes X, Y, and Z.
          * - :py:attr:`~lcidc13`
            - Get or set the Load curve ID specifying direction cosine Cij as a function of time.  Cij is defined as:where ai (i=1,2,3) are mutually perpendicular unit vectors fixed in PIDA and bj (j=1,2,3) are mutually perpendicular unit vectors fixed in PIDB.  If PIDA=0 then aj (j=1,2,3) are unit vectors aligned, respectively, with the global axes X, Y, and Z.
          * - :py:attr:`~lcidc21`
            - Get or set the Load curve ID specifying direction cosine Cij as a function of time.  Cij is defined as:where ai (i=1,2,3) are mutually perpendicular unit vectors fixed in PIDA and bj (j=1,2,3) are mutually perpendicular unit vectors fixed in PIDB.  If PIDA=0 then aj (j=1,2,3) are unit vectors aligned, respectively, with the global axes X, Y, and Z.
          * - :py:attr:`~lcidc22`
            - Get or set the Load curve ID specifying direction cosine Cij as a function of time.  Cij is defined as:where ai (i=1,2,3) are mutually perpendicular unit vectors fixed in PIDA and bj (j=1,2,3) are mutually perpendicular unit vectors fixed in PIDB.  If PIDA=0 then aj (j=1,2,3) are unit vectors aligned, respectively, with the global axes X, Y, and Z.
          * - :py:attr:`~lcidc23`
            - Get or set the Load curve ID specifying direction cosine Cij as a function of time.  Cij is defined as:where ai (i=1,2,3) are mutually perpendicular unit vectors fixed in PIDA and bj (j=1,2,3) are mutually perpendicular unit vectors fixed in PIDB.  If PIDA=0 then aj (j=1,2,3) are unit vectors aligned, respectively, with the global axes X, Y, and Z.
          * - :py:attr:`~lcidc31`
            - Get or set the Load curve ID specifying direction cosine Cij as a function of time.  Cij is defined as:where ai (i=1,2,3) are mutually perpendicular unit vectors fixed in PIDA and bj (j=1,2,3) are mutually perpendicular unit vectors fixed in PIDB.  If PIDA=0 then aj (j=1,2,3) are unit vectors aligned, respectively, with the global axes X, Y, and Z.
          * - :py:attr:`~lcidc32`
            - Get or set the Load curve ID specifying direction cosine Cij as a function of time.  Cij is defined as:where ai (i=1,2,3) are mutually perpendicular unit vectors fixed in PIDA and bj (j=1,2,3) are mutually perpendicular unit vectors fixed in PIDB.  If PIDA=0 then aj (j=1,2,3) are unit vectors aligned, respectively, with the global axes X, Y, and Z.
          * - :py:attr:`~lcidc33`
            - Get or set the Load curve ID specifying direction cosine Cij as a function of time.  Cij is defined as:where ai (i=1,2,3) are mutually perpendicular unit vectors fixed in PIDA and bj (j=1,2,3) are mutually perpendicular unit vectors fixed in PIDB.  If PIDA=0 then aj (j=1,2,3) are unit vectors aligned, respectively, with the global axes X, Y, and Z.


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

    from boundary_prescribed_orientation_rigid_dircos import BoundaryPrescribedOrientationRigidDircos

Property detail
---------------

.. py:property:: pidb
   :type: Optional[int]


   
   Get or set the Part ID for rigid body B whose orientation is prescribed
















   ..
       !! processed by numpydoc !!

.. py:property:: pida
   :type: Optional[int]


   
   Get or set the Part ID for rigid body A.  The orientation of PIDB is measured with respect to the coordinate system of PIDA, as defined by LCO on *MAT_RIGID.  If zero then orientation of PIDB is measured with respect to the global reference frame except for BODY=1 in the ANGLES option
















   ..
       !! processed by numpydoc !!

.. py:property:: intrp
   :type: int


   
   Get or set the Interpolation method used on time history curves:
   EQ.1: Linear interpolation (default)
















   ..
       !! processed by numpydoc !!

.. py:property:: birth
   :type: float


   
   Get or set the Prior to this time the body moves freely under the action of other agents.
















   ..
       !! processed by numpydoc !!

.. py:property:: death
   :type: float


   
   Get or set the The body is freed at this time and subsequently allowed to move under the action of other agents
















   ..
       !! processed by numpydoc !!

.. py:property:: toffset
   :type: int


   
   Get or set the Time offset flag:
   EQ.0:   No time offset is applied.
   EQ.1:   The time value of all load curves will be offset by the birth time,
   EQ.0:   no time offset is applied
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidc11
   :type: Optional[int]


   
   Get or set the Load curve ID specifying direction cosine Cij as a function of time.  Cij is defined as:where ai (i=1,2,3) are mutually perpendicular unit vectors fixed in PIDA and bj (j=1,2,3) are mutually perpendicular unit vectors fixed in PIDB.  If PIDA=0 then aj (j=1,2,3) are unit vectors aligned, respectively, with the global axes X, Y, and Z.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidc12
   :type: Optional[int]


   
   Get or set the Load curve ID specifying direction cosine Cij as a function of time.  Cij is defined as:where ai (i=1,2,3) are mutually perpendicular unit vectors fixed in PIDA and bj (j=1,2,3) are mutually perpendicular unit vectors fixed in PIDB.  If PIDA=0 then aj (j=1,2,3) are unit vectors aligned, respectively, with the global axes X, Y, and Z.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidc13
   :type: Optional[int]


   
   Get or set the Load curve ID specifying direction cosine Cij as a function of time.  Cij is defined as:where ai (i=1,2,3) are mutually perpendicular unit vectors fixed in PIDA and bj (j=1,2,3) are mutually perpendicular unit vectors fixed in PIDB.  If PIDA=0 then aj (j=1,2,3) are unit vectors aligned, respectively, with the global axes X, Y, and Z.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidc21
   :type: Optional[int]


   
   Get or set the Load curve ID specifying direction cosine Cij as a function of time.  Cij is defined as:where ai (i=1,2,3) are mutually perpendicular unit vectors fixed in PIDA and bj (j=1,2,3) are mutually perpendicular unit vectors fixed in PIDB.  If PIDA=0 then aj (j=1,2,3) are unit vectors aligned, respectively, with the global axes X, Y, and Z.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidc22
   :type: Optional[int]


   
   Get or set the Load curve ID specifying direction cosine Cij as a function of time.  Cij is defined as:where ai (i=1,2,3) are mutually perpendicular unit vectors fixed in PIDA and bj (j=1,2,3) are mutually perpendicular unit vectors fixed in PIDB.  If PIDA=0 then aj (j=1,2,3) are unit vectors aligned, respectively, with the global axes X, Y, and Z.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidc23
   :type: Optional[int]


   
   Get or set the Load curve ID specifying direction cosine Cij as a function of time.  Cij is defined as:where ai (i=1,2,3) are mutually perpendicular unit vectors fixed in PIDA and bj (j=1,2,3) are mutually perpendicular unit vectors fixed in PIDB.  If PIDA=0 then aj (j=1,2,3) are unit vectors aligned, respectively, with the global axes X, Y, and Z.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidc31
   :type: Optional[int]


   
   Get or set the Load curve ID specifying direction cosine Cij as a function of time.  Cij is defined as:where ai (i=1,2,3) are mutually perpendicular unit vectors fixed in PIDA and bj (j=1,2,3) are mutually perpendicular unit vectors fixed in PIDB.  If PIDA=0 then aj (j=1,2,3) are unit vectors aligned, respectively, with the global axes X, Y, and Z.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidc32
   :type: Optional[int]


   
   Get or set the Load curve ID specifying direction cosine Cij as a function of time.  Cij is defined as:where ai (i=1,2,3) are mutually perpendicular unit vectors fixed in PIDA and bj (j=1,2,3) are mutually perpendicular unit vectors fixed in PIDB.  If PIDA=0 then aj (j=1,2,3) are unit vectors aligned, respectively, with the global axes X, Y, and Z.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidc33
   :type: Optional[int]


   
   Get or set the Load curve ID specifying direction cosine Cij as a function of time.  Cij is defined as:where ai (i=1,2,3) are mutually perpendicular unit vectors fixed in PIDA and bj (j=1,2,3) are mutually perpendicular unit vectors fixed in PIDB.  If PIDA=0 then aj (j=1,2,3) are unit vectors aligned, respectively, with the global axes X, Y, and Z.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'PRESCRIBED_ORIENTATION_RIGID_DIRCOS'






