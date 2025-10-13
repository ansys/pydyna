





:class:`DefineSphActiveRegion`
==============================


.. py:class:: define_sph_active_region.DefineSphActiveRegion(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_SPH_ACTIVE_REGION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineSphActiveRegion

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Part Set ID/Part ID.
          * - :py:attr:`~type`
            - Get or set the EQ.0: Part set
          * - :py:attr:`~stype`
            - Get or set the Type of the region.
          * - :py:attr:`~cycle`
            - Get or set the Number of cycles between each check.
          * - :py:attr:`~nid`
            - Get or set the Referential nodal ID, SPH box will move with this node.
          * - :py:attr:`~icid`
            - Get or set the Local coordinate system ID.
          * - :py:attr:`~ibuff`
            - Get or set the Buffer zone flag, only used when STYPE = 0:
          * - :py:attr:`~ximin`
            - Get or set the Minimum x, coordinate of the inner box.
          * - :py:attr:`~yimin`
            - Get or set the Minimum y coordinate of the inner box.
          * - :py:attr:`~zimin`
            - Get or set the Minimum z coordinate of the inner box.
          * - :py:attr:`~ximax`
            - Get or set the Maximum x coordinates of the inner box.
          * - :py:attr:`~yimax`
            - Get or set the Maximum y coordinates of the inner box.
          * - :py:attr:`~zimax`
            - Get or set the Maximum z coordinates of the inner box.
          * - :py:attr:`~xomin`
            - Get or set the Minimum x, coordinate of the outer box.
          * - :py:attr:`~yomin`
            - Get or set the Minimum y coordinate of the outer box.
          * - :py:attr:`~zomin`
            - Get or set the Minimum z coordinate of the outer box.
          * - :py:attr:`~xomax`
            - Get or set the Maximum x coordinates of the outer box.
          * - :py:attr:`~yomax`
            - Get or set the Maximum y coordinates of the outer box.
          * - :py:attr:`~zomax`
            - Get or set the Maximum z coordinates of the outer box.
          * - :py:attr:`~x0`
            - Get or set the Coordinates of the cylinder center. This point also serves as the tail for the vector specifying the direction of the cylinders axis.
          * - :py:attr:`~y0`
            - Get or set the Coordinates of the cylinder center. This point also serves as the tail for the vector specifying the direction of the cylinders axis.
          * - :py:attr:`~z0`
            - Get or set the Coordinates of the cylinder center. This point also serves as the tail for the vector specifying the direction of the cylinders axis.
          * - :py:attr:`~xh`
            - Get or set the Coordinates for the head of the cylinders axial direction vector.
          * - :py:attr:`~yh`
            - Get or set the Coordinates for the head of the cylinders axial direction vector.
          * - :py:attr:`~zh`
            - Get or set the Coordinates for the head of the cylinders axial direction vector.
          * - :py:attr:`~rmin`
            - Get or set the Radius of the interior cylinder.
          * - :py:attr:`~zmin`
            - Get or set the Length of the interior cylinder.
          * - :py:attr:`~rmax`
            - Get or set the Radius of the outer cylinder.
          * - :py:attr:`~zmax`
            - Get or set the Length of the outer cylinder.
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

    from define_sph_active_region import DefineSphActiveRegion

Property detail
---------------

.. py:property:: id
   :type: int


   
   Get or set the Part Set ID/Part ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: type
   :type: int


   
   Get or set the EQ.0: Part set
   EQ.1: Part.
















   ..
       !! processed by numpydoc !!

.. py:property:: stype
   :type: int


   
   Get or set the Type of the region.
   EQ.0: Rectangular box
   EQ.1: Cylinder
   EQ.2: Sphere.
















   ..
       !! processed by numpydoc !!

.. py:property:: cycle
   :type: int


   
   Get or set the Number of cycles between each check.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Referential nodal ID, SPH box will move with this node.
















   ..
       !! processed by numpydoc !!

.. py:property:: icid
   :type: Optional[int]


   
   Get or set the Local coordinate system ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: ibuff
   :type: int


   
   Get or set the Buffer zone flag, only used when STYPE = 0:
   EQ.0: particles on the edge of the outer box don't get any special treatment.
   EQ.1 : particles on the edge of the outer box are frozen in space and act as neighbors for active particles inside the box.
   This option is mainly used for fluid simulations to prevent the fluid from spilling out of the activation box..
















   ..
       !! processed by numpydoc !!

.. py:property:: ximin
   :type: float


   
   Get or set the Minimum x, coordinate of the inner box.
















   ..
       !! processed by numpydoc !!

.. py:property:: yimin
   :type: float


   
   Get or set the Minimum y coordinate of the inner box.
















   ..
       !! processed by numpydoc !!

.. py:property:: zimin
   :type: float


   
   Get or set the Minimum z coordinate of the inner box.
















   ..
       !! processed by numpydoc !!

.. py:property:: ximax
   :type: float


   
   Get or set the Maximum x coordinates of the inner box.
















   ..
       !! processed by numpydoc !!

.. py:property:: yimax
   :type: float


   
   Get or set the Maximum y coordinates of the inner box.
















   ..
       !! processed by numpydoc !!

.. py:property:: zimax
   :type: float


   
   Get or set the Maximum z coordinates of the inner box.
















   ..
       !! processed by numpydoc !!

.. py:property:: xomin
   :type: float


   
   Get or set the Minimum x, coordinate of the outer box.
















   ..
       !! processed by numpydoc !!

.. py:property:: yomin
   :type: float


   
   Get or set the Minimum y coordinate of the outer box.
















   ..
       !! processed by numpydoc !!

.. py:property:: zomin
   :type: float


   
   Get or set the Minimum z coordinate of the outer box.
















   ..
       !! processed by numpydoc !!

.. py:property:: xomax
   :type: float


   
   Get or set the Maximum x coordinates of the outer box.
















   ..
       !! processed by numpydoc !!

.. py:property:: yomax
   :type: float


   
   Get or set the Maximum y coordinates of the outer box.
















   ..
       !! processed by numpydoc !!

.. py:property:: zomax
   :type: float


   
   Get or set the Maximum z coordinates of the outer box.
















   ..
       !! processed by numpydoc !!

.. py:property:: x0
   :type: float


   
   Get or set the Coordinates of the cylinder center. This point also serves as the tail for the vector specifying the direction of the cylinders axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: y0
   :type: float


   
   Get or set the Coordinates of the cylinder center. This point also serves as the tail for the vector specifying the direction of the cylinders axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: z0
   :type: float


   
   Get or set the Coordinates of the cylinder center. This point also serves as the tail for the vector specifying the direction of the cylinders axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: xh
   :type: float


   
   Get or set the Coordinates for the head of the cylinders axial direction vector.
















   ..
       !! processed by numpydoc !!

.. py:property:: yh
   :type: float


   
   Get or set the Coordinates for the head of the cylinders axial direction vector.
















   ..
       !! processed by numpydoc !!

.. py:property:: zh
   :type: float


   
   Get or set the Coordinates for the head of the cylinders axial direction vector.
















   ..
       !! processed by numpydoc !!

.. py:property:: rmin
   :type: float


   
   Get or set the Radius of the interior cylinder.
















   ..
       !! processed by numpydoc !!

.. py:property:: zmin
   :type: float


   
   Get or set the Length of the interior cylinder.
















   ..
       !! processed by numpydoc !!

.. py:property:: rmax
   :type: float


   
   Get or set the Radius of the outer cylinder.
















   ..
       !! processed by numpydoc !!

.. py:property:: zmax
   :type: float


   
   Get or set the Length of the outer cylinder.
















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
   :value: 'SPH_ACTIVE_REGION'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





