





:class:`EmRotationAxis`
=======================


.. py:class:: em_rotation_axis.EmRotationAxis(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_ROTATION_AXIS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmRotationAxis

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~xp`
            - Get or set the x coordinate of the point
          * - :py:attr:`~yp`
            - Get or set the y coordinate of the point
          * - :py:attr:`~zp`
            - Get or set the z coordinate of the point
          * - :py:attr:`~xd`
            - Get or set the x coordinate of the direction of the axis
          * - :py:attr:`~yd`
            - Get or set the y coordinate of the direction of the axis
          * - :py:attr:`~zd`
            - Get or set the z coordinate of the direction of the axis
          * - :py:attr:`~numsec`
            - Get or set the Number of Sectors. This field gives the ratio of the full circle to the angular extension of the mesh.This has to be a power of two. For example, NUMSEC = 4 means that the mesh of the part represents one fourth of the total circle.If NUMSEC = 0 for *EM_2DAXI, the solver will replace it with this value.


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

    from em_rotation_axis import EmRotationAxis

Property detail
---------------

.. py:property:: xp
   :type: Optional[float]


   
   Get or set the x coordinate of the point
















   ..
       !! processed by numpydoc !!

.. py:property:: yp
   :type: Optional[float]


   
   Get or set the y coordinate of the point
















   ..
       !! processed by numpydoc !!

.. py:property:: zp
   :type: Optional[float]


   
   Get or set the z coordinate of the point
















   ..
       !! processed by numpydoc !!

.. py:property:: xd
   :type: Optional[float]


   
   Get or set the x coordinate of the direction of the axis
















   ..
       !! processed by numpydoc !!

.. py:property:: yd
   :type: Optional[float]


   
   Get or set the y coordinate of the direction of the axis
















   ..
       !! processed by numpydoc !!

.. py:property:: zd
   :type: Optional[float]


   
   Get or set the z coordinate of the direction of the axis
















   ..
       !! processed by numpydoc !!

.. py:property:: numsec
   :type: Optional[int]


   
   Get or set the Number of Sectors. This field gives the ratio of the full circle to the angular extension of the mesh.This has to be a power of two. For example, NUMSEC = 4 means that the mesh of the part represents one fourth of the total circle.If NUMSEC = 0 for *EM_2DAXI, the solver will replace it with this value.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'ROTATION_AXIS'






