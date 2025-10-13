





:class:`ElementInertiaOffset`
=============================


.. py:class:: element_inertia_offset.ElementInertiaOffset(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ELEMENT_INERTIA_OFFSET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ElementInertiaOffset

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~eid`
            - Get or set the Element ID. A unique number must be used.
          * - :py:attr:`~nid`
            - Get or set the Node ID. Node to which the mass is assigned.
          * - :py:attr:`~csid`
            - Get or set the Coordinate set ID.
          * - :py:attr:`~ixx`
            - Get or set the XX component of inertia tensor.
          * - :py:attr:`~ixy`
            - Get or set the XY component of inertia tensor.
          * - :py:attr:`~ixz`
            - Get or set the XZ component of inertia tensor.
          * - :py:attr:`~iyy`
            - Get or set the YY component of inertia tensor.
          * - :py:attr:`~iyz`
            - Get or set the YZ component of inertia tensor.
          * - :py:attr:`~izz`
            - Get or set the ZZ component of inertia tensor.
          * - :py:attr:`~mass`
            - Get or set the Lumped mass.
          * - :py:attr:`~x_off`
            - Get or set the x-offset from nodal point.
          * - :py:attr:`~y_off`
            - Get or set the y-offset from nodal point.
          * - :py:attr:`~z_off`
            - Get or set the Z-offset from nodal point.


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

    from element_inertia_offset import ElementInertiaOffset

Property detail
---------------

.. py:property:: eid
   :type: Optional[int]


   
   Get or set the Element ID. A unique number must be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Node ID. Node to which the mass is assigned.
















   ..
       !! processed by numpydoc !!

.. py:property:: csid
   :type: Optional[int]


   
   Get or set the Coordinate set ID.
   EQ.0: global inertia tensor,
   EQ.1: principal moments of inertias with orientation vectors defined by coordinate set, CSID.  See *DEFINE_COORDINATE_SYSTEM and *DEFINE_COORDINATE_VECTOR.
















   ..
       !! processed by numpydoc !!

.. py:property:: ixx
   :type: float


   
   Get or set the XX component of inertia tensor.
















   ..
       !! processed by numpydoc !!

.. py:property:: ixy
   :type: float


   
   Get or set the XY component of inertia tensor.
















   ..
       !! processed by numpydoc !!

.. py:property:: ixz
   :type: float


   
   Get or set the XZ component of inertia tensor.
















   ..
       !! processed by numpydoc !!

.. py:property:: iyy
   :type: float


   
   Get or set the YY component of inertia tensor.
















   ..
       !! processed by numpydoc !!

.. py:property:: iyz
   :type: float


   
   Get or set the YZ component of inertia tensor.
















   ..
       !! processed by numpydoc !!

.. py:property:: izz
   :type: float


   
   Get or set the ZZ component of inertia tensor.
















   ..
       !! processed by numpydoc !!

.. py:property:: mass
   :type: float


   
   Get or set the Lumped mass.
















   ..
       !! processed by numpydoc !!

.. py:property:: x_off
   :type: float


   
   Get or set the x-offset from nodal point.
















   ..
       !! processed by numpydoc !!

.. py:property:: y_off
   :type: float


   
   Get or set the y-offset from nodal point.
















   ..
       !! processed by numpydoc !!

.. py:property:: z_off
   :type: float


   
   Get or set the Z-offset from nodal point.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ELEMENT'


.. py:attribute:: subkeyword
   :value: 'INERTIA_OFFSET'






