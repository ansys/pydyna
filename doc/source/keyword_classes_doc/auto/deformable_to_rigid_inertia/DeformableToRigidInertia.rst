





:class:`DeformableToRigidInertia`
=================================


.. py:class:: deformable_to_rigid_inertia.DeformableToRigidInertia(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFORMABLE_TO_RIGID_INERTIA keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DeformableToRigidInertia

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID, see *PART.
          * - :py:attr:`~xc`
            - Get or set the x-coordinate of center of mass
          * - :py:attr:`~yc`
            - Get or set the y-coordinate of center of mass
          * - :py:attr:`~zc`
            - Get or set the z-coordinate of center of mass
          * - :py:attr:`~tm`
            - Get or set the Translational mass
          * - :py:attr:`~ixx`
            - Get or set the Ixx , xx component of inertia tensor.
          * - :py:attr:`~ixy`
            - Get or set the Ixy, xy component of inertia tensor.
          * - :py:attr:`~ixz`
            - Get or set the Ixz, xz component of inertia tensor.
          * - :py:attr:`~iyy`
            - Get or set the Iyy, yy component of inertia tensor.
          * - :py:attr:`~iyz`
            - Get or set the Iyz, yz component of inertia tensor.
          * - :py:attr:`~izz`
            - Get or set the Izz, zz component of inertia tensor.


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

    from deformable_to_rigid_inertia import DeformableToRigidInertia

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID, see *PART.
















   ..
       !! processed by numpydoc !!

.. py:property:: xc
   :type: Optional[float]


   
   Get or set the x-coordinate of center of mass
















   ..
       !! processed by numpydoc !!

.. py:property:: yc
   :type: Optional[float]


   
   Get or set the y-coordinate of center of mass
















   ..
       !! processed by numpydoc !!

.. py:property:: zc
   :type: Optional[float]


   
   Get or set the z-coordinate of center of mass
















   ..
       !! processed by numpydoc !!

.. py:property:: tm
   :type: Optional[float]


   
   Get or set the Translational mass
















   ..
       !! processed by numpydoc !!

.. py:property:: ixx
   :type: Optional[float]


   
   Get or set the Ixx , xx component of inertia tensor.
















   ..
       !! processed by numpydoc !!

.. py:property:: ixy
   :type: float


   
   Get or set the Ixy, xy component of inertia tensor.
















   ..
       !! processed by numpydoc !!

.. py:property:: ixz
   :type: float


   
   Get or set the Ixz, xz component of inertia tensor.
















   ..
       !! processed by numpydoc !!

.. py:property:: iyy
   :type: Optional[float]


   
   Get or set the Iyy, yy component of inertia tensor.
















   ..
       !! processed by numpydoc !!

.. py:property:: iyz
   :type: float


   
   Get or set the Iyz, yz component of inertia tensor.
















   ..
       !! processed by numpydoc !!

.. py:property:: izz
   :type: Optional[float]


   
   Get or set the Izz, zz component of inertia tensor.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DEFORMABLE'


.. py:attribute:: subkeyword
   :value: 'TO_RIGID_INERTIA'






