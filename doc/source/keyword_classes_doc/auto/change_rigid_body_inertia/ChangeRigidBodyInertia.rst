





:class:`ChangeRigidBodyInertia`
===============================


.. py:class:: change_rigid_body_inertia.ChangeRigidBodyInertia(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CHANGE_RIGID_BODY_INERTIA keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ChangeRigidBodyInertia

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the ID for this change inertia input.
          * - :py:attr:`~pid`
            - Get or set the Part ID, see *PART.
          * - :py:attr:`~tm`
            - Get or set the Translational mass.
          * - :py:attr:`~ixx`
            - Get or set the Ixx, xx component of inertia tensor.
          * - :py:attr:`~ixy`
            - Get or set the Ixy.
          * - :py:attr:`~ixz`
            - Get or set the Ixy.
          * - :py:attr:`~iyy`
            - Get or set the Iyy, yy component of inertia tensor.
          * - :py:attr:`~iyz`
            - Get or set the Iyz.
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

    from change_rigid_body_inertia import ChangeRigidBodyInertia

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the ID for this change inertia input.
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: int


   
   Get or set the Part ID, see *PART.
















   ..
       !! processed by numpydoc !!

.. py:property:: tm
   :type: float


   
   Get or set the Translational mass.
















   ..
       !! processed by numpydoc !!

.. py:property:: ixx
   :type: Optional[float]


   
   Get or set the Ixx, xx component of inertia tensor.
















   ..
       !! processed by numpydoc !!

.. py:property:: ixy
   :type: float


   
   Get or set the Ixy.
















   ..
       !! processed by numpydoc !!

.. py:property:: ixz
   :type: float


   
   Get or set the Ixy.
















   ..
       !! processed by numpydoc !!

.. py:property:: iyy
   :type: float


   
   Get or set the Iyy, yy component of inertia tensor.
















   ..
       !! processed by numpydoc !!

.. py:property:: iyz
   :type: float


   
   Get or set the Iyz.
















   ..
       !! processed by numpydoc !!

.. py:property:: izz
   :type: float


   
   Get or set the Izz, zz component of inertia tensor.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CHANGE'


.. py:attribute:: subkeyword
   :value: 'RIGID_BODY_INERTIA'






