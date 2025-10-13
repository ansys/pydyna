





:class:`InitialVelocityNode`
============================


.. py:class:: initial_velocity_node.InitialVelocityNode(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INITIAL_VELOCITY_NODE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InitialVelocityNode

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nid`
            - Get or set the Node ID.
          * - :py:attr:`~vx`
            - Get or set the Initial translational velocity in x-direction.
          * - :py:attr:`~vy`
            - Get or set the Initial translational velocity in y-direction.
          * - :py:attr:`~vz`
            - Get or set the Initial translational velocity in z-direction.
          * - :py:attr:`~vxr`
            - Get or set the Initial rotational velocity about the x-axis.
          * - :py:attr:`~vyr`
            - Get or set the Initial rotational velocity about the y-axis.
          * - :py:attr:`~vzr`
            - Get or set the Initial rotational velocity about the z-axis.
          * - :py:attr:`~icid`
            - Get or set the Local coordinate system ID. The specified velocities are in the local system if ICID is greater than zero.


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

    from initial_velocity_node import InitialVelocityNode

Property detail
---------------

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Node ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: vx
   :type: float


   
   Get or set the Initial translational velocity in x-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: vy
   :type: float


   
   Get or set the Initial translational velocity in y-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: vz
   :type: float


   
   Get or set the Initial translational velocity in z-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: vxr
   :type: float


   
   Get or set the Initial rotational velocity about the x-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: vyr
   :type: float


   
   Get or set the Initial rotational velocity about the y-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: vzr
   :type: float


   
   Get or set the Initial rotational velocity about the z-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: icid
   :type: Optional[int]


   
   Get or set the Local coordinate system ID. The specified velocities are in the local system if ICID is greater than zero.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INITIAL'


.. py:attribute:: subkeyword
   :value: 'VELOCITY_NODE'






