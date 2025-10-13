





:class:`ChangeVelocity`
=======================


.. py:class:: change_velocity.ChangeVelocity(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CHANGE_VELOCITY keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ChangeVelocity

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nsid`
            - Get or set the Nodal set ID containing nodes for initial velocity.
          * - :py:attr:`~vx`
            - Get or set the Velocity in x-direction.
          * - :py:attr:`~vy`
            - Get or set the Velocity in y-direction.
          * - :py:attr:`~vz`
            - Get or set the Velocity in z-direction.
          * - :py:attr:`~vxr`
            - Get or set the Rotational velocity about the x-axis.
          * - :py:attr:`~vyr`
            - Get or set the Rotational velocity about the y-axis.
          * - :py:attr:`~vzr`
            - Get or set the Rotational velocity about the z-axis.


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

    from change_velocity import ChangeVelocity

Property detail
---------------

.. py:property:: nsid
   :type: Optional[int]


   
   Get or set the Nodal set ID containing nodes for initial velocity.
















   ..
       !! processed by numpydoc !!

.. py:property:: vx
   :type: float


   
   Get or set the Velocity in x-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: vy
   :type: float


   
   Get or set the Velocity in y-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: vz
   :type: float


   
   Get or set the Velocity in z-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: vxr
   :type: float


   
   Get or set the Rotational velocity about the x-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: vyr
   :type: float


   
   Get or set the Rotational velocity about the y-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: vzr
   :type: float


   
   Get or set the Rotational velocity about the z-axis.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CHANGE'


.. py:attribute:: subkeyword
   :value: 'VELOCITY'






