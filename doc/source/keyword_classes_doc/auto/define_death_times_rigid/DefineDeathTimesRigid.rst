





:class:`DefineDeathTimesRigid`
==============================


.. py:class:: define_death_times_rigid.DefineDeathTimesRigid(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_DEATH_TIMES_RIGID keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineDeathTimesRigid

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~geo`
            - Get or set the Geometric entity type. =1 plane, =2 infinite cylinder, =3 sphere
          * - :py:attr:`~n1`
            - Get or set the Node defining the origin of the geometric entity (optional).
          * - :py:attr:`~n2`
            - Get or set the Node defining the tail of the orientation vector (optional).
          * - :py:attr:`~n3`
            - Get or set the Node defining the head of the orientation vector (optional).
          * - :py:attr:`~x_t`
            - Get or set the X coordinate of the origin of the geometric entity and the tail of the orientation vector
          * - :py:attr:`~y_t`
            - Get or set the Y coordinate of the origin of the geometric entity and the tail of the orientation vector.
          * - :py:attr:`~z_t`
            - Get or set the Z coordinate of the origin of the geometric entity and the tail of the orientation vector
          * - :py:attr:`~x_h`
            - Get or set the X coordinate of the head of the orientation vector.
          * - :py:attr:`~y_h`
            - Get or set the Y coordinate of the head of the orientation vector.
          * - :py:attr:`~r`
            - Get or set the Radius of cylinder or sphere.
          * - :py:attr:`~flag`
            - Get or set the +1 for killing motion when the node is outside of the geometric entity or on the positive side of the plane as defined by the normal direction, or -1 for the inside.
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

    from define_death_times_rigid import DefineDeathTimesRigid

Property detail
---------------

.. py:property:: geo
   :type: Optional[int]


   
   Get or set the Geometric entity type. =1 plane, =2 infinite cylinder, =3 sphere
















   ..
       !! processed by numpydoc !!

.. py:property:: n1
   :type: Optional[int]


   
   Get or set the Node defining the origin of the geometric entity (optional).
















   ..
       !! processed by numpydoc !!

.. py:property:: n2
   :type: Optional[int]


   
   Get or set the Node defining the tail of the orientation vector (optional).
















   ..
       !! processed by numpydoc !!

.. py:property:: n3
   :type: Optional[int]


   
   Get or set the Node defining the head of the orientation vector (optional).
















   ..
       !! processed by numpydoc !!

.. py:property:: x_t
   :type: Optional[float]


   
   Get or set the X coordinate of the origin of the geometric entity and the tail of the orientation vector
















   ..
       !! processed by numpydoc !!

.. py:property:: y_t
   :type: Optional[float]


   
   Get or set the Y coordinate of the origin of the geometric entity and the tail of the orientation vector.
















   ..
       !! processed by numpydoc !!

.. py:property:: z_t
   :type: Optional[float]


   
   Get or set the Z coordinate of the origin of the geometric entity and the tail of the orientation vector
















   ..
       !! processed by numpydoc !!

.. py:property:: x_h
   :type: Optional[float]


   
   Get or set the X coordinate of the head of the orientation vector.
















   ..
       !! processed by numpydoc !!

.. py:property:: y_h
   :type: Optional[float]


   
   Get or set the Y coordinate of the head of the orientation vector.
















   ..
       !! processed by numpydoc !!

.. py:property:: r
   :type: Optional[float]


   
   Get or set the Radius of cylinder or sphere.
















   ..
       !! processed by numpydoc !!

.. py:property:: flag
   :type: Optional[int]


   
   Get or set the +1 for killing motion when the node is outside of the geometric entity or on the positive side of the plane as defined by the normal direction, or -1 for the inside.
















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
   :value: 'DEATH_TIMES_RIGID'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





