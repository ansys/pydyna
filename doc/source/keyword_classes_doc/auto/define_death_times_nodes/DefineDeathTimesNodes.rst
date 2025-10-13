





:class:`DefineDeathTimesNodes`
==============================


.. py:class:: define_death_times_nodes.DefineDeathTimesNodes(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_DEATH_TIMES_NODES keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineDeathTimesNodes

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
          * - :py:attr:`~nid1`
            - Get or set the First node ID
          * - :py:attr:`~nid2`
            - Get or set the Second node ID.
          * - :py:attr:`~nid3`
            - Get or set the Third node ID
          * - :py:attr:`~nid4`
            - Get or set the Fourth node ID.
          * - :py:attr:`~nid5`
            - Get or set the Fifth node ID
          * - :py:attr:`~nid6`
            - Get or set the Sixth node ID
          * - :py:attr:`~nid7`
            - Get or set the Seventh node ID.
          * - :py:attr:`~nid8`
            - Get or set the Eighth node ID.
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

    from define_death_times_nodes import DefineDeathTimesNodes

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

.. py:property:: nid1
   :type: Optional[int]


   
   Get or set the First node ID
















   ..
       !! processed by numpydoc !!

.. py:property:: nid2
   :type: Optional[int]


   
   Get or set the Second node ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid3
   :type: Optional[int]


   
   Get or set the Third node ID
















   ..
       !! processed by numpydoc !!

.. py:property:: nid4
   :type: Optional[int]


   
   Get or set the Fourth node ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid5
   :type: Optional[int]


   
   Get or set the Fifth node ID
















   ..
       !! processed by numpydoc !!

.. py:property:: nid6
   :type: Optional[int]


   
   Get or set the Sixth node ID
















   ..
       !! processed by numpydoc !!

.. py:property:: nid7
   :type: Optional[int]


   
   Get or set the Seventh node ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid8
   :type: Optional[int]


   
   Get or set the Eighth node ID.
















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
   :value: 'DEATH_TIMES_NODES'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





