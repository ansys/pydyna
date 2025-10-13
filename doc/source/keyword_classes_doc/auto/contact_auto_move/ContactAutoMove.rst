





:class:`ContactAutoMove`
========================


.. py:class:: contact_auto_move.ContactAutoMove(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTACT_AUTO_MOVE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ContactAutoMove

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the ID for this auto positioning input
          * - :py:attr:`~cid`
            - Get or set the Contact ID
          * - :py:attr:`~vid`
            - Get or set the Vector ID for a vector oriented in the direction of the movement of the master surface. See *DEFINE_VECTOR. The origin of this vector is unimportant since the direction consines of the vector are computed and used
          * - :py:attr:`~lcid`
            - Get or set the Optional loas curve ID defining velocity versus time. The load curve should be defined by four points, and its shape should resemble a trapzoid with the longest parallel side along the abcissa. The abcissa is adjusted(shortened)in the flat part of the curve where the velocity is constant to account for the movement
          * - :py:attr:`~atime`
            - Get or set the Activeation time. A this time the master surface is moved
          * - :py:attr:`~offset`
            - Get or set the Time at which a master surface will move to close a gap distance,


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

    from contact_auto_move import ContactAutoMove

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the ID for this auto positioning input
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: Optional[int]


   
   Get or set the Contact ID
















   ..
       !! processed by numpydoc !!

.. py:property:: vid
   :type: Optional[int]


   
   Get or set the Vector ID for a vector oriented in the direction of the movement of the master surface. See *DEFINE_VECTOR. The origin of this vector is unimportant since the direction consines of the vector are computed and used
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Optional loas curve ID defining velocity versus time. The load curve should be defined by four points, and its shape should resemble a trapzoid with the longest parallel side along the abcissa. The abcissa is adjusted(shortened)in the flat part of the curve where the velocity is constant to account for the movement
















   ..
       !! processed by numpydoc !!

.. py:property:: atime
   :type: Optional[float]


   
   Get or set the Activeation time. A this time the master surface is moved
















   ..
       !! processed by numpydoc !!

.. py:property:: offset
   :type: float


   
   Get or set the Time at which a master surface will move to close a gap distance,
   which may happen following the move of another master surface.
   This is useful in sequential multiple flanging or press hemming
   simulation. Simulation time (CPU) is much faster based on the
   shortened tool travel (no change to the termination time).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTACT'


.. py:attribute:: subkeyword
   :value: 'AUTO_MOVE'






