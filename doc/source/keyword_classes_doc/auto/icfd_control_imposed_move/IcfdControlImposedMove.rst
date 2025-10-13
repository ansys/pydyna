





:class:`IcfdControlImposedMove`
===============================


.. py:class:: icfd_control_imposed_move.IcfdControlImposedMove(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_CONTROL_IMPOSED_MOVE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdControlImposedMove

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the This can be any part ID referenced in *ICFD_PART or *ICFD_PART_VOL. If PID = 0,then the whole volume mesh will be used.
          * - :py:attr:`~lcvx`
            - Get or set the LCID for the velocity in the three directions (X,Y,Z).
          * - :py:attr:`~lcvy`
            - Get or set the LCID for the velocity in the three directions (X,Y,Z).
          * - :py:attr:`~lcvz`
            - Get or set the LCID for the velocity in the three directions (X,Y,Z).
          * - :py:attr:`~vadt`
            - Get or set the Velocity/Displacements flag for translation components
          * - :py:attr:`~alphal`
            - Get or set the LCID for the three Euler angle rotational velocities in the local reference frame.
          * - :py:attr:`~betal`
            - Get or set the LCID for the three Euler angle rotational velocities in the local reference frame.
          * - :py:attr:`~gammal`
            - Get or set the LCID for the three Euler angle rotational velocities in the local reference frame.
          * - :py:attr:`~alphag`
            - Get or set the LCID for the three Euler angle rotational velocities in the global reference frame.
          * - :py:attr:`~betag`
            - Get or set the LCID for the three Euler angle rotational velocities in the global reference frame.
          * - :py:attr:`~gammag`
            - Get or set the LCID for the three Euler angle rotational velocities in the global reference frame.
          * - :py:attr:`~vadr`
            - Get or set the Velocity/Displacements flag for rotation components
          * - :py:attr:`~ptid`
            - Get or set the Point ID for the origin of the local reference frame.If not defined, the barycenter of the volume mesh will be used.
          * - :py:attr:`~x1`
            - Get or set the Three components of the local reference X1 axis./n If not defined, the global x axis will be used.
          * - :py:attr:`~y1`
            - Get or set the Three components of the local reference X1 axis./n If not defined, the global x axis will be used.
          * - :py:attr:`~z1`
            - Get or set the Three components of the local reference X1 axis./n If not defined, the global x axis will be used.
          * - :py:attr:`~x2`
            - Get or set the Three components of the local reference X2 axis./n If not defined, the global y axis will be used.
          * - :py:attr:`~y2`
            - Get or set the Three components of the local reference X2 axis./n If not defined, the global y axis will be used.
          * - :py:attr:`~z2`
            - Get or set the Three components of the local reference X2 axis./n If not defined, the global y axis will be used.
          * - :py:attr:`~ptido`
            - Get or set the Point ID (See ICFD_DEFINE_POINT) for the center of rotation.
          * - :py:attr:`~axe`
            - Get or set the Rotation axis (X=1, Y=2, Z=3).
          * - :py:attr:`~ptidv`
            - Get or set the Point ID (See ICFD_DEFINE_POINT) for the rotation velocity. If point is static, no rotation will occur.


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

    from icfd_control_imposed_move import IcfdControlImposedMove

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the This can be any part ID referenced in *ICFD_PART or *ICFD_PART_VOL. If PID = 0,then the whole volume mesh will be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcvx
   :type: Optional[int]


   
   Get or set the LCID for the velocity in the three directions (X,Y,Z).
















   ..
       !! processed by numpydoc !!

.. py:property:: lcvy
   :type: Optional[int]


   
   Get or set the LCID for the velocity in the three directions (X,Y,Z).
















   ..
       !! processed by numpydoc !!

.. py:property:: lcvz
   :type: Optional[int]


   
   Get or set the LCID for the velocity in the three directions (X,Y,Z).
















   ..
       !! processed by numpydoc !!

.. py:property:: vadt
   :type: int


   
   Get or set the Velocity/Displacements flag for translation components
   EQ.0:Prescribe Velocity
   EQ.1:Prescribe Displacements
















   ..
       !! processed by numpydoc !!

.. py:property:: alphal
   :type: Optional[int]


   
   Get or set the LCID for the three Euler angle rotational velocities in the local reference frame.
















   ..
       !! processed by numpydoc !!

.. py:property:: betal
   :type: Optional[int]


   
   Get or set the LCID for the three Euler angle rotational velocities in the local reference frame.
















   ..
       !! processed by numpydoc !!

.. py:property:: gammal
   :type: Optional[int]


   
   Get or set the LCID for the three Euler angle rotational velocities in the local reference frame.
















   ..
       !! processed by numpydoc !!

.. py:property:: alphag
   :type: Optional[int]


   
   Get or set the LCID for the three Euler angle rotational velocities in the global reference frame.
















   ..
       !! processed by numpydoc !!

.. py:property:: betag
   :type: Optional[int]


   
   Get or set the LCID for the three Euler angle rotational velocities in the global reference frame.
















   ..
       !! processed by numpydoc !!

.. py:property:: gammag
   :type: Optional[int]


   
   Get or set the LCID for the three Euler angle rotational velocities in the global reference frame.
















   ..
       !! processed by numpydoc !!

.. py:property:: vadr
   :type: int


   
   Get or set the Velocity/Displacements flag for rotation components
   EQ.0:Prescribe Velocity
   EQ.1:Prescribe Displacements
















   ..
       !! processed by numpydoc !!

.. py:property:: ptid
   :type: int


   
   Get or set the Point ID for the origin of the local reference frame.If not defined, the barycenter of the volume mesh will be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: x1
   :type: float


   
   Get or set the Three components of the local reference X1 axis./n If not defined, the global x axis will be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: y1
   :type: float


   
   Get or set the Three components of the local reference X1 axis./n If not defined, the global x axis will be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: z1
   :type: float


   
   Get or set the Three components of the local reference X1 axis./n If not defined, the global x axis will be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: x2
   :type: float


   
   Get or set the Three components of the local reference X2 axis./n If not defined, the global y axis will be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: y2
   :type: float


   
   Get or set the Three components of the local reference X2 axis./n If not defined, the global y axis will be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: z2
   :type: float


   
   Get or set the Three components of the local reference X2 axis./n If not defined, the global y axis will be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: ptido
   :type: int


   
   Get or set the Point ID (See ICFD_DEFINE_POINT) for the center of rotation.
















   ..
       !! processed by numpydoc !!

.. py:property:: axe
   :type: int


   
   Get or set the Rotation axis (X=1, Y=2, Z=3).
















   ..
       !! processed by numpydoc !!

.. py:property:: ptidv
   :type: int


   
   Get or set the Point ID (See ICFD_DEFINE_POINT) for the rotation velocity. If point is static, no rotation will occur.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'CONTROL_IMPOSED_MOVE'






