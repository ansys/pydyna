





:class:`InitialVehicleKinematics`
=================================


.. py:class:: initial_vehicle_kinematics.InitialVehicleKinematics(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INITIAL_VEHICLE_KINEMATICS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InitialVehicleKinematics

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~grav`
            - Get or set the Gravity direction code:
          * - :py:attr:`~psid`
            - Get or set the Part set ID, see also *SET_PART.
          * - :py:attr:`~xo`
            - Get or set the x-coordinate of initial position of mass center.
          * - :py:attr:`~yo`
            - Get or set the y-coordinate of initial position of mass center.
          * - :py:attr:`~zo`
            - Get or set the z-coordinate of initial position of mass center.
          * - :py:attr:`~xf`
            - Get or set the x-coordinate of final position of mass center.
          * - :py:attr:`~yf`
            - Get or set the y-coordinate of final position of mass center.
          * - :py:attr:`~zf`
            - Get or set the z-coordinate of final position of mass center.
          * - :py:attr:`~vx`
            - Get or set the x-component of mass center velocity.
          * - :py:attr:`~vy`
            - Get or set the y-component of mass center velocity.
          * - :py:attr:`~vz`
            - Get or set the z-component of mass center velocity.
          * - :py:attr:`~aaxis`
            - Get or set the First rotation axis code:
          * - :py:attr:`~baxis`
            - Get or set the Second rotation axis code:
          * - :py:attr:`~caxis`
            - Get or set the Third rotation axis code:
          * - :py:attr:`~aang`
            - Get or set the Rotation angle about the first rotation axis (degrees).
          * - :py:attr:`~bang`
            - Get or set the Rotation angle about the second rotation axis (degrees).
          * - :py:attr:`~cang`
            - Get or set the Rotation angle about the third rotation axis (degrees).
          * - :py:attr:`~wa`
            - Get or set the Angular velocity component for the first axis (radian/second).
          * - :py:attr:`~wb`
            - Get or set the Angular velocity component for the second axis (radian/second).
          * - :py:attr:`~wc`
            - Get or set the Angular velocity component for the third axis (radian/second).


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

    from initial_vehicle_kinematics import InitialVehicleKinematics

Property detail
---------------

.. py:property:: grav
   :type: int


   
   Get or set the Gravity direction code:
   EQ.1: Global +x direction,
   EQ.-1: Global -x direction,
   EQ.2: Global +y direction,
   EQ.-2: Global -y direction,
   EQ.3 Global +z direction,
   EQ.-3: Global -z direction.
   Note: This must be the same for all vehicles present in the model.
















   ..
       !! processed by numpydoc !!

.. py:property:: psid
   :type: Optional[int]


   
   Get or set the Part set ID, see also *SET_PART.
















   ..
       !! processed by numpydoc !!

.. py:property:: xo
   :type: float


   
   Get or set the x-coordinate of initial position of mass center.
















   ..
       !! processed by numpydoc !!

.. py:property:: yo
   :type: float


   
   Get or set the y-coordinate of initial position of mass center.
















   ..
       !! processed by numpydoc !!

.. py:property:: zo
   :type: float


   
   Get or set the z-coordinate of initial position of mass center.
















   ..
       !! processed by numpydoc !!

.. py:property:: xf
   :type: float


   
   Get or set the x-coordinate of final position of mass center.
















   ..
       !! processed by numpydoc !!

.. py:property:: yf
   :type: float


   
   Get or set the y-coordinate of final position of mass center.
















   ..
       !! processed by numpydoc !!

.. py:property:: zf
   :type: float


   
   Get or set the z-coordinate of final position of mass center.
















   ..
       !! processed by numpydoc !!

.. py:property:: vx
   :type: float


   
   Get or set the x-component of mass center velocity.
















   ..
       !! processed by numpydoc !!

.. py:property:: vy
   :type: float


   
   Get or set the y-component of mass center velocity.
















   ..
       !! processed by numpydoc !!

.. py:property:: vz
   :type: float


   
   Get or set the z-component of mass center velocity.
















   ..
       !! processed by numpydoc !!

.. py:property:: aaxis
   :type: int


   
   Get or set the First rotation axis code:
   EQ.1: Initially aligned with global x-axis,
   EQ.2: Initially aligned with global y-axis,
   EQ.3: Initially aligned with global z-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: baxis
   :type: int


   
   Get or set the Second rotation axis code:
   EQ.1: Initially aligned with global x-axis,
   EQ.2: Initially aligned with global y-axis,
   EQ.3: Initially aligned with global z-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: caxis
   :type: int


   
   Get or set the Third rotation axis code:
   EQ.1: Initially aligned with global x-axis,
   EQ.2: Initially aligned with global y-axis,
   EQ.3: Initially aligned with global z-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: aang
   :type: float


   
   Get or set the Rotation angle about the first rotation axis (degrees).
















   ..
       !! processed by numpydoc !!

.. py:property:: bang
   :type: float


   
   Get or set the Rotation angle about the second rotation axis (degrees).
















   ..
       !! processed by numpydoc !!

.. py:property:: cang
   :type: float


   
   Get or set the Rotation angle about the third rotation axis (degrees).
















   ..
       !! processed by numpydoc !!

.. py:property:: wa
   :type: float


   
   Get or set the Angular velocity component for the first axis (radian/second).
















   ..
       !! processed by numpydoc !!

.. py:property:: wb
   :type: float


   
   Get or set the Angular velocity component for the second axis (radian/second).
















   ..
       !! processed by numpydoc !!

.. py:property:: wc
   :type: float


   
   Get or set the Angular velocity component for the third axis (radian/second).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INITIAL'


.. py:attribute:: subkeyword
   :value: 'VEHICLE_KINEMATICS'






