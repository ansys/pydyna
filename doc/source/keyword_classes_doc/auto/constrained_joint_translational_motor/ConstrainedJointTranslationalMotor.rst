





:class:`ConstrainedJointTranslationalMotor`
===========================================


.. py:class:: constrained_joint_translational_motor.ConstrainedJointTranslationalMotor(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_JOINT_TRANSLATIONAL_MOTOR keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedJointTranslationalMotor

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~n1`
            - Get or set the Node 1, in rigid body A.
          * - :py:attr:`~n2`
            - Get or set the Node 2, in rigid body B.
          * - :py:attr:`~n3`
            - Get or set the Node 3, in rigid body A.
          * - :py:attr:`~n4`
            - Get or set the Node 4, in rigid body B.
          * - :py:attr:`~n5`
            - Get or set the Node 5, in rigid body A.
          * - :py:attr:`~n6`
            - Get or set the Node 6, in rigid body B.
          * - :py:attr:`~rps`
            - Get or set the Relative penalty stiffness (default=1.0).
          * - :py:attr:`~damp`
            - Get or set the Not to be defined.
          * - :py:attr:`~parm`
            - Get or set the Not to be defined.
          * - :py:attr:`~lcid`
            - Get or set the Define load curve ID for joint.
          * - :py:attr:`~type`
            - Get or set the Define integer flag for joints as follows:
          * - :py:attr:`~r1`
            - Get or set the Not to be defined.


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

    from constrained_joint_translational_motor import ConstrainedJointTranslationalMotor

Property detail
---------------

.. py:property:: n1
   :type: Optional[int]


   
   Get or set the Node 1, in rigid body A.
















   ..
       !! processed by numpydoc !!

.. py:property:: n2
   :type: Optional[int]


   
   Get or set the Node 2, in rigid body B.
















   ..
       !! processed by numpydoc !!

.. py:property:: n3
   :type: Optional[int]


   
   Get or set the Node 3, in rigid body A.
















   ..
       !! processed by numpydoc !!

.. py:property:: n4
   :type: Optional[int]


   
   Get or set the Node 4, in rigid body B.
















   ..
       !! processed by numpydoc !!

.. py:property:: n5
   :type: Optional[int]


   
   Get or set the Node 5, in rigid body A.
















   ..
       !! processed by numpydoc !!

.. py:property:: n6
   :type: Optional[int]


   
   Get or set the Node 6, in rigid body B.
















   ..
       !! processed by numpydoc !!

.. py:property:: rps
   :type: float


   
   Get or set the Relative penalty stiffness (default=1.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: damp
   :type: float


   
   Get or set the Not to be defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: parm
   :type: float


   
   Get or set the Not to be defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Define load curve ID for joint.
















   ..
       !! processed by numpydoc !!

.. py:property:: type
   :type: int


   
   Get or set the Define integer flag for joints as follows:
   EQ.0: translational/rotational velocity,
   EQ.1: translational/rotational acceleration,
   EQ.2: translational/rotational displacement.
















   ..
       !! processed by numpydoc !!

.. py:property:: r1
   :type: Optional[float]


   
   Get or set the Not to be defined.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'JOINT_TRANSLATIONAL_MOTOR'






