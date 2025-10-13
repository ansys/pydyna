





:class:`ConstrainedJointRevolute`
=================================


.. py:class:: constrained_joint_revolute.ConstrainedJointRevolute(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_JOINT_REVOLUTE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedJointRevolute

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
            - Get or set the Not to be defined.
          * - :py:attr:`~n6`
            - Get or set the Not to be defined.
          * - :py:attr:`~rps`
            - Get or set the Relative penalty stiffness (default=1.0).
          * - :py:attr:`~damp`
            - Get or set the Damping scale factor on default damping value (default=1.0).


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

    from constrained_joint_revolute import ConstrainedJointRevolute

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
   :type: int


   
   Get or set the Not to be defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: n6
   :type: int


   
   Get or set the Not to be defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: rps
   :type: float


   
   Get or set the Relative penalty stiffness (default=1.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: damp
   :type: float


   
   Get or set the Damping scale factor on default damping value (default=1.0).
   LE.0.01 and GT.0.0: no damping is used.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'JOINT_REVOLUTE'






