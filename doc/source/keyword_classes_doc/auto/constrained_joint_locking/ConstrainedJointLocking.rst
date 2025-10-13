





:class:`ConstrainedJointLocking`
================================


.. py:class:: constrained_joint_locking.ConstrainedJointLocking(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_JOINT_LOCKING keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedJointLocking

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

    from constrained_joint_locking import ConstrainedJointLocking

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



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'JOINT_LOCKING'






