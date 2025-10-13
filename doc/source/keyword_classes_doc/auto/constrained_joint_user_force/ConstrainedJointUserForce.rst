





:class:`ConstrainedJointUserForce`
==================================


.. py:class:: constrained_joint_user_force.ConstrainedJointUserForce(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_JOINT_USER_FORCE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedJointUserForce

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~fid`
            - Get or set the Joint user force ID.
          * - :py:attr:`~jid`
            - Get or set the Joint ID for which this user force input applies.
          * - :py:attr:`~nhisv`
            - Get or set the Number of history variables required for this definition.  An array NHISV long is allocated and passed into the user subroutine.  This array is updated in the user subroutine.
          * - :py:attr:`~const1`
            - Get or set the A constant which is passed into the user subroutine.
          * - :py:attr:`~const2`
            - Get or set the A constant which is passed into the user subroutine.
          * - :py:attr:`~const3`
            - Get or set the A constant which is passed into the user subroutine.
          * - :py:attr:`~const4`
            - Get or set the A constant which is passed into the user subroutine.
          * - :py:attr:`~const5`
            - Get or set the A constant which is passed into the user subroutine.
          * - :py:attr:`~const6`
            - Get or set the A constant which is passed into the user subroutine.
          * - :py:attr:`~const7`
            - Get or set the A constant which is passed into the user subroutine.
          * - :py:attr:`~const8`
            - Get or set the A constant which is passed into the user subroutine.


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

    from constrained_joint_user_force import ConstrainedJointUserForce

Property detail
---------------

.. py:property:: fid
   :type: Optional[int]


   
   Get or set the Joint user force ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: jid
   :type: Optional[int]


   
   Get or set the Joint ID for which this user force input applies.
















   ..
       !! processed by numpydoc !!

.. py:property:: nhisv
   :type: int


   
   Get or set the Number of history variables required for this definition.  An array NHISV long is allocated and passed into the user subroutine.  This array is updated in the user subroutine.
















   ..
       !! processed by numpydoc !!

.. py:property:: const1
   :type: Optional[int]


   
   Get or set the A constant which is passed into the user subroutine.
















   ..
       !! processed by numpydoc !!

.. py:property:: const2
   :type: Optional[int]


   
   Get or set the A constant which is passed into the user subroutine.
















   ..
       !! processed by numpydoc !!

.. py:property:: const3
   :type: Optional[int]


   
   Get or set the A constant which is passed into the user subroutine.
















   ..
       !! processed by numpydoc !!

.. py:property:: const4
   :type: Optional[int]


   
   Get or set the A constant which is passed into the user subroutine.
















   ..
       !! processed by numpydoc !!

.. py:property:: const5
   :type: Optional[int]


   
   Get or set the A constant which is passed into the user subroutine.
















   ..
       !! processed by numpydoc !!

.. py:property:: const6
   :type: Optional[int]


   
   Get or set the A constant which is passed into the user subroutine.
















   ..
       !! processed by numpydoc !!

.. py:property:: const7
   :type: Optional[int]


   
   Get or set the A constant which is passed into the user subroutine.
















   ..
       !! processed by numpydoc !!

.. py:property:: const8
   :type: Optional[int]


   
   Get or set the A constant which is passed into the user subroutine.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'JOINT_USER_FORCE'






