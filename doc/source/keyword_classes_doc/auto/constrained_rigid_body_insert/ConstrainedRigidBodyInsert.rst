





:class:`ConstrainedRigidBodyInsert`
===================================


.. py:class:: constrained_rigid_body_insert.ConstrainedRigidBodyInsert(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_RIGID_BODY_INSERT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedRigidBodyInsert

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Insert ID.
          * - :py:attr:`~pidl`
            - Get or set the Lead (die) rigid body part ID, see *PART.
          * - :py:attr:`~pidc`
            - Get or set the Constraned (die insert) rigid body part ID, see *PART.
          * - :py:attr:`~coordid`
            - Get or set the Coordinate ID. The x direction is the direction the insert moves independently of the die.
          * - :py:attr:`~idir`
            - Get or set the Direction in which the insert moves independently of the die:
          * - :py:attr:`~mflag`
            - Get or set the Motion flag.
          * - :py:attr:`~mcid`
            - Get or set the Curve defining the motion of the die insert relative to the die.
          * - :py:attr:`~deathm`
            - Get or set the Death time of the imposed motion. If it is equal to 0.0, the motion is imposed for the entire analysis.
          * - :py:attr:`~partb`
            - Get or set the Part ID for a discrete beam connected between the insert and die.
          * - :py:attr:`~deathb`
            - Get or set the Death time for the discrete beam specified by BPART.


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

    from constrained_rigid_body_insert import ConstrainedRigidBodyInsert

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Insert ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: pidl
   :type: Optional[int]


   
   Get or set the Lead (die) rigid body part ID, see *PART.
















   ..
       !! processed by numpydoc !!

.. py:property:: pidc
   :type: Optional[int]


   
   Get or set the Constraned (die insert) rigid body part ID, see *PART.
















   ..
       !! processed by numpydoc !!

.. py:property:: coordid
   :type: Optional[int]


   
   Get or set the Coordinate ID. The x direction is the direction the insert moves independently of the die.
















   ..
       !! processed by numpydoc !!

.. py:property:: idir
   :type: int


   
   Get or set the Direction in which the insert moves independently of the die:
   EQ.1:   Local x - direction
   EQ.2 : Local y - direction
   EQ.3 : Local z - direction(default)
















   ..
       !! processed by numpydoc !!

.. py:property:: mflag
   :type: int


   
   Get or set the Motion flag.
   EQ.0:   Relative motion is unconstrained.
   EQ.1:   The displacement of the insert relative to the die is imposed.
   EQ.2:   The velocity of the insert relative to the die is imposed.
   EQ.3:   The acceleration of the insert relative to the die is imposed..
















   ..
       !! processed by numpydoc !!

.. py:property:: mcid
   :type: Optional[int]


   
   Get or set the Curve defining the motion of the die insert relative to the die.
















   ..
       !! processed by numpydoc !!

.. py:property:: deathm
   :type: float


   
   Get or set the Death time of the imposed motion. If it is equal to 0.0, the motion is imposed for the entire analysis.
















   ..
       !! processed by numpydoc !!

.. py:property:: partb
   :type: Optional[int]


   
   Get or set the Part ID for a discrete beam connected between the insert and die.
















   ..
       !! processed by numpydoc !!

.. py:property:: deathb
   :type: float


   
   Get or set the Death time for the discrete beam specified by BPART.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'RIGID_BODY_INSERT'






