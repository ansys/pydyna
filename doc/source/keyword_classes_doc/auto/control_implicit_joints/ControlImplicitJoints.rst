





:class:`ControlImplicitJoints`
==============================


.. py:class:: control_implicit_joints.ControlImplicitJoints(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_IMPLICIT_JOINTS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlImplicitJoints

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ispher`
            - Get or set the Treatment of spherical joints
          * - :py:attr:`~irevol`
            - Get or set the Treatment of revolute joints
          * - :py:attr:`~icylin`
            - Get or set the Treatment of cylindrical joints.


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

    from control_implicit_joints import ControlImplicitJoints

Property detail
---------------

.. py:property:: ispher
   :type: int


   
   Get or set the Treatment of spherical joints
   EQ.1: use constraint method for all spherical joints (default)
   EQ.2: use penalty method for all spherical joints
















   ..
       !! processed by numpydoc !!

.. py:property:: irevol
   :type: int


   
   Get or set the Treatment of revolute joints
   EQ.1: use constraint method for all revolute joints (default)
   EQ.2: use penalty method for all revolute joints
















   ..
       !! processed by numpydoc !!

.. py:property:: icylin
   :type: int


   
   Get or set the Treatment of cylindrical joints.
   EQ.1: use constraint method for all cylindrical joints (default)
   EQ.2: use penalty method for all cylindrical joints
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'IMPLICIT_JOINTS'






