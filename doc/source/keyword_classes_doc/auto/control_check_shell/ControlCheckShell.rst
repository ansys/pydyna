





:class:`ControlCheckShell`
==========================


.. py:class:: control_check_shell.ControlCheckShell(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_CHECK_SHELL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlCheckShell

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID to be checked:
          * - :py:attr:`~ifauto`
            - Get or set the Flag to automatically correct bad elements:
          * - :py:attr:`~convex`
            - Get or set the Check element convexity (internal angles less than 180 degrees)
          * - :py:attr:`~adpt`
            - Get or set the Check adaptive constraints
          * - :py:attr:`~aratio`
            - Get or set the Minimum allowable aspect ratio. Elements which do not meet minimum aspect ratio test will be treated according to IFAUTO above
          * - :py:attr:`~angke`
            - Get or set the Maximum allowable internal angle. Elements which fail this test will be treated according to IFAUTO above.
          * - :py:attr:`~smin`
            - Get or set the Minimum element size. Elements which fail this test will be treated according to IFAUTO above


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

    from control_check_shell import ControlCheckShell

Property detail
---------------

.. py:property:: pid
   :type: int


   
   Get or set the Part ID to be checked:
   EQ.0: Do not check
   GT.0: Part ID
   LT.0: Part set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: ifauto
   :type: int


   
   Get or set the Flag to automatically correct bad elements:
   EQ.0: Write warning message only
   EQ.1 Fix bad element, write message.
















   ..
       !! processed by numpydoc !!

.. py:property:: convex
   :type: int


   
   Get or set the Check element convexity (internal angles less than 180 degrees)
   EQ.0: Do not check
   EQ.1: Check.
















   ..
       !! processed by numpydoc !!

.. py:property:: adpt
   :type: int


   
   Get or set the Check adaptive constraints
   EQ.0: Do not check
   EQ.1: Check
















   ..
       !! processed by numpydoc !!

.. py:property:: aratio
   :type: float


   
   Get or set the Minimum allowable aspect ratio. Elements which do not meet minimum aspect ratio test will be treated according to IFAUTO above
















   ..
       !! processed by numpydoc !!

.. py:property:: angke
   :type: float


   
   Get or set the Maximum allowable internal angle. Elements which fail this test will be treated according to IFAUTO above.
















   ..
       !! processed by numpydoc !!

.. py:property:: smin
   :type: float


   
   Get or set the Minimum element size. Elements which fail this test will be treated according to IFAUTO above
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'CHECK_SHELL'






