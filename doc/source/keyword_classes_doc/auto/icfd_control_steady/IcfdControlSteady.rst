





:class:`IcfdControlSteady`
==========================


.. py:class:: icfd_control_steady.IcfdControlSteady(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_CONTROL_STEADY keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdControlSteady

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~its`
            - Get or set the Maximum number of iterations to reach convergence.
          * - :py:attr:`~tol1`
            - Get or set the Tolerance limits for the momentum pressure and temperature equations respectfully.
          * - :py:attr:`~tol2`
            - Get or set the Tolerance limits for the momentum pressure and temperature equations respectfully.
          * - :py:attr:`~tol3`
            - Get or set the Tolerance limits for the momentum pressure and temperature equations respectfully.
          * - :py:attr:`~rel1`
            - Get or set the Relaxation parameters for the velocity and pressure respectfully. Decreasing those values may add stability but more iterations may be needed to reach convergence.
          * - :py:attr:`~rel2`
            - Get or set the Relaxation parameters for the velocity and pressure respectfully. Decreasing those values may add stability but more iterations may be needed to reach convergence.
          * - :py:attr:`~urel`
            - Get or set the Under relaxation parameter. Lowering this value may improve the final accuracy of the solution but more iterations may be needed to achieve convergence.
          * - :py:attr:`~order`
            - Get or set the Analysis order :


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

    from icfd_control_steady import IcfdControlSteady

Property detail
---------------

.. py:property:: its
   :type: int


   
   Get or set the Maximum number of iterations to reach convergence.
















   ..
       !! processed by numpydoc !!

.. py:property:: tol1
   :type: float


   
   Get or set the Tolerance limits for the momentum pressure and temperature equations respectfully.
















   ..
       !! processed by numpydoc !!

.. py:property:: tol2
   :type: float


   
   Get or set the Tolerance limits for the momentum pressure and temperature equations respectfully.
















   ..
       !! processed by numpydoc !!

.. py:property:: tol3
   :type: float


   
   Get or set the Tolerance limits for the momentum pressure and temperature equations respectfully.
















   ..
       !! processed by numpydoc !!

.. py:property:: rel1
   :type: float


   
   Get or set the Relaxation parameters for the velocity and pressure respectfully. Decreasing those values may add stability but more iterations may be needed to reach convergence.
















   ..
       !! processed by numpydoc !!

.. py:property:: rel2
   :type: float


   
   Get or set the Relaxation parameters for the velocity and pressure respectfully. Decreasing those values may add stability but more iterations may be needed to reach convergence.
















   ..
       !! processed by numpydoc !!

.. py:property:: urel
   :type: float


   
   Get or set the Under relaxation parameter. Lowering this value may improve the final accuracy of the solution but more iterations may be needed to achieve convergence.
















   ..
       !! processed by numpydoc !!

.. py:property:: order
   :type: int


   
   Get or set the Analysis order :
   EQ.0:   Second order. More accurate but more time consuming.
   EQ.1:   First order: More stable and faster but may be less accurate.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'CONTROL_STEADY'






