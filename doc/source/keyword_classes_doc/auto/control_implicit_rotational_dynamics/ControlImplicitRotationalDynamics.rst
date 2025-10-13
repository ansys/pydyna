





:class:`ControlImplicitRotationalDynamics`
==========================================


.. py:class:: control_implicit_rotational_dynamics.ControlImplicitRotationalDynamics(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_IMPLICIT_ROTATIONAL_DYNAMICS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlImplicitRotationalDynamics

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the Set ID of the rotational components.
          * - :py:attr:`~stype`
            - Get or set the Set type:
          * - :py:attr:`~omega`
            - Get or set the Rotating speed.
          * - :py:attr:`~vid`
            - Get or set the Vector ID to define the rotating axis. It is defined in *DEFINE_VECTOR, and the tail of the vector should be set as the rotating center.
          * - :py:attr:`~nomeg`
            - Get or set the Number of rotating speeds. This feature is intended to automatically preform parameter studies with respect to the rotation speed. The keyword *CONTROL_IMPLICIT_EIGENVALUE must be included if NOMEG>0.
          * - :py:attr:`~iref`
            - Get or set the Reference frame:
          * - :py:attr:`~omegadr`
            - Get or set the Rotating speed defined in dynamic relaxation.
          * - :py:attr:`~omeg1`
            - Get or set the The 1th rotating speed.
          * - :py:attr:`~omeg2`
            - Get or set the The 2th rotating speed.
          * - :py:attr:`~omeg3`
            - Get or set the The 3th rotating speed.
          * - :py:attr:`~omeg4`
            - Get or set the The 4th rotating speed.
          * - :py:attr:`~omeg5`
            - Get or set the The 5th rotating speed.
          * - :py:attr:`~omeg6`
            - Get or set the The 6th rotating speed.
          * - :py:attr:`~omeg7`
            - Get or set the The 7th rotating speed.
          * - :py:attr:`~omeg8`
            - Get or set the The 8th rotating speed.


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

    from control_implicit_rotational_dynamics import ControlImplicitRotationalDynamics

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Set ID of the rotational components.
















   ..
       !! processed by numpydoc !!

.. py:property:: stype
   :type: int


   
   Get or set the Set type:
   EQ.0:   Part;
   EQ.1:   Part set.
















   ..
       !! processed by numpydoc !!

.. py:property:: omega
   :type: Optional[float]


   
   Get or set the Rotating speed.
   LT.0:   curve ID = (-OMEGA) gives rotating speed as a function of time.
















   ..
       !! processed by numpydoc !!

.. py:property:: vid
   :type: Optional[int]


   
   Get or set the Vector ID to define the rotating axis. It is defined in *DEFINE_VECTOR, and the tail of the vector should be set as the rotating center.
















   ..
       !! processed by numpydoc !!

.. py:property:: nomeg
   :type: int


   
   Get or set the Number of rotating speeds. This feature is intended to automatically preform parameter studies with respect to the rotation speed. The keyword *CONTROL_IMPLICIT_EIGENVALUE must be included if NOMEG>0.
















   ..
       !! processed by numpydoc !!

.. py:property:: iref
   :type: int


   
   Get or set the Reference frame:
   EQ.0:   Rotating coordinate system;
   EQ.1:   Fixed coordinate system.
   EQ.2:   Rotating coordinate system, but rotate rotating parts for visualization purpose.
















   ..
       !! processed by numpydoc !!

.. py:property:: omegadr
   :type: Optional[float]


   
   Get or set the Rotating speed defined in dynamic relaxation.
   GT.0: rotating speed defined in dynamic relaxation.
   LT.0:curve ID = (-OMEGA) gives rotating speed as a function of time.
















   ..
       !! processed by numpydoc !!

.. py:property:: omeg1
   :type: Optional[float]


   
   Get or set the The 1th rotating speed.
















   ..
       !! processed by numpydoc !!

.. py:property:: omeg2
   :type: Optional[float]


   
   Get or set the The 2th rotating speed.
















   ..
       !! processed by numpydoc !!

.. py:property:: omeg3
   :type: Optional[float]


   
   Get or set the The 3th rotating speed.
















   ..
       !! processed by numpydoc !!

.. py:property:: omeg4
   :type: Optional[float]


   
   Get or set the The 4th rotating speed.
















   ..
       !! processed by numpydoc !!

.. py:property:: omeg5
   :type: Optional[float]


   
   Get or set the The 5th rotating speed.
















   ..
       !! processed by numpydoc !!

.. py:property:: omeg6
   :type: Optional[float]


   
   Get or set the The 6th rotating speed.
















   ..
       !! processed by numpydoc !!

.. py:property:: omeg7
   :type: Optional[float]


   
   Get or set the The 7th rotating speed.
















   ..
       !! processed by numpydoc !!

.. py:property:: omeg8
   :type: Optional[float]


   
   Get or set the The 8th rotating speed.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'IMPLICIT_ROTATIONAL_DYNAMICS'






