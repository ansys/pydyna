





:class:`IcfdDefineNoninertial`
==============================


.. py:class:: icfd_define_noninertial.IcfdDefineNoninertial(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_DEFINE_NONINERTIAL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdDefineNoninertial

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~w1`
            - Get or set the Rotational Velocity along the X,Y,Z axes
          * - :py:attr:`~w2`
            - Get or set the Rotational Velocity along the X,Y,Z axes
          * - :py:attr:`~w3`
            - Get or set the Rotational Velocity along the X,Y,Z axes
          * - :py:attr:`~r`
            - Get or set the Radius of the rotating reference frame
          * - :py:attr:`~ptid`
            - Get or set the Starting point ID for the reference frame (See *ICFD_DEFINE_POINT)
          * - :py:attr:`~l`
            - Get or set the Length of the rotating reference frame.
          * - :py:attr:`~lcid`
            - Get or set the Load curve for scaling factor of w.
          * - :py:attr:`~relv`
            - Get or set the Velocities computed and displayed:


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

    from icfd_define_noninertial import IcfdDefineNoninertial

Property detail
---------------

.. py:property:: w1
   :type: Optional[float]


   
   Get or set the Rotational Velocity along the X,Y,Z axes
















   ..
       !! processed by numpydoc !!

.. py:property:: w2
   :type: Optional[float]


   
   Get or set the Rotational Velocity along the X,Y,Z axes
















   ..
       !! processed by numpydoc !!

.. py:property:: w3
   :type: Optional[float]


   
   Get or set the Rotational Velocity along the X,Y,Z axes
















   ..
       !! processed by numpydoc !!

.. py:property:: r
   :type: Optional[float]


   
   Get or set the Radius of the rotating reference frame
















   ..
       !! processed by numpydoc !!

.. py:property:: ptid
   :type: Optional[int]


   
   Get or set the Starting point ID for the reference frame (See *ICFD_DEFINE_POINT)
















   ..
       !! processed by numpydoc !!

.. py:property:: l
   :type: Optional[float]


   
   Get or set the Length of the rotating reference frame.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve for scaling factor of w.
















   ..
       !! processed by numpydoc !!

.. py:property:: relv
   :type: int


   
   Get or set the Velocities computed and displayed:
   EQ.0: Relative velocity, only the non-rotating components of the velocity are used and displayed.
   EQ.1: Absolute velocity . All the components of the velocity are used. Useful in cases where several or at least one noninertial reference frame is combined with an inertial "classic" reference frame.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'DEFINE_NONINERTIAL'






