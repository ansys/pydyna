





:class:`IcfdInitial`
====================


.. py:class:: icfd_initial.IcfdInitial(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_INITIAL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdInitial

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID for the volume elements or the surface elements where the values are initialized (see *ICFD_PART_VOL and *ICFD_PART).PID = 0 to assign the initial condition to all nodes at once.
          * - :py:attr:`~vx`
            - Get or set the x coordinate for the velocity.
          * - :py:attr:`~vy`
            - Get or set the y coordinate for the velocity.
          * - :py:attr:`~vz`
            - Get or set the z coordinate for the velocity.
          * - :py:attr:`~t`
            - Get or set the Initial temperature.
          * - :py:attr:`~p`
            - Get or set the Initial Pressure.
          * - :py:attr:`~dfunc`
            - Get or set the Option to define initial conditions using *DEFINE_FUNCTION


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

    from icfd_initial import IcfdInitial

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID for the volume elements or the surface elements where the values are initialized (see *ICFD_PART_VOL and *ICFD_PART).PID = 0 to assign the initial condition to all nodes at once.
















   ..
       !! processed by numpydoc !!

.. py:property:: vx
   :type: Optional[float]


   
   Get or set the x coordinate for the velocity.
















   ..
       !! processed by numpydoc !!

.. py:property:: vy
   :type: Optional[float]


   
   Get or set the y coordinate for the velocity.
















   ..
       !! processed by numpydoc !!

.. py:property:: vz
   :type: Optional[float]


   
   Get or set the z coordinate for the velocity.
















   ..
       !! processed by numpydoc !!

.. py:property:: t
   :type: Optional[float]


   
   Get or set the Initial temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: p
   :type: Optional[float]


   
   Get or set the Initial Pressure.
















   ..
       !! processed by numpydoc !!

.. py:property:: dfunc
   :type: int


   
   Get or set the Option to define initial conditions using *DEFINE_FUNCTION
   EQ.0:   Turned off.
   EQ.1:   Turned on. All previous flags for initial velocity, pressure and temperature now refer to *DEFINE_FUNCTION IDs. The following parameters are allowed : f(x,y,z), allowing to define initial profiles function of coordinates.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'INITIAL'






