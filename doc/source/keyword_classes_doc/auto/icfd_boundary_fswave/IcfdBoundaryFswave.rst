





:class:`IcfdBoundaryFswave`
===========================


.. py:class:: icfd_boundary_fswave.IcfdBoundaryFswave(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_BOUNDARY_FSWAVE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdBoundaryFswave

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the PID for a fluid surface.
          * - :py:attr:`~wtype`
            - Get or set the Wave Type:
          * - :py:attr:`~h0`
            - Get or set the Water level (from the bottom of the channel) for the unperturbed condition
          * - :py:attr:`~wamp`
            - Get or set the Wave amplitude or height for WTYPE=1 to WTYPE=4. Significant wave height for WTYPE=5,6,7.
          * - :py:attr:`~wleng`
            - Get or set the Wave Length for WTYPE=1 and WTYPE=2. Wave Period for WTYPE=3. Not used for WTYPE=4. Minimum wave frequency in spectrum (rad/sec) for WTYPE=5,6,7
          * - :py:attr:`~wmax`
            - Get or set the Maximum wave frequency in spectrum (rad/sec) for WTYPE = 5, 6, and 7. Angle between the boundary and the incident waves (in degrees) for WTYPE = 3.
          * - :py:attr:`~sflcid`
            - Get or set the Scale factor LCID on the wave amplitude for WTYPE=1, WTYPE=2 and WTYPE=3. Number of Wave modes (default=1024) for WTYPE=5,6,7
          * - :py:attr:`~wang`
            - Get or set the Angle between incoming wave direction and x-axis for z and y-aligned gravity vector, or angle between incoming wave direction and y-axis for x-aligned gravity vector.).
          * - :py:attr:`~wpeak`
            - Get or set the Peak wave frequency in spectrum [rad/sec] for wtype=7. For wtype=6, WPEAK= 0.4*sqrt(g/Hs)) with g the gravity and Hs the significant wave height


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

    from icfd_boundary_fswave import IcfdBoundaryFswave

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the PID for a fluid surface.
















   ..
       !! processed by numpydoc !!

.. py:property:: wtype
   :type: Optional[int]


   
   Get or set the Wave Type:
   EQ.1:Stokes wave of first order.
   EQ.2:   Stokes wave of second order
   EQ.3:   Stokes wave of fifth order
   EQ.4 : Solitary wave
   EQ.5 : Irregular waves using JONSWAP spectrum
   EQ.6 : Irregular waves using One Parameter Pierson - Moskowitz spectrum
   EQ.7 : Irregular waves using Two Parameter Pierson - Moskowitz spectrum
















   ..
       !! processed by numpydoc !!

.. py:property:: h0
   :type: Optional[float]


   
   Get or set the Water level (from the bottom of the channel) for the unperturbed condition
















   ..
       !! processed by numpydoc !!

.. py:property:: wamp
   :type: Optional[float]


   
   Get or set the Wave amplitude or height for WTYPE=1 to WTYPE=4. Significant wave height for WTYPE=5,6,7.
















   ..
       !! processed by numpydoc !!

.. py:property:: wleng
   :type: Optional[float]


   
   Get or set the Wave Length for WTYPE=1 and WTYPE=2. Wave Period for WTYPE=3. Not used for WTYPE=4. Minimum wave frequency in spectrum (rad/sec) for WTYPE=5,6,7
















   ..
       !! processed by numpydoc !!

.. py:property:: wmax
   :type: Optional[float]


   
   Get or set the Maximum wave frequency in spectrum (rad/sec) for WTYPE = 5, 6, and 7. Angle between the boundary and the incident waves (in degrees) for WTYPE = 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: sflcid
   :type: Optional[int]


   
   Get or set the Scale factor LCID on the wave amplitude for WTYPE=1, WTYPE=2 and WTYPE=3. Number of Wave modes (default=1024) for WTYPE=5,6,7
















   ..
       !! processed by numpydoc !!

.. py:property:: wang
   :type: Optional[float]


   
   Get or set the Angle between incoming wave direction and x-axis for z and y-aligned gravity vector, or angle between incoming wave direction and y-axis for x-aligned gravity vector.).
















   ..
       !! processed by numpydoc !!

.. py:property:: wpeak
   :type: Optional[float]


   
   Get or set the Peak wave frequency in spectrum [rad/sec] for wtype=7. For wtype=6, WPEAK= 0.4*sqrt(g/Hs)) with g the gravity and Hs the significant wave height
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'BOUNDARY_FSWAVE'






