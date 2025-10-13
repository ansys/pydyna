





:class:`BoundaryAcousticPressureSpectral`
=========================================


.. py:class:: boundary_acoustic_pressure_spectral.BoundaryAcousticPressureSpectral(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_ACOUSTIC_PRESSURE_SPECTRAL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryAcousticPressureSpectral

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ssid`
            - Get or set the Segment set ID for the fluid boundary faces.
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID to specify pressure as a function of time.
          * - :py:attr:`~sf`
            - Get or set the Load curve scale factor.
          * - :py:attr:`~tdeath`
            - Get or set the Time when the pressure boundary condition should no longer be applied.


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

    from boundary_acoustic_pressure_spectral import BoundaryAcousticPressureSpectral

Property detail
---------------

.. py:property:: ssid
   :type: Optional[int]


   
   Get or set the Segment set ID for the fluid boundary faces.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID to specify pressure as a function of time.
















   ..
       !! processed by numpydoc !!

.. py:property:: sf
   :type: float


   
   Get or set the Load curve scale factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: tdeath
   :type: float


   
   Get or set the Time when the pressure boundary condition should no longer be applied.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'ACOUSTIC_PRESSURE_SPECTRAL'






