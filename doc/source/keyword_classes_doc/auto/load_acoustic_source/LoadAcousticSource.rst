





:class:`LoadAcousticSource`
===========================


.. py:class:: load_acoustic_source.LoadAcousticSource(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_ACOUSTIC_SOURCE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadAcousticSource

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nid_ssid`
            - Get or set the Node ID of the acoustic point source for SRCTYP = 1 and 5.
          * - :py:attr:`~srctyp`
            - Get or set the Acoustic source type:
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID of curve specifying the variation of the load with frequency or time. If LCID is undefined, then the loading is constant.
          * - :py:attr:`~data1`
            - Get or set the SRCTYP.EQ.1:     Magnitude of the harmonic nodal source strength
          * - :py:attr:`~data2`
            - Get or set the SRCTYP.EQ.1:     Phase angle of the nodal source strength in radians
          * - :py:attr:`~data3`
            - Get or set the SRCTYP.EQ.11:    Direction cosine
          * - :py:attr:`~data4`
            - Get or set the SRCTYP.EQ.11:    Direction cosine
          * - :py:attr:`~data5`
            - Get or set the For SRCTYP = 12, reference radius, where the pressure equals .


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

    from load_acoustic_source import LoadAcousticSource

Property detail
---------------

.. py:property:: nid_ssid
   :type: Optional[int]


   
   Get or set the Node ID of the acoustic point source for SRCTYP = 1 and 5.
   Segment set ID of structural faces on the external fluid-structure boundary exposed to the acoustic wave source for SRCTYP = 11 and 12
















   ..
       !! processed by numpydoc !!

.. py:property:: srctyp
   :type: int


   
   Get or set the Acoustic source type:
   EQ.1:   Harmonic nodal point source.DATA1, DATA2,and LCID define the harmonic nodal source strength Q;
   the phase angle of the nodal source strengthand frequency variation for a point source at node NID.
   EQ.5 : Transient nodal point source.DATA1 and LCID define the transient nodal source strength(Q) and temporal variation for a point source at node NID.
   EQ.11 : Harmonic plane wave : DATA1 and LCID define the magnitude and frequency variation for a harmonic plane wave with direction cosines given in
   DATA2, DATA3,and DATA4.SSID is the segment set ID of the external structural(coupled) surface.
   EQ.12 : Harmonic spherical wave.DATA1, DATA5,and LCID define the magnitude, reference radiusand frequency variation for
   a harmonic spherical wave centered at coordinates x_0, y_0,and z_0 specified with DATA2, DATA3,and DATA4.SSID is the segment set ID of the external structural(coupled) surface
   .
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID of curve specifying the variation of the load with frequency or time. If LCID is undefined, then the loading is constant.
















   ..
       !! processed by numpydoc !!

.. py:property:: data1
   :type: float


   
   Get or set the SRCTYP.EQ.1:     Magnitude of the harmonic nodal source strength
   SRCTYP.EQ.5:    Magnitude of the transient nodal source strength
   SRCTYP.EQ.11:   Pressure of the harmonic plane wave
   SRCTYP.EQ.12:   Pressure of the harmonic spherical wave.
















   ..
       !! processed by numpydoc !!

.. py:property:: data2
   :type: float


   
   Get or set the SRCTYP.EQ.1:     Phase angle of the nodal source strength in radians
   SRCTYP.EQ.11:   Direction cosine
   SRCTYP.EQ.12:   x coordinate of center of spherical wave.
















   ..
       !! processed by numpydoc !!

.. py:property:: data3
   :type: float


   
   Get or set the SRCTYP.EQ.11:    Direction cosine
   SRCTYP.EQ.12:   y coordinate of center of spherical wave.
















   ..
       !! processed by numpydoc !!

.. py:property:: data4
   :type: float


   
   Get or set the SRCTYP.EQ.11:    Direction cosine
   SRCTYP.EQ.12:   z coordinate of center of spherical wave.
















   ..
       !! processed by numpydoc !!

.. py:property:: data5
   :type: float


   
   Get or set the For SRCTYP = 12, reference radius, where the pressure equals .
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'ACOUSTIC_SOURCE'






