





:class:`FrequencyDomainAcousticIncidentWave`
============================================


.. py:class:: frequency_domain_acoustic_incident_wave.FrequencyDomainAcousticIncidentWave(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA FREQUENCY_DOMAIN_ACOUSTIC_INCIDENT_WAVE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: FrequencyDomainAcousticIncidentWave

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~type`
            - Get or set the Type of incident sound wave:
          * - :py:attr:`~mag`
            - Get or set the Magnitude of the incident sound wave.
          * - :py:attr:`~xc`
            - Get or set the Direction cosines for the place wave (TYPE=1), or coordinates of the point source for the spherical wave (TYPE=2).
          * - :py:attr:`~yc`
            - Get or set the Direction cosines for the place wave (TYPE=1), or coordinates of the point source for the spherical wave (TYPE=2).
          * - :py:attr:`~zc`
            - Get or set the Direction cosines for the place wave (TYPE=1), or coordinates of the point source for the spherical wave (TYPE=2).


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

    from frequency_domain_acoustic_incident_wave import FrequencyDomainAcousticIncidentWave

Property detail
---------------

.. py:property:: type
   :type: int


   
   Get or set the Type of incident sound wave:
   EQ.1: plane wave.
   EQ.2: spherical wave.
















   ..
       !! processed by numpydoc !!

.. py:property:: mag
   :type: Optional[float]


   
   Get or set the Magnitude of the incident sound wave.
















   ..
       !! processed by numpydoc !!

.. py:property:: xc
   :type: Optional[float]


   
   Get or set the Direction cosines for the place wave (TYPE=1), or coordinates of the point source for the spherical wave (TYPE=2).
















   ..
       !! processed by numpydoc !!

.. py:property:: yc
   :type: Optional[float]


   
   Get or set the Direction cosines for the place wave (TYPE=1), or coordinates of the point source for the spherical wave (TYPE=2).
















   ..
       !! processed by numpydoc !!

.. py:property:: zc
   :type: Optional[float]


   
   Get or set the Direction cosines for the place wave (TYPE=1), or coordinates of the point source for the spherical wave (TYPE=2).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'FREQUENCY'


.. py:attribute:: subkeyword
   :value: 'DOMAIN_ACOUSTIC_INCIDENT_WAVE'






