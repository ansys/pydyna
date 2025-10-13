





:class:`BoundaryAcousticPrescribedMotion`
=========================================


.. py:class:: boundary_acoustic_prescribed_motion.BoundaryAcousticPrescribedMotion(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_ACOUSTIC_PRESCRIBED_MOTION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryAcousticPrescribedMotion

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ssid`
            - Get or set the Segment set ID for the fluid boundary faces.
          * - :py:attr:`~vad`
            - Get or set the Velocity/acceleration/displacement flag (see Remark 2):
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID to describe motion as a function of time or frequency (see Remarks 1 and 2).
          * - :py:attr:`~sf`
            - Get or set the Load curve scale factor.


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

    from boundary_acoustic_prescribed_motion import BoundaryAcousticPrescribedMotion

Property detail
---------------

.. py:property:: ssid
   :type: Optional[int]


   
   Get or set the Segment set ID for the fluid boundary faces.
















   ..
       !! processed by numpydoc !!

.. py:property:: vad
   :type: Optional[int]


   
   Get or set the Velocity/acceleration/displacement flag (see Remark 2):
   EQ.0:   Velocity
   EQ.1 : Acceleration
   EQ.2 : Displacement.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID to describe motion as a function of time or frequency (see Remarks 1 and 2).
















   ..
       !! processed by numpydoc !!

.. py:property:: sf
   :type: float


   
   Get or set the Load curve scale factor.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'ACOUSTIC_PRESCRIBED_MOTION'






