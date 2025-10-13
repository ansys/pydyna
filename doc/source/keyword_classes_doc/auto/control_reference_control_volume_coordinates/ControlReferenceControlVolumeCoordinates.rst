





:class:`ControlReferenceControlVolumeCoordinates`
=================================================


.. py:class:: control_reference_control_volume_coordinates.ControlReferenceControlVolumeCoordinates(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_REFERENCE_CONTROL_VOLUME_COORDINATES keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlReferenceControlVolumeCoordinates

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~filename`
            - Get or set the The file name contains the *INITIAL_FOAM_REFERENCE_GEOMETRY, *INITIAL_STRESS_SOLID, and *INITIAL_STRESS_SHELL to be read or written.
          * - :py:attr:`~opt`
            - Get or set the OPT = 0 writes the reference geometry to the specified file.
          * - :py:attr:`~psid`
            - Get or set the the part set defining the elements to be read or written.


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

    from control_reference_control_volume_coordinates import ControlReferenceControlVolumeCoordinates

Property detail
---------------

.. py:property:: filename
   :type: Optional[str]


   
   Get or set the The file name contains the *INITIAL_FOAM_REFERENCE_GEOMETRY, *INITIAL_STRESS_SOLID, and *INITIAL_STRESS_SHELL to be read or written.
















   ..
       !! processed by numpydoc !!

.. py:property:: opt
   :type: int


   
   Get or set the OPT = 0 writes the reference geometry to the specified file.
   OPT = 1 reads the reference geometry from the specified file.
   OPT = 2 skips the pressure initialization.
















   ..
       !! processed by numpydoc !!

.. py:property:: psid
   :type: Optional[int]


   
   Get or set the the part set defining the elements to be read or written.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'REFERENCE_CONTROL_VOLUME_COORDINATES'






