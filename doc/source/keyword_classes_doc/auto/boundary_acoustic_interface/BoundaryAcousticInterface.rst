





:class:`BoundaryAcousticInterface`
==================================


.. py:class:: boundary_acoustic_interface.BoundaryAcousticInterface(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_ACOUSTIC_INTERFACE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryAcousticInterface

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ssid`
            - Get or set the Segment set ID, see *SET_SEGMENT.  This set defines the structural segments being coupled to the acoustic elements.
          * - :py:attr:`~ifid`
            - Get or set the Interface ID from the file baifac.db created by *INTERFACE_ACOUSTIC. It will be used to drive the acoustic fluid boundary.


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

    from boundary_acoustic_interface import BoundaryAcousticInterface

Property detail
---------------

.. py:property:: ssid
   :type: Optional[int]


   
   Get or set the Segment set ID, see *SET_SEGMENT.  This set defines the structural segments being coupled to the acoustic elements.
















   ..
       !! processed by numpydoc !!

.. py:property:: ifid
   :type: Optional[int]


   
   Get or set the Interface ID from the file baifac.db created by *INTERFACE_ACOUSTIC. It will be used to drive the acoustic fluid boundary.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'ACOUSTIC_INTERFACE'






