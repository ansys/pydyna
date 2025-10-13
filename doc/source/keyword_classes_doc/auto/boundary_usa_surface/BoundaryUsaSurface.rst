





:class:`BoundaryUsaSurface`
===========================


.. py:class:: boundary_usa_surface.BoundaryUsaSurface(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_USA_SURFACE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryUsaSurface

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ssid`
            - Get or set the Segment set ID, see *SET_SEGMENT.
          * - :py:attr:`~wetdry`
            - Get or set the Wet surface flag:
          * - :py:attr:`~nbeam`
            - Get or set the The number of nodes touched by USA Surface-of-Revolution (SOR) elements. It is not necessary that the LS-DYNA model has beams where USA has beams (i.e., SOR elements), merely that the LS-DYNA model has nodes to receive the forces that USA will return.


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

    from boundary_usa_surface import BoundaryUsaSurface

Property detail
---------------

.. py:property:: ssid
   :type: Optional[int]


   
   Get or set the Segment set ID, see *SET_SEGMENT.
















   ..
       !! processed by numpydoc !!

.. py:property:: wetdry
   :type: int


   
   Get or set the Wet surface flag:
   EQ.0: dry, no coupling (default),
   EQ.1: wet, coupled with USA.
















   ..
       !! processed by numpydoc !!

.. py:property:: nbeam
   :type: int


   
   Get or set the The number of nodes touched by USA Surface-of-Revolution (SOR) elements. It is not necessary that the LS-DYNA model has beams where USA has beams (i.e., SOR elements), merely that the LS-DYNA model has nodes to receive the forces that USA will return.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'USA_SURFACE'






