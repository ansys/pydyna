





:class:`BoundaryFluxSet`
========================


.. py:class:: boundary_flux_set.BoundaryFluxSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_FLUX_SET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryFluxSet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ssid`
            - Get or set the Segment set ID, see *SET_SEGMENT.
          * - :py:attr:`~pserod`
            - Get or set the Part set ID for updating boundary segments exposed to the environment as solid elements erode.
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID for heat flux, see *DEFINE_CURVE:
          * - :py:attr:`~mlc1`
            - Get or set the Curve multiplier at node N1.
          * - :py:attr:`~mlc2`
            - Get or set the Curve multiplier at node N2.
          * - :py:attr:`~mlc3`
            - Get or set the Curve multiplier at node N3.
          * - :py:attr:`~mlc4`
            - Get or set the Curve multiplier at node N4.
          * - :py:attr:`~loc`
            - Get or set the Application of surface for thermal shell elements, see paramter, TSHELL, in the *CONTROL_SHELL input:
          * - :py:attr:`~nhisv`
            - Get or set the Number of history variables associated with the flux definition:
          * - :py:attr:`~nhisv1`
            - Get or set the Initial value of history variable 1
          * - :py:attr:`~nhisv2`
            - Get or set the Initial value of history variable 2
          * - :py:attr:`~nhisv3`
            - Get or set the Initial value of history variable 3
          * - :py:attr:`~nhisv4`
            - Get or set the Initial value of history variable 4
          * - :py:attr:`~nhisv5`
            - Get or set the Initial value of history variable 5
          * - :py:attr:`~nhisv6`
            - Get or set the Initial value of history variable 6
          * - :py:attr:`~nhisv7`
            - Get or set the Initial value of history variable 7
          * - :py:attr:`~nhisv8`
            - Get or set the Initial value of history variable 8


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

    from boundary_flux_set import BoundaryFluxSet

Property detail
---------------

.. py:property:: ssid
   :type: Optional[int]


   
   Get or set the Segment set ID, see *SET_SEGMENT.
















   ..
       !! processed by numpydoc !!

.. py:property:: pserod
   :type: Optional[int]


   
   Get or set the Part set ID for updating boundary segments exposed to the environment as solid elements erode.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID for heat flux, see *DEFINE_CURVE:
   GT.0: function versus time,
   EQ.0: use constant multiplier values at nodes,
   LT.0: function versus temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: mlc1
   :type: float


   
   Get or set the Curve multiplier at node N1.
















   ..
       !! processed by numpydoc !!

.. py:property:: mlc2
   :type: float


   
   Get or set the Curve multiplier at node N2.
















   ..
       !! processed by numpydoc !!

.. py:property:: mlc3
   :type: float


   
   Get or set the Curve multiplier at node N3.
















   ..
       !! processed by numpydoc !!

.. py:property:: mlc4
   :type: float


   
   Get or set the Curve multiplier at node N4.
















   ..
       !! processed by numpydoc !!

.. py:property:: loc
   :type: int


   
   Get or set the Application of surface for thermal shell elements, see paramter, TSHELL, in the *CONTROL_SHELL input:
   EQ.-1: lower surface of thermal shell element,
   EQ. 1: upper surface of thermal shell element
















   ..
       !! processed by numpydoc !!

.. py:property:: nhisv
   :type: int


   
   Get or set the Number of history variables associated with the flux definition:
   GT.0: A user defined subroutine will be called to compute the flux
















   ..
       !! processed by numpydoc !!

.. py:property:: nhisv1
   :type: float


   
   Get or set the Initial value of history variable 1
















   ..
       !! processed by numpydoc !!

.. py:property:: nhisv2
   :type: float


   
   Get or set the Initial value of history variable 2
















   ..
       !! processed by numpydoc !!

.. py:property:: nhisv3
   :type: float


   
   Get or set the Initial value of history variable 3
















   ..
       !! processed by numpydoc !!

.. py:property:: nhisv4
   :type: float


   
   Get or set the Initial value of history variable 4
















   ..
       !! processed by numpydoc !!

.. py:property:: nhisv5
   :type: float


   
   Get or set the Initial value of history variable 5
















   ..
       !! processed by numpydoc !!

.. py:property:: nhisv6
   :type: float


   
   Get or set the Initial value of history variable 6
















   ..
       !! processed by numpydoc !!

.. py:property:: nhisv7
   :type: float


   
   Get or set the Initial value of history variable 7
















   ..
       !! processed by numpydoc !!

.. py:property:: nhisv8
   :type: float


   
   Get or set the Initial value of history variable 8
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'FLUX_SET'






