





:class:`Em2Daxi`
================


.. py:class:: em_2daxi.Em2Daxi(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_2DAXI keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: Em2Daxi

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID of the part to be solved using 2D axisymmetry.
          * - :py:attr:`~ssid`
            - Get or set the Segment Set ID : Segment that will define the 2D cross section of the part where the EM field is solved.
          * - :py:attr:`~starssid`
            - Get or set the Used by the 2D axisymmetric solver to make the connection between two corresponding boundaries on each side of a slice when the model is a slice of the full 360 circle.
          * - :py:attr:`~endssid`
            - Get or set the Used by the 2D axisymmetric solver to make the connection between two corresponding boundaries on each side of a slice when the model is a slice of the full 360 circle.
          * - :py:attr:`~numsec`
            - Get or set the Number of Sectors. This field gives the ratio of the full circle to the angular extension of the mesh. This has to be a power of two.


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

    from em_2daxi import Em2Daxi

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID of the part to be solved using 2D axisymmetry.
















   ..
       !! processed by numpydoc !!

.. py:property:: ssid
   :type: Optional[int]


   
   Get or set the Segment Set ID : Segment that will define the 2D cross section of the part where the EM field is solved.
















   ..
       !! processed by numpydoc !!

.. py:property:: starssid
   :type: Optional[int]


   
   Get or set the Used by the 2D axisymmetric solver to make the connection between two corresponding boundaries on each side of a slice when the model is a slice of the full 360 circle.
















   ..
       !! processed by numpydoc !!

.. py:property:: endssid
   :type: Optional[int]


   
   Get or set the Used by the 2D axisymmetric solver to make the connection between two corresponding boundaries on each side of a slice when the model is a slice of the full 360 circle.
















   ..
       !! processed by numpydoc !!

.. py:property:: numsec
   :type: Optional[int]


   
   Get or set the Number of Sectors. This field gives the ratio of the full circle to the angular extension of the mesh. This has to be a power of two.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: '2DAXI'






