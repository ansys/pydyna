





:class:`BoundaryAcousticNonReflecting`
======================================


.. py:class:: boundary_acoustic_non_reflecting.BoundaryAcousticNonReflecting(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_ACOUSTIC_NON_REFLECTING keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryAcousticNonReflecting

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ssid`
            - Get or set the Segment set ID of an acoustic surface.
          * - :py:attr:`~nrbtyp`
            - Get or set the Absorbing boundary type:
          * - :py:attr:`~crvopt`
            - Get or set the Curvature specification option for NRBTYP = 2:
          * - :py:attr:`~data1`
            - Get or set the CRVOPT.EQ.1:     Average curvature, 1/R
          * - :py:attr:`~data2`
            - Get or set the Coordinate Yc for CRVOPT = 2.
          * - :py:attr:`~data3`
            - Get or set the Coordinate Zc for CRVOPT = 2.


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

    from boundary_acoustic_non_reflecting import BoundaryAcousticNonReflecting

Property detail
---------------

.. py:property:: ssid
   :type: Optional[int]


   
   Get or set the Segment set ID of an acoustic surface.
















   ..
       !! processed by numpydoc !!

.. py:property:: nrbtyp
   :type: Optional[int]


   
   Get or set the Absorbing boundary type:
   EQ.1:   Plane wave absorbing boundary
   EQ.2 : Curved wave absorbing boundary(see CRVOPT).
















   ..
       !! processed by numpydoc !!

.. py:property:: crvopt
   :type: Optional[int]


   
   Get or set the Curvature specification option for NRBTYP = 2:
   EQ.1:   Provide average curvature, 1/R
   EQ.2 : Provide coordinates of the center of curvature, (Xc,Yc,Zc).
















   ..
       !! processed by numpydoc !!

.. py:property:: data1
   :type: Optional[float]


   
   Get or set the CRVOPT.EQ.1:     Average curvature, 1/R
   CRVOPT.EQ.2:    Coordinate Xc.
















   ..
       !! processed by numpydoc !!

.. py:property:: data2
   :type: Optional[float]


   
   Get or set the Coordinate Yc for CRVOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: data3
   :type: Optional[float]


   
   Get or set the Coordinate Zc for CRVOPT = 2.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'ACOUSTIC_NON_REFLECTING'






