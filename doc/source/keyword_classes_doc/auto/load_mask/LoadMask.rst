





:class:`LoadMask`
=================


.. py:class:: load_mask.LoadMask(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_MASK keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadMask

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID. This part must consist of 3D shell elements. To use this option with solid element the surface of the solid elements must be covered with null shells, see *MAT_NULL.
          * - :py:attr:`~lcid`
            - Get or set the Curve ID defining the pressure time history, see *DEFINE_CURVE.
          * - :py:attr:`~vid1`
            - Get or set the Vector ID normal to the suface on which the applied pressure acts. Positive pressure acts in a direction that is in the opposite direction. This vector may be used if the surface on which the pressure acts is relatively flat. If zero, the pressure load depends on the orientation of the shell elements.
          * - :py:attr:`~off`
            - Get or set the Pressure loads will be discontinued if | VID1*n | < OFF, where n is the normal vector to the shell element.
          * - :py:attr:`~boxid`
            - Get or set the Only elements inside the box with part ID, SSID , are considered. If no ID is given all elements of part ID, SSID, are included. When the active list of elements are updated, elements outside the box will no longer have pressure applied, i.e., the current configuration is always used.
          * - :py:attr:`~lcidm`
            - Get or set the Curve ID defining the mask. This curve gives (x,y) pairs of points in a local coordinate system defined by the vector ID, VID2. See also *DEFINE_CURVE. Curve should be flagged as DATTYP = 1.
          * - :py:attr:`~vid2`
            - Get or set the Vector ID used to project the masking curve onto the surface of part ID, PID. The origin of this vector determines the origin of the local system that the coordinates of the PID are transformed into prior to determining the pressure distribution in the local system. This curve must be defined if LCIDM is nonzero.
          * - :py:attr:`~inout`
            - Get or set the EQ.0: Elements whose center falls inside the projected curve are considered (default),
          * - :py:attr:`~icycle`
            - Get or set the Number of time steps between updating the list of active elements (default=200). The list update can be quite expensive and should be done at a reasonable interval. The default is not be appropiate for all problems.


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

    from load_mask import LoadMask

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID. This part must consist of 3D shell elements. To use this option with solid element the surface of the solid elements must be covered with null shells, see *MAT_NULL.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Curve ID defining the pressure time history, see *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: vid1
   :type: int


   
   Get or set the Vector ID normal to the suface on which the applied pressure acts. Positive pressure acts in a direction that is in the opposite direction. This vector may be used if the surface on which the pressure acts is relatively flat. If zero, the pressure load depends on the orientation of the shell elements.
















   ..
       !! processed by numpydoc !!

.. py:property:: off
   :type: float


   
   Get or set the Pressure loads will be discontinued if | VID1*n | < OFF, where n is the normal vector to the shell element.
















   ..
       !! processed by numpydoc !!

.. py:property:: boxid
   :type: int


   
   Get or set the Only elements inside the box with part ID, SSID , are considered. If no ID is given all elements of part ID, SSID, are included. When the active list of elements are updated, elements outside the box will no longer have pressure applied, i.e., the current configuration is always used.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidm
   :type: int


   
   Get or set the Curve ID defining the mask. This curve gives (x,y) pairs of points in a local coordinate system defined by the vector ID, VID2. See also *DEFINE_CURVE. Curve should be flagged as DATTYP = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: vid2
   :type: Optional[int]


   
   Get or set the Vector ID used to project the masking curve onto the surface of part ID, PID. The origin of this vector determines the origin of the local system that the coordinates of the PID are transformed into prior to determining the pressure distribution in the local system. This curve must be defined if LCIDM is nonzero.
















   ..
       !! processed by numpydoc !!

.. py:property:: inout
   :type: int


   
   Get or set the EQ.0: Elements whose center falls inside the projected curve are considered (default),
   EQ.1: Elements whose center falls outside the projected curve are considered.
















   ..
       !! processed by numpydoc !!

.. py:property:: icycle
   :type: int


   
   Get or set the Number of time steps between updating the list of active elements (default=200). The list update can be quite expensive and should be done at a reasonable interval. The default is not be appropiate for all problems.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'MASK'






