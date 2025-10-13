





:class:`DefineTransform`
========================


.. py:class:: define_transform.DefineTransform(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_TRANSFORM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineTransform

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~tranid`
            - Get or set the Transform ID.
          * - :py:attr:`~option`
            - Get or set the .
          * - :py:attr:`~a1`
            - Get or set the Specified entity.
          * - :py:attr:`~a2`
            - Get or set the Specified entity.
          * - :py:attr:`~a3`
            - Get or set the Specified entity.
          * - :py:attr:`~a4`
            - Get or set the Specified entity.
          * - :py:attr:`~a5`
            - Get or set the Specified entity.
          * - :py:attr:`~a6`
            - Get or set the Specified entity.
          * - :py:attr:`~a7`
            - Get or set the Specified entity.
          * - :py:attr:`~title`
            - Get or set the Additional title line


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~keyword`
            - 
          * - :py:attr:`~subkeyword`
            - 
          * - :py:attr:`~option_specs`
            - Get the card format type.






Import detail
-------------

.. code-block:: python

    from define_transform import DefineTransform

Property detail
---------------

.. py:property:: tranid
   :type: Optional[int]


   
   Get or set the Transform ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: option
   :type: str


   
   Get or set the .
   Parameters.  0-1 below for the available options.
   MIRROR
   a1, a2, a3, a4, a5, a6, a7
   Reflect, about a mirror plane defined to contain the point (a1, a2, a3) having its normal pointing from point (a1, a2, a3) toward (a4, a5, a6).  Setting a7=1 reflects the coordinate system as well, i.e., the mirrored coordinate system uses the left-hand-rule to determine the local z-axis.
   SCALE
   a1, a2, a3
   Scale the global x, y, and z coordinates of a point by a1, a2, and a3, respectively.  If zero, a default of unity is set.
   ROTATE
   a1, a2, a3, a4, a5, a6, a7Rotate through an angle (deg), a7, about a line with direction cosines a1, a2, and a3 passing through the point with coordinates a4, a5, and a6.If a4 through a7 are zero, then a1 and a2 are the ID's of two POINTs and a3 defines the rotation angle. The axis of rotation is defined by a vector going from point with ID a1 to point with ID a2.
   ROTATE3NA
   a1, a2, a3, a4
   Rotate through an angle (deg), a4. The axis of rotation is defined by a vector going from node with ID a1 to node with ID a2 and passing through node with ID a3 (a3 could be the same as a1 or a2). The three nodes must be defined before they are referenced.
   TRANSL
   a1, a2, a3
   Translate the x, y, and z coordinates of a point by a1, a2, and a3, respectively.
   TRANSL2ND
   a1, a2, a3
   Translate by distance a3. The direction is defined by a vector going from node with ID a1 to node with ID a2. The two nodes must be defined before they are referenced.
   POINT
   a1,a2,a3,a4
   Define a point with ID, a1, with the initial coordinates a2, a3, and a4.
   POS6P
   a1, a2, a3, a4, a5, a6
   Positioning by 6 points. Affine transformation (rotation and translation, no scaling) given by three start points a1, a2, and a3 and three target points a4, a5, and a6. The six POINTs must be defined before they are referenced. Only 1 POS6P option is permitted within a *DEFINE_TRANSFORMATION definition.
   POS6N
   a1, a2, a3, a4, a5, a6
   Positioning by 6 nodes. Affine transformation (rotation and translation, no scaling) given by three start nodes a1, a2, and a3 and three target nodes a4, a5, and a6. The six nodes must be defined before they are referenced. Only 1 POS6N option is permitted within a *DEFINE_TRANSFORMATION definition..
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: Optional[float]


   
   Get or set the Specified entity.
   See Keyword Manual Section 10.25.
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the Specified entity.
   See Keyword Manual Section 10.25.
















   ..
       !! processed by numpydoc !!

.. py:property:: a3
   :type: Optional[float]


   
   Get or set the Specified entity.
   See Keyword Manual Section 10.25.
















   ..
       !! processed by numpydoc !!

.. py:property:: a4
   :type: Optional[float]


   
   Get or set the Specified entity.
   See Keyword Manual Section 10.25.
















   ..
       !! processed by numpydoc !!

.. py:property:: a5
   :type: Optional[float]


   
   Get or set the Specified entity.
   See Keyword Manual Section 10.25.
















   ..
       !! processed by numpydoc !!

.. py:property:: a6
   :type: Optional[float]


   
   Get or set the Specified entity.
   See Keyword Manual Section 10.25.
















   ..
       !! processed by numpydoc !!

.. py:property:: a7
   :type: Optional[float]


   
   Get or set the Specified entity.
   See Keyword Manual Section 10.25.
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Additional title line
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DEFINE'


.. py:attribute:: subkeyword
   :value: 'TRANSFORM'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





