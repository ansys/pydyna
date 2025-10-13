





:class:`DefinePlane`
====================


.. py:class:: define_plane.DefinePlane(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_PLANE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefinePlane

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Plane ID. A unique number has to be defined.
          * - :py:attr:`~x1`
            - Get or set the X-coordinate of point 1.
          * - :py:attr:`~y1`
            - Get or set the Y-coordinate of point 1.
          * - :py:attr:`~z1`
            - Get or set the Z-coordinate of point 1.
          * - :py:attr:`~x2`
            - Get or set the X-coordinate of point 2.
          * - :py:attr:`~y2`
            - Get or set the Y-coordinate of point 2.
          * - :py:attr:`~z2`
            - Get or set the Z-coordinate of point 2.
          * - :py:attr:`~cid`
            - Get or set the Coordinate system ID applied to the coordinates used to define the current plane. The coordinates X1, Y1, Z1, X2, Y2, Z2, X3, Y3 and Z3 are defined with respect to the coordinate system CID.
          * - :py:attr:`~x3`
            - Get or set the X-coordinate of point 3.
          * - :py:attr:`~y3`
            - Get or set the Y-coordinate of point 3.
          * - :py:attr:`~z3`
            - Get or set the Z-coordinate of point 3.
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

    from define_plane import DefinePlane

Property detail
---------------

.. py:property:: pid
   :type: int


   
   Get or set the Plane ID. A unique number has to be defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: x1
   :type: float


   
   Get or set the X-coordinate of point 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: y1
   :type: float


   
   Get or set the Y-coordinate of point 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: z1
   :type: float


   
   Get or set the Z-coordinate of point 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: x2
   :type: float


   
   Get or set the X-coordinate of point 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: y2
   :type: float


   
   Get or set the Y-coordinate of point 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: z2
   :type: float


   
   Get or set the Z-coordinate of point 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: int


   
   Get or set the Coordinate system ID applied to the coordinates used to define the current plane. The coordinates X1, Y1, Z1, X2, Y2, Z2, X3, Y3 and Z3 are defined with respect to the coordinate system CID.
















   ..
       !! processed by numpydoc !!

.. py:property:: x3
   :type: float


   
   Get or set the X-coordinate of point 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: y3
   :type: float


   
   Get or set the Y-coordinate of point 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: z3
   :type: float


   
   Get or set the Z-coordinate of point 3.
















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
   :value: 'PLANE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





