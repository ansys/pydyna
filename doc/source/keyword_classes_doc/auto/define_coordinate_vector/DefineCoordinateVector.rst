





:class:`DefineCoordinateVector`
===============================


.. py:class:: define_coordinate_vector.DefineCoordinateVector(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_COORDINATE_VECTOR keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineCoordinateVector

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~cid`
            - Get or set the Coordinate system ID. A unique number has to be defined.
          * - :py:attr:`~xx`
            - Get or set the x-coordinate on local x-axis. Origin lies at (0,0,0).
          * - :py:attr:`~yx`
            - Get or set the y-coordinate on local x-axis.
          * - :py:attr:`~zx`
            - Get or set the z-coordinate on local x-axis.
          * - :py:attr:`~xv`
            - Get or set the x-coordinate of local x-y vector.
          * - :py:attr:`~yv`
            - Get or set the y-coordinate of local x-y vector.
          * - :py:attr:`~zv`
            - Get or set the z-coordinate of local x-y vector.
          * - :py:attr:`~nid`
            - Get or set the Optional nodal point ID.  The coordinate system rotates with the rotation of this node.  If the node is not defined, the coordinate system is stationary
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

    from define_coordinate_vector import DefineCoordinateVector

Property detail
---------------

.. py:property:: cid
   :type: int


   
   Get or set the Coordinate system ID. A unique number has to be defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: xx
   :type: float


   
   Get or set the x-coordinate on local x-axis. Origin lies at (0,0,0).
















   ..
       !! processed by numpydoc !!

.. py:property:: yx
   :type: float


   
   Get or set the y-coordinate on local x-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: zx
   :type: float


   
   Get or set the z-coordinate on local x-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: xv
   :type: float


   
   Get or set the x-coordinate of local x-y vector.
















   ..
       !! processed by numpydoc !!

.. py:property:: yv
   :type: float


   
   Get or set the y-coordinate of local x-y vector.
















   ..
       !! processed by numpydoc !!

.. py:property:: zv
   :type: float


   
   Get or set the z-coordinate of local x-y vector.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid
   :type: int


   
   Get or set the Optional nodal point ID.  The coordinate system rotates with the rotation of this node.  If the node is not defined, the coordinate system is stationary
















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
   :value: 'COORDINATE_VECTOR'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





