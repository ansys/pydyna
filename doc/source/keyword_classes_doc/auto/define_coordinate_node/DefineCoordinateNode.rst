





:class:`DefineCoordinateNode`
=============================


.. py:class:: define_coordinate_node.DefineCoordinateNode(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_COORDINATE_NODE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineCoordinateNode

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~cid`
            - Get or set the Coordinate system ID. A unique number has to be defined.
          * - :py:attr:`~n1`
            - Get or set the ID of node located at local origin.
          * - :py:attr:`~n2`
            - Get or set the ID of node located along local x-axis if DIR=X, the y-axis if DIR=Y, and along the z-axis if DIR=Z
          * - :py:attr:`~n3`
            - Get or set the ID of node located in local xy plane if DIR=X, the local yz plane if DIR=Y, and the local zx plane if DIR=Z
          * - :py:attr:`~flag`
            - Get or set the Set to unity, 1, if the local system is to be updated each time step for the BOUNDARY_SPC nodal constraints and ELEMENT_BEAM type 6, the discrete beam element. Generally, this option when used with nodal SPC's is not recommended since it can cause excursions in the energy balance because the constraint forces at the node may go through a displacement if the node is partially constrained
          * - :py:attr:`~dir`
            - Get or set the Axis defined by node N2 moving from the origin node N1. The default direction is the x-axis.
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

    from define_coordinate_node import DefineCoordinateNode

Property detail
---------------

.. py:property:: cid
   :type: Optional[int]


   
   Get or set the Coordinate system ID. A unique number has to be defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: n1
   :type: Optional[int]


   
   Get or set the ID of node located at local origin.
















   ..
       !! processed by numpydoc !!

.. py:property:: n2
   :type: Optional[int]


   
   Get or set the ID of node located along local x-axis if DIR=X, the y-axis if DIR=Y, and along the z-axis if DIR=Z
















   ..
       !! processed by numpydoc !!

.. py:property:: n3
   :type: Optional[int]


   
   Get or set the ID of node located in local xy plane if DIR=X, the local yz plane if DIR=Y, and the local zx plane if DIR=Z
















   ..
       !! processed by numpydoc !!

.. py:property:: flag
   :type: Optional[int]


   
   Get or set the Set to unity, 1, if the local system is to be updated each time step for the BOUNDARY_SPC nodal constraints and ELEMENT_BEAM type 6, the discrete beam element. Generally, this option when used with nodal SPC's is not recommended since it can cause excursions in the energy balance because the constraint forces at the node may go through a displacement if the node is partially constrained
















   ..
       !! processed by numpydoc !!

.. py:property:: dir
   :type: str


   
   Get or set the Axis defined by node N2 moving from the origin node N1. The default direction is the x-axis.
















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
   :value: 'COORDINATE_NODE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





