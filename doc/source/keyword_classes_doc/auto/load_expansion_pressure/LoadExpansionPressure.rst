





:class:`LoadExpansionPressure`
==============================


.. py:class:: load_expansion_pressure.LoadExpansionPressure(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_EXPANSION_PRESSURE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadExpansionPressure

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ssid`
            - Get or set the Segment set ID which specifies the interior of the chamber. As the edge moves, the pressure is applied or could be applied to these segments.
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID that defines the pressure as a function of time.
          * - :py:attr:`~sf`
            - Get or set the Load curve scale factor
          * - :py:attr:`~at`
            - Get or set the Activation time which is the time at which the pressure begins to be applied. Before this time, pressure will not be applied to the chamber.
          * - :py:attr:`~nsid`
            - Get or set the Node set ID that defines the moving edge/plane of the dynamic chamber.  Note that this node set must include at least 3 nodes to define a plane.
          * - :py:attr:`~xn`
            - Get or set the X component of the initial outward normal of the moving plane/edge.
          * - :py:attr:`~yn`
            - Get or set the Y component of the initial outward normal of the moving plane/edge
          * - :py:attr:`~zn`
            - Get or set the Z component of the initial outward normal of the moving plane/edge .


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

    from load_expansion_pressure import LoadExpansionPressure

Property detail
---------------

.. py:property:: ssid
   :type: Optional[int]


   
   Get or set the Segment set ID which specifies the interior of the chamber. As the edge moves, the pressure is applied or could be applied to these segments.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID that defines the pressure as a function of time.
















   ..
       !! processed by numpydoc !!

.. py:property:: sf
   :type: float


   
   Get or set the Load curve scale factor
















   ..
       !! processed by numpydoc !!

.. py:property:: at
   :type: float


   
   Get or set the Activation time which is the time at which the pressure begins to be applied. Before this time, pressure will not be applied to the chamber.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsid
   :type: Optional[int]


   
   Get or set the Node set ID that defines the moving edge/plane of the dynamic chamber.  Note that this node set must include at least 3 nodes to define a plane.
















   ..
       !! processed by numpydoc !!

.. py:property:: xn
   :type: Optional[float]


   
   Get or set the X component of the initial outward normal of the moving plane/edge.
















   ..
       !! processed by numpydoc !!

.. py:property:: yn
   :type: Optional[float]


   
   Get or set the Y component of the initial outward normal of the moving plane/edge
















   ..
       !! processed by numpydoc !!

.. py:property:: zn
   :type: Optional[float]


   
   Get or set the Z component of the initial outward normal of the moving plane/edge .
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'EXPANSION_PRESSURE'






