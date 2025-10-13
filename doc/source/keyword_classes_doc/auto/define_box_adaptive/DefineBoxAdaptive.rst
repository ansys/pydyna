





:class:`DefineBoxAdaptive`
==========================


.. py:class:: define_box_adaptive.DefineBoxAdaptive(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_BOX_ADAPTIVE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineBoxAdaptive

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~boxid`
            - Get or set the Box ID. Define unique numbers.
          * - :py:attr:`~xmn`
            - Get or set the Minimum x-coordinate.
          * - :py:attr:`~xmx`
            - Get or set the Maximum x-coordinate.
          * - :py:attr:`~ymn`
            - Get or set the Minimum y-coordinate.
          * - :py:attr:`~ymx`
            - Get or set the Maximum y-coordinate.
          * - :py:attr:`~zmn`
            - Get or set the Minimum z-coordinate.
          * - :py:attr:`~zmx`
            - Get or set the Maximum z-coordinate.
          * - :py:attr:`~pid`
            - Get or set the Part ID. If zero, all active elements within box are considered.
          * - :py:attr:`~level`
            - Get or set the Maximum number of refinement levels for elements that are contained in the box. Values of 1, 2, 3, 4,... allow a maximum of 1, 4, 16, 64, ...  elements, respectively, to be created for each original element.
          * - :py:attr:`~lidx_ndid`
            - Get or set the Load curve ID/Node ID.
          * - :py:attr:`~lidy`
            - Get or set the Load curve ID.
          * - :py:attr:`~lidz`
            - Get or set the Load curve ID.
          * - :py:attr:`~brmin`
            - Get or set the Minimum mesh size in 3D tetrahedron adaptivity.
          * - :py:attr:`~brmax`
            - Get or set the Maximum mesh size in 3D tetrahedron adaptivity.
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

    from define_box_adaptive import DefineBoxAdaptive

Property detail
---------------

.. py:property:: boxid
   :type: Optional[int]


   
   Get or set the Box ID. Define unique numbers.
















   ..
       !! processed by numpydoc !!

.. py:property:: xmn
   :type: float


   
   Get or set the Minimum x-coordinate.
















   ..
       !! processed by numpydoc !!

.. py:property:: xmx
   :type: float


   
   Get or set the Maximum x-coordinate.
















   ..
       !! processed by numpydoc !!

.. py:property:: ymn
   :type: float


   
   Get or set the Minimum y-coordinate.
















   ..
       !! processed by numpydoc !!

.. py:property:: ymx
   :type: float


   
   Get or set the Maximum y-coordinate.
















   ..
       !! processed by numpydoc !!

.. py:property:: zmn
   :type: float


   
   Get or set the Minimum z-coordinate.
















   ..
       !! processed by numpydoc !!

.. py:property:: zmx
   :type: float


   
   Get or set the Maximum z-coordinate.
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: int


   
   Get or set the Part ID. If zero, all active elements within box are considered.
















   ..
       !! processed by numpydoc !!

.. py:property:: level
   :type: int


   
   Get or set the Maximum number of refinement levels for elements that are contained in the box. Values of 1, 2, 3, 4,... allow a maximum of 1, 4, 16, 64, ...  elements, respectively, to be created for each original element.
















   ..
       !! processed by numpydoc !!

.. py:property:: lidx_ndid
   :type: int


   
   Get or set the Load curve ID/Node ID.
   GT.0: load curve ID. Define adaptive box movement (displacement
   vs. time) in global X axis.
   LT.0: absolute value is a node ID, whose movement will be
   followed by the moving adaptive box. The node ID can be
   on a moving rigid body.
   EQ.0: no movement.
















   ..
       !! processed by numpydoc !!

.. py:property:: lidy
   :type: int


   
   Get or set the Load curve ID.
   GT.0: load curve ID. Define adaptive box movement (displacement
   vs. time) in global Y axis.
   EQ.0: no movement.
















   ..
       !! processed by numpydoc !!

.. py:property:: lidz
   :type: int


   
   Get or set the Load curve ID.
   GT.0: load curve ID. Define adaptive box movement (displacement
   vs. time) in global Y axis.
   EQ.0: no movement.
















   ..
       !! processed by numpydoc !!

.. py:property:: brmin
   :type: float


   
   Get or set the Minimum mesh size in 3D tetrahedron adaptivity.
















   ..
       !! processed by numpydoc !!

.. py:property:: brmax
   :type: float


   
   Get or set the Maximum mesh size in 3D tetrahedron adaptivity.
















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
   :value: 'BOX_ADAPTIVE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





