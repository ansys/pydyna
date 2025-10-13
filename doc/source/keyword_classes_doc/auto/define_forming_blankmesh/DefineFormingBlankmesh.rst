





:class:`DefineFormingBlankmesh`
===============================


.. py:class:: define_forming_blankmesh.DefineFormingBlankmesh(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_FORMING_BLANKMESH keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineFormingBlankmesh

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~idmsh`
            - Get or set the ID of the blankmesh (not the blank PID); must be unique.
          * - :py:attr:`~eleng`
            - Get or set the Element edge length.
          * - :py:attr:`~xleng`
            - Get or set the Length of the rectangular blank along X-axis in the coordinate system (CID) defined.
          * - :py:attr:`~yleng`
            - Get or set the Length of the rectangular blank along Y-axis in the coordinate system (CID) defined.
          * - :py:attr:`~anglex`
            - Get or set the An angle defined about Z-axis of the CID specified, starting from the X-axis as the zero degree,
          * - :py:attr:`~nplane`
            - Get or set the Plane in which a flat blank to be generated, in reference to the coordinate system defined (CID):
          * - :py:attr:`~cid`
            - Get or set the ID of a local coordinate system defined by *DEFINE_COORDINATE_SYSTEM. Default is 0 representing the global coordinate system.
          * - :py:attr:`~pidbk`
            - Get or set the Part ID of the blank, as defined by *PART.
          * - :py:attr:`~nid`
            - Get or set the Starting node ID of the blank to be generated.
          * - :py:attr:`~eid`
            - Get or set the Starting element ID of the blank to be generated.
          * - :py:attr:`~xcent`
            - Get or set the X-coordinate of the center of the blank.
          * - :py:attr:`~ycent`
            - Get or set the Y-coordinate of the center of the blank.
          * - :py:attr:`~zcent`
            - Get or set the Z-coordinate of the center of the blank.
          * - :py:attr:`~xshift`
            - Get or set the Blank shifting distance in X-axis in coordinate system (CID) defined.
          * - :py:attr:`~yshift`
            - Get or set the Blank shifting distance in Y-axis in coordinate system (CID) defined.
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

    from define_forming_blankmesh import DefineFormingBlankmesh

Property detail
---------------

.. py:property:: idmsh
   :type: Optional[int]


   
   Get or set the ID of the blankmesh (not the blank PID); must be unique.
















   ..
       !! processed by numpydoc !!

.. py:property:: eleng
   :type: float


   
   Get or set the Element edge length.
















   ..
       !! processed by numpydoc !!

.. py:property:: xleng
   :type: float


   
   Get or set the Length of the rectangular blank along X-axis in the coordinate system (CID) defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: yleng
   :type: float


   
   Get or set the Length of the rectangular blank along Y-axis in the coordinate system (CID) defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: anglex
   :type: float


   
   Get or set the An angle defined about Z-axis of the CID specified, starting from the X-axis as the zero degree,
   to rotate the blank and the orientation of the mesh to be generated. The sign of the rotation angle follows the right-hand rule.
















   ..
       !! processed by numpydoc !!

.. py:property:: nplane
   :type: int


   
   Get or set the Plane in which a flat blank to be generated, in reference to the coordinate system defined (CID):
   EQ.0 or 1:      XY-plane (default)
   EQ.2:   XZ-plane
   EQ.3:   YZ-plan.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: Optional[int]


   
   Get or set the ID of a local coordinate system defined by *DEFINE_COORDINATE_SYSTEM. Default is 0 representing the global coordinate system.
















   ..
       !! processed by numpydoc !!

.. py:property:: pidbk
   :type: Optional[int]


   
   Get or set the Part ID of the blank, as defined by *PART.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Starting node ID of the blank to be generated.
















   ..
       !! processed by numpydoc !!

.. py:property:: eid
   :type: Optional[int]


   
   Get or set the Starting element ID of the blank to be generated.
















   ..
       !! processed by numpydoc !!

.. py:property:: xcent
   :type: float


   
   Get or set the X-coordinate of the center of the blank.
















   ..
       !! processed by numpydoc !!

.. py:property:: ycent
   :type: float


   
   Get or set the Y-coordinate of the center of the blank.
















   ..
       !! processed by numpydoc !!

.. py:property:: zcent
   :type: float


   
   Get or set the Z-coordinate of the center of the blank.
















   ..
       !! processed by numpydoc !!

.. py:property:: xshift
   :type: float


   
   Get or set the Blank shifting distance in X-axis in coordinate system (CID) defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: yshift
   :type: float


   
   Get or set the Blank shifting distance in Y-axis in coordinate system (CID) defined.
















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
   :value: 'FORMING_BLANKMESH'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





