





:class:`ControlFormingBlankmesh`
================================


.. py:class:: control_forming_blankmesh.ControlFormingBlankmesh(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_FORMING_BLANKMESH keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlFormingBlankmesh

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~idmesh`
            - Get or set the ID of the blankmesh (not the blank PID); must be unique
          * - :py:attr:`~eleng`
            - Get or set the Element edge length.
          * - :py:attr:`~xleng`
            - Get or set the Length of the rectangular blank along X-axis in the coordinate system (CID) defined
          * - :py:attr:`~yleng`
            - Get or set the Length of the rectangular blank along Y-axis in the coordinate system (CID) defined
          * - :py:attr:`~angelx`
            - Get or set the X-coordinate of a reference point for the net to be generated
          * - :py:attr:`~nplane`
            - Get or set the Plane in which a flat blank to be generated, in reference to the coordinate system defined (CID):
          * - :py:attr:`~cid`
            - Get or set the ID of the local coordinate system
          * - :py:attr:`~pidbk`
            - Get or set the Part ID of the blank, as defined by *PART
          * - :py:attr:`~nid`
            - Get or set the Starting node ID of the blank to be generated
          * - :py:attr:`~eid`
            - Get or set the Starting element ID of the blank to be generated
          * - :py:attr:`~xcent`
            - Get or set the X-coordinate of the center of the blank
          * - :py:attr:`~ycent`
            - Get or set the Y-coordinate of the center of the blank
          * - :py:attr:`~zcent`
            - Get or set the Z-coordinate of the center of the blank
          * - :py:attr:`~xshift`
            - Get or set the Blank shift distance in X-axis in coordinate system defined (CID).
          * - :py:attr:`~yshift`
            - Get or set the Blank shift distance in Y-axis in coordinate system defined (CID).


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

    from control_forming_blankmesh import ControlFormingBlankmesh

Property detail
---------------

.. py:property:: idmesh
   :type: Optional[int]


   
   Get or set the ID of the blankmesh (not the blank PID); must be unique
















   ..
       !! processed by numpydoc !!

.. py:property:: eleng
   :type: float


   
   Get or set the Element edge length.
















   ..
       !! processed by numpydoc !!

.. py:property:: xleng
   :type: float


   
   Get or set the Length of the rectangular blank along X-axis in the coordinate system (CID) defined
















   ..
       !! processed by numpydoc !!

.. py:property:: yleng
   :type: float


   
   Get or set the Length of the rectangular blank along Y-axis in the coordinate system (CID) defined
















   ..
       !! processed by numpydoc !!

.. py:property:: angelx
   :type: float


   
   Get or set the X-coordinate of a reference point for the net to be generated
















   ..
       !! processed by numpydoc !!

.. py:property:: nplane
   :type: int


   
   Get or set the Plane in which a flat blank to be generated, in reference to the coordinate system defined (CID):
   EQ.1: XY-plane,
   EQ.2: XZ-plane,
   EQ.3: YZ-plane
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: int


   
   Get or set the ID of the local coordinate system
















   ..
       !! processed by numpydoc !!

.. py:property:: pidbk
   :type: Optional[int]


   
   Get or set the Part ID of the blank, as defined by *PART
















   ..
       !! processed by numpydoc !!

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Starting node ID of the blank to be generated
















   ..
       !! processed by numpydoc !!

.. py:property:: eid
   :type: Optional[int]


   
   Get or set the Starting element ID of the blank to be generated
















   ..
       !! processed by numpydoc !!

.. py:property:: xcent
   :type: float


   
   Get or set the X-coordinate of the center of the blank
















   ..
       !! processed by numpydoc !!

.. py:property:: ycent
   :type: float


   
   Get or set the Y-coordinate of the center of the blank
















   ..
       !! processed by numpydoc !!

.. py:property:: zcent
   :type: float


   
   Get or set the Z-coordinate of the center of the blank
















   ..
       !! processed by numpydoc !!

.. py:property:: xshift
   :type: float


   
   Get or set the Blank shift distance in X-axis in coordinate system defined (CID).
















   ..
       !! processed by numpydoc !!

.. py:property:: yshift
   :type: float


   
   Get or set the Blank shift distance in Y-axis in coordinate system defined (CID).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'FORMING_BLANKMESH'






