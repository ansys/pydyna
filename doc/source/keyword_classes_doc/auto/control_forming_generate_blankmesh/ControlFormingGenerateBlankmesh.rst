





:class:`ControlFormingGenerateBlankmesh`
========================================


.. py:class:: control_forming_generate_blankmesh.ControlFormingGenerateBlankmesh(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_FORMING_GENERATE_BLANKMESH keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlFormingGenerateBlankmesh

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~gentyp`
            - Get or set the EQ.1: Rectangle shape.
          * - :py:attr:`~eleng`
            - Get or set the edge-length of quad
          * - :py:attr:`~center`
            - Get or set the center of rectangle.
          * - :py:attr:`~xleng`
            - Get or set the length of rectangle blank regarding global/local x-axis
          * - :py:attr:`~yleng`
            - Get or set the length of rectangle blank regarding global/local y-axis
          * - :py:attr:`~align`
            - Get or set the alignment of elements refers to global/local x-axis
          * - :py:attr:`~plane`
            - Get or set the principale plane in which flat blank will be generated if blank outline as iges curve are not flat then projection to principal plane.
          * - :py:attr:`~cid`
            - Get or set the local coordinate system
          * - :py:attr:`~bpid`
            - Get or set the part id of generated blank
          * - :py:attr:`~snid`
            - Get or set the start node id
          * - :py:attr:`~seid`
            - Get or set the start element id
          * - :py:attr:`~xcent`
            - Get or set the absolute x-coordinate for center
          * - :py:attr:`~ycent`
            - Get or set the absolute y-coordinate for center
          * - :py:attr:`~zcent`
            - Get or set the absolute z-coordinate for center
          * - :py:attr:`~xshift`
            - Get or set the shift length of blank regarding global/local x-axix
          * - :py:attr:`~yshift`
            - Get or set the shift length of blank regarding global/local y-axix
          * - :py:attr:`~filename`
            - Get or set the


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

    from control_forming_generate_blankmesh import ControlFormingGenerateBlankmesh

Property detail
---------------

.. py:property:: gentyp
   :type: Optional[int]


   
   Get or set the EQ.1: Rectangle shape.
   Find the smallest possible untrimmed rectangle depending on the requested element length and edge size in z and y, create quad elements with the same edge length and afterwards trim the elements at the requested edge size in z and y (like *ELEMENT_BALNKING). In order to make sure that the last row of elements does not have a smaller element lenght (bad time step size) after trimming than the requested eleng the last two wlement rows will be optimized and then the element size can be even larger;
   .EQ.2: import blank outline as IGES-Curve.
   Import an iges file of the desired blank shape as a curve. Find the smallest possible untrimmed rectangle depending on the requested element lenght and the blank outline, create quad elements with the same edge elength and afterwards trim the elements at the blank outline ( like *ELEMENT_BLANKING). In order to make sure that the last row of elements geoes not have a smaller wlement lenght (bad time step size) after trimming than the requested eleng the last two wlement rows will be optimized and then the element size can be even larger
















   ..
       !! processed by numpydoc !!

.. py:property:: eleng
   :type: Optional[float]


   
   Get or set the edge-length of quad
















   ..
       !! processed by numpydoc !!

.. py:property:: center
   :type: int


   
   Get or set the center of rectangle.
   EQ.0 0,0,0 of global coordinate system or 0.0.0 of local coordinate system if cid is defined
   EQ.1  center via absolute coordinates define xcent, ycent and zcent
















   ..
       !! processed by numpydoc !!

.. py:property:: xleng
   :type: Optional[float]


   
   Get or set the length of rectangle blank regarding global/local x-axis
















   ..
       !! processed by numpydoc !!

.. py:property:: yleng
   :type: Optional[float]


   
   Get or set the length of rectangle blank regarding global/local y-axis
















   ..
       !! processed by numpydoc !!

.. py:property:: align
   :type: Optional[float]


   
   Get or set the alignment of elements refers to global/local x-axis
















   ..
       !! processed by numpydoc !!

.. py:property:: plane
   :type: int


   
   Get or set the principale plane in which flat blank will be generated if blank outline as iges curve are not flat then projection to principal plane.
   1: xy-plane of local or global coord system
   2: xz-plane of local or global coord system
   3: zy-plane of local or global coord system
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: Optional[int]


   
   Get or set the local coordinate system
















   ..
       !! processed by numpydoc !!

.. py:property:: bpid
   :type: Optional[int]


   
   Get or set the part id of generated blank
















   ..
       !! processed by numpydoc !!

.. py:property:: snid
   :type: Optional[int]


   
   Get or set the start node id
















   ..
       !! processed by numpydoc !!

.. py:property:: seid
   :type: Optional[int]


   
   Get or set the start element id
















   ..
       !! processed by numpydoc !!

.. py:property:: xcent
   :type: Optional[float]


   
   Get or set the absolute x-coordinate for center
















   ..
       !! processed by numpydoc !!

.. py:property:: ycent
   :type: Optional[float]


   
   Get or set the absolute y-coordinate for center
















   ..
       !! processed by numpydoc !!

.. py:property:: zcent
   :type: Optional[float]


   
   Get or set the absolute z-coordinate for center
















   ..
       !! processed by numpydoc !!

.. py:property:: xshift
   :type: Optional[float]


   
   Get or set the shift length of blank regarding global/local x-axix
















   ..
       !! processed by numpydoc !!

.. py:property:: yshift
   :type: Optional[float]


   
   Get or set the shift length of blank regarding global/local y-axix
















   ..
       !! processed by numpydoc !!

.. py:property:: filename
   :type: Optional[str]


   
   Get or set the 
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'FORMING_GENERATE_BLANKMESH'






