





:class:`SectionPointSource`
===========================


.. py:class:: section_point_source.SectionPointSource(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SECTION_POINT_SOURCE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SectionPointSource

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~secid`
            - Get or set the Section ID. SECID is referenced on the *PART card and must be unique.
          * - :py:attr:`~lcidt`
            - Get or set the Temperature load curve ID.
          * - :py:attr:`~lcidvolr`
            - Get or set the Relative volume load curve ID.
          * - :py:attr:`~lcidvel`
            - Get or set the Inlet flow velocity load curve ID.
          * - :py:attr:`~nlc001`
            - Get or set the Node ID defining a local coordinate system. If defined, the vectors defining the inlet flow direction follow the rotation of this system.
          * - :py:attr:`~nlc002`
            - Get or set the Node ID defining a local coordinate system. If defined, the vectors defining the inlet flow direction follow the rotation of this system.
          * - :py:attr:`~nlc003`
            - Get or set the Node ID defining a local coordinate system. If defined, the vectors defining the inlet flow direction follow the rotation of this system.
          * - :py:attr:`~nodeid`
            - Get or set the Node ID defining the location of the point source.
          * - :py:attr:`~vecid`
            - Get or set the Vector ID defining the inlet flow direction in a local coordinate system defined by NID1-NID3.  If NID1-NID3 are not defined, the vector is assumed to be defined in the global coordinate system.
          * - :py:attr:`~orifarea`
            - Get or set the Point source orifice area
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

    from section_point_source import SectionPointSource

Property detail
---------------

.. py:property:: secid
   :type: Optional[int]


   
   Get or set the Section ID. SECID is referenced on the *PART card and must be unique.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidt
   :type: Optional[int]


   
   Get or set the Temperature load curve ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidvolr
   :type: Optional[int]


   
   Get or set the Relative volume load curve ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidvel
   :type: Optional[int]


   
   Get or set the Inlet flow velocity load curve ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: nlc001
   :type: Optional[int]


   
   Get or set the Node ID defining a local coordinate system. If defined, the vectors defining the inlet flow direction follow the rotation of this system.
















   ..
       !! processed by numpydoc !!

.. py:property:: nlc002
   :type: Optional[int]


   
   Get or set the Node ID defining a local coordinate system. If defined, the vectors defining the inlet flow direction follow the rotation of this system.
















   ..
       !! processed by numpydoc !!

.. py:property:: nlc003
   :type: Optional[int]


   
   Get or set the Node ID defining a local coordinate system. If defined, the vectors defining the inlet flow direction follow the rotation of this system.
















   ..
       !! processed by numpydoc !!

.. py:property:: nodeid
   :type: Optional[int]


   
   Get or set the Node ID defining the location of the point source.
















   ..
       !! processed by numpydoc !!

.. py:property:: vecid
   :type: Optional[int]


   
   Get or set the Vector ID defining the inlet flow direction in a local coordinate system defined by NID1-NID3.  If NID1-NID3 are not defined, the vector is assumed to be defined in the global coordinate system.
















   ..
       !! processed by numpydoc !!

.. py:property:: orifarea
   :type: Optional[float]


   
   Get or set the Point source orifice area
















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
   :value: 'SECTION'


.. py:attribute:: subkeyword
   :value: 'POINT_SOURCE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





