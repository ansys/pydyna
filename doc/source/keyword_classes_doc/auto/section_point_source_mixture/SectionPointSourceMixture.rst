





:class:`SectionPointSourceMixture`
==================================


.. py:class:: section_point_source_mixture.SectionPointSourceMixture(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SECTION_POINT_SOURCE_MIXTURE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SectionPointSourceMixture

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~secid`
            - Get or set the Section ID Number
          * - :py:attr:`~lcidt`
            - Get or set the Inflator gas mixture average stagnation temperature load curve ID.
          * - :py:attr:`~notused`
            - Get or set the Not used.
          * - :py:attr:`~lcidvel`
            - Get or set the inflator gas mixture average velocity load curve ID..  If LCIDVEL=0 or blank, LSDYNA will estimate the inlet gas velocity.
          * - :py:attr:`~nidlc001`
            - Get or set the The 1st Node ID defining a local coordinate system.
          * - :py:attr:`~nidlc002`
            - Get or set the The 2nd Node ID defining a local coordinate system.
          * - :py:attr:`~nidlc003`
            - Get or set the the 3rd Node ID defining a local coordinate system.
          * - :py:attr:`~idir`
            - Get or set the A flag for constraining the nodal velocity of the nodes of the ALE element containing a point source.  If IDIR=0 (default), then the ALE nodes behind the point source (relative position of nodes based on the vector direction of flow of point source) will have zero velocity.  If IDIR=1, then all ALE nodes will have velocity distributed based on energy conservation.  The latter option seems to be more robust in airbag modeling.
          * - :py:attr:`~lcmdot1`
            - Get or set the The mass flow rate load curve ID of the 1st gas in the mixture.
          * - :py:attr:`~lcmdot2`
            - Get or set the The mass flow rate load curve ID of the 1st gas in the mixture.
          * - :py:attr:`~lcmdot3`
            - Get or set the The mass flow rate load curve ID of the 1st gas in the mixture.
          * - :py:attr:`~lcmdot4`
            - Get or set the The mass flow rate load curve ID of the 1st gas in the mixture.
          * - :py:attr:`~lcmdot5`
            - Get or set the The mass flow rate load curve ID of the 1st gas in the mixture.
          * - :py:attr:`~lcmdot6`
            - Get or set the The mass flow rate load curve ID of the 1st gas in the mixture.
          * - :py:attr:`~lcmdot7`
            - Get or set the The mass flow rate load curve ID of the 1st gas in the mixture.
          * - :py:attr:`~lcmdot8`
            - Get or set the The mass flow rate load curve ID of the 1st gas in the mixture.
          * - :py:attr:`~nodeid`
            - Get or set the The node ID(s) defining the point source(s).
          * - :py:attr:`~vecid`
            - Get or set the The vector ID defining the direction of flow at each point source.
          * - :py:attr:`~orifarea`
            - Get or set the The orifice area at each point source
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

    from section_point_source_mixture import SectionPointSourceMixture

Property detail
---------------

.. py:property:: secid
   :type: Optional[int]


   
   Get or set the Section ID Number
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidt
   :type: Optional[int]


   
   Get or set the Inflator gas mixture average stagnation temperature load curve ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: notused
   :type: Optional[int]


   
   Get or set the Not used.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidvel
   :type: Optional[int]


   
   Get or set the inflator gas mixture average velocity load curve ID..  If LCIDVEL=0 or blank, LSDYNA will estimate the inlet gas velocity.
















   ..
       !! processed by numpydoc !!

.. py:property:: nidlc001
   :type: Optional[int]


   
   Get or set the The 1st Node ID defining a local coordinate system.
















   ..
       !! processed by numpydoc !!

.. py:property:: nidlc002
   :type: Optional[int]


   
   Get or set the The 2nd Node ID defining a local coordinate system.
















   ..
       !! processed by numpydoc !!

.. py:property:: nidlc003
   :type: Optional[int]


   
   Get or set the the 3rd Node ID defining a local coordinate system.
















   ..
       !! processed by numpydoc !!

.. py:property:: idir
   :type: Optional[int]


   
   Get or set the A flag for constraining the nodal velocity of the nodes of the ALE element containing a point source.  If IDIR=0 (default), then the ALE nodes behind the point source (relative position of nodes based on the vector direction of flow of point source) will have zero velocity.  If IDIR=1, then all ALE nodes will have velocity distributed based on energy conservation.  The latter option seems to be more robust in airbag modeling.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcmdot1
   :type: int


   
   Get or set the The mass flow rate load curve ID of the 1st gas in the mixture.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcmdot2
   :type: int


   
   Get or set the The mass flow rate load curve ID of the 1st gas in the mixture.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcmdot3
   :type: int


   
   Get or set the The mass flow rate load curve ID of the 1st gas in the mixture.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcmdot4
   :type: int


   
   Get or set the The mass flow rate load curve ID of the 1st gas in the mixture.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcmdot5
   :type: int


   
   Get or set the The mass flow rate load curve ID of the 1st gas in the mixture.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcmdot6
   :type: int


   
   Get or set the The mass flow rate load curve ID of the 1st gas in the mixture.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcmdot7
   :type: int


   
   Get or set the The mass flow rate load curve ID of the 1st gas in the mixture.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcmdot8
   :type: int


   
   Get or set the The mass flow rate load curve ID of the 1st gas in the mixture.
















   ..
       !! processed by numpydoc !!

.. py:property:: nodeid
   :type: int


   
   Get or set the The node ID(s) defining the point source(s).
















   ..
       !! processed by numpydoc !!

.. py:property:: vecid
   :type: int


   
   Get or set the The vector ID defining the direction of flow at each point source.
















   ..
       !! processed by numpydoc !!

.. py:property:: orifarea
   :type: float


   
   Get or set the The orifice area at each point source
















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
   :value: 'POINT_SOURCE_MIXTURE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





