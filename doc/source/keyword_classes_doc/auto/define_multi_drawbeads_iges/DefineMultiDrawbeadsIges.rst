





:class:`DefineMultiDrawbeadsIges`
=================================


.. py:class:: define_multi_drawbeads_iges.DefineMultiDrawbeadsIges(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_MULTI_DRAWBEADS_IGES keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineMultiDrawbeadsIges

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~filename`
            - Get or set the IGES file that has the draw bead curve segment definitions
          * - :py:attr:`~dbid`
            - Get or set the Draw bead set ID, which may consists many draw bead segments.
          * - :py:attr:`~vid`
            - Get or set the Vector ID, as defined by *DEFINE_VECTOR. This vector is used to
          * - :py:attr:`~pid`
            - Get or set the Part ID of the rigid tool to which the curves are projected and attached.
          * - :py:attr:`~blkid`
            - Get or set the Part set ID of the blank.
          * - :py:attr:`~ncur`
            - Get or set the Number of draw bead curve segments (in the IGES file) to be defined.
          * - :py:attr:`~crvid`
            - Get or set the IGES curve ID for each segment.
          * - :py:attr:`~bforce`
            - Get or set the Draw bead force for each segment.
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

    from define_multi_drawbeads_iges import DefineMultiDrawbeadsIges

Property detail
---------------

.. py:property:: filename
   :type: Optional[str]


   
   Get or set the IGES file that has the draw bead curve segment definitions
















   ..
       !! processed by numpydoc !!

.. py:property:: dbid
   :type: Optional[int]


   
   Get or set the Draw bead set ID, which may consists many draw bead segments.
















   ..
       !! processed by numpydoc !!

.. py:property:: vid
   :type: Optional[int]


   
   Get or set the Vector ID, as defined by *DEFINE_VECTOR. This vector is used to
   project the supplied curves to the rigid tool, defined by variable PID.
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID of the rigid tool to which the curves are projected and attached.
















   ..
       !! processed by numpydoc !!

.. py:property:: blkid
   :type: Optional[int]


   
   Get or set the Part set ID of the blank.
















   ..
       !! processed by numpydoc !!

.. py:property:: ncur
   :type: Optional[int]


   
   Get or set the Number of draw bead curve segments (in the IGES file) to be defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: crvid
   :type: Optional[int]


   
   Get or set the IGES curve ID for each segment.
















   ..
       !! processed by numpydoc !!

.. py:property:: bforce
   :type: float


   
   Get or set the Draw bead force for each segment.
















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
   :value: 'MULTI_DRAWBEADS_IGES'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





