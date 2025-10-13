





:class:`SetSegment`
===================


.. py:class:: set_segment.SetSegment(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SET_SEGMENT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SetSegment

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the Segment set ID. All segment sets should have a unique set ID.
          * - :py:attr:`~da1`
            - Get or set the First segment attribute default value is 0.0.
          * - :py:attr:`~da2`
            - Get or set the Second segment attribute default value is 0.0.
          * - :py:attr:`~da3`
            - Get or set the Third segment attribute default value is 0.0.
          * - :py:attr:`~da4`
            - Get or set the Fourth segment attribute default value is 0.0.
          * - :py:attr:`~solver`
            - Get or set the EQ.MECH: mechanics.
          * - :py:attr:`~its`
            - Get or set the Define coupling type across different scales in two-scale co-simulation. This flag should only be included for segment sets that provide coupling information in the input file referred to by *INCLUDE_COSIM;
          * - :py:attr:`~segments`
            - Get the table of segments.
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

    from set_segment import SetSegment

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Segment set ID. All segment sets should have a unique set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: da1
   :type: float


   
   Get or set the First segment attribute default value is 0.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: da2
   :type: float


   
   Get or set the Second segment attribute default value is 0.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: da3
   :type: float


   
   Get or set the Third segment attribute default value is 0.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: da4
   :type: float


   
   Get or set the Fourth segment attribute default value is 0.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: solver
   :type: str


   
   Get or set the EQ.MECH: mechanics.
   EQ.CESE: CE/SE compressible fluid flow solver.
   EQ.ICFD: Incompressible fluid flow solver.
















   ..
       !! processed by numpydoc !!

.. py:property:: its
   :type: Optional[int]


   
   Get or set the Define coupling type across different scales in two-scale co-simulation. This flag should only be included for segment sets that provide coupling information in the input file referred to by *INCLUDE_COSIM;
   EQ.1:   Tied contact coupling
   EQ.2 : Solid - in - shell immersed coupling
















   ..
       !! processed by numpydoc !!

.. py:property:: segments
   :type: pandas.DataFrame


   
   Get the table of segments.
















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
   :value: 'SET'


.. py:attribute:: subkeyword
   :value: 'SEGMENT'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





