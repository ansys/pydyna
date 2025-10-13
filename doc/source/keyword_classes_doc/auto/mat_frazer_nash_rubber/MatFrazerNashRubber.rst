





:class:`MatFrazerNashRubber`
============================


.. py:class:: mat_frazer_nash_rubber.MatFrazerNashRubber(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_FRAZER-NASH_RUBBER keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatFrazerNashRubber

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number has to be used.
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~pr`
            - Get or set the Poisson's ratio. Values between .49 and .50 are suggested.
          * - :py:attr:`~c100`
            - Get or set the C100 (EQ.1.0 if term is in the least squares fit.).
          * - :py:attr:`~c200`
            - Get or set the C200 (EQ.1.0 if term is in the least squares fit.).
          * - :py:attr:`~c300`
            - Get or set the C300 (EQ.1.0 if term is in the least squares fit.).
          * - :py:attr:`~c400`
            - Get or set the C400 (EQ.1.0 if term is in the least squares fit.).
          * - :py:attr:`~c110`
            - Get or set the C110 (EQ.1.0 if term is in the least squares fit.).
          * - :py:attr:`~c210`
            - Get or set the C210 (EQ.1.0 if term is in the least squares fit.).
          * - :py:attr:`~c010`
            - Get or set the C010 (EQ.1.0 if term is in the least squares fit.).
          * - :py:attr:`~c020`
            - Get or set the C020 (EQ.1.0 if term is in the least squares fit.).
          * - :py:attr:`~exit`
            - Get or set the Exit option:
          * - :py:attr:`~emax`
            - Get or set the Maximum strain limit, (Green-St, Venant Strain).
          * - :py:attr:`~emin`
            - Get or set the Minimum strain limit, (Green-St, Venant Strain).
          * - :py:attr:`~ref`
            - Get or set the Use reference geometry to initialize the stress tensor ,see *INITIAL_FOAM_REFERENCE_ GEOMETRY (only 8-noded-solid elements with on integration point):
          * - :py:attr:`~sgl`
            - Get or set the Specimen gauge length.
          * - :py:attr:`~sw`
            - Get or set the Specimen width.
          * - :py:attr:`~st`
            - Get or set the Specimen thickness.
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID, see DEFINE_CURVE, giving the force versus actual change in gauge length.
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

    from mat_frazer_nash_rubber import MatFrazerNashRubber

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density.
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: Optional[float]


   
   Get or set the Poisson's ratio. Values between .49 and .50 are suggested.
















   ..
       !! processed by numpydoc !!

.. py:property:: c100
   :type: Optional[float]


   
   Get or set the C100 (EQ.1.0 if term is in the least squares fit.).
















   ..
       !! processed by numpydoc !!

.. py:property:: c200
   :type: Optional[float]


   
   Get or set the C200 (EQ.1.0 if term is in the least squares fit.).
















   ..
       !! processed by numpydoc !!

.. py:property:: c300
   :type: Optional[float]


   
   Get or set the C300 (EQ.1.0 if term is in the least squares fit.).
















   ..
       !! processed by numpydoc !!

.. py:property:: c400
   :type: Optional[float]


   
   Get or set the C400 (EQ.1.0 if term is in the least squares fit.).
















   ..
       !! processed by numpydoc !!

.. py:property:: c110
   :type: Optional[float]


   
   Get or set the C110 (EQ.1.0 if term is in the least squares fit.).
















   ..
       !! processed by numpydoc !!

.. py:property:: c210
   :type: Optional[float]


   
   Get or set the C210 (EQ.1.0 if term is in the least squares fit.).
















   ..
       !! processed by numpydoc !!

.. py:property:: c010
   :type: Optional[float]


   
   Get or set the C010 (EQ.1.0 if term is in the least squares fit.).
















   ..
       !! processed by numpydoc !!

.. py:property:: c020
   :type: Optional[float]


   
   Get or set the C020 (EQ.1.0 if term is in the least squares fit.).
















   ..
       !! processed by numpydoc !!

.. py:property:: exit
   :type: Optional[float]


   
   Get or set the Exit option:
   EQ.0.0: stop if strain limits are exceeded (recommended),
   NE.0.0: continue if strain limits are exceeded. The curve is then extrapolated.
















   ..
       !! processed by numpydoc !!

.. py:property:: emax
   :type: Optional[float]


   
   Get or set the Maximum strain limit, (Green-St, Venant Strain).
















   ..
       !! processed by numpydoc !!

.. py:property:: emin
   :type: Optional[float]


   
   Get or set the Minimum strain limit, (Green-St, Venant Strain).
















   ..
       !! processed by numpydoc !!

.. py:property:: ref
   :type: float


   
   Get or set the Use reference geometry to initialize the stress tensor ,see *INITIAL_FOAM_REFERENCE_ GEOMETRY (only 8-noded-solid elements with on integration point):
   EQ.0.0: off (default),
   EQ.1.0: on.
















   ..
       !! processed by numpydoc !!

.. py:property:: sgl
   :type: Optional[float]


   
   Get or set the Specimen gauge length.
















   ..
       !! processed by numpydoc !!

.. py:property:: sw
   :type: Optional[float]


   
   Get or set the Specimen width.
















   ..
       !! processed by numpydoc !!

.. py:property:: st
   :type: Optional[float]


   
   Get or set the Specimen thickness.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: int


   
   Get or set the Load curve ID, see DEFINE_CURVE, giving the force versus actual change in gauge length.
















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
   :value: 'MAT'


.. py:attribute:: subkeyword
   :value: 'FRAZER-NASH_RUBBER'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





