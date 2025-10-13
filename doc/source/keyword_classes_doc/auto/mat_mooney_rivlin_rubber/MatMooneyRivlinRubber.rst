





:class:`MatMooneyRivlinRubber`
==============================


.. py:class:: mat_mooney_rivlin_rubber.MatMooneyRivlinRubber(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_MOONEY-RIVLIN_RUBBER keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatMooneyRivlinRubber

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
            - Get or set the Poisson's ratio (> 0.49 is recommended, smaller values may not work).
          * - :py:attr:`~a`
            - Get or set the Constant, see literature and equations defined in keyword manual page 114 (volume two).
          * - :py:attr:`~b`
            - Get or set the Constant, see literature and equations defined in keyword manual page 114 (volume two).
          * - :py:attr:`~ref`
            - Get or set the Use reference geometry to initialize the stress tensor, see *INITIAL_FOAM_REFERENCE_ GEOMETRY (only 8-noded solid elements with one point integration):
          * - :py:attr:`~sgl`
            - Get or set the Specimen gauge length l0.
          * - :py:attr:`~sw`
            - Get or set the Specimen  gauge width.
          * - :py:attr:`~st`
            - Get or set the Specimen gauge thickness.
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID, see *DEFINE_CURVE, giving the force versus actual change dL in the gauge length.
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

    from mat_mooney_rivlin_rubber import MatMooneyRivlinRubber

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


   
   Get or set the Poisson's ratio (> 0.49 is recommended, smaller values may not work).
















   ..
       !! processed by numpydoc !!

.. py:property:: a
   :type: Optional[float]


   
   Get or set the Constant, see literature and equations defined in keyword manual page 114 (volume two).
















   ..
       !! processed by numpydoc !!

.. py:property:: b
   :type: Optional[float]


   
   Get or set the Constant, see literature and equations defined in keyword manual page 114 (volume two).
















   ..
       !! processed by numpydoc !!

.. py:property:: ref
   :type: float


   
   Get or set the Use reference geometry to initialize the stress tensor, see *INITIAL_FOAM_REFERENCE_ GEOMETRY (only 8-noded solid elements with one point integration):
   EQ.0.0: off (default),
   EQ.1.0: on.
















   ..
       !! processed by numpydoc !!

.. py:property:: sgl
   :type: Optional[float]


   
   Get or set the Specimen gauge length l0.
















   ..
       !! processed by numpydoc !!

.. py:property:: sw
   :type: Optional[float]


   
   Get or set the Specimen  gauge width.
















   ..
       !! processed by numpydoc !!

.. py:property:: st
   :type: Optional[float]


   
   Get or set the Specimen gauge thickness.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID, see *DEFINE_CURVE, giving the force versus actual change dL in the gauge length.
   For stress versus strain curve definition set SGL, SW, ST to 1.0..
















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
   :value: 'MOONEY-RIVLIN_RUBBER'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





