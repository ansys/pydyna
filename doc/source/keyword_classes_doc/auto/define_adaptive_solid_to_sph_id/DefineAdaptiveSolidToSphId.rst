





:class:`DefineAdaptiveSolidToSphId`
===================================


.. py:class:: define_adaptive_solid_to_sph_id.DefineAdaptiveSolidToSphId(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_ADAPTIVE_SOLID_TO_SPH_ID keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineAdaptiveSolidToSphId

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~did`
            - Get or set the Definition ID. This must be a unique number..
          * - :py:attr:`~heading`
            - Get or set the Definition descriptor. It is suggested that unique descriptions be       used.
          * - :py:attr:`~ipid`
            - Get or set the Solid Element part or part Set ID.
          * - :py:attr:`~itype`
            - Get or set the Set type of the IPID
          * - :py:attr:`~nq`
            - Get or set the Adaptive option:
          * - :py:attr:`~ipsph`
            - Get or set the Part ID for newly generated SPH elements
          * - :py:attr:`~issph`
            - Get or set the Section ID for SPH elements
          * - :py:attr:`~icpl`
            - Get or set the Coupling of newly generated SPH elements to the adjacent solid elements:
          * - :py:attr:`~iopt`
            - Get or set the Coupling method.
          * - :py:attr:`~cpcd`
            - Get or set the Thermal coupling conductivity between SPH part and solid part for ICPL = 3 option.
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

    from define_adaptive_solid_to_sph_id import DefineAdaptiveSolidToSphId

Property detail
---------------

.. py:property:: did
   :type: Optional[int]


   
   Get or set the Definition ID. This must be a unique number..
















   ..
       !! processed by numpydoc !!

.. py:property:: heading
   :type: Optional[str]


   
   Get or set the Definition descriptor. It is suggested that unique descriptions be       used.
















   ..
       !! processed by numpydoc !!

.. py:property:: ipid
   :type: Optional[int]


   
   Get or set the Solid Element part or part Set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: itype
   :type: int


   
   Get or set the Set type of the IPID
   EQ. 0: Part ID
   NE. 0: Part set ID
















   ..
       !! processed by numpydoc !!

.. py:property:: nq
   :type: int


   
   Get or set the Adaptive option:
   EQ. 1: Adapt one solid element to one SPH element
   EQ. 2: Adapt one solid element to 8 SPH elements (Hexahedra only)
   EQ. 3: Adapt one solid element to 27 SPH elements (Hexahedra only)
   For the adaptive scheme for Tetrahedron and Prism elements,
















   ..
       !! processed by numpydoc !!

.. py:property:: ipsph
   :type: Optional[int]


   
   Get or set the Part ID for newly generated SPH elements
















   ..
       !! processed by numpydoc !!

.. py:property:: issph
   :type: Optional[int]


   
   Get or set the Section ID for SPH elements
















   ..
       !! processed by numpydoc !!

.. py:property:: icpl
   :type: int


   
   Get or set the Coupling of newly generated SPH elements to the adjacent solid elements:
   EQ.0:   Failure without coupling (debris simulation),
   EQ.1:   Coupled to solid element,
   EQ.3:   Provide only thermal coupling between SPH part and solid part (must be combined with IOPT = 0 option; see Remark 4).
















   ..
       !! processed by numpydoc !!

.. py:property:: iopt
   :type: int


   
   Get or set the Coupling method.
   EQ. 0: Coupling from beginning (used as constraint between SPH elements and Solid elements)
   EQ. 1: Coupling begins when Lagrange element fails
















   ..
       !! processed by numpydoc !!

.. py:property:: cpcd
   :type: Optional[float]


   
   Get or set the Thermal coupling conductivity between SPH part and solid part for ICPL = 3 option.
   The default value is set as the average value of the conductivity from SPH part and the conductivity from solid part
















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
   :value: 'ADAPTIVE_SOLID_TO_SPH_ID'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





