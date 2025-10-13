





:class:`DefineAdaptiveSolidToDesId`
===================================


.. py:class:: define_adaptive_solid_to_des_id.DefineAdaptiveSolidToDesId(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_ADAPTIVE_SOLID_TO_DES_ID keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineAdaptiveSolidToDesId

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
            - Get or set the ID of the solid part or part set to transform.
          * - :py:attr:`~itype`
            - Get or set the Set type of the IPID
          * - :py:attr:`~nq`
            - Get or set the Adaptive option for hexahedral elements. For tetrahedral and
          * - :py:attr:`~ipdes`
            - Get or set the Part ID for newly generated discrete elements, See Remark 2.
          * - :py:attr:`~isdes`
            - Get or set the Section ID for discrete elements, See Remark 2.
          * - :py:attr:`~rsf`
            - Get or set the DES radius scale down factor, which is the ratio of the radius of the generated DES to the calculated radius based on volume consistency.
          * - :py:attr:`~outdes`
            - Get or set the Allow user output generated discrete element nodes and DES properties toa keyword file.
          * - :py:attr:`~ibond`
            - Get or set the Allow user define bonds between DES generated from the same solid element.
          * - :py:attr:`~pbn`
            - Get or set the Parallel-bond modulus [Pa]. See Remark 1 in *DEFINE_DE_BOND.
          * - :py:attr:`~pbs`
            - Get or set the Parallel-bond stiffness ratio. Shear stiffness/normal stiffness. See Remark 2 in *DEFINE_DE_BOND
          * - :py:attr:`~pbn_s`
            - Get or set the Parallel-bond maximum normal stress. A zero value defines an infinite maximum normal stress
          * - :py:attr:`~pbs_s`
            - Get or set the Parallel-bond maximum shear stress. A zero value defines an infinite maximum shear stress.
          * - :py:attr:`~sfa`
            - Get or set the Bond radius multiplier. Default is 1.
          * - :py:attr:`~alpha`
            - Get or set the Numerical damping.
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

    from define_adaptive_solid_to_des_id import DefineAdaptiveSolidToDesId

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


   
   Get or set the ID of the solid part or part set to transform.
















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


   
   Get or set the Adaptive option for hexahedral elements. For tetrahedral and
   pentahedral elements, see Remark 1:
   EQ.1: Adapt one solid element to one discrete element,
   EQ.2: Adapt one solid element to 8 discrete elements,
   EQ.3: Adapt one solid element to 27 discrete elements.
















   ..
       !! processed by numpydoc !!

.. py:property:: ipdes
   :type: Optional[int]


   
   Get or set the Part ID for newly generated discrete elements, See Remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: isdes
   :type: Optional[int]


   
   Get or set the Section ID for discrete elements, See Remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: rsf
   :type: float


   
   Get or set the DES radius scale down factor, which is the ratio of the radius of the generated DES to the calculated radius based on volume consistency.
















   ..
       !! processed by numpydoc !!

.. py:property:: outdes
   :type: int


   
   Get or set the Allow user output generated discrete element nodes and DES properties toa keyword file.
   EQ.0:   No output. (Default)
   EQ.1:   Write data under filename, desvfill.inc.
















   ..
       !! processed by numpydoc !!

.. py:property:: ibond
   :type: int


   
   Get or set the Allow user define bonds between DES generated from the same solid element.
   EQ.0:   No bonds. (Default)
   EQ.1:   Bonds generated, need to define Card 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: pbn
   :type: Optional[float]


   
   Get or set the Parallel-bond modulus [Pa]. See Remark 1 in *DEFINE_DE_BOND.
















   ..
       !! processed by numpydoc !!

.. py:property:: pbs
   :type: Optional[float]


   
   Get or set the Parallel-bond stiffness ratio. Shear stiffness/normal stiffness. See Remark 2 in *DEFINE_DE_BOND
















   ..
       !! processed by numpydoc !!

.. py:property:: pbn_s
   :type: Optional[float]


   
   Get or set the Parallel-bond maximum normal stress. A zero value defines an infinite maximum normal stress
















   ..
       !! processed by numpydoc !!

.. py:property:: pbs_s
   :type: Optional[float]


   
   Get or set the Parallel-bond maximum shear stress. A zero value defines an infinite maximum shear stress.
















   ..
       !! processed by numpydoc !!

.. py:property:: sfa
   :type: float


   
   Get or set the Bond radius multiplier. Default is 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha
   :type: float


   
   Get or set the Numerical damping.
















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
   :value: 'ADAPTIVE_SOLID_TO_DES_ID'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





