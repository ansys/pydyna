





:class:`DefineDeBondByPart`
===========================


.. py:class:: define_de_bond_by_part.DefineDeBondByPart(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_DE_BOND_BY_PART keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineDeBondByPart

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the Part set/part/node set ID
          * - :py:attr:`~stype`
            - Get or set the EQ.0: DES node set
          * - :py:attr:`~pbn`
            - Get or set the Parallel-bond modulus [Pa]. See Remarks 1 and 2
          * - :py:attr:`~pbs`
            - Get or set the Parallel-bond stiffness ratio. shear stiffness/normal stiffness. See Remark 2
          * - :py:attr:`~pbn_s`
            - Get or set the Parallel-bond maximum normal stress. A zero value defines an infinite maximum normal stress.
          * - :py:attr:`~pbs_s`
            - Get or set the Parallel-bond maximum shear stress. A zero value defines an infinite maximum shear stress.
          * - :py:attr:`~sfa`
            - Get or set the Bond radius multiplier.
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

    from define_de_bond_by_part import DefineDeBondByPart

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Part set/part/node set ID
















   ..
       !! processed by numpydoc !!

.. py:property:: stype
   :type: int


   
   Get or set the EQ.0: DES node set
   EQ.1: DES node
   EQ.2: DES part set
   EQ.3: DES part
















   ..
       !! processed by numpydoc !!

.. py:property:: pbn
   :type: Optional[float]


   
   Get or set the Parallel-bond modulus [Pa]. See Remarks 1 and 2
















   ..
       !! processed by numpydoc !!

.. py:property:: pbs
   :type: Optional[float]


   
   Get or set the Parallel-bond stiffness ratio. shear stiffness/normal stiffness. See Remark 2
















   ..
       !! processed by numpydoc !!

.. py:property:: pbn_s
   :type: Optional[float]


   
   Get or set the Parallel-bond maximum normal stress. A zero value defines an infinite maximum normal stress.
















   ..
       !! processed by numpydoc !!

.. py:property:: pbs_s
   :type: Optional[float]


   
   Get or set the Parallel-bond maximum shear stress. A zero value defines an infinite maximum shear stress.
















   ..
       !! processed by numpydoc !!

.. py:property:: sfa
   :type: float


   
   Get or set the Bond radius multiplier.
















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
   :value: 'DE_BOND_BY_PART'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





