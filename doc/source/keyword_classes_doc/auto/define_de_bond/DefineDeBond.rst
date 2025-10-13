





:class:`DefineDeBond`
=====================


.. py:class:: define_de_bond.DefineDeBond(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_DE_BOND keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineDeBond

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the Node set, part set, or part ID for which bond properties apply
          * - :py:attr:`~stype`
            - Get or set the EQ.0: DES node set
          * - :py:attr:`~bdform`
            - Get or set the Bond formulation:
          * - :py:attr:`~pbn`
            - Get or set the Parallel-bond normal stiffness
          * - :py:attr:`~pbs`
            - Get or set the Parallel-bond shear stiffness
          * - :py:attr:`~pbn_s`
            - Get or set the Parallel-bond maximum normal stress.
          * - :py:attr:`~pbs_s`
            - Get or set the Parallel-bond maximum shear stress.
          * - :py:attr:`~sfa`
            - Get or set the Bond radius multiplier.
          * - :py:attr:`~alpha`
            - Get or set the Numerical damping.
          * - :py:attr:`~maxgap`
            - Get or set the Maximum gap between two bonded spheres
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

    from define_de_bond import DefineDeBond

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Node set, part set, or part ID for which bond properties apply
















   ..
       !! processed by numpydoc !!

.. py:property:: stype
   :type: int


   
   Get or set the EQ.0: DES node set
   EQ.2: DES part set
   EQ.3: DES part
















   ..
       !! processed by numpydoc !!

.. py:property:: bdform
   :type: int


   
   Get or set the Bond formulation:
   EQ.1: Linear bond formulation.
















   ..
       !! processed by numpydoc !!

.. py:property:: pbn
   :type: Optional[float]


   
   Get or set the Parallel-bond normal stiffness
















   ..
       !! processed by numpydoc !!

.. py:property:: pbs
   :type: Optional[float]


   
   Get or set the Parallel-bond shear stiffness
















   ..
       !! processed by numpydoc !!

.. py:property:: pbn_s
   :type: Optional[float]


   
   Get or set the Parallel-bond maximum normal stress.
















   ..
       !! processed by numpydoc !!

.. py:property:: pbs_s
   :type: Optional[float]


   
   Get or set the Parallel-bond maximum shear stress.
















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

.. py:property:: maxgap
   :type: float


   
   Get or set the Maximum gap between two bonded spheres
   GT.0.0: defines the ratio of the smaller radius of two bonded spheres as the maximum gap, i.e. MAXGAPxmin(r1,r2)
   LT.0.0: absolute value is used as the maximum gap.
















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
   :value: 'DE_BOND'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





