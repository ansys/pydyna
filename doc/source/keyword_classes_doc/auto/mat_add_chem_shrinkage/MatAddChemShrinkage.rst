





:class:`MatAddChemShrinkage`
============================


.. py:class:: mat_add_chem_shrinkage.MatAddChemShrinkage(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_ADD_CHEM_SHRINKAGE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatAddChemShrinkage

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID for which the chemical shrinkage effect applies
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID (see *DEFINE_CURVE) defining the chemical shrinkage coefficient, β, or a proxy in experiments for the chemical shrinkage coefficient, α as a function of temperature, T. If α as a function of T is defined, α is converted to the chemical shrinkage coefficient by LS-DYNA
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

    from mat_add_chem_shrinkage import MatAddChemShrinkage

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID for which the chemical shrinkage effect applies
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID (see *DEFINE_CURVE) defining the chemical shrinkage coefficient, β, or a proxy in experiments for the chemical shrinkage coefficient, α as a function of temperature, T. If α as a function of T is defined, α is converted to the chemical shrinkage coefficient by LS-DYNA
















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
   :value: 'ADD_CHEM_SHRINKAGE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





