





:class:`MatPmlElasticFluid`
===========================


.. py:class:: mat_pml_elastic_fluid.MatPmlElasticFluid(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_PML_ELASTIC_FLUID keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatPmlElasticFluid

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification.  A unique number or label must be specified.(see *PART)
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~k`
            - Get or set the Bulk modulus.
          * - :py:attr:`~vc`
            - Get or set the Tensor viscosity coefficient.
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

    from mat_pml_elastic_fluid import MatPmlElasticFluid

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification.  A unique number or label must be specified.(see *PART)
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density.
















   ..
       !! processed by numpydoc !!

.. py:property:: k
   :type: Optional[float]


   
   Get or set the Bulk modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: vc
   :type: Optional[float]


   
   Get or set the Tensor viscosity coefficient.
















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
   :value: 'PML_ELASTIC_FLUID'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





