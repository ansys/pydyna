





:class:`MatAddPropertyDependenceTime`
=====================================


.. py:class:: mat_add_property_dependence_time.MatAddPropertyDependenceTime(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_ADD_PROPERTY_DEPENDENCE_TIME keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatAddPropertyDependenceTime

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification for which the property dependence applies
          * - :py:attr:`~prop`
            - Get or set the Name of the property (same as the variable for a material model in keyword card). For example, “E” is used for Young’s modulus in *MAT_‌ELASTIC
          * - :py:attr:`~lcid`
            - Get or set the Curve ID to define the property dependence. For the FREQ keyword option, the abscissa values define frequency; for the TIME keyword option, the abscissa values define time. The ordinate values define the property at each frequency or each time
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

    from mat_add_property_dependence_time import MatAddPropertyDependenceTime

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification for which the property dependence applies
















   ..
       !! processed by numpydoc !!

.. py:property:: prop
   :type: Optional[str]


   
   Get or set the Name of the property (same as the variable for a material model in keyword card). For example, “E” is used for Young’s modulus in *MAT_‌ELASTIC
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Curve ID to define the property dependence. For the FREQ keyword option, the abscissa values define frequency; for the TIME keyword option, the abscissa values define time. The ordinate values define the property at each frequency or each time
















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
   :value: 'ADD_PROPERTY_DEPENDENCE_TIME'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





