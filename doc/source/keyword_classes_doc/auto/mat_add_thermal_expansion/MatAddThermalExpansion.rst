





:class:`MatAddThermalExpansion`
===============================


.. py:class:: mat_add_thermal_expansion.MatAddThermalExpansion(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_ADD_THERMAL_EXPANSION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatAddThermalExpansion

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID for which the thermal expansion property applies
          * - :py:attr:`~lcid`
            - Get or set the LCID.GT.0: Load curve ID defining thermal expansion coefficient as a function of temperature LCID.
          * - :py:attr:`~mult`
            - Get or set the Scale factor scaling load curve given by LCID
          * - :py:attr:`~multy`
            - Get or set the Scale factor scaling load curve given by LCIDY
          * - :py:attr:`~multz`
            - Get or set the Scale factor scaling load curve given by LCIDZ
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

    from mat_add_thermal_expansion import MatAddThermalExpansion

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID for which the thermal expansion property applies
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the LCID.GT.0: Load curve ID defining thermal expansion coefficient as a function of temperature LCID.
   EQ.0: Thermal expansion coefficient given by constant MULT
















   ..
       !! processed by numpydoc !!

.. py:property:: mult
   :type: float


   
   Get or set the Scale factor scaling load curve given by LCID
















   ..
       !! processed by numpydoc !!

.. py:property:: multy
   :type: float


   
   Get or set the Scale factor scaling load curve given by LCIDY
















   ..
       !! processed by numpydoc !!

.. py:property:: multz
   :type: float


   
   Get or set the Scale factor scaling load curve given by LCIDZ
















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
   :value: 'ADD_THERMAL_EXPANSION'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





