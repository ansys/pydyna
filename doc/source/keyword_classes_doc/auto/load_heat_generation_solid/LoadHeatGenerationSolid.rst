





:class:`LoadHeatGenerationSolid`
================================


.. py:class:: load_heat_generation_solid.LoadHeatGenerationSolid(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_HEAT_GENERATION_SOLID keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadHeatGenerationSolid

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the Solid element ID, *ELEMENT_SOLID.
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID for volumetric heat generation rate:
          * - :py:attr:`~cmult`
            - Get or set the Curve multiplier for  heat generation rate (LCID). Depending on the definition of LCID this value is either used for scaling or for constant heat generation.
          * - :py:attr:`~wblcid`
            - Get or set the Load curve ID defining the blood persusion rate as a function of time
          * - :py:attr:`~cblcid`
            - Get or set the Load curve ID defining the blood heat capacity as a function of the blood temperature
          * - :py:attr:`~tblcid`
            - Get or set the Load curve ID defining the blood temperature as a function of time


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~keyword`
            - 
          * - :py:attr:`~subkeyword`
            - 






Import detail
-------------

.. code-block:: python

    from load_heat_generation_solid import LoadHeatGenerationSolid

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Solid element ID, *ELEMENT_SOLID.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID for volumetric heat generation rate:
   GT.0: function versus time,
   EQ.0: use multiplier value CMULT only,
   LT.0: function versus temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: cmult
   :type: float


   
   Get or set the Curve multiplier for  heat generation rate (LCID). Depending on the definition of LCID this value is either used for scaling or for constant heat generation.
















   ..
       !! processed by numpydoc !!

.. py:property:: wblcid
   :type: Optional[int]


   
   Get or set the Load curve ID defining the blood persusion rate as a function of time
















   ..
       !! processed by numpydoc !!

.. py:property:: cblcid
   :type: Optional[int]


   
   Get or set the Load curve ID defining the blood heat capacity as a function of the blood temperature
















   ..
       !! processed by numpydoc !!

.. py:property:: tblcid
   :type: Optional[int]


   
   Get or set the Load curve ID defining the blood temperature as a function of time
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'HEAT_GENERATION_SOLID'






