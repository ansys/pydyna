





:class:`DefineDeInjectShape`
============================


.. py:class:: define_de_inject_shape.DefineDeInjectShape(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_DE_INJECT_SHAPE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineDeInjectShape

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the The ID of the shape pattern
          * - :py:attr:`~nde`
            - Get or set the Number of DEs in this pattern
          * - :py:attr:`~iauto`
            - Get or set the Flag for how to specify the bonded shape patterns:
          * - :py:attr:`~itype`
            - Get or set the Bond particles patterns when IAUTO = 1:
          * - :py:attr:`~x`
            - Get or set the Coordinates of the DEs in this pattern.
          * - :py:attr:`~y`
            - Get or set the Coordinates of the DEs in this pattern.
          * - :py:attr:`~z`
            - Get or set the Coordinates of the DEs in this pattern.
          * - :py:attr:`~r`
            - Get or set the Radii of the DEs in this pattern
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

    from define_de_inject_shape import DefineDeInjectShape

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the The ID of the shape pattern
















   ..
       !! processed by numpydoc !!

.. py:property:: nde
   :type: Optional[int]


   
   Get or set the Number of DEs in this pattern
















   ..
       !! processed by numpydoc !!

.. py:property:: iauto
   :type: int


   
   Get or set the Flag for how to specify the bonded shape patterns:
   EQ.0:   Give each particle’s relative position and radius
   EQ.1 : Use predefined pattern types
















   ..
       !! processed by numpydoc !!

.. py:property:: itype
   :type: int


   
   Get or set the Bond particles patterns when IAUTO = 1:
   EQ.1:   Line
   EQ.2 : Cuboid
   EQ.3 : Prism with equilateral triangle faces
















   ..
       !! processed by numpydoc !!

.. py:property:: x
   :type: Optional[float]


   
   Get or set the Coordinates of the DEs in this pattern.
















   ..
       !! processed by numpydoc !!

.. py:property:: y
   :type: Optional[float]


   
   Get or set the Coordinates of the DEs in this pattern.
















   ..
       !! processed by numpydoc !!

.. py:property:: z
   :type: Optional[float]


   
   Get or set the Coordinates of the DEs in this pattern.
















   ..
       !! processed by numpydoc !!

.. py:property:: r
   :type: Optional[float]


   
   Get or set the Radii of the DEs in this pattern
















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
   :value: 'DE_INJECT_SHAPE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





