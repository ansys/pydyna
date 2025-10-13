





:class:`IgaIncludeBezier`
=========================


.. py:class:: iga_include_bezier.IgaIncludeBezier(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA IGA_INCLUDE_BEZIER keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IgaIncludeBezier

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~filename`
            - Get or set the Name of the file to be included;
          * - :py:attr:`~filetype`
            - Get or set the Type of the file to be included:EQ.1:    ASCII
          * - :py:attr:`~pid`
            - Get or set the Part ID
          * - :py:attr:`~dim`
            - Get or set the Parametric dimension:


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

    from iga_include_bezier import IgaIncludeBezier

Property detail
---------------

.. py:property:: filename
   :type: Optional[str]


   
   Get or set the Name of the file to be included;
















   ..
       !! processed by numpydoc !!

.. py:property:: filetype
   :type: Optional[int]


   
   Get or set the Type of the file to be included:EQ.1:    ASCII
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID
















   ..
       !! processed by numpydoc !!

.. py:property:: dim
   :type: Optional[int]


   
   Get or set the Parametric dimension:
   EQ.2:   Surface
   EQ.3 : Volume
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'IGA'


.. py:attribute:: subkeyword
   :value: 'INCLUDE_BEZIER'






