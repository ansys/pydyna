





:class:`InterfaceWeldlineDevelopment`
=====================================


.. py:class:: interface_weldline_development.InterfaceWeldlineDevelopment(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INTERFACE_WELDLINE_DEVELOPMENT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InterfaceWeldlineDevelopment

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ioption`
            - Get or set the Welding curve development options:


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

    from interface_weldline_development import InterfaceWeldlineDevelopment

Property detail
---------------

.. py:property:: ioption
   :type: int


   
   Get or set the Welding curve development options:
   EQ.1:   Calculate initial weld curve from final(given) weld curve, with output file name weldline.ibo, which will be on the initial blank mesh.
   EQ. - 1 : Calculate final weld curve from initial weld curve, with output file name weldline_f.ibo, which will be on the formed blank mesh.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INTERFACE'


.. py:attribute:: subkeyword
   :value: 'WELDLINE_DEVELOPMENT'






