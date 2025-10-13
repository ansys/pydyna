





:class:`InterfaceComponentFile`
===============================


.. py:class:: interface_component_file.InterfaceComponentFile(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INTERFACE_COMPONENT_FILE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InterfaceComponentFile

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~filename`
            - Get or set the Name of the file where the component data will be written.
          * - :py:attr:`~format`
            - Get or set the File format to use:


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

    from interface_component_file import InterfaceComponentFile

Property detail
---------------

.. py:property:: filename
   :type: Optional[str]


   
   Get or set the Name of the file where the component data will be written.
















   ..
       !! processed by numpydoc !!

.. py:property:: format
   :type: int


   
   Get or set the File format to use:
   EQ.1: Use old binary file format
   EQ.2: Use new LSDA file format
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INTERFACE'


.. py:attribute:: subkeyword
   :value: 'COMPONENT_FILE'






