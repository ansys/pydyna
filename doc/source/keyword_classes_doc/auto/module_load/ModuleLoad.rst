





:class:`ModuleLoad`
===================


.. py:class:: module_load.ModuleLoad(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MODULE_LOAD keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ModuleLoad

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mdlid`
            - Get or set the Module identification. A unique string label must be specified.
          * - :py:attr:`~title`
            - Get or set the Description of the module.
          * - :py:attr:`~filename`
            - Get or set the File name of the library to be loaded, 80 characters maximum. If the file name has no path component, LS-DYNA will search in all directories specified in *MODULE_PATH first. If not found and the file name starts with �+� (a plus sign), LS-DYNA will continue to search all directories specified in the system environment variable LD_LIBRARY_PATH..


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

    from module_load import ModuleLoad

Property detail
---------------

.. py:property:: mdlid
   :type: Optional[int]


   
   Get or set the Module identification. A unique string label must be specified.
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Description of the module.
















   ..
       !! processed by numpydoc !!

.. py:property:: filename
   :type: Optional[str]


   
   Get or set the File name of the library to be loaded, 80 characters maximum. If the file name has no path component, LS-DYNA will search in all directories specified in *MODULE_PATH first. If not found and the file name starts with �+� (a plus sign), LS-DYNA will continue to search all directories specified in the system environment variable LD_LIBRARY_PATH..
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'MODULE'


.. py:attribute:: subkeyword
   :value: 'LOAD'






