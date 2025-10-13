





:class:`IncludePathRelative`
============================


.. py:class:: include_path_relative.IncludePathRelative(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INCLUDE_PATH_RELATIVE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IncludePathRelative

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~path`
            - Get or set the define a directory in which to look for the include files.


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

    from include_path_relative import IncludePathRelative

Property detail
---------------

.. py:property:: path
   :type: Optional[str]


   
   Get or set the define a directory in which to look for the include files.
   If path length is greater then 80 chareaters, put space and '+' to continue on next line.
   Lsprepost will output that format automatically
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INCLUDE'


.. py:attribute:: subkeyword
   :value: 'PATH_RELATIVE'






