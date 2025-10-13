





:class:`LoadStiffenPart`
========================


.. py:class:: load_stiffen_part.LoadStiffenPart(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_STIFFEN_PART keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadStiffenPart

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID
          * - :py:attr:`~lc`
            - Get or set the Direction: enter 1, 2 or 3 for X, Y or Z
          * - :py:attr:`~stga`
            - Get or set the Construction stage at which part is added (optional)
          * - :py:attr:`~stgr`
            - Get or set the Construction stage at which part is removed (optional)


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

    from load_stiffen_part import LoadStiffenPart

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID
















   ..
       !! processed by numpydoc !!

.. py:property:: lc
   :type: Optional[int]


   
   Get or set the Direction: enter 1, 2 or 3 for X, Y or Z
















   ..
       !! processed by numpydoc !!

.. py:property:: stga
   :type: Optional[int]


   
   Get or set the Construction stage at which part is added (optional)
















   ..
       !! processed by numpydoc !!

.. py:property:: stgr
   :type: Optional[int]


   
   Get or set the Construction stage at which part is removed (optional)
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'STIFFEN_PART'






