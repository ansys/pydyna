





:class:`DefineDeInternalSkip`
=============================


.. py:class:: define_de_internal_skip.DefineDeInternalSkip(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_DE_INTERNAL_SKIP keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineDeInternalSkip

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part set ID or part ID.  TYPE below indicates the ID type.
          * - :py:attr:`~type`
            - Get or set the PID type :
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

    from define_de_internal_skip import DefineDeInternalSkip

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part set ID or part ID.  TYPE below indicates the ID type.
















   ..
       !! processed by numpydoc !!

.. py:property:: type
   :type: int


   
   Get or set the PID type :
   EQ.0 : Part set
   EQ.1 : Part
















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
   :value: 'DE_INTERNAL_SKIP'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





