





:class:`DefineCrashfront`
=========================


.. py:class:: define_crashfront.DefineCrashfront(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_CRASHFRONT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineCrashfront

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the Crash front node set ID for initial crashfront nodes
          * - :py:attr:`~type`
            - Get or set the ID type of SID:
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

    from define_crashfront import DefineCrashfront

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Crash front node set ID for initial crashfront nodes
















   ..
       !! processed by numpydoc !!

.. py:property:: type
   :type: int


   
   Get or set the ID type of SID:
   EQ.0:   segment set ID,
   EQ.1:   shell element set ID,
   EQ.2:   part set ID,
   EQ.3:   part ID,
   EQ.4:   node set ID.
















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
   :value: 'CRASHFRONT'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





