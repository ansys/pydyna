





:class:`InitialCrashfront`
==========================


.. py:class:: initial_crashfront.InitialCrashfront(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INITIAL_CRASHFRONT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InitialCrashfront

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the Set ID from which the initial crashfront nodes are defined
          * - :py:attr:`~stype`
            - Get or set the ID type of SID:


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

    from initial_crashfront import InitialCrashfront

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Set ID from which the initial crashfront nodes are defined
















   ..
       !! processed by numpydoc !!

.. py:property:: stype
   :type: Optional[int]


   
   Get or set the ID type of SID:
   EQ.0:   segment set ID,
   EQ.1 : shell element set ID,
   EQ.2 : part set ID,
   EQ.3 : part ID,
   EQ.4 : node set ID..
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INITIAL'


.. py:attribute:: subkeyword
   :value: 'CRASHFRONT'






