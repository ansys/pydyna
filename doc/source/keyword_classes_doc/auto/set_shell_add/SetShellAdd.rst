





:class:`SetShellAdd`
====================


.. py:class:: set_shell_add.SetShellAdd(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SET_SHELL_ADD keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SetShellAdd

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the Shell element set ID. All shell sets should have a unique set ID.
          * - :py:attr:`~shells`
            - dynamic array of shell set ids..
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

    from set_shell_add import SetShellAdd

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Shell element set ID. All shell sets should have a unique set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: shells
   :type: ansys.dyna.core.lib.series_card.SeriesCard


   
   dynamic array of shell set ids..
















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
   :value: 'SET'


.. py:attribute:: subkeyword
   :value: 'SHELL_ADD'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





