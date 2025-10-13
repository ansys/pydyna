





:class:`ControlAirbag`
======================


.. py:class:: control_airbag.ControlAirbag(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_AIRBAG keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlAirbag

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ckerr`
            - Get or set the Flag to check and report of CV airbag segments for the input


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

    from control_airbag import ControlAirbag

Property detail
---------------

.. py:property:: ckerr
   :type: int


   
   Get or set the Flag to check and report of CV airbag segments for the input
   a.open(free) edge
   b.segment should come from SHELL / SOLID element
   EQ.0:   Do not check(default).
   EQ.1 : Check for free edges,and if there is a free edge in the airbag surface, output nodes of the free edge to d3hsp, issue a warning,and continue the run.Check for segment,and if there is a segment not from an element, output the segment to d3hsp, issue a error messageand terminate the run.
   EQ.2 : Check for free edgesand if there is a free edge in the airbag surface, output nodes of the free edge to d3hsp, issue an error,and terminate the run.Check for segment,and if there is a segment not from an element, output the segment to d3hsp, issue a error messageand terminate the run.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'AIRBAG'






