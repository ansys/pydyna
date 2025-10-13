





:class:`ControlConstranined`
============================


.. py:class:: control_constranined.ControlConstranined(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_CONSTRANINED keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlConstranined

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sprchk`
            - Get or set the SPR2/SPR3 initialization check:


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

    from control_constranined import ControlConstranined

Property detail
---------------

.. py:property:: sprchk
   :type: int


   
   Get or set the SPR2/SPR3 initialization check:
   EQ.0:   automatically increase search radius to find enough nodes(default)
   EQ.1 : same as 0 but also write a warning
   EQ.2 : error termination if not enough nodes found immediately
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'CONSTRANINED'






