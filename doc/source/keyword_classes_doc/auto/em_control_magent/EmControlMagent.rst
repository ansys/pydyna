





:class:`EmControlMagent`
========================


.. py:class:: em_control_magent.EmControlMagent(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_CONTROL_MAGENT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmControlMagent

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mcomp`
            - Get or set the Magnetization vector recomputation:


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

    from em_control_magent import EmControlMagent

Property detail
---------------

.. py:property:: mcomp
   :type: int


   
   Get or set the Magnetization vector recomputation:
   EQ.0:   Off.See Remark 1.
   EQ.1 : On.Magnetization recomputation is controlled by NCYCM.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'CONTROL_MAGENT'






