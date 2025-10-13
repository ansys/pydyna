





:class:`EmControlErosion`
=========================


.. py:class:: em_control_erosion.EmControlErosion(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_CONTROL_EROSION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmControlErosion

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ectrl`
            - Get or set the Erosion search:


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

    from em_control_erosion import EmControlErosion

Property detail
---------------

.. py:property:: ectrl
   :type: int


   
   Get or set the Erosion search:
   EQ.0: Off. This means that the EM solver will ignore eroded elements and still consider them part of the EM problem.
   EQ.1: On. The EM solver will look for potential elements that are eroded and remove them from the EM solve by updating its matrix system.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'CONTROL_EROSION'






