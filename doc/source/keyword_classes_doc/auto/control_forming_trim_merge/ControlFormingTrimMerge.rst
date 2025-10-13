





:class:`ControlFormingTrimMerge`
================================


.. py:class:: control_forming_trim_merge.ControlFormingTrimMerge(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_FORMING_TRIM_MERGE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlFormingTrimMerge

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~imerge`
            - Get or set the Activation flag.  Set to '1' (default) to activate this feature.
          * - :py:attr:`~gapm`
            - Get or set the Gap distance between two open ends of a trim loop curve in the model.


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

    from control_forming_trim_merge import ControlFormingTrimMerge

Property detail
---------------

.. py:property:: imerge
   :type: int


   
   Get or set the Activation flag.  Set to '1' (default) to activate this feature.
















   ..
       !! processed by numpydoc !!

.. py:property:: gapm
   :type: float


   
   Get or set the Gap distance between two open ends of a trim loop curve in the model.
   If the gap is smaller than GAPM, the two open ends of a trim curve will be closed automatically.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'FORMING_TRIM_MERGE'






