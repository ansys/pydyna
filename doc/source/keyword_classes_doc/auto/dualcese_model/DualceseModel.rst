





:class:`DualceseModel`
======================


.. py:class:: dualcese_model.DualceseModel(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DUALCESE_MODEL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DualceseModel

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~unitsys`
            - Get or set the Name of the unit system of this dual CESE model (defined with *UNIT_SYSTEM).
          * - :py:attr:`~filename`
            - Get or set the Filename of the keyword file containing the dual CESE model. Note that only *DUALCESE_... keyword cards are allowed in this file


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

    from dualcese_model import DualceseModel

Property detail
---------------

.. py:property:: unitsys
   :type: Optional[str]


   
   Get or set the Name of the unit system of this dual CESE model (defined with *UNIT_SYSTEM).
   EQ.<BLANK>:     Use same units as the presumed units of the entire problem
















   ..
       !! processed by numpydoc !!

.. py:property:: filename
   :type: Optional[str]


   
   Get or set the Filename of the keyword file containing the dual CESE model. Note that only *DUALCESE_... keyword cards are allowed in this file
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DUALCESE'


.. py:attribute:: subkeyword
   :value: 'MODEL'






