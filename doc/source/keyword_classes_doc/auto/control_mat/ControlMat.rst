





:class:`ControlMat`
===================


.. py:class:: control_mat.ControlMat(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_MAT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlMat

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~maef`
            - Get or set the EQ.0:    failure of  *MAT_ADD_EROSION definitions are active.
          * - :py:attr:`~umchk`
            - Get or set the User material check. Initially in the first calculation cycle, it is checked if true user-defined material models are applied or whether only the default, unmodified subroutines already present in the native dyn21 files are called.


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

    from control_mat import ControlMat

Property detail
---------------

.. py:property:: maef
   :type: int


   
   Get or set the EQ.0:    failure of  *MAT_ADD_EROSION definitions are active.
   EQ.1:   switch off all *MAT_ADD_EROSION definitions globally. This replaces the need to remove every *MAT_ADD_EROSION card in large models.
















   ..
       !! processed by numpydoc !!

.. py:property:: umchk
   :type: int


   
   Get or set the User material check. Initially in the first calculation cycle, it is checked if true user-defined material models are applied or whether only the default, unmodified subroutines already present in the native dyn21 files are called.
   EQ.0:   Warning is issued.
   EQ.1 : Error termination occurs.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'MAT'






