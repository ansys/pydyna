





:class:`ControlBulkViscosity`
=============================


.. py:class:: control_bulk_viscosity.ControlBulkViscosity(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_BULK_VISCOSITY keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlBulkViscosity

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~q1`
            - Get or set the Default quadratic viscosity coefficient (default = 1.5).
          * - :py:attr:`~q2`
            - Get or set the Default linear viscosity coefficient (default = 0.06).
          * - :py:attr:`~type`
            - Get or set the Default bulk viscosity type, IBQ (default = 1):
          * - :py:attr:`~btype`
            - Get or set the Beam bulk viscosity type (Default=0)
          * - :py:attr:`~tstype`
            - Get or set the Beam bulk viscosity for thick shells (default = 0):


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

    from control_bulk_viscosity import ControlBulkViscosity

Property detail
---------------

.. py:property:: q1
   :type: float


   
   Get or set the Default quadratic viscosity coefficient (default = 1.5).
















   ..
       !! processed by numpydoc !!

.. py:property:: q2
   :type: float


   
   Get or set the Default linear viscosity coefficient (default = 0.06).
















   ..
       !! processed by numpydoc !!

.. py:property:: type
   :type: int


   
   Get or set the Default bulk viscosity type, IBQ (default = 1):
   EQ.-2: same as - 1 but the internal energy dissipated by the viscosity in the shell elements is computed and included in the overall energy balance.
   EQ.-1: same as 1 but also includes viscosity in shell formulations 2, 4, 10, 16,and 17.  The internal energy is not computed in the shell elements.
   EQ.1: standard bulk viscosity.Solid elements only and internal energy is always computed and included in the overall energy balance.
   EQ.2: Richards - Wilkins bulk viscosity.Two - dimensional plane strain and axisymmetric solid elements only.
   Internal energy is always computed and included in the overall energy balance.
















   ..
       !! processed by numpydoc !!

.. py:property:: btype
   :type: int


   
   Get or set the Beam bulk viscosity type (Default=0)
   EQ. 0: The bulk viscosity is turned off for beams.
   EQ. 1: The bulk viscosity is turned on for beam types 1 and 11. The energy contribution is not included in the overall energy balance.
   EQ. 2: The bulk viscosity is turned on for beam type 1 and 11.  The energy contribution is included in the overall energy balance.
















   ..
       !! processed by numpydoc !!

.. py:property:: tstype
   :type: int


   
   Get or set the Beam bulk viscosity for thick shells (default = 0):
   EQ.0:   The bulk viscosity is turned off for thick shells.
   EQ.1:   The bulk viscosity is turned on for thick shells forms 5, 6 and 7
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'BULK_VISCOSITY'






