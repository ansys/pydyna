





:class:`HourglassCardSet`
=========================


.. py:class:: hourglass.HourglassCardSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.cards.Cards`


   
   CardSet.
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: HourglassCardSet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~hgid`
            - Get or set the Hourglass ID.  A unique number or label must be specified.  This ID is referenced by HGID in the *PART command.
          * - :py:attr:`~ihq`
            - Get or set the Hourglass control type. For solid elements six options are available. For quadrilateral shell and membrane elements the hourglass control is based on the formulation of Belytschko and Tsay, i.e., options 1-3 are identical, and options 4-6 are identical:
          * - :py:attr:`~qm`
            - Get or set the Hourglass coefficient. Values of QM that exceed 0.15 (for IHQ<6) may cause instabilities. Values of QM that exceed .15 (for IHQ<6) may cause instabilities. The recommended default applies to all options. The stiffness forms, however, can stiffen the response especially if deformations are large and therefore should be used with care. For the shell and membrane elements QM is taken as the membrane hourglass coefficient, the bending as QB, and warping as QW. These coefficients can be specified independently, but generally, QM=QB=QW, is adequate. For type 6 solid element hourglass control, QM=1.0 gives an accurate coarse mesh bending stiffness that does not lock in the incompressible limit. For type 6 values such as 0.001-0.01 will avoid an overly stiff response.
          * - :py:attr:`~ibq`
            - Get or set the Not used.  Bulk viscosity is always on for solids.  Bulk viscosity for beams and shells can only be turned on using the variable TYPE in *CONTROL_‌BULK_‌VISCOSITY; however, the coefficients can be set using Q1 and Q2 below.
          * - :py:attr:`~q1`
            - Get or set the Quadratic bulk viscosity coefficient.
          * - :py:attr:`~q2`
            - Get or set the Linear bulk viscosity coefficient.
          * - :py:attr:`~qb_vdc`
            - Get or set the Hourglass coefficient for shell bending. The default is QB=QM.
          * - :py:attr:`~qw`
            - Get or set the Hourglass coefficient for shell warping. The default is QB=QW.
          * - :py:attr:`~title`
            - Get or set the Additional title line
          * - :py:attr:`~parent`
            - Get the parent keyword.


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~option_specs`
            - Get the card format type.






Import detail
-------------

.. code-block:: python

    from hourglass import HourglassCardSet

Property detail
---------------

.. py:property:: hgid
   :type: int


   
   Get or set the Hourglass ID.  A unique number or label must be specified.  This ID is referenced by HGID in the *PART command.
















   ..
       !! processed by numpydoc !!

.. py:property:: ihq
   :type: int


   
   Get or set the Hourglass control type. For solid elements six options are available. For quadrilateral shell and membrane elements the hourglass control is based on the formulation of Belytschko and Tsay, i.e., options 1-3 are identical, and options 4-6 are identical:
   EQ.0: default=1 regardless of IHQ in *control_hourglass,
   EQ.1:standard LS-DYNA viscous form,
   EQ.2:Flanagan-Belytschko viscous form,
   EQ.3: Flanagan-Belytschko viscous form with exact volume integration for solid elements,
   EQ.4:Flanagan-Belytschko stiffness form,
   EQ.5:Flanagan-Belytschko stiffness form with exact volume integration for solid elements,
   EQ:6:Belytschko-Bindeman [1993] assumed strain co-rotational stiffness form for 2D and 3D solid elements only. This form is available for explicit and IMPLICIT solution medhods. Type 6 is mandatory for the implicit options,
   EQ.7:  Linear total strain form of type 6 hourglass control.  This form is available for explicit and implicit solution method (See remark 6 below).
   EQ:8:Applicable to the type 16 fully integrated shell element.
   IHQ=8 EQ.8:Activates the full projection warping stiffness for shell formulations 16 and -16, and is the default for these formulations.  A speed penalty of 25% is common for this option.
   EQ.9:   Puso [2000] enhanced assumed strain stiffness form for 3D hexahedral elements.
   EQ.10:  Cosserat Point Element (CPE) developed by Jabareen and Rubin [2008] and Jabareen et.al. [2013], see *CONTROL_HOURGLASS
















   ..
       !! processed by numpydoc !!

.. py:property:: qm
   :type: float


   
   Get or set the Hourglass coefficient. Values of QM that exceed 0.15 (for IHQ<6) may cause instabilities. Values of QM that exceed .15 (for IHQ<6) may cause instabilities. The recommended default applies to all options. The stiffness forms, however, can stiffen the response especially if deformations are large and therefore should be used with care. For the shell and membrane elements QM is taken as the membrane hourglass coefficient, the bending as QB, and warping as QW. These coefficients can be specified independently, but generally, QM=QB=QW, is adequate. For type 6 solid element hourglass control, QM=1.0 gives an accurate coarse mesh bending stiffness that does not lock in the incompressible limit. For type 6 values such as 0.001-0.01 will avoid an overly stiff response.
















   ..
       !! processed by numpydoc !!

.. py:property:: ibq
   :type: Optional[int]


   
   Get or set the Not used.  Bulk viscosity is always on for solids.  Bulk viscosity for beams and shells can only be turned on using the variable TYPE in *CONTROL_‌BULK_‌VISCOSITY; however, the coefficients can be set using Q1 and Q2 below.
















   ..
       !! processed by numpydoc !!

.. py:property:: q1
   :type: float


   
   Get or set the Quadratic bulk viscosity coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: q2
   :type: float


   
   Get or set the Linear bulk viscosity coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: qb_vdc
   :type: float


   
   Get or set the Hourglass coefficient for shell bending. The default is QB=QM.
















   ..
       !! processed by numpydoc !!

.. py:property:: qw
   :type: float


   
   Get or set the Hourglass coefficient for shell warping. The default is QB=QW.
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Additional title line
















   ..
       !! processed by numpydoc !!

.. py:property:: parent
   :type: ansys.dyna.core.lib.keyword_base.KeywordBase


   
   Get the parent keyword.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





