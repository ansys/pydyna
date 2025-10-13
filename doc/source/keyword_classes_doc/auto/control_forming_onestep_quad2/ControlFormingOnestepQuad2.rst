





:class:`ControlFormingOnestepQuad2`
===================================


.. py:class:: control_forming_onestep_quad2.ControlFormingOnestepQuad2(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_FORMING_ONESTEP_QUAD2 keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlFormingOnestepQuad2

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~option`
            - Get or set the One-step solution method: EQ.7: Invokes a one-step solution with blank unfolding that accounts for part undercut.
          * - :py:attr:`~tsclmax`
            - Get or set the If not zero, it defines a thickness scale factor limiting the maximum thickness in the part.
          * - :py:attr:`~autobd`
            - Get or set the Apply a fraction of a fully locked bead force along the entire periphery of the blank.  The fully locked bead force is automatically calculated based on a material hardening curve input.  AUTOBD can be increased to easily introduce more thinning and effective plastic strain in the part.
          * - :py:attr:`~tsclmin`
            - Get or set the If not zero, it defines a thickness scale factor limiting the maximum thickness reduction.
          * - :py:attr:`~epsmax`
            - Get or set the If not zero, it defines the maximum effective plastic strain allowed. All computed effective plastic strains that are greater than this value in the blank will be set to this value.
          * - :py:attr:`~lcsdg`
            - Get or set the Load curve ID defining equivalent plastic strain to failure vs. stress triaxiality, see *MAT_ADD_EROSION.
          * - :py:attr:`~dmgexp`
            - Get or set the Exponent for nonlinear damage accumulation, see *MAT_ADD_EROSION.  Damage accumulation is written as history variable #6 in the file onestepresult.
          * - :py:attr:`~flatname`
            - Get or set the File name of the initial unfolded blank by LS-PrePost (see remarks). This is needed only for the OPTION=6.  Leave a blank line for OPTION=7.


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

    from control_forming_onestep_quad2 import ControlFormingOnestepQuad2

Property detail
---------------

.. py:property:: option
   :type: int


   
   Get or set the One-step solution method: EQ.7: Invokes a one-step solution with blank unfolding that accounts for part undercut.
















   ..
       !! processed by numpydoc !!

.. py:property:: tsclmax
   :type: float


   
   Get or set the If not zero, it defines a thickness scale factor limiting the maximum thickness in the part.
   For example, if the maximum thickness allowed is 0.8mm for a blank with initial thickness of 0.75mm TSCLMAX can be set to 1.0667.  All thicknesses that are computed as more than 0.8mm in the sheet blank will be reset to 0.8mm.  The scale factor is useful in advance feasibility analysis where part design and stamping process have not been finalized and could potentially cause large splits or severe wrinkles during unfolding, rendering the forming results unusable for crash/safety simulation..
















   ..
       !! processed by numpydoc !!

.. py:property:: autobd
   :type: float


   
   Get or set the Apply a fraction of a fully locked bead force along the entire periphery of the blank.  The fully locked bead force is automatically calculated based on a material hardening curve input.  AUTOBD can be increased to easily introduce more thinning and effective plastic strain in the part.
   LT.0.0: Turns off the “auto-bead” feature.
   EQ.0.0: Automatically applies 30% of fully locked force.
   GT.0.0: Fraction input will be used to scale the fully locked force.
















   ..
       !! processed by numpydoc !!

.. py:property:: tsclmin
   :type: float


   
   Get or set the If not zero, it defines a thickness scale factor limiting the maximum thickness reduction.
   For example, if the minimum thickness allowed is 0.6mm for a blank with initial thickness of 0.75mm TSCLMIN can be set to 0.8.  All thicknesses that are computed as less than 0.6mm in the sheet blank will be reset to 0.6mm.  The scale factor is useful in advance feasibility analysis where part design and stamping process have not been finalized and could potentially cause large splits or severe wrinkles during unfolding, rendering the forming results unusable for crash/safety simulation.
















   ..
       !! processed by numpydoc !!

.. py:property:: epsmax
   :type: float


   
   Get or set the If not zero, it defines the maximum effective plastic strain allowed. All computed effective plastic strains that are greater than this value in the blank will be set to this value.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcsdg
   :type: Optional[int]


   
   Get or set the Load curve ID defining equivalent plastic strain to failure vs. stress triaxiality, see *MAT_ADD_EROSION.
















   ..
       !! processed by numpydoc !!

.. py:property:: dmgexp
   :type: Optional[float]


   
   Get or set the Exponent for nonlinear damage accumulation, see *MAT_ADD_EROSION.  Damage accumulation is written as history variable #6 in the file onestepresult.
















   ..
       !! processed by numpydoc !!

.. py:property:: flatname
   :type: Optional[str]


   
   Get or set the File name of the initial unfolded blank by LS-PrePost (see remarks). This is needed only for the OPTION=6.  Leave a blank line for OPTION=7.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'FORMING_ONESTEP_QUAD2'






