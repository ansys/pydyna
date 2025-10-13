





:class:`MatGeneralNonlinear1DofDiscreteBeam`
============================================


.. py:class:: mat_general_nonlinear_1dof_discrete_beam.MatGeneralNonlinear1DofDiscreteBeam(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_GENERAL_NONLINEAR_1DOF_DISCRETE_BEAM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatGeneralNonlinear1DofDiscreteBeam

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification.  A unique number has to be chosen.
          * - :py:attr:`~ro`
            - Get or set the Mass density, see also volume in *SECTION_BEAM definition.
          * - :py:attr:`~k`
            - Get or set the Translational stiffness for unloading option 2.0.
          * - :py:attr:`~iunld`
            - Get or set the Unloading option (Also see Figure 20.34 in the User's manual):
          * - :py:attr:`~offset`
            - Get or set the Offset to determine a permanent set upon unloading if the UNLDOPT=3.0.  The permanent sets in compression and tension are equal to the product of this offset value and the maximum compressive and tensile displacements, respectively.
          * - :py:attr:`~dampf`
            - Get or set the Damping factor for stability.  Values in the neighborhood of unity are recommended.  This damping factor is properly scaled to eliminate time step size dependency.  Also, it is active if and only if the local stiffness is defined.
          * - :py:attr:`~lcidt`
            - Get or set the Load curve ID defining translational force resultant along the axis versus relative translational displacement.  If zero, no stiffness related forces are generated for this degree of freedom.  The loading curves must be defined from the most negative displacement to the most positive displacement.  The force does not need to increase montonically for the loading curve.  The curves in this input are extrapolated when the displacement range falls outside the curve definition.
          * - :py:attr:`~lcidtu`
            - Get or set the Load curve ID defining translational force resultant along the axis versus relative translational displacement during unloading.  The force values defined by this curve must increase monotonically from the most negative displacement to the most positive displacement.  For UNLDOPT=1.0, the slope of this curve must equal or exceed the loading curve for stability reasons.  This is not the case for UNLDOPT=2.0.   For loading and unloading to follow the same path simply set LCIDTU=LCIDT.
          * - :py:attr:`~lcidtd`
            - Get or set the Load curve ID defining translational damping force resultant along local the axis versus relative translational velocity.
          * - :py:attr:`~lcidte`
            - Get or set the Load curve ID defining translational damping force scale factor versus relative displacement in along axis.
          * - :py:attr:`~utfail`
            - Get or set the Optional, translational displacement at failure in tension.  If zero, failure in tension is not considered.
          * - :py:attr:`~ucfail`
            - Get or set the Optional, translational displacement at failure in compression.  If zero, failure in compression is not considered.
          * - :py:attr:`~iu`
            - Get or set the Initial translational displacement along axis.
          * - :py:attr:`~title`
            - Get or set the Additional title line


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~keyword`
            - 
          * - :py:attr:`~subkeyword`
            - 
          * - :py:attr:`~option_specs`
            - Get the card format type.






Import detail
-------------

.. code-block:: python

    from mat_general_nonlinear_1dof_discrete_beam import MatGeneralNonlinear1DofDiscreteBeam

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification.  A unique number has to be chosen.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density, see also volume in *SECTION_BEAM definition.
















   ..
       !! processed by numpydoc !!

.. py:property:: k
   :type: Optional[float]


   
   Get or set the Translational stiffness for unloading option 2.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: iunld
   :type: Optional[int]


   
   Get or set the Unloading option (Also see Figure 20.34 in the User's manual):
   EQ.0.0:  Loading and unloading follow loading curve
   EQ.1.0:  Loading follows loading curve, unloading follows loading curve.  (Also see Figure 20.35 in the User's Manual).  The unloading curve ID if defined is ignored.
   EQ.2.0:  Loading follows loading curve, unloading follows unloading stiffness, K,  to the unloading curve.  The loading and unloading curves intersect at the origin of the axes.
   EQ.3.0:  Quadratic unloading from peak displacement value to permanent set.
















   ..
       !! processed by numpydoc !!

.. py:property:: offset
   :type: Optional[float]


   
   Get or set the Offset to determine a permanent set upon unloading if the UNLDOPT=3.0.  The permanent sets in compression and tension are equal to the product of this offset value and the maximum compressive and tensile displacements, respectively.
















   ..
       !! processed by numpydoc !!

.. py:property:: dampf
   :type: Optional[float]


   
   Get or set the Damping factor for stability.  Values in the neighborhood of unity are recommended.  This damping factor is properly scaled to eliminate time step size dependency.  Also, it is active if and only if the local stiffness is defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidt
   :type: Optional[int]


   
   Get or set the Load curve ID defining translational force resultant along the axis versus relative translational displacement.  If zero, no stiffness related forces are generated for this degree of freedom.  The loading curves must be defined from the most negative displacement to the most positive displacement.  The force does not need to increase montonically for the loading curve.  The curves in this input are extrapolated when the displacement range falls outside the curve definition.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidtu
   :type: Optional[int]


   
   Get or set the Load curve ID defining translational force resultant along the axis versus relative translational displacement during unloading.  The force values defined by this curve must increase monotonically from the most negative displacement to the most positive displacement.  For UNLDOPT=1.0, the slope of this curve must equal or exceed the loading curve for stability reasons.  This is not the case for UNLDOPT=2.0.   For loading and unloading to follow the same path simply set LCIDTU=LCIDT.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidtd
   :type: Optional[int]


   
   Get or set the Load curve ID defining translational damping force resultant along local the axis versus relative translational velocity.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidte
   :type: Optional[int]


   
   Get or set the Load curve ID defining translational damping force scale factor versus relative displacement in along axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: utfail
   :type: Optional[float]


   
   Get or set the Optional, translational displacement at failure in tension.  If zero, failure in tension is not considered.
















   ..
       !! processed by numpydoc !!

.. py:property:: ucfail
   :type: Optional[float]


   
   Get or set the Optional, translational displacement at failure in compression.  If zero, failure in compression is not considered.
















   ..
       !! processed by numpydoc !!

.. py:property:: iu
   :type: Optional[float]


   
   Get or set the Initial translational displacement along axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Additional title line
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'MAT'


.. py:attribute:: subkeyword
   :value: 'GENERAL_NONLINEAR_1DOF_DISCRETE_BEAM'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





