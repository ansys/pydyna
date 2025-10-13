





:class:`InterfaceCompensation3DMultiSteps`
==========================================


.. py:class:: interface_compensation_3d_multi_steps.InterfaceCompensation3DMultiSteps(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INTERFACE_COMPENSATION_3D_MULTI_STEPS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InterfaceCompensation3DMultiSteps

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~method`
            - Get or set the There are several extrapolation methods for the addendum and binder outside of trim lines, see Remarks.
          * - :py:attr:`~sl`
            - Get or set the The smooth level parameter controls the smoothness of the modified
          * - :py:attr:`~sf`
            - Get or set the Shape compensation scale factor. The value scales the spring back
          * - :py:attr:`~elref`
            - Get or set the Element refinement option:
          * - :py:attr:`~psidp`
            - Get or set the Define the part set ID for primary parts of the tooling.  Properly choosing the parts for the primary side is important since it affects what kinds of modifications will be made to the tooling. Usually, only one side of the tool will be chosen as the primary side, and the modifications made to the other side (secondary side) depend solely on the changes in the primary side.  This specification allows the two sides to be coupled while maintaining a constant (tool) gap between the two sides.  If both sides are chosen to be primary, the gap between the two sides might change and become inhomogeneous.
          * - :py:attr:`~undct`
            - Get or set the Tool undercut treatment option:
          * - :py:attr:`~angle`
            - Get or set the An angle defining the undercut.
          * - :py:attr:`~nlinear`
            - Get or set the Activate nonlinear extrapolation.


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

    from interface_compensation_3d_multi_steps import InterfaceCompensation3DMultiSteps

Property detail
---------------

.. py:property:: method
   :type: int


   
   Get or set the There are several extrapolation methods for the addendum and binder outside of trim lines, see Remarks.
















   ..
       !! processed by numpydoc !!

.. py:property:: sl
   :type: float


   
   Get or set the The smooth level parameter controls the smoothness of the modified
   surfaces. A large value makes the surface smoother. Typically the value ranges from 5 to 10. If spring back is large, the transition
   region is expected to be large. However, by using a smaller value of SL, the region of transition can be reduced.
















   ..
       !! processed by numpydoc !!

.. py:property:: sf
   :type: float


   
   Get or set the Shape compensation scale factor. The value scales the spring back
   amount of the blank and the scaled amount is used to compensate the tooling.
   GT.0: compensate in the opposite direction of the spring back;
   LT.0: compensate in the punch moving direction (for undercut).
   This scale factor scales how much of the shape deviation is
   compensated. For example, if 10 mm of spring back is predicted,
   and the scale factor is chosen as 0.75, then the compensation in the
   opposite direction will only be 7.5 mm.
   Experience shows that the best scale factor for reaching a converged
   solution (within part tolerance) is case dependent. In some cases, a
   scale factor range of 0.5 to 0.75 is best; while in others, larger values
   are indicated. Sometimes, the best value can be larger than 1.1.
   Note that within an automatic compensation loop, this factor does               not need to be varied.
   Since it is impossible to choose the best value for each application up
   front 0.75 is recommended for the first attempt. If the spring back
   cannot be effectively compensated and the calculation diverges, the
   factor can be moved upward or downward to obtain a converged
   solution, or more iterations must be used with the initial trial value
   to compensate the remaining shape deviation.
   For channel shaped parts that have a twisting mode of spring back,
   the scale factor is more important. It was found that a small change
   of the tool shape might change the twisting mode. If this occurs,
   using a small value (<0.5) is suggested.
















   ..
       !! processed by numpydoc !!

.. py:property:: elref
   :type: int


   
   Get or set the Element refinement option:
   EQ.1: special element refinement is used with the tool elements (default);
   EQ.2: special element refinement is turned off.
















   ..
       !! processed by numpydoc !!

.. py:property:: psidp
   :type: Optional[float]


   
   Get or set the Define the part set ID for primary parts of the tooling.  Properly choosing the parts for the primary side is important since it affects what kinds of modifications will be made to the tooling. Usually, only one side of the tool will be chosen as the primary side, and the modifications made to the other side (secondary side) depend solely on the changes in the primary side.  This specification allows the two sides to be coupled while maintaining a constant (tool) gap between the two sides.  If both sides are chosen to be primary, the gap between the two sides might change and become inhomogeneous.
   When using METHOD 7, the choice of primary side will affect the result when applied to three-piece draw models.  At this time, when the punch and binder are chosen as the primary side, the binder region will not be changed.  Otherwise, when the die is chosen as primary side, the binder will be changed since the changes extend to the edges of the primary tool
















   ..
       !! processed by numpydoc !!

.. py:property:: undct
   :type: Optional[float]


   
   Get or set the Tool undercut treatment option:
   EQ.0: no check (default);
   EQ.1: check and fix undercut.
















   ..
       !! processed by numpydoc !!

.. py:property:: angle
   :type: float


   
   Get or set the An angle defining the undercut.
















   ..
       !! processed by numpydoc !!

.. py:property:: nlinear
   :type: int


   
   Get or set the Activate nonlinear extrapolation.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INTERFACE'


.. py:attribute:: subkeyword
   :value: 'COMPENSATION_3D_MULTI_STEPS'






