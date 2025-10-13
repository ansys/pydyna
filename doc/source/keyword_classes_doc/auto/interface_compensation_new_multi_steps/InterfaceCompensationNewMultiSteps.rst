





:class:`InterfaceCompensationNewMultiSteps`
===========================================


.. py:class:: interface_compensation_new_multi_steps.InterfaceCompensationNewMultiSteps(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INTERFACE_COMPENSATION_NEW_MULTI_STEPS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InterfaceCompensationNewMultiSteps

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~method`
            - Get or set the There are several extrapolation methods for the addendum and binder outside of trim lines, see Remarks
          * - :py:attr:`~sl`
            - Get or set the The smooth level parameter controls the smoothness of the modified surfaces. A large value makes the surface smoother. The commonly used value is between 5 and 10.  If springback is large, the transition region is expected to be large.  However, by using a smaller value of SL, the region of transition can be reduced
          * - :py:attr:`~sf`
            - Get or set the This scales how much of the shape deviation is compensated. For example, if 10 mm springback is predicted, and the scale factor is chosen as 0.75, then the compensation in the opposite direction will only be 7.5 mm.Through many parameter studies, it is found that the best scale factor is case dependent. For some cases, a scale factor of 0.75 is best, while for others, larger values are better. Sometimes, the best value can be larger than 1.1.   Since it is impossible to choose the best value for each application it is suggested that for a new application, the initial trial is 0.75.  If the springback cannot be effectively compensated, more iterations must be used to compensate the remaining shape deviation.For channel with twisting, the scale factor is more important. It was found that a small change of the tool shape might change the twisting mode.  If this occurs, using a small value (<0.5) is suggested.
          * - :py:attr:`~elref`
            - Get or set the EQ.1: special element refinement is used with the tool elements (default)
          * - :py:attr:`~psidm`
            - Get or set the Define the Part set ID for master parts.  It is important to properly choose the parts for the master side.  Usually, only one side (master side) of the tool will be chosen as the master side, and the modification of the other side (slave side) depends solely on the change, which occurs in the master side.  In this way, the two sides are coupled and a constant gap between the two sides is maintained. If both sides are chosen as master side, the gap between the two sides might change and the gap might become inhomogeneous.  The choice of Master side will have effect on the final result for method 7 for three-piece draw. At this time, when the punch and binder are chosen as the master side, the binder region will not be changed. Otherwise, when the die is chosen as Master side the binder will be changed, since the changes extend to the edges of the Master tool.
          * - :py:attr:`~undct`
            - Get or set the EQ.0: Default    EQ.1: Check and fix undercut.
          * - :py:attr:`~angle`
            - Get or set the An angle defining the undercut
          * - :py:attr:`~nlinea`
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

    from interface_compensation_new_multi_steps import InterfaceCompensationNewMultiSteps

Property detail
---------------

.. py:property:: method
   :type: int


   
   Get or set the There are several extrapolation methods for the addendum and binder outside of trim lines, see Remarks
















   ..
       !! processed by numpydoc !!

.. py:property:: sl
   :type: float


   
   Get or set the The smooth level parameter controls the smoothness of the modified surfaces. A large value makes the surface smoother. The commonly used value is between 5 and 10.  If springback is large, the transition region is expected to be large.  However, by using a smaller value of SL, the region of transition can be reduced
















   ..
       !! processed by numpydoc !!

.. py:property:: sf
   :type: float


   
   Get or set the This scales how much of the shape deviation is compensated. For example, if 10 mm springback is predicted, and the scale factor is chosen as 0.75, then the compensation in the opposite direction will only be 7.5 mm.Through many parameter studies, it is found that the best scale factor is case dependent. For some cases, a scale factor of 0.75 is best, while for others, larger values are better. Sometimes, the best value can be larger than 1.1.   Since it is impossible to choose the best value for each application it is suggested that for a new application, the initial trial is 0.75.  If the springback cannot be effectively compensated, more iterations must be used to compensate the remaining shape deviation.For channel with twisting, the scale factor is more important. It was found that a small change of the tool shape might change the twisting mode.  If this occurs, using a small value (<0.5) is suggested.
















   ..
       !! processed by numpydoc !!

.. py:property:: elref
   :type: int


   
   Get or set the EQ.1: special element refinement is used with the tool elements (default)
   EQ.2: special element refinement is turned off
















   ..
       !! processed by numpydoc !!

.. py:property:: psidm
   :type: Optional[float]


   
   Get or set the Define the Part set ID for master parts.  It is important to properly choose the parts for the master side.  Usually, only one side (master side) of the tool will be chosen as the master side, and the modification of the other side (slave side) depends solely on the change, which occurs in the master side.  In this way, the two sides are coupled and a constant gap between the two sides is maintained. If both sides are chosen as master side, the gap between the two sides might change and the gap might become inhomogeneous.  The choice of Master side will have effect on the final result for method 7 for three-piece draw. At this time, when the punch and binder are chosen as the master side, the binder region will not be changed. Otherwise, when the die is chosen as Master side the binder will be changed, since the changes extend to the edges of the Master tool.
















   ..
       !! processed by numpydoc !!

.. py:property:: undct
   :type: float


   
   Get or set the EQ.0: Default    EQ.1: Check and fix undercut.
















   ..
       !! processed by numpydoc !!

.. py:property:: angle
   :type: float


   
   Get or set the An angle defining the undercut
















   ..
       !! processed by numpydoc !!

.. py:property:: nlinea
   :type: int


   
   Get or set the Activate nonlinear extrapolation.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INTERFACE'


.. py:attribute:: subkeyword
   :value: 'COMPENSATION_NEW_MULTI_STEPS'






