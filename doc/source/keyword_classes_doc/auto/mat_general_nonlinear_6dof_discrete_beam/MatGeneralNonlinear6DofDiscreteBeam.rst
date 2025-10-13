





:class:`MatGeneralNonlinear6DofDiscreteBeam`
============================================


.. py:class:: mat_general_nonlinear_6dof_discrete_beam.MatGeneralNonlinear6DofDiscreteBeam(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_GENERAL_NONLINEAR_6DOF_DISCRETE_BEAM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatGeneralNonlinear6DofDiscreteBeam

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number has to be used.
          * - :py:attr:`~ro`
            - Get or set the Mass density, see also volume in *SECTION_BEAM definition.
          * - :py:attr:`~kt`
            - Get or set the Translational stiffness for IUNLD = 2.0.  However, if IFLAG = 2, then it is the translational stiffness for unloading along the local r-axis.
          * - :py:attr:`~kr`
            - Get or set the Rotational stiffness for IUNLD = 2.0.  However, if IFLAG =‌ 2, then KR is the rotational stiffness for unloading along the local r-axis.
          * - :py:attr:`~iunld`
            - Get or set the Unloading option (see Figure 0-1):
          * - :py:attr:`~offset`
            - Get or set the Offset factor between 0 and 1.0 to determine permanent set upon unloading if the UNLDOPT=3.0. The permanent sets in compression and tension are equal to the product of this offset value and the maximum compressive and tensile displacements, respectively.
          * - :py:attr:`~dampf`
            - Get or set the Damping factor for stability.  Values in the neighborhood of unity are recommended. This damping factor is properly scaled to eliminate time step size dependency. Also, it is active if and only if the local stiffness is defined.
          * - :py:attr:`~iflag`
            - Get or set the Formulation flag:
          * - :py:attr:`~lcidtr`
            - Get or set the Load curve ID defining translational force resultant along local r-axis versus relative translational displacement.
          * - :py:attr:`~lcidts`
            - Get or set the Load curve ID defining translational force resultant along local s-axis versus relative translational displacement.
          * - :py:attr:`~lcidtt`
            - Get or set the Load curve ID defining translational force resultant along local t-axis versus relative translational displacement.
          * - :py:attr:`~lcidrr`
            - Get or set the Load curve for rotational moment resultant about the local r-axis:
          * - :py:attr:`~lcidrs`
            - Get or set the Load curve for rotational moment resultant about local r-axis:
          * - :py:attr:`~lcidrt`
            - Get or set the Load curve for rotational moment resultant about local -axis:
          * - :py:attr:`~lcidtur`
            - Get or set the Load curve ID defining translational force resultant along local r-axis as a function of relative translational displacement during unloading.
          * - :py:attr:`~lcidtus`
            - Get or set the Load curve ID defining translational force resultant along local s-axis as a function of relative translational displacement during unloading (IFLAG = 0 or 1 only).
          * - :py:attr:`~lcidtut`
            - Get or set the Load curve ID defining translational force resultant along local t-axis as a function of relative translational displacement during unloading (IFLAG = 0 or 1 only).
          * - :py:attr:`~lcidrur`
            - Get or set the Load curve ID defining rotational moment resultant about local r-axis as a function of relative rotational displacement during unloading.
          * - :py:attr:`~lcidrus`
            - Get or set the Load curve for rotational moment resultant about local s-axis:
          * - :py:attr:`~lcidrut`
            - Get or set the Load curve ID defining rotational moment resultant about local -axis:
          * - :py:attr:`~lcidtdr`
            - Get or set the Load curve ID defining translational damping force resultant along local r-
          * - :py:attr:`~lcidtds`
            - Get or set the Load curve ID defining translational damping force resultant along local
          * - :py:attr:`~lcidtdt`
            - Get or set the Load curve ID defining translational damping force resultant along local
          * - :py:attr:`~lcidrdr`
            - Get or set the Load curve ID defining rotational damping moment resultant about local
          * - :py:attr:`~lcidrds`
            - Get or set the Load curve ID defining rotational damping moment resultant about local
          * - :py:attr:`~lcidrdt`
            - Get or set the Load curve ID defining rotational damping moment resultant about local
          * - :py:attr:`~lcidter`
            - Get or set the Load curve ID defining translational damping force scale factor as a function of relative displacement in local r-direction.
          * - :py:attr:`~lcidtes`
            - Get or set the Load curve ID defining translational damping force scale factor as a function of relative displacement in local s-direction.
          * - :py:attr:`~lcidtet`
            - Get or set the Load curve ID defining translational damping force scale factor as a function of relative displacement in local t-direction.
          * - :py:attr:`~lcidrer`
            - Get or set the Load curve ID defining rotational damping moment resultant scale factor as a function of relative displacement in local r-rotation.
          * - :py:attr:`~lcidres`
            - Get or set the Load curve ID defining rotational damping moment resultant scale factor as a function of relative displacement in local s-rotation.
          * - :py:attr:`~lcidret`
            - Get or set the Load curve ID defining rotational damping moment resultant scale factor as a function of relative displacement in local t-rotation.
          * - :py:attr:`~utfailr`
            - Get or set the Optional, translational displacement at failure in tension. If zero, the corresponding displacement, Ur, is not considered in the failure calculation.
          * - :py:attr:`~utfails`
            - Get or set the Optional, translational displacement at failure in tension. If zero, the corresponding displacement, Us, is not considered in the failure calculation.
          * - :py:attr:`~utfailt`
            - Get or set the Optional, translational displacement at failure in tension. If zero, the corresponding displacement, Ut, is not considered in the failure calculation.
          * - :py:attr:`~wtfailr`
            - Get or set the Optional, rotational displacement at failure in tension. If zero, the corresponding rotation, theta-r, is not considered in the failure calculation.
          * - :py:attr:`~wtfails`
            - Get or set the Optional, rotational displacement at failure in tension. If zero, the corresponding rotation, theta-s, is not considered in the failure calculation.
          * - :py:attr:`~wtfailt`
            - Get or set the Optional, rotational displacement at failure in tension. If zero, the corresponding rotation, theta-t, is not considered in the failure calculation.
          * - :py:attr:`~fcrit`
            - Get or set the Failure criterion (see Remark 1):
          * - :py:attr:`~ucfailr`
            - Get or set the Optional, translational displacement at failure in compression. If zero, the corresponding displacement, Ur, is not considered in the failure calculation.
          * - :py:attr:`~ucfails`
            - Get or set the Optional, translational displacement at failure in compression. If zero, the corresponding displacement, Us, is not considered in the failure calculation.
          * - :py:attr:`~ucfailt`
            - Get or set the Optional, translational displacement at failure in compression. If zero, the corresponding displacement, Ut, is not considered in the failure calculation.
          * - :py:attr:`~wcfailr`
            - Get or set the Optional, rotational displacement at failure in compression. If zero, the corresponding rotation, theta-r, is not considered in the failure calculation.
          * - :py:attr:`~wcfails`
            - Get or set the Optional, rotational displacement at failure in compression. If zero, the corresponding rotation, theta-s, is not considered in the failure calculation.
          * - :py:attr:`~wcfailt`
            - Get or set the Optional, rotational displacement at failure in compression. If zero, the corresponding rotation, theta-t, is not considered in the failure calculation.
          * - :py:attr:`~iur`
            - Get or set the Initial translational displacement along local r-axis
          * - :py:attr:`~ius`
            - Get or set the Initial translational displacement along local s-axis
          * - :py:attr:`~iut`
            - Get or set the Initial translational displacement along local t-axis
          * - :py:attr:`~iwr`
            - Get or set the Initial rotational displacement along local r-axis
          * - :py:attr:`~iws`
            - Get or set the Initial rotational displacement along local s-axis
          * - :py:attr:`~iwt`
            - Get or set the Initial rotational displacement along local t-axis
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

    from mat_general_nonlinear_6dof_discrete_beam import MatGeneralNonlinear6DofDiscreteBeam

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density, see also volume in *SECTION_BEAM definition.
















   ..
       !! processed by numpydoc !!

.. py:property:: kt
   :type: Optional[float]


   
   Get or set the Translational stiffness for IUNLD = 2.0.  However, if IFLAG = 2, then it is the translational stiffness for unloading along the local r-axis.
   If left blank, a value calculated by LS-DYNA will be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: kr
   :type: Optional[float]


   
   Get or set the Rotational stiffness for IUNLD = 2.0.  However, if IFLAG =‌ 2, then KR is the rotational stiffness for unloading along the local r-axis.
   If left blank, a value calculated by LS-DYNA will be used
















   ..
       !! processed by numpydoc !!

.. py:property:: iunld
   :type: Optional[int]


   
   Get or set the Unloading option (see Figure 0-1):
   EQ.0.0: loading and unloading follow loading curve
   EQ.1.0: loading follows loading curve, unloading follows unloading curve. The unloading curve ID if undefined is taken as the loading curve.
   EQ.2.0: loading follows loading curve, unloading follows unloading stiffness, KT or KR, to the unloading curve.
   The loading and unloading curves may only intersect at the origin of the axes.
   EQ.3.0: quadratic unloading from peak displacement value to a permanent offset.
















   ..
       !! processed by numpydoc !!

.. py:property:: offset
   :type: Optional[float]


   
   Get or set the Offset factor between 0 and 1.0 to determine permanent set upon unloading if the UNLDOPT=3.0. The permanent sets in compression and tension are equal to the product of this offset value and the maximum compressive and tensile displacements, respectively.
















   ..
       !! processed by numpydoc !!

.. py:property:: dampf
   :type: Optional[float]


   
   Get or set the Damping factor for stability.  Values in the neighborhood of unity are recommended. This damping factor is properly scaled to eliminate time step size dependency. Also, it is active if and only if the local stiffness is defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: iflag
   :type: int


   
   Get or set the Formulation flag:
   EQ.0:   displacement formulation which is used in all other models
   EQ.1:   linear strain formulation.  The displacements and velocities are divided by the initial length of the beam.
   EQ.2:   a displacement formulation to simulate the buckling behavior of crushable frames.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidtr
   :type: Optional[int]


   
   Get or set the Load curve ID defining translational force resultant along local r-axis versus relative translational displacement.
   If zero, no stiffness related forces are generated for this degree of freedom. The loading curves must be defined from the most negative displacement to the most positive displacement.  The force does not need to increase montonically. The curves in this input are linearly extrapolated when the displacement range falls outside the curve definition.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidts
   :type: Optional[int]


   
   Get or set the Load curve ID defining translational force resultant along local s-axis versus relative translational displacement.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidtt
   :type: Optional[int]


   
   Get or set the Load curve ID defining translational force resultant along local t-axis versus relative translational displacement.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidrr
   :type: Optional[int]


   
   Get or set the Load curve for rotational moment resultant about the local r-axis:
   IFLAG.NE.2:     load curve ID defining rotational moment resultant about local r-axis as a function of relative rotational displacement
   IFLAG.EQ.2:     load curve ID defining rotational moment resultant about local r-axis as a function of relative rotational displacement at node 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidrs
   :type: Optional[int]


   
   Get or set the Load curve for rotational moment resultant about local r-axis:
   IFLAG.NE.2:     load curve ID defining rotational moment resultant about local r-axis as a function of relative rotational displacement
   IFLAG.EQ.2:     load curve ID defining rotational moment resultant about local r-axis as a function of relative rotational displacement at node 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidrt
   :type: Optional[int]


   
   Get or set the Load curve for rotational moment resultant about local -axis:
   IFLAG.NE.2:     load curve ID defining rotational moment resultant about local s-axis as a function of relative rotational displacement
   IFLAG.EQ.2:     load curve ID defining rotational moment resultant about local s-axis as a function of relative rotational displacement at node 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidtur
   :type: Optional[int]


   
   Get or set the Load curve ID defining translational force resultant along local r-axis as a function of relative translational displacement during unloading.
   The force values defined by this curve must increase monotonically from the most negative displacement to the most positive displacement.
   For IUNLD = 1.0, the slope of this curve must equal or exceed the loading curve for stability reasons.  This is not the case for IUNLD = 2.0.
   For loading and unloading to follow the same path simply set LCIDTUR = LCIDTR.  For options IUNLD = 0.0 or 3.0 the unloading curve is not required.
   For IUNLD = 2.0, if LCIDTUR is left blank or zero, the default is to use the same curve for unloading as for loading.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidtus
   :type: Optional[int]


   
   Get or set the Load curve ID defining translational force resultant along local s-axis as a function of relative translational displacement during unloading (IFLAG = 0 or 1 only).
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidtut
   :type: Optional[int]


   
   Get or set the Load curve ID defining translational force resultant along local t-axis as a function of relative translational displacement during unloading (IFLAG = 0 or 1 only).
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidrur
   :type: Optional[int]


   
   Get or set the Load curve ID defining rotational moment resultant about local r-axis as a function of relative rotational displacement during unloading.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidrus
   :type: Optional[int]


   
   Get or set the Load curve for rotational moment resultant about local s-axis:
   IFLAG.NE.2:     load curve ID defining rotational moment resultant about local s-axis as a function of relative rotational displacement during unloading
   IFLAG.EQ.2:     load curve ID defining rotational moment resultant about local s-axis as a function of relative rotational displacement during unloading at node 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidrut
   :type: Optional[int]


   
   Get or set the Load curve ID defining rotational moment resultant about local -axis:
   IFLAG.NE.2:     load curve ID defining rotational moment resultant about local -axis as a function of relative rotational displacement during unloading.  If zero, no viscous forces are generated for this degree of freedom
   IFLAG.EQ.2:     load curve ID defining rotational moment resultant about local -axis as a function of relative rotational displacement during unloading at node 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidtdr
   :type: Optional[int]


   
   Get or set the Load curve ID defining translational damping force resultant along local r-
   axis as a function of relative translational velocity.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidtds
   :type: Optional[int]


   
   Get or set the Load curve ID defining translational damping force resultant along local 
   s-axis as a function relative translational velocity.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidtdt
   :type: Optional[int]


   
   Get or set the Load curve ID defining translational damping force resultant along local 
   t-axis as a function of relative translational velocity.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidrdr
   :type: Optional[int]


   
   Get or set the Load curve ID defining rotational damping moment resultant about local 
   r-axis as a function of relative rotational velocity.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidrds
   :type: Optional[int]


   
   Get or set the Load curve ID defining rotational damping moment resultant about local 
   s-axis as a function of relative rotational velocity.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidrdt
   :type: Optional[int]


   
   Get or set the Load curve ID defining rotational damping moment resultant about local 
   t-axis as a function of relative rotational velocity.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidter
   :type: Optional[int]


   
   Get or set the Load curve ID defining translational damping force scale factor as a function of relative displacement in local r-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidtes
   :type: Optional[int]


   
   Get or set the Load curve ID defining translational damping force scale factor as a function of relative displacement in local s-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidtet
   :type: Optional[int]


   
   Get or set the Load curve ID defining translational damping force scale factor as a function of relative displacement in local t-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidrer
   :type: Optional[int]


   
   Get or set the Load curve ID defining rotational damping moment resultant scale factor as a function of relative displacement in local r-rotation.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidres
   :type: Optional[int]


   
   Get or set the Load curve ID defining rotational damping moment resultant scale factor as a function of relative displacement in local s-rotation.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidret
   :type: Optional[int]


   
   Get or set the Load curve ID defining rotational damping moment resultant scale factor as a function of relative displacement in local t-rotation.
















   ..
       !! processed by numpydoc !!

.. py:property:: utfailr
   :type: Optional[float]


   
   Get or set the Optional, translational displacement at failure in tension. If zero, the corresponding displacement, Ur, is not considered in the failure calculation.
















   ..
       !! processed by numpydoc !!

.. py:property:: utfails
   :type: Optional[float]


   
   Get or set the Optional, translational displacement at failure in tension. If zero, the corresponding displacement, Us, is not considered in the failure calculation.
















   ..
       !! processed by numpydoc !!

.. py:property:: utfailt
   :type: Optional[float]


   
   Get or set the Optional, translational displacement at failure in tension. If zero, the corresponding displacement, Ut, is not considered in the failure calculation.
















   ..
       !! processed by numpydoc !!

.. py:property:: wtfailr
   :type: Optional[float]


   
   Get or set the Optional, rotational displacement at failure in tension. If zero, the corresponding rotation, theta-r, is not considered in the failure calculation.
















   ..
       !! processed by numpydoc !!

.. py:property:: wtfails
   :type: Optional[float]


   
   Get or set the Optional, rotational displacement at failure in tension. If zero, the corresponding rotation, theta-s, is not considered in the failure calculation.
















   ..
       !! processed by numpydoc !!

.. py:property:: wtfailt
   :type: Optional[float]


   
   Get or set the Optional, rotational displacement at failure in tension. If zero, the corresponding rotation, theta-t, is not considered in the failure calculation.
















   ..
       !! processed by numpydoc !!

.. py:property:: fcrit
   :type: Optional[float]


   
   Get or set the Failure criterion (see Remark 1):
   EQ.0.0: two separate criteria, one for negative displacements and rotations, another for positive displacements and rotations
   EQ.1.0: one criterion that considers both positive and negative displacements and rotations.
















   ..
       !! processed by numpydoc !!

.. py:property:: ucfailr
   :type: Optional[float]


   
   Get or set the Optional, translational displacement at failure in compression. If zero, the corresponding displacement, Ur, is not considered in the failure calculation.
   Define as a positive number.
















   ..
       !! processed by numpydoc !!

.. py:property:: ucfails
   :type: Optional[float]


   
   Get or set the Optional, translational displacement at failure in compression. If zero, the corresponding displacement, Us, is not considered in the failure calculation.
   Define as a positive number.
















   ..
       !! processed by numpydoc !!

.. py:property:: ucfailt
   :type: Optional[float]


   
   Get or set the Optional, translational displacement at failure in compression. If zero, the corresponding displacement, Ut, is not considered in the failure calculation.
   Define as a positive number.
















   ..
       !! processed by numpydoc !!

.. py:property:: wcfailr
   :type: Optional[float]


   
   Get or set the Optional, rotational displacement at failure in compression. If zero, the corresponding rotation, theta-r, is not considered in the failure calculation.
   Define as a positive number.
















   ..
       !! processed by numpydoc !!

.. py:property:: wcfails
   :type: Optional[float]


   
   Get or set the Optional, rotational displacement at failure in compression. If zero, the corresponding rotation, theta-s, is not considered in the failure calculation.
   Define as a positive number.
















   ..
       !! processed by numpydoc !!

.. py:property:: wcfailt
   :type: Optional[float]


   
   Get or set the Optional, rotational displacement at failure in compression. If zero, the corresponding rotation, theta-t, is not considered in the failure calculation.
   Define as a positive number.
















   ..
       !! processed by numpydoc !!

.. py:property:: iur
   :type: Optional[float]


   
   Get or set the Initial translational displacement along local r-axis
















   ..
       !! processed by numpydoc !!

.. py:property:: ius
   :type: Optional[float]


   
   Get or set the Initial translational displacement along local s-axis
















   ..
       !! processed by numpydoc !!

.. py:property:: iut
   :type: Optional[float]


   
   Get or set the Initial translational displacement along local t-axis
















   ..
       !! processed by numpydoc !!

.. py:property:: iwr
   :type: Optional[float]


   
   Get or set the Initial rotational displacement along local r-axis
















   ..
       !! processed by numpydoc !!

.. py:property:: iws
   :type: Optional[float]


   
   Get or set the Initial rotational displacement along local s-axis
















   ..
       !! processed by numpydoc !!

.. py:property:: iwt
   :type: Optional[float]


   
   Get or set the Initial rotational displacement along local t-axis
















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
   :value: 'GENERAL_NONLINEAR_6DOF_DISCRETE_BEAM'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





