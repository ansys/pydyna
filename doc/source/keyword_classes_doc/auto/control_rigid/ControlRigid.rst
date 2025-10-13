





:class:`ControlRigid`
=====================


.. py:class:: control_rigid.ControlRigid(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_RIGID keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlRigid

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~lmf`
            - Get or set the Joint formulation flag for explicit analysis.  This flag can be used to switch to an implicit formulation for joints (*CONSTRAINED_JOINT_Option) which uses Lagrange multipliers to impose prescribed kinematic boundary conditions and joint constraints.  There is a slight cost overhead due to the assembly of sparse matrix equations which are solved using standard procedures for nonlinear problems in rigid multi-body dynamics.
          * - :py:attr:`~jntf`
            - Get or set the Generalized joint stiffness formulation:
          * - :py:attr:`~orthmd`
            - Get or set the Othogonalize modes with respect to each other:
          * - :py:attr:`~partm`
            - Get or set the Use global mass matrix to determine part mass distribution. This mass matrix may contain mass from other parts that share nodes.
          * - :py:attr:`~sparse`
            - Get or set the Use sparse matrix multiply subroutines for the modal stuffness and damping matrices.
          * - :py:attr:`~metalf`
            - Get or set the Metalforming option, which should not be used for crash and other applications involving rigid bodies. Use fast update of rigid body nodes. If this option is active the rotational motion of all rigid bodies should be surpressed.
          * - :py:attr:`~plotel`
            - Get or set the Automatic generation of *ELEMENT_PLOTEL for *CONSTRAINED_NODAL_RIGID_BODY.
          * - :py:attr:`~rbsms`
            - Get or set the Flag to apply consistent treatment of rigid bodies in selective mass scaling.
          * - :py:attr:`~norbic`
            - Get or set the Circumvent rigid body inertia check, see Remark 5.
          * - :py:attr:`~gjadstf`
            - Get or set the Rotational stiffness is added to all joints in the model as a mean to model a small resistance, such as joint friction, or to avoid zero energy modes in implicit. This is equivalent to defining a *CONSTRAINED_‌JOINT_‌STIFFNESS_‌GENERALIZED to the free rotational degrees of freedom of all joints, using a slope in LCIDPH, LCIDT, LCIDPS that equal to GJADSTF.  For models with many joints this field is advantageous since it only has to be defined once for all joints, whereas the equivalent keyword must be defined for each joint
          * - :py:attr:`~gjadvsc`
            - Get or set the Rotational damping is added to all joints in the model as a mean to model a small resistance, such as joint friction, or to avoid zero energy modes in implicit. This is equivalent to defining a *CONSTRAINED_‌JOINT_‌STIFFNESS_‌GENERALIZED to the free rotational degrees of freedom of all joints, using a slope in DLCIDPH, DLCIDT, DLCIDPS that equal to GJADVSC.  Like GJADSTF, for models many joints this field is advantageous since it only has to be defined once for all joints, whereas the equivalent keyword must be defined for each joint.
          * - :py:attr:`~tjastf`
            - Get or set the Translational stiffness is added to all joints in the model as a mean to model a small resistance, such as joint friction, or to avoid zero energy modes in implicit. This is equivalent to defining a *CONSTRAINED_‌JOINT_‌STIFFNESS_‌TRANSLATIONAL to the free translational degrees of freedom of all joints, using a slope in LCIDX, LCIDY, LCIDZ that equal to TJADSTF.  Like GJADSTF, for models many joints this field is advantageous since it only has to be defined once for all joints, whereas the equivalent keyword must be defined for each joint
          * - :py:attr:`~tjadvsc`
            - Get or set the Translational damping is added to all joints in the model as a mean to model a small resistance, such as joint friction, or to avoid zero energy modes in implicit. This is equivalent to defining a *CONSTRAINED_‌JOINT_‌STIFFNESS_‌TRANSLATIONAL to the free translational degrees of freedom of all joints, using a slope in DLCIDX, DLCIDY, DLCIDZ that equal to TJADVSC.  Like GJADSTF, for models many joints this field is advantageous since it only has to be defined once for all joints, whereas the equivalent keyword must be defined for each joint


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

    from control_rigid import ControlRigid

Property detail
---------------

.. py:property:: lmf
   :type: int


   
   Get or set the Joint formulation flag for explicit analysis.  This flag can be used to switch to an implicit formulation for joints (*CONSTRAINED_JOINT_Option) which uses Lagrange multipliers to impose prescribed kinematic boundary conditions and joint constraints.  There is a slight cost overhead due to the assembly of sparse matrix equations which are solved using standard procedures for nonlinear problems in rigid multi-body dynamics.
   EQ.0:   penalty formulation for joints(default)
   EQ.1 : Lagrange - multiplier - based formulation for joints
















   ..
       !! processed by numpydoc !!

.. py:property:: jntf
   :type: int


   
   Get or set the Generalized joint stiffness formulation:
   EQ.0: incremental update,
   EQ.1: total formulation (exact).
   EQ.2: total formulation intended for implicit analysis.
















   ..
       !! processed by numpydoc !!

.. py:property:: orthmd
   :type: int


   
   Get or set the Othogonalize modes with respect to each other:
   EQ.0: true.
   EQ.1: total formulation (exact).
















   ..
       !! processed by numpydoc !!

.. py:property:: partm
   :type: int


   
   Get or set the Use global mass matrix to determine part mass distribution. This mass matrix may contain mass from other parts that share nodes.
   EQ.0: true,
   EQ.1: false.
















   ..
       !! processed by numpydoc !!

.. py:property:: sparse
   :type: int


   
   Get or set the Use sparse matrix multiply subroutines for the modal stuffness and damping matrices.
   EQ.0: false, do full matrix multiplies (frequently faster),
   EQ.1: true.
















   ..
       !! processed by numpydoc !!

.. py:property:: metalf
   :type: int


   
   Get or set the Metalforming option, which should not be used for crash and other applications involving rigid bodies. Use fast update of rigid body nodes. If this option is active the rotational motion of all rigid bodies should be surpressed.
   EQ.0: full treatment is used,
   EQ.1: fast update for metalforming applications.
















   ..
       !! processed by numpydoc !!

.. py:property:: plotel
   :type: int


   
   Get or set the Automatic generation of *ELEMENT_PLOTEL for *CONSTRAINED_NODAL_RIGID_BODY.
   EQ.0: no generation
   EQ.1: one part is generated for all nodal rigid bodies with the PID set to 1000000.
   EQ.2: one part is generated for each nodal rigid body in the problem with a part ID of 1000000+PID, where PID is the nodal rigid body ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: rbsms
   :type: int


   
   Get or set the Flag to apply consistent treatment of rigid bodies in selective mass scaling.
   EQ.0: Off
   EQ.1: On
















   ..
       !! processed by numpydoc !!

.. py:property:: norbic
   :type: int


   
   Get or set the Circumvent rigid body inertia check, see Remark 5.
   EQ.0:   Off
   EQ.1:   On.
















   ..
       !! processed by numpydoc !!

.. py:property:: gjadstf
   :type: float


   
   Get or set the Rotational stiffness is added to all joints in the model as a mean to model a small resistance, such as joint friction, or to avoid zero energy modes in implicit. This is equivalent to defining a *CONSTRAINED_‌JOINT_‌STIFFNESS_‌GENERALIZED to the free rotational degrees of freedom of all joints, using a slope in LCIDPH, LCIDT, LCIDPS that equal to GJADSTF.  For models with many joints this field is advantageous since it only has to be defined once for all joints, whereas the equivalent keyword must be defined for each joint
















   ..
       !! processed by numpydoc !!

.. py:property:: gjadvsc
   :type: float


   
   Get or set the Rotational damping is added to all joints in the model as a mean to model a small resistance, such as joint friction, or to avoid zero energy modes in implicit. This is equivalent to defining a *CONSTRAINED_‌JOINT_‌STIFFNESS_‌GENERALIZED to the free rotational degrees of freedom of all joints, using a slope in DLCIDPH, DLCIDT, DLCIDPS that equal to GJADVSC.  Like GJADSTF, for models many joints this field is advantageous since it only has to be defined once for all joints, whereas the equivalent keyword must be defined for each joint.
















   ..
       !! processed by numpydoc !!

.. py:property:: tjastf
   :type: float


   
   Get or set the Translational stiffness is added to all joints in the model as a mean to model a small resistance, such as joint friction, or to avoid zero energy modes in implicit. This is equivalent to defining a *CONSTRAINED_‌JOINT_‌STIFFNESS_‌TRANSLATIONAL to the free translational degrees of freedom of all joints, using a slope in LCIDX, LCIDY, LCIDZ that equal to TJADSTF.  Like GJADSTF, for models many joints this field is advantageous since it only has to be defined once for all joints, whereas the equivalent keyword must be defined for each joint
















   ..
       !! processed by numpydoc !!

.. py:property:: tjadvsc
   :type: float


   
   Get or set the Translational damping is added to all joints in the model as a mean to model a small resistance, such as joint friction, or to avoid zero energy modes in implicit. This is equivalent to defining a *CONSTRAINED_‌JOINT_‌STIFFNESS_‌TRANSLATIONAL to the free translational degrees of freedom of all joints, using a slope in DLCIDX, DLCIDY, DLCIDZ that equal to TJADVSC.  Like GJADSTF, for models many joints this field is advantageous since it only has to be defined once for all joints, whereas the equivalent keyword must be defined for each joint
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'RIGID'






