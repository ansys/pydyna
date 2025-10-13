





:class:`ControlImplicitStaticCondensation`
==========================================


.. py:class:: control_implicit_static_condensation.ControlImplicitStaticCondensation(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_IMPLICIT_STATIC_CONDENSATION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlImplicitStaticCondensation

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sc_flag`
            - Get or set the Static Condensation Control Flag
          * - :py:attr:`~sc_nsid`
            - Get or set the Node set ID for nodes to be preserved in the static condensation procedure.  Required when SC_FLAG = 1
          * - :py:attr:`~sc_psid`
            - Get or set the Part set ID for parts to be included in the static condensation procedure.  When SC_FLAG = 1 SC_PSID can be used to specify a subset of the model with the default being the entire model.  When SC_FLAG = 2 SC_PSID is required.  SC_PSID = 0 implies that the entire model is condensed.
          * - :py:attr:`~se_mass`
            - Get or set the Name of the superelement mass matrix.  If left blank it is not generated
          * - :py:attr:`~se_stiff`
            - Get or set the Name of the superelement stiffness matrix.  If left blank it is not generated.
          * - :py:attr:`~se_inert`
            - Get or set the Name of the superelement inertia matrix, required for gravity loading applications of the superelement.  If left blank it is not generated.
          * - :py:attr:`~filename`
            - Get or set the If any of SE_MASS, SE_DAMP, SE_STIFF, or SE_INERT is blank then the second line is required and contains the file name for the superelement.


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

    from control_implicit_static_condensation import ControlImplicitStaticCondensation

Property detail
---------------

.. py:property:: sc_flag
   :type: int


   
   Get or set the Static Condensation Control Flag
   EQ.0:  no static condensation will be performed
   EQ.1:  create superelement representation based on static condensation.
   EQ.2:  use static condensation to build a linearized representation for a part and use that linearized representation in the following analysis.
















   ..
       !! processed by numpydoc !!

.. py:property:: sc_nsid
   :type: Optional[int]


   
   Get or set the Node set ID for nodes to be preserved in the static condensation procedure.  Required when SC_FLAG = 1
















   ..
       !! processed by numpydoc !!

.. py:property:: sc_psid
   :type: Optional[int]


   
   Get or set the Part set ID for parts to be included in the static condensation procedure.  When SC_FLAG = 1 SC_PSID can be used to specify a subset of the model with the default being the entire model.  When SC_FLAG = 2 SC_PSID is required.  SC_PSID = 0 implies that the entire model is condensed.
















   ..
       !! processed by numpydoc !!

.. py:property:: se_mass
   :type: Optional[str]


   
   Get or set the Name of the superelement mass matrix.  If left blank it is not generated
















   ..
       !! processed by numpydoc !!

.. py:property:: se_stiff
   :type: Optional[str]


   
   Get or set the Name of the superelement stiffness matrix.  If left blank it is not generated.
















   ..
       !! processed by numpydoc !!

.. py:property:: se_inert
   :type: Optional[str]


   
   Get or set the Name of the superelement inertia matrix, required for gravity loading applications of the superelement.  If left blank it is not generated.
















   ..
       !! processed by numpydoc !!

.. py:property:: filename
   :type: Optional[str]


   
   Get or set the If any of SE_MASS, SE_DAMP, SE_STIFF, or SE_INERT is blank then the second line is required and contains the file name for the superelement.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'IMPLICIT_STATIC_CONDENSATION'






