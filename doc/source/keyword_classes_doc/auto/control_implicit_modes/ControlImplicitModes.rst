





:class:`ControlImplicitModes`
=============================


.. py:class:: control_implicit_modes.ControlImplicitModes(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_IMPLICIT_MODES keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlImplicitModes

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nsidc`
            - Get or set the Node set ID for constraint modes:
          * - :py:attr:`~nsida`
            - Get or set the Node set ID for attachment modes
          * - :py:attr:`~neig`
            - Get or set the Number of eigenmodes. EQ.0:  no attachment modes will be generated
          * - :py:attr:`~ibase`
            - Get or set the Offset for numbering of the generalized internal degrees of freedom for the superelement.
          * - :py:attr:`~se_mass`
            - Get or set the Name of the superelement mass matrix.  If left blank it is not generated.
          * - :py:attr:`~se_damp`
            - Get or set the Name of the superelement damping matrix.  If left blank it is not generated.
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

    from control_implicit_modes import ControlImplicitModes

Property detail
---------------

.. py:property:: nsidc
   :type: int


   
   Get or set the Node set ID for constraint modes:
   EQ.0: no constraint modes will be generated
















   ..
       !! processed by numpydoc !!

.. py:property:: nsida
   :type: int


   
   Get or set the Node set ID for attachment modes
   EQ.0: no attachment modes will be generated
















   ..
       !! processed by numpydoc !!

.. py:property:: neig
   :type: Optional[int]


   
   Get or set the Number of eigenmodes. EQ.0:  no attachment modes will be generated
















   ..
       !! processed by numpydoc !!

.. py:property:: ibase
   :type: Optional[int]


   
   Get or set the Offset for numbering of the generalized internal degrees of freedom for the superelement.
















   ..
       !! processed by numpydoc !!

.. py:property:: se_mass
   :type: Optional[str]


   
   Get or set the Name of the superelement mass matrix.  If left blank it is not generated.
















   ..
       !! processed by numpydoc !!

.. py:property:: se_damp
   :type: Optional[str]


   
   Get or set the Name of the superelement damping matrix.  If left blank it is not generated.
















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
   :value: 'IMPLICIT_MODES'






