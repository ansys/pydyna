





:class:`ControlAdapt`
=====================


.. py:class:: control_adapt.ControlAdapt(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_ADAPT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlAdapt

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~adpfreq`
            - Get or set the Time interval between adaptive refinements.
          * - :py:attr:`~adptol`
            - Get or set the Adaptive error tolerance in degrees for ADPOPT set to 1 or 2 below. If ADPOPT is set to 8, ADPTOL is the characteristic element size (default = 1.0E+20).
          * - :py:attr:`~adptyp`
            - Get or set the Adaptive options:
          * - :py:attr:`~maxlvl`
            - Get or set the Maximum number of refinement levels (default = 3).
          * - :py:attr:`~tbirth`
            - Get or set the Birth time at which the adaptive remeshing begins (default = 0.0).
          * - :py:attr:`~tdeath`
            - Get or set the Death time at which the adaptive remeshing ends (default = 1.0E+20).
          * - :py:attr:`~lcadp`
            - Get or set the Adaptive interval is changed as a function of time given by load curve ID, LCADP.
          * - :py:attr:`~ioflag`
            - Get or set the Flag to generate adaptive mesh at exit including *NODE, *ELEMENT, *SHELL, *BOUNDARY_, *CONTACT_NODE_, and *CONSTRAINED_ ADAPTIVITY to be saved in the file, adapt.msh.
          * - :py:attr:`~adpsize`
            - Get or set the Minimum element size to be adapted based on element edge length. If undefined the edge length limit is ignored (default = 0.0).
          * - :py:attr:`~adpass`
            - Get or set the One or two pass adaptivity flag:
          * - :py:attr:`~ireflg`
            - Get or set the Uniform refinement level. A values of 1, 2, 3, ... allow 4, 16, 64, ....  elements, respectively, to be created uniformly for each original element.
          * - :py:attr:`~adpene`
            - Get or set the Adapt the mesh when the contact surfaces approach or penetrate the tooling surface.
          * - :py:attr:`~adpth`
            - Get or set the Absolute shell thickness level below which adaptive remeshing should begin.
          * - :py:attr:`~memory`
            - Get or set the See keyword manual.
          * - :py:attr:`~orient`
            - Get or set the This option applies to the FORMING contact option only.
          * - :py:attr:`~maxel`
            - Get or set the Adaptivity is stopped if this number of elements is exceeded


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

    from control_adapt import ControlAdapt

Property detail
---------------

.. py:property:: adpfreq
   :type: Optional[float]


   
   Get or set the Time interval between adaptive refinements.
















   ..
       !! processed by numpydoc !!

.. py:property:: adptol
   :type: float


   
   Get or set the Adaptive error tolerance in degrees for ADPOPT set to 1 or 2 below. If ADPOPT is set to 8, ADPTOL is the characteristic element size (default = 1.0E+20).
















   ..
       !! processed by numpydoc !!

.. py:property:: adptyp
   :type: int


   
   Get or set the Adaptive options:
   EQ.1: angle change in degrees per adaptive refinement relative to the surrounding elements for each element to be refined (default).
   EQ.2: total angle change in degrees relative to the surrounding element for each element to be refined.
   Adapts when the shell error in the energy norm, Δe, exceeds ADPTOL/100 times the mean energy norm within the part.
   EQ.7: 3D r-adaptive remeshing for solid elements.  Tetrahedrons are used in the adaptive remeshing process (solid formulation 10 or 13, or if EFG, formulation 42), or in the case of 3D axisymmetry (orbital) adaptivity, hexahedral and pentahedral elements are used in the adaptive remeshing.  A completely new mesh is generated which is initialized from the old mesh using a least squares approximation.  The mesh size is currently based on the minimum and maximum edge lengths defined on the *CONTROL_REMESHING keyword input.  This option remains under development, and we are not sure of its reliability on complex geometries.
   EQ.8/-8: 2D r-adaptive remeshing for plane stress, plane strain, and axisymmetric continuum elements,that is, shell formulations 12 through 15.
   A completely new mesh is generated which is initialized from the old mesh using a least squares approximation.
   The mesh size is currently based on the value, ADPTOL, which gives the characteristic element size.
   This option is based on earlier work by Dick and Harris[1992].
   If ADPTYP is negative, then self-contacting material will not be merged together.
   The self-merging is often preferred since it eliminates sharp folds in the boundary;
   however, if the sharp fold is being simulated, unexpected results are generated.
















   ..
       !! processed by numpydoc !!

.. py:property:: maxlvl
   :type: int


   
   Get or set the Maximum number of refinement levels (default = 3).
















   ..
       !! processed by numpydoc !!

.. py:property:: tbirth
   :type: float


   
   Get or set the Birth time at which the adaptive remeshing begins (default = 0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: tdeath
   :type: float


   
   Get or set the Death time at which the adaptive remeshing ends (default = 1.0E+20).
















   ..
       !! processed by numpydoc !!

.. py:property:: lcadp
   :type: int


   
   Get or set the Adaptive interval is changed as a function of time given by load curve ID, LCADP.
   EQ.0: ADPFREQ is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: ioflag
   :type: int


   
   Get or set the Flag to generate adaptive mesh at exit including *NODE, *ELEMENT, *SHELL, *BOUNDARY_, *CONTACT_NODE_, and *CONSTRAINED_ ADAPTIVITY to be saved in the file, adapt.msh.
   EQ.0: no adaptive mesh generation at the exit,
   EQ.1: adaptive mesh generation at the exit.
















   ..
       !! processed by numpydoc !!

.. py:property:: adpsize
   :type: float


   
   Get or set the Minimum element size to be adapted based on element edge length. If undefined the edge length limit is ignored (default = 0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: adpass
   :type: int


   
   Get or set the One or two pass adaptivity flag:
   EQ.0: two pass adaptivity,
   EQ.1: one pass adaptivity.
















   ..
       !! processed by numpydoc !!

.. py:property:: ireflg
   :type: int


   
   Get or set the Uniform refinement level. A values of 1, 2, 3, ... allow 4, 16, 64, ....  elements, respectively, to be created uniformly for each original element.
















   ..
       !! processed by numpydoc !!

.. py:property:: adpene
   :type: float


   
   Get or set the Adapt the mesh when the contact surfaces approach or penetrate the tooling surface.
















   ..
       !! processed by numpydoc !!

.. py:property:: adpth
   :type: float


   
   Get or set the Absolute shell thickness level below which adaptive remeshing should begin.
   EQ.0: ADPTH is ignored (default).
   This option works only if ADPTOL is nonzero.
















   ..
       !! processed by numpydoc !!

.. py:property:: memory
   :type: int


   
   Get or set the See keyword manual.
   EQ.0: MEMORY is ignored (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: orient
   :type: int


   
   Get or set the This option applies to the FORMING contact option only.
   EQ.0: LS-DYNA sets the global orientation of the contact surface the first time a potential contact is observed after the birth time,
   EQ.1: the user orientation for the contact interface is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: maxel
   :type: int


   
   Get or set the Adaptivity is stopped if this number of elements is exceeded
   EQ.0: MAXEL is ignored (default).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'ADAPT'






