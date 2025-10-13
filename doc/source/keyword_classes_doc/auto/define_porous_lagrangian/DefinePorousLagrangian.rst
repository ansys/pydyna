





:class:`DefinePorousLagrangian`
===============================


.. py:class:: define_porous_lagrangian.DefinePorousLagrangian(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_POROUS_LAGRANGIAN keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefinePorousLagrangian

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~eidbeg`
            - Get or set the EIDBEG, EIDEND > 0: Range of thick porous element IDs. These are solids in 3D and shells in 2D.
          * - :py:attr:`~eidend`
            - Get or set the EIDBEG, EIDEND > 0: Range of thick porous element IDs. These are solids in 3D and shells in 2D.
          * - :py:attr:`~local`
            - Get or set the Flag to activate an element coordinate system:
          * - :py:attr:`~vecid1`
            - Get or set the *DEFINE_VECTOR IDs to define a specific coordinate system. VECID1
          * - :py:attr:`~vecid2`
            - Get or set the *DEFINE_VECTOR IDs to define a specific coordinate system. VECID1
          * - :py:attr:`~userdef`
            - Get or set the Flag to compute Aij and Bij with a user defined routine in the file
          * - :py:attr:`~axx`
            - Get or set the Viscous matrix for the porous flow Ergun equation.
          * - :py:attr:`~axy`
            - Get or set the Viscous matrix for the porous flow Ergun equation.
          * - :py:attr:`~axz`
            - Get or set the Viscous matrix for the porous flow Ergun equation.
          * - :py:attr:`~bxx`
            - Get or set the Inertial matrix for the porous flow Ergun equation.
          * - :py:attr:`~bxy`
            - Get or set the Inertial matrix for the porous flow Ergun equation.
          * - :py:attr:`~bxz`
            - Get or set the Inertial matrix for the porous flow Ergun equation.
          * - :py:attr:`~ayx`
            - Get or set the Viscous matrix for the porous flow Ergun equation.
          * - :py:attr:`~ayy`
            - Get or set the Viscous matrix for the porous flow Ergun equation.
          * - :py:attr:`~ayz`
            - Get or set the Viscous matrix for the porous flow Ergun equation.
          * - :py:attr:`~byx`
            - Get or set the Inertial matrix for the porous flow Ergun equation.
          * - :py:attr:`~byy`
            - Get or set the Inertial matrix for the porous flow Ergun equation.
          * - :py:attr:`~byz`
            - Get or set the Inertial matrix for the porous flow Ergun equation.
          * - :py:attr:`~azx`
            - Get or set the Viscous matrix for the porous flow Ergun equation.
          * - :py:attr:`~azy`
            - Get or set the Viscous matrix for the porous flow Ergun equation.
          * - :py:attr:`~azz`
            - Get or set the Viscous matrix for the porous flow Ergun equation.
          * - :py:attr:`~bzx`
            - Get or set the Inertial matrix for the porous flow Ergun equation.
          * - :py:attr:`~bzy`
            - Get or set the Inertial matrix for the porous flow Ergun equation.
          * - :py:attr:`~bzz`
            - Get or set the Inertial matrix for the porous flow Ergun equation.
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

    from define_porous_lagrangian import DefinePorousLagrangian

Property detail
---------------

.. py:property:: eidbeg
   :type: Optional[int]


   
   Get or set the EIDBEG, EIDEND > 0: Range of thick porous element IDs. These are solids in 3D and shells in 2D.
   EIDBEG, EIDEND < 0: Range of thin porous element IDs.   These are shells in 3D and beams in 2D.
   The ALE option does not support thin porous elements.
   EIDBEG > 0, EIDEND = 0: EIDBEG is a set of thick porous
   elements
   EIDBEG > 0, EIDEND < 0: EIDBEG is a set of thin porous elements.
















   ..
       !! processed by numpydoc !!

.. py:property:: eidend
   :type: int


   
   Get or set the EIDBEG, EIDEND > 0: Range of thick porous element IDs. These are solids in 3D and shells in 2D.
   EIDBEG, EIDEND < 0: Range of thin porous element IDs.   These are shells in 3D and beams in 2D.
   The ALE option does not support thin porous elements.
   EIDBEG > 0, EIDEND = 0: EIDBEG is a set of thick porous
   elements
   EIDBEG > 0, EIDEND < 0: EIDBEG is a set of thin porous elements.
















   ..
       !! processed by numpydoc !!

.. py:property:: local
   :type: int


   
   Get or set the Flag to activate an element coordinate system:
   EQ.0: The forces are applied in the global directions.
   EQ.1: The forces are applied in a local system attached to the
   element. The system is consistent with DIREC = 1 and
   CTYPE = 12 in *CONSTRAINED_LAGRANGE_IN_SOLID.
   For CTYPE = 11, LOCAL is always 1 and the 𝑥-axis is
   aligned with the element normal while the 𝑦-axis passes
   through the element center and the first node in the element
   connectivity (*ELEMENT_BEAM in 2D or *ELEMENT_SHELL in 3D).
















   ..
       !! processed by numpydoc !!

.. py:property:: vecid1
   :type: int


   
   Get or set the *DEFINE_VECTOR IDs to define a specific coordinate system. VECID1
   and VECID2 give the 𝑥- and 𝑦-direction respectively. The 𝑧-
   vector is a cross product of VECID1 and VECID2. If this latter is not
   orthogonal to VECID1, its direction will be corrected with a crossproduct
   of 𝑧- and 𝑥-vectors. The vectors are stored as isoparametric
   locations to update their directions if the element deforms or rotates.
















   ..
       !! processed by numpydoc !!

.. py:property:: vecid2
   :type: int


   
   Get or set the *DEFINE_VECTOR IDs to define a specific coordinate system. VECID1
   and VECID2 give the 𝑥- and 𝑦-direction respectively. The 𝑧-
   vector is a cross product of VECID1 and VECID2. If this latter is not
   orthogonal to VECID1, its direction will be corrected with a crossproduct
   of 𝑧- and 𝑥-vectors. The vectors are stored as isoparametric
   locations to update their directions if the element deforms or rotates.
















   ..
       !! processed by numpydoc !!

.. py:property:: userdef
   :type: int


   
   Get or set the Flag to compute Aij and Bij with a user defined routine in the file
   dyn21.F called lagpor_getab_userdef. The file is part of the general package usermat..
















   ..
       !! processed by numpydoc !!

.. py:property:: axx
   :type: float


   
   Get or set the Viscous matrix for the porous flow Ergun equation.
















   ..
       !! processed by numpydoc !!

.. py:property:: axy
   :type: float


   
   Get or set the Viscous matrix for the porous flow Ergun equation.
















   ..
       !! processed by numpydoc !!

.. py:property:: axz
   :type: float


   
   Get or set the Viscous matrix for the porous flow Ergun equation.
















   ..
       !! processed by numpydoc !!

.. py:property:: bxx
   :type: float


   
   Get or set the Inertial matrix for the porous flow Ergun equation.
















   ..
       !! processed by numpydoc !!

.. py:property:: bxy
   :type: float


   
   Get or set the Inertial matrix for the porous flow Ergun equation.
















   ..
       !! processed by numpydoc !!

.. py:property:: bxz
   :type: float


   
   Get or set the Inertial matrix for the porous flow Ergun equation.
















   ..
       !! processed by numpydoc !!

.. py:property:: ayx
   :type: float


   
   Get or set the Viscous matrix for the porous flow Ergun equation.
















   ..
       !! processed by numpydoc !!

.. py:property:: ayy
   :type: float


   
   Get or set the Viscous matrix for the porous flow Ergun equation.
















   ..
       !! processed by numpydoc !!

.. py:property:: ayz
   :type: float


   
   Get or set the Viscous matrix for the porous flow Ergun equation.
















   ..
       !! processed by numpydoc !!

.. py:property:: byx
   :type: float


   
   Get or set the Inertial matrix for the porous flow Ergun equation.
















   ..
       !! processed by numpydoc !!

.. py:property:: byy
   :type: float


   
   Get or set the Inertial matrix for the porous flow Ergun equation.
















   ..
       !! processed by numpydoc !!

.. py:property:: byz
   :type: float


   
   Get or set the Inertial matrix for the porous flow Ergun equation.
















   ..
       !! processed by numpydoc !!

.. py:property:: azx
   :type: float


   
   Get or set the Viscous matrix for the porous flow Ergun equation.
















   ..
       !! processed by numpydoc !!

.. py:property:: azy
   :type: float


   
   Get or set the Viscous matrix for the porous flow Ergun equation.
















   ..
       !! processed by numpydoc !!

.. py:property:: azz
   :type: float


   
   Get or set the Viscous matrix for the porous flow Ergun equation.
















   ..
       !! processed by numpydoc !!

.. py:property:: bzx
   :type: float


   
   Get or set the Inertial matrix for the porous flow Ergun equation.
















   ..
       !! processed by numpydoc !!

.. py:property:: bzy
   :type: float


   
   Get or set the Inertial matrix for the porous flow Ergun equation.
















   ..
       !! processed by numpydoc !!

.. py:property:: bzz
   :type: float


   
   Get or set the Inertial matrix for the porous flow Ergun equation.
















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
   :value: 'DEFINE'


.. py:attribute:: subkeyword
   :value: 'POROUS_LAGRANGIAN'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





