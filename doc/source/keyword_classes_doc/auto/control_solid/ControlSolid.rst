





:class:`ControlSolid`
=====================


.. py:class:: control_solid.ControlSolid(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_SOLID keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlSolid

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~esort`
            - Get or set the Automatic sorting of tetrahedron and pentahedron elements to treat degenerate tetrahedron and pentahedron elements as tetrahedron (formulation 10)  and pentahedron (formulation 15) solids, respective. See *SECTION_SOLID.
          * - :py:attr:`~fmatrix`
            - Get or set the Default method used in the calculation of the defomation gradient matrix.
          * - :py:attr:`~niptets`
            - Get or set the Number of integration points used in the quadratic tetrahedron elements. Either 4 or 5 can be specified. This option applies to the type 4 and type 16 tetrahedron elements.
          * - :py:attr:`~swlocl`
            - Get or set the Output option for stresses in solid elements used as spot welds with material *MAT_SPOTWELD.
          * - :py:attr:`~psfail`
            - Get or set the A nonzero PSFAIL has the same effect as setting ERODE = 1 in *CONTROL_TIMESTEP except that solid element erosion due to negative volume is limited to only the solid elements in part set PSFAIL.In other words, when PSFAIL is nonzero, the time-step-based criterion for erosion (TSMIN) applies to all solid elements (except formulations 11 and 12) while the negative volume criterion for erosion applies only to solids in part set PSFAIL.
          * - :py:attr:`~t10jtol`
            - Get or set the Tolerance for jacobian in 4-point 10-noded quadratic tetrahedra (type 16).If the quotient between the minimum and maximum jacobian value falls below this tolerance, a warning message is issued in the messag file. This is useful for tracking badly shaped elements in implicit analysis that deteriorates convergence, a value of 1.0 indicates a perfectly shaped element.
          * - :py:attr:`~icoh`
            - Get or set the Breaking LS-DYNA convention ICOH is interpreted digit-wise, namely as,
          * - :py:attr:`~tet13k`
            - Get or set the Set to 1 to invoke a consistent tangent stiffness matrix for the pressure averaged tetrahedron (type 13). This is a feature only for implicit analysis and only supported in SMP. This element type averages the volumetric strain over adjacent elements to alleviate volumetric locking, which implies that the corresponding material tangent stiffness should be treated accordingly. Due to the vaste amount of neighbors any given element may have in an arbitrary tetrahedral mesh, the expense for the matrix assembly is at the moment too high for this to pay off in a nonlinear implicit simulation. Whence this is an option that preferably is activated only in linear or eigenvalue analysis to exploit the stiffness characteristics of the type 13 tetrahedron.
          * - :py:attr:`~pm1`
            - Get or set the Components of a permutation vector for nodes that define the 10-node tetrahedron. The nodal numbering of 10-node tetrahedron elements is somewhat arbitrary. The permutation vector allows other numbering schemes to be used. Unless defined, this permutation vector is not used. PM1-PM10 are unique number between 1 to 10 inclusive that reorders the input node ID s for a 10-node tetrahedron into the order used by LS-DYNA.
          * - :py:attr:`~pm2`
            - Get or set the Components of a permutation vector for nodes that define the 10-node tetrahedron. The nodal numbering of 10-node tetrahedron elements is somewhat arbitrary. The permutation vector allows other numbering schemes to be used. Unless defined, this permutation vector is not used. PM1-PM10 are unique number between 1 to 10 inclusive that reorders the input node ID s for a 10-node tetrahedron into the order used by LS-DYNA.
          * - :py:attr:`~pm3`
            - Get or set the Components of a permutation vector for nodes that define the 10-node tetrahedron. The nodal numbering of 10-node tetrahedron elements is somewhat arbitrary. The permutation vector allows other numbering schemes to be used. Unless defined, this permutation vector is not used. PM1-PM10 are unique number between 1 to 10 inclusive that reorders the input node ID s for a 10-node tetrahedron into the order used by LS-DYNA.
          * - :py:attr:`~pm4`
            - Get or set the Components of a permutation vector for nodes that define the 10-node tetrahedron. The nodal numbering of 10-node tetrahedron elements is somewhat arbitrary. The permutation vector allows other numbering schemes to be used. Unless defined, this permutation vector is not used. PM1-PM10 are unique number between 1 to 10 inclusive that reorders the input node ID s for a 10-node tetrahedron into the order used by LS-DYNA.
          * - :py:attr:`~pm5`
            - Get or set the Components of a permutation vector for nodes that define the 10-node tetrahedron. The nodal numbering of 10-node tetrahedron elements is somewhat arbitrary. The permutation vector allows other numbering schemes to be used. Unless defined, this permutation vector is not used. PM1-PM10 are unique number between 1 to 10 inclusive that reorders the input node ID s for a 10-node tetrahedron into the order used by LS-DYNA.
          * - :py:attr:`~pm6`
            - Get or set the Components of a permutation vector for nodes that define the 10-node tetrahedron. The nodal numbering of 10-node tetrahedron elements is somewhat arbitrary. The permutation vector allows other numbering schemes to be used. Unless defined, this permutation vector is not used. PM1-PM10 are unique number between 1 to 10 inclusive that reorders the input node ID s for a 10-node tetrahedron into the order used by LS-DYNA.
          * - :py:attr:`~pm7`
            - Get or set the Components of a permutation vector for nodes that define the 10-node tetrahedron. The nodal numbering of 10-node tetrahedron elements is somewhat arbitrary. The permutation vector allows other numbering schemes to be used. Unless defined, this permutation vector is not used. PM1-PM10 are unique number between 1 to 10 inclusive that reorders the input node ID s for a 10-node tetrahedron into the order used by LS-DYNA.
          * - :py:attr:`~pm8`
            - Get or set the Components of a permutation vector for nodes that define the 10-node tetrahedron. The nodal numbering of 10-node tetrahedron elements is somewhat arbitrary. The permutation vector allows other numbering schemes to be used. Unless defined, this permutation vector is not used. PM1-PM10 are unique number between 1 to 10 inclusive that reorders the input node ID s for a 10-node tetrahedron into the order used by LS-DYNA.
          * - :py:attr:`~pm9`
            - Get or set the Components of a permutation vector for nodes that define the 10-node tetrahedron. The nodal numbering of 10-node tetrahedron elements is somewhat arbitrary. The permutation vector allows other numbering schemes to be used. Unless defined, this permutation vector is not used. PM1-PM10 are unique number between 1 to 10 inclusive that reorders the input node ID s for a 10-node tetrahedron into the order used by LS-DYNA.
          * - :py:attr:`~pm10`
            - Get or set the Components of a permutation vector for nodes that define the 10-node tetrahedron. The nodal numbering of 10-node tetrahedron elements is somewhat arbitrary. The permutation vector allows other numbering schemes to be used. Unless defined, this permutation vector is not used. PM1-PM10 are unique number between 1 to 10 inclusive that reorders the input node ID s for a 10-node tetrahedron into the order used by LS-DYNA.
          * - :py:attr:`~tet13v`
            - Get or set the Choice of type 13 solid implementation:


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

    from control_solid import ControlSolid

Property detail
---------------

.. py:property:: esort
   :type: int


   
   Get or set the Automatic sorting of tetrahedron and pentahedron elements to treat degenerate tetrahedron and pentahedron elements as tetrahedron (formulation 10)  and pentahedron (formulation 15) solids, respective. See *SECTION_SOLID.
   EQ.0: no sorting(default).
   EQ.1: sort tetrahedron to type 10, pentahedron to type 15.
   EQ.2: sort tetrahedron to type 10, 1-point integrated pentahedron to type 115, fully integrated pentahedron to type 15.
   EQ.3: same as EQ.1 but also print switched elements in message file.
   EQ.4: same as EQ.2 but also print switched elements in message file
















   ..
       !! processed by numpydoc !!

.. py:property:: fmatrix
   :type: int


   
   Get or set the Default method used in the calculation of the defomation gradient matrix.
   EQ.1: Update incrementally in time. This is the default for explicit.
   EQ.2: Directly compute F. This is the default for implicit and implicit/explicit switching.
















   ..
       !! processed by numpydoc !!

.. py:property:: niptets
   :type: int


   
   Get or set the Number of integration points used in the quadratic tetrahedron elements. Either 4 or 5 can be specified. This option applies to the type 4 and type 16 tetrahedron elements.
















   ..
       !! processed by numpydoc !!

.. py:property:: swlocl
   :type: int


   
   Get or set the Output option for stresses in solid elements used as spot welds with material *MAT_SPOTWELD.
   EQ.1: Global (default),
   EQ.2: Local
















   ..
       !! processed by numpydoc !!

.. py:property:: psfail
   :type: int


   
   Get or set the A nonzero PSFAIL has the same effect as setting ERODE = 1 in *CONTROL_TIMESTEP except that solid element erosion due to negative volume is limited to only the solid elements in part set PSFAIL.In other words, when PSFAIL is nonzero, the time-step-based criterion for erosion (TSMIN) applies to all solid elements (except formulations 11 and 12) while the negative volume criterion for erosion applies only to solids in part set PSFAIL.
















   ..
       !! processed by numpydoc !!

.. py:property:: t10jtol
   :type: float


   
   Get or set the Tolerance for jacobian in 4-point 10-noded quadratic tetrahedra (type 16).If the quotient between the minimum and maximum jacobian value falls below this tolerance, a warning message is issued in the messag file. This is useful for tracking badly shaped elements in implicit analysis that deteriorates convergence, a value of 1.0 indicates a perfectly shaped element.
















   ..
       !! processed by numpydoc !!

.. py:property:: icoh
   :type: int


   
   Get or set the Breaking LS-DYNA convention ICOH is interpreted digit-wise, namely as,
   ICOH = [LK] = K + 10×L .
   The first digit, in the one’s place, which we shall call K is interpreted as follows:K.EQ.0:    No cohesive element deletion due to neighbor failure.
   K.EQ.1: Solid elements having ELFORM = 19 – 22 (or ELFORM = 1, 2, 15 being used with * MAT_169) will be eroded when neighboring shell or solid elements fail.This works for nodewise connected partsand tied contacts.
   The second digit, in the ten’s place is, which we shall call L is interpreted as stated below.Note that if ICOH is less than 10 (having a single digit) then L defaults to zero.
   L.EQ.0 : Default stable time step estimate, which is computed from the stiffnessand the nodal masses of the topand bottom as with discrete elements.
   L.EQ.1 : Most conservative(smallest) stable time step estimate.This method calculates mass by integrating the density.
   L.EQ.2 : Intermediate stable time step estimate.Same as the default except reduced by a factor of 1 / √2 corresponding to halving the masses.
















   ..
       !! processed by numpydoc !!

.. py:property:: tet13k
   :type: int


   
   Get or set the Set to 1 to invoke a consistent tangent stiffness matrix for the pressure averaged tetrahedron (type 13). This is a feature only for implicit analysis and only supported in SMP. This element type averages the volumetric strain over adjacent elements to alleviate volumetric locking, which implies that the corresponding material tangent stiffness should be treated accordingly. Due to the vaste amount of neighbors any given element may have in an arbitrary tetrahedral mesh, the expense for the matrix assembly is at the moment too high for this to pay off in a nonlinear implicit simulation. Whence this is an option that preferably is activated only in linear or eigenvalue analysis to exploit the stiffness characteristics of the type 13 tetrahedron.
















   ..
       !! processed by numpydoc !!

.. py:property:: pm1
   :type: Optional[int]


   
   Get or set the Components of a permutation vector for nodes that define the 10-node tetrahedron. The nodal numbering of 10-node tetrahedron elements is somewhat arbitrary. The permutation vector allows other numbering schemes to be used. Unless defined, this permutation vector is not used. PM1-PM10 are unique number between 1 to 10 inclusive that reorders the input node ID s for a 10-node tetrahedron into the order used by LS-DYNA.
















   ..
       !! processed by numpydoc !!

.. py:property:: pm2
   :type: Optional[int]


   
   Get or set the Components of a permutation vector for nodes that define the 10-node tetrahedron. The nodal numbering of 10-node tetrahedron elements is somewhat arbitrary. The permutation vector allows other numbering schemes to be used. Unless defined, this permutation vector is not used. PM1-PM10 are unique number between 1 to 10 inclusive that reorders the input node ID s for a 10-node tetrahedron into the order used by LS-DYNA.
















   ..
       !! processed by numpydoc !!

.. py:property:: pm3
   :type: Optional[int]


   
   Get or set the Components of a permutation vector for nodes that define the 10-node tetrahedron. The nodal numbering of 10-node tetrahedron elements is somewhat arbitrary. The permutation vector allows other numbering schemes to be used. Unless defined, this permutation vector is not used. PM1-PM10 are unique number between 1 to 10 inclusive that reorders the input node ID s for a 10-node tetrahedron into the order used by LS-DYNA.
















   ..
       !! processed by numpydoc !!

.. py:property:: pm4
   :type: Optional[int]


   
   Get or set the Components of a permutation vector for nodes that define the 10-node tetrahedron. The nodal numbering of 10-node tetrahedron elements is somewhat arbitrary. The permutation vector allows other numbering schemes to be used. Unless defined, this permutation vector is not used. PM1-PM10 are unique number between 1 to 10 inclusive that reorders the input node ID s for a 10-node tetrahedron into the order used by LS-DYNA.
















   ..
       !! processed by numpydoc !!

.. py:property:: pm5
   :type: Optional[int]


   
   Get or set the Components of a permutation vector for nodes that define the 10-node tetrahedron. The nodal numbering of 10-node tetrahedron elements is somewhat arbitrary. The permutation vector allows other numbering schemes to be used. Unless defined, this permutation vector is not used. PM1-PM10 are unique number between 1 to 10 inclusive that reorders the input node ID s for a 10-node tetrahedron into the order used by LS-DYNA.
















   ..
       !! processed by numpydoc !!

.. py:property:: pm6
   :type: Optional[int]


   
   Get or set the Components of a permutation vector for nodes that define the 10-node tetrahedron. The nodal numbering of 10-node tetrahedron elements is somewhat arbitrary. The permutation vector allows other numbering schemes to be used. Unless defined, this permutation vector is not used. PM1-PM10 are unique number between 1 to 10 inclusive that reorders the input node ID s for a 10-node tetrahedron into the order used by LS-DYNA.
















   ..
       !! processed by numpydoc !!

.. py:property:: pm7
   :type: Optional[int]


   
   Get or set the Components of a permutation vector for nodes that define the 10-node tetrahedron. The nodal numbering of 10-node tetrahedron elements is somewhat arbitrary. The permutation vector allows other numbering schemes to be used. Unless defined, this permutation vector is not used. PM1-PM10 are unique number between 1 to 10 inclusive that reorders the input node ID s for a 10-node tetrahedron into the order used by LS-DYNA.
















   ..
       !! processed by numpydoc !!

.. py:property:: pm8
   :type: Optional[int]


   
   Get or set the Components of a permutation vector for nodes that define the 10-node tetrahedron. The nodal numbering of 10-node tetrahedron elements is somewhat arbitrary. The permutation vector allows other numbering schemes to be used. Unless defined, this permutation vector is not used. PM1-PM10 are unique number between 1 to 10 inclusive that reorders the input node ID s for a 10-node tetrahedron into the order used by LS-DYNA.
















   ..
       !! processed by numpydoc !!

.. py:property:: pm9
   :type: Optional[int]


   
   Get or set the Components of a permutation vector for nodes that define the 10-node tetrahedron. The nodal numbering of 10-node tetrahedron elements is somewhat arbitrary. The permutation vector allows other numbering schemes to be used. Unless defined, this permutation vector is not used. PM1-PM10 are unique number between 1 to 10 inclusive that reorders the input node ID s for a 10-node tetrahedron into the order used by LS-DYNA.
















   ..
       !! processed by numpydoc !!

.. py:property:: pm10
   :type: Optional[int]


   
   Get or set the Components of a permutation vector for nodes that define the 10-node tetrahedron. The nodal numbering of 10-node tetrahedron elements is somewhat arbitrary. The permutation vector allows other numbering schemes to be used. Unless defined, this permutation vector is not used. PM1-PM10 are unique number between 1 to 10 inclusive that reorders the input node ID s for a 10-node tetrahedron into the order used by LS-DYNA.
















   ..
       !! processed by numpydoc !!

.. py:property:: tet13v
   :type: Optional[int]


   
   Get or set the Choice of type 13 solid implementation:
   EQ.0:   Efficient version(default).With the single precision version of LS - DYNA, a little noise in the solution for elements that are moving long distances with rigid body motion could be observed.
   EQ.1 : More accurate version(smoother results) with an additional cost of about 15 % .
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'SOLID'






