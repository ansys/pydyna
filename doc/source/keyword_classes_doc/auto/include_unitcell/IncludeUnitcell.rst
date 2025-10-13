





:class:`IncludeUnitcell`
========================


.. py:class:: include_unitcell.IncludeUnitcell(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INCLUDE_UNITCELL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IncludeUnitcell

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~filename`
            - Get or set the Name of the keyword file containing X, Y, Z coordinates as defined using keyword *DEFINE_CURVE_TRIM_3D..
          * - :py:attr:`~inpt`
            - Get or set the Type of input:
          * - :py:attr:`~oupt`
            - Get or set the Type of output:
          * - :py:attr:`~nedof`
            - Get or set the Number of extra nodal degrees of freedom (DOFs) for user-defined element. In the current implementation, the limit of NEDOF is 15
          * - :py:attr:`~dx`
            - Get or set the Length of the unit cell in the  x-direction
          * - :py:attr:`~dy`
            - Get or set the Length of the unit cell in the y -direction
          * - :py:attr:`~dz`
            - Get or set the Length of the unit cell in the  z-direction
          * - :py:attr:`~nex`
            - Get or set the Number of elements along the  x-direction
          * - :py:attr:`~ney`
            - Get or set the Number of elements along the  y-direction
          * - :py:attr:`~nez`
            - Get or set the Number of elements along the  z-direction
          * - :py:attr:`~nnpe`
            - Get or set the Number of nodes per element. The current implementation supports only 4-node tetrahedron or 8-node hexahedron elements
          * - :py:attr:`~tol`
            - Get or set the Tolerance for searching for each pair of nodes in the periodic positions to create the periodic boundary conditions. This tolerance may be needed because numerical errors in the mesh can cause the coordinates of the pairs of nodes to not be exactly in the periodic positions. The default tolerance is computed based on the size of unit cell
          * - :py:attr:`~noff`
            - Get or set the Offset of node IDs
          * - :py:attr:`~eoff`
            - Get or set the Offset of element IDs
          * - :py:attr:`~pnm`
            - Get or set the Part ID
          * - :py:attr:`~cnx`
            - Get or set the Node ID of the 1st control point for the constraint in the  x direction
          * - :py:attr:`~cny`
            - Get or set the Node ID of the 2nd control point for the constraint in the  y direction
          * - :py:attr:`~cnz`
            - Get or set the Node ID of the 3rd control point for the constraint in the  z direction
          * - :py:attr:`~ecnx`
            - Get or set the Node ID of extra control point for the constraint in x direction of 3 extra nodal DOFs
          * - :py:attr:`~ecny`
            - Get or set the Node ID of extra control point for the constraint in y  direction of 3 extra nodal DOFs
          * - :py:attr:`~ecnz`
            - Get or set the Node ID of extra control point for the constraint in  z direction of 3 extra nodal DOFs


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

    from include_unitcell import IncludeUnitcell

Property detail
---------------

.. py:property:: filename
   :type: Optional[str]


   
   Get or set the Name of the keyword file containing X, Y, Z coordinates as defined using keyword *DEFINE_CURVE_TRIM_3D..
















   ..
       !! processed by numpydoc !!

.. py:property:: inpt
   :type: int


   
   Get or set the Type of input:
   EQ.0:   Read * NODE information from the include file and add periodic boundary conditions to the include file.
   EQ.1 : Create a unit cell mesh with periodic boundary conditions,and output to the include file
















   ..
       !! processed by numpydoc !!

.. py:property:: oupt
   :type: int


   
   Get or set the Type of output:
   EQ.1:   Create a new main keyword file where the keyword * INCLUDE_?UNITCELL is replaced by * INCLUDE with the include file name
















   ..
       !! processed by numpydoc !!

.. py:property:: nedof
   :type: int


   
   Get or set the Number of extra nodal degrees of freedom (DOFs) for user-defined element. In the current implementation, the limit of NEDOF is 15
















   ..
       !! processed by numpydoc !!

.. py:property:: dx
   :type: float


   
   Get or set the Length of the unit cell in the  x-direction
















   ..
       !! processed by numpydoc !!

.. py:property:: dy
   :type: float


   
   Get or set the Length of the unit cell in the y -direction
















   ..
       !! processed by numpydoc !!

.. py:property:: dz
   :type: float


   
   Get or set the Length of the unit cell in the  z-direction
















   ..
       !! processed by numpydoc !!

.. py:property:: nex
   :type: int


   
   Get or set the Number of elements along the  x-direction
















   ..
       !! processed by numpydoc !!

.. py:property:: ney
   :type: int


   
   Get or set the Number of elements along the  y-direction
















   ..
       !! processed by numpydoc !!

.. py:property:: nez
   :type: int


   
   Get or set the Number of elements along the  z-direction
















   ..
       !! processed by numpydoc !!

.. py:property:: nnpe
   :type: int


   
   Get or set the Number of nodes per element. The current implementation supports only 4-node tetrahedron or 8-node hexahedron elements
















   ..
       !! processed by numpydoc !!

.. py:property:: tol
   :type: Optional[float]


   
   Get or set the Tolerance for searching for each pair of nodes in the periodic positions to create the periodic boundary conditions. This tolerance may be needed because numerical errors in the mesh can cause the coordinates of the pairs of nodes to not be exactly in the periodic positions. The default tolerance is computed based on the size of unit cell
















   ..
       !! processed by numpydoc !!

.. py:property:: noff
   :type: Optional[int]


   
   Get or set the Offset of node IDs
















   ..
       !! processed by numpydoc !!

.. py:property:: eoff
   :type: Optional[int]


   
   Get or set the Offset of element IDs
















   ..
       !! processed by numpydoc !!

.. py:property:: pnm
   :type: Optional[int]


   
   Get or set the Part ID
















   ..
       !! processed by numpydoc !!

.. py:property:: cnx
   :type: Optional[int]


   
   Get or set the Node ID of the 1st control point for the constraint in the  x direction
















   ..
       !! processed by numpydoc !!

.. py:property:: cny
   :type: Optional[int]


   
   Get or set the Node ID of the 2nd control point for the constraint in the  y direction
















   ..
       !! processed by numpydoc !!

.. py:property:: cnz
   :type: Optional[int]


   
   Get or set the Node ID of the 3rd control point for the constraint in the  z direction
















   ..
       !! processed by numpydoc !!

.. py:property:: ecnx
   :type: Optional[int]


   
   Get or set the Node ID of extra control point for the constraint in x direction of 3 extra nodal DOFs
















   ..
       !! processed by numpydoc !!

.. py:property:: ecny
   :type: Optional[int]


   
   Get or set the Node ID of extra control point for the constraint in y  direction of 3 extra nodal DOFs
















   ..
       !! processed by numpydoc !!

.. py:property:: ecnz
   :type: Optional[int]


   
   Get or set the Node ID of extra control point for the constraint in  z direction of 3 extra nodal DOFs
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INCLUDE'


.. py:attribute:: subkeyword
   :value: 'UNITCELL'






