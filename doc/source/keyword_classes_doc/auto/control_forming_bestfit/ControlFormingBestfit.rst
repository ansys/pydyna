





:class:`ControlFormingBestfit`
==============================


.. py:class:: control_forming_bestfit.ControlFormingBestfit(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_FORMING_BESTFIT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlFormingBestfit

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ifit`
            - Get or set the Best fit program activation flag:
          * - :py:attr:`~nskip`
            - Get or set the Number of nodes to skip in bucket searching.  NSKIP of “1” does not skip any nodes in searching therefore computing speed is the slowest but accuracy is the highest.  Higher values of NSKIP speed up the calculation time with slightly deteriorating accuracies.  NSKIP of “2” is recommended with IFAST=1.  See Table 0-1 for the effect of NSKIP on the accuracy of the fitting.
          * - :py:attr:`~gaponly`
            - Get or set the Separation distance calculation flag:
          * - :py:attr:`~ifast`
            - Get or set the Computing performance optimization flag:
          * - :py:attr:`~ifset`
            - Get or set the Optional flag to define a node set to be included or excluded in the best fitting.  The node set can be defined in a file that also includes the part to be best fitted to the target mesh.  Only keyword cards *NODE, *ELEMENT_SHELL and *CONSTRAINED_ADAPTIVITY (if applicable) need to be present in the file. The file can be included in an input deck (Example 1) using *INCLUDE.  A node set can be defined using LS-PrePost via menu options Model→CreEnt→Set Data→*SET_NODE→Cre.
          * - :py:attr:`~nsets`
            - Get or set the An optional node set ID of three nodes from the source mesh.  The nodes should be selected based on distinctive geometry features, such as, the center of an arc, the center of a dart, or the end node of a take-up bead (see Example 3 and Figure 0-2).  The three nodes must not be aligned in one straight line.  Define NSETS only if the orientation of the source mesh deviates from the target is large (>~more than 30 degrees in any direction).  This is the recommended method.
          * - :py:attr:`~nsett`
            - Get or set the An optional node set ID from the target mesh, consists of the corresponding three nodes from the same geometry features of the source mesh.  The three nodes should be input in the same order as those from the source mesh.  Approximate locations are acceptable.  Define NSETT only if NSETS is defined.  See Example 3 and Figure 0-2 for details.  This is the recommended method.
          * - :py:attr:`~filename`
            - Get or set the Target mesh in keyword format, where only *NODE and *ELEMENT_SHELL should be included.  The target mesh is typically the scanned part converted from the STL format file.


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

    from control_forming_bestfit import ControlFormingBestfit

Property detail
---------------

.. py:property:: ifit
   :type: int


   
   Get or set the Best fit program activation flag:
   IFIT.EQ.0:      do not perform best-fit.
   IFIT.EQ.1:      activate the best-fit program.
















   ..
       !! processed by numpydoc !!

.. py:property:: nskip
   :type: int


   
   Get or set the Number of nodes to skip in bucket searching.  NSKIP of “1” does not skip any nodes in searching therefore computing speed is the slowest but accuracy is the highest.  Higher values of NSKIP speed up the calculation time with slightly deteriorating accuracies.  NSKIP of “2” is recommended with IFAST=1.  See Table 0-1 for the effect of NSKIP on the accuracy of the fitting.
















   ..
       !! processed by numpydoc !!

.. py:property:: gaponly
   :type: int


   
   Get or set the Separation distance calculation flag:
   GAPONLY.EQ.0:   perform best-fit, calculate separation distances between the two best-fitted mesh parts.
   GAPONLY.EQ.1:   no best-fit, just calculate separation distances between the two existing mesh parts.
   GAPONLY.EQ.2:   User is responsible to move the parts closer in distance and orientation, in situation where target and source are not similar in shape.  Also see NSETS and NSETT (recommended method).
















   ..
       !! processed by numpydoc !!

.. py:property:: ifast
   :type: int


   
   Get or set the Computing performance optimization flag:
   IFAST.EQ.0:     no computing speed optimization.
   IFAST.EQ.1:     activate computing speed optimization, and is recommended.
















   ..
       !! processed by numpydoc !!

.. py:property:: ifset
   :type: int


   
   Get or set the Optional flag to define a node set to be included or excluded in the best fitting.  The node set can be defined in a file that also includes the part to be best fitted to the target mesh.  Only keyword cards *NODE, *ELEMENT_SHELL and *CONSTRAINED_ADAPTIVITY (if applicable) need to be present in the file. The file can be included in an input deck (Example 1) using *INCLUDE.  A node set can be defined using LS-PrePost via menu options Model→CreEnt→Set Data→*SET_NODE→Cre.
   IFSET.EQ.0:     all nodes in the included file will be best fitted.
   IFSET.GT.0:     the input value is a node set ID; only the nodes in the set will be best fitted.
   IFSET.LT.0:     the absolute value is a node set ID; all nodes excluding those in the set will be best fitted.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsets
   :type: Optional[int]


   
   Get or set the An optional node set ID of three nodes from the source mesh.  The nodes should be selected based on distinctive geometry features, such as, the center of an arc, the center of a dart, or the end node of a take-up bead (see Example 3 and Figure 0-2).  The three nodes must not be aligned in one straight line.  Define NSETS only if the orientation of the source mesh deviates from the target is large (>~more than 30 degrees in any direction).  This is the recommended method.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsett
   :type: Optional[int]


   
   Get or set the An optional node set ID from the target mesh, consists of the corresponding three nodes from the same geometry features of the source mesh.  The three nodes should be input in the same order as those from the source mesh.  Approximate locations are acceptable.  Define NSETT only if NSETS is defined.  See Example 3 and Figure 0-2 for details.  This is the recommended method.
















   ..
       !! processed by numpydoc !!

.. py:property:: filename
   :type: Optional[str]


   
   Get or set the Target mesh in keyword format, where only *NODE and *ELEMENT_SHELL should be included.  The target mesh is typically the scanned part converted from the STL format file.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'FORMING_BESTFIT'






