





:class:`BoundaryCyclic`
=======================


.. py:class:: boundary_cyclic.BoundaryCyclic(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_CYCLIC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryCyclic

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~xc`
            - Get or set the x-component axis vector of axis of rotation.
          * - :py:attr:`~yc`
            - Get or set the y-component axis vector of axis of rotation.
          * - :py:attr:`~zc`
            - Get or set the z-component axis vector of axis of rotation.
          * - :py:attr:`~nsid1`
            - Get or set the Node set ID for first boundary plane.
          * - :py:attr:`~nsid2`
            - Get or set the Node set ID for second boundary plane. Each boundary node in this boundary plane is constrained to its corresponding node in the first node set. Node sets NSID1 and NSID2 must contain the same number of nodal points. Care has to be taken that the nodes in both node sets have a location which, if given in cylindrical coordinates, differ all by the same angle.
          * - :py:attr:`~iglobal`
            - Get or set the Flag for repeating symmetry:
          * - :py:attr:`~isort`
            - Get or set the Flag for automatic sorting of boundary nodes:


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

    from boundary_cyclic import BoundaryCyclic

Property detail
---------------

.. py:property:: xc
   :type: Optional[float]


   
   Get or set the x-component axis vector of axis of rotation.
















   ..
       !! processed by numpydoc !!

.. py:property:: yc
   :type: Optional[float]


   
   Get or set the y-component axis vector of axis of rotation.
















   ..
       !! processed by numpydoc !!

.. py:property:: zc
   :type: Optional[float]


   
   Get or set the z-component axis vector of axis of rotation.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsid1
   :type: Optional[int]


   
   Get or set the Node set ID for first boundary plane.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsid2
   :type: Optional[int]


   
   Get or set the Node set ID for second boundary plane. Each boundary node in this boundary plane is constrained to its corresponding node in the first node set. Node sets NSID1 and NSID2 must contain the same number of nodal points. Care has to be taken that the nodes in both node sets have a location which, if given in cylindrical coordinates, differ all by the same angle.
















   ..
       !! processed by numpydoc !!

.. py:property:: iglobal
   :type: int


   
   Get or set the Flag for repeating symmetry:
   EQ. 0: Cyclic symmetry (default).
   EQ. 1: Repeating symmetry in planes normal to global X.
   EQ. 2: Repeating symmetry in planes normal to global Y.
   EQ. 3: Repeating symmetry in planes normal to global Z.
















   ..
       !! processed by numpydoc !!

.. py:property:: isort
   :type: int


   
   Get or set the Flag for automatic sorting of boundary nodes:
   EQ. 0: No automatic sorting (default).
   EQ. 1: Automatic sorting of nodes.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'CYCLIC'






