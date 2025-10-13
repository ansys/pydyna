





:class:`EmBoundaryPrescribed`
=============================


.. py:class:: em_boundary_prescribed.EmBoundaryPrescribed(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_BOUNDARY_PRESCRIBED keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmBoundaryPrescribed

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~bpid`
            - Get or set the ID of the Prescribed boundary.
          * - :py:attr:`~bptype`
            - Get or set the Boundary Prescribed type:
          * - :py:attr:`~settype`
            - Get or set the Set type:
          * - :py:attr:`~setid`
            - Get or set the set ID
          * - :py:attr:`~val`
            - Get or set the Value of the Resistance, current density or potential depending on BPTYPE.Ignored if LCID is defined
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID defining the value of the resistance, voltage or current function of time
          * - :py:attr:`~birtht`
            - Get or set the Birth times for that prescribed boundary.
          * - :py:attr:`~deatht`
            - Get or set the Death times for that prescribed boundary.


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

    from em_boundary_prescribed import EmBoundaryPrescribed

Property detail
---------------

.. py:property:: bpid
   :type: Optional[int]


   
   Get or set the ID of the Prescribed boundary.
   .
















   ..
       !! processed by numpydoc !!

.. py:property:: bptype
   :type: int


   
   Get or set the Boundary Prescribed type:
   EQ.1:Short (Scalar Potential set to 0.)
   EQ.2:Prescribed Resistance (Robin B.C).
   EQ.3:Prescribed Scalar Potential (Dirichlet B.C)
   EQ.4:Prescribed Current Density (Neumann B.C).
















   ..
       !! processed by numpydoc !!

.. py:property:: settype
   :type: int


   
   Get or set the Set type:
   EQ.1:Segment Set.
   EQ.2: Node Set.
   EQ.3: Fluid part. See *ICFD_PART.
















   ..
       !! processed by numpydoc !!

.. py:property:: setid
   :type: Optional[int]


   
   Get or set the set ID
   .
















   ..
       !! processed by numpydoc !!

.. py:property:: val
   :type: float


   
   Get or set the Value of the Resistance, current density or potential depending on BPTYPE.Ignored if LCID is defined
   .
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID defining the value of the resistance, voltage or current function of time
   .
















   ..
       !! processed by numpydoc !!

.. py:property:: birtht
   :type: float


   
   Get or set the Birth times for that prescribed boundary.
















   ..
       !! processed by numpydoc !!

.. py:property:: deatht
   :type: float


   
   Get or set the Death times for that prescribed boundary.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'BOUNDARY_PRESCRIBED'






