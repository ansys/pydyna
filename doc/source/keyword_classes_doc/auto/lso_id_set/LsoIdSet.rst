





:class:`LsoIdSet`
=================


.. py:class:: lso_id_set.LsoIdSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LSO_ID_SET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LsoIdSet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~setid`
            - Get or set the Identifier for this ID set.
          * - :py:attr:`~type`
            - Get or set the The kind of IDs in this set:
          * - :py:attr:`~solver`
            - Get or set the Name of the solver (MECH, ICFD, CESE, EM,  ).
          * - :py:attr:`~id1`
            - Get or set the IDs of the TYPE kind.
          * - :py:attr:`~id2`
            - Get or set the IDs of the TYPE kind.
          * - :py:attr:`~id3`
            - Get or set the IDs of the TYPE kind.
          * - :py:attr:`~id4`
            - Get or set the IDs of the TYPE kind.
          * - :py:attr:`~id5`
            - Get or set the IDs of the TYPE kind.
          * - :py:attr:`~id6`
            - Get or set the IDs of the TYPE kind.
          * - :py:attr:`~id7`
            - Get or set the IDs of the TYPE kind.
          * - :py:attr:`~id8`
            - Get or set the IDs of the TYPE kind.


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

    from lso_id_set import LsoIdSet

Property detail
---------------

.. py:property:: setid
   :type: Optional[int]


   
   Get or set the Identifier for this ID set.
















   ..
       !! processed by numpydoc !!

.. py:property:: type
   :type: str


   
   Get or set the The kind of IDs in this set:
   EQ.SEG_SETS: Each ID is a segment set connected with SOLVER.
   EQ.CIRCUIT: Each ID is a circuit ID (from *EM cards)
   EQ.SURF_PARTS: Each ID is a surface part number (See*MESH_SURFACE_ELEMENT)
   EQ.VOL_PARTS: Each ID is a volume part number (See *MESH_VOLUME)
   EQ.SURF_ELES: Each ID is a surface element number (See *MESH_SURFACE_ELEMENT)
















   ..
       !! processed by numpydoc !!

.. py:property:: solver
   :type: str


   
   Get or set the Name of the solver (MECH, ICFD, CESE, EM,  ).
















   ..
       !! processed by numpydoc !!

.. py:property:: id1
   :type: Optional[int]


   
   Get or set the IDs of the TYPE kind.
















   ..
       !! processed by numpydoc !!

.. py:property:: id2
   :type: Optional[int]


   
   Get or set the IDs of the TYPE kind.
















   ..
       !! processed by numpydoc !!

.. py:property:: id3
   :type: Optional[int]


   
   Get or set the IDs of the TYPE kind.
















   ..
       !! processed by numpydoc !!

.. py:property:: id4
   :type: Optional[int]


   
   Get or set the IDs of the TYPE kind.
















   ..
       !! processed by numpydoc !!

.. py:property:: id5
   :type: Optional[int]


   
   Get or set the IDs of the TYPE kind.
















   ..
       !! processed by numpydoc !!

.. py:property:: id6
   :type: Optional[int]


   
   Get or set the IDs of the TYPE kind.
















   ..
       !! processed by numpydoc !!

.. py:property:: id7
   :type: Optional[int]


   
   Get or set the IDs of the TYPE kind.
















   ..
       !! processed by numpydoc !!

.. py:property:: id8
   :type: Optional[int]


   
   Get or set the IDs of the TYPE kind.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LSO'


.. py:attribute:: subkeyword
   :value: 'ID_SET'






