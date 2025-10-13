





:class:`PartDuplicate`
======================


.. py:class:: part_duplicate.PartDuplicate(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA PART_DUPLICATE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: PartDuplicate

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ptype`
            - Get or set the Set to "PART" to duplicate a single part or "PSET" to duplicate a part set.
          * - :py:attr:`~typeid`
            - Get or set the ID of part or part set to be duplicated.
          * - :py:attr:`~idpoff`
            - Get or set the ID offset of newly created parts
          * - :py:attr:`~ideoff`
            - Get or set the ID offset of newly created elements.
          * - :py:attr:`~idnoff`
            - Get or set the ID offset of newly created nodes
          * - :py:attr:`~tranid`
            - Get or set the ID of *DEFINE_TRANSFORMATION to transform the existing nodes in a part or part set..
          * - :py:attr:`~boxid`
            - Get or set the ID of box defining the boundary of the transformed nodal coordinates; see Remark 6.
          * - :py:attr:`~zmin`
            - Get or set the ID of box defining the boundary of the transformed nodal coordinates; see Remark 6.
          * - :py:attr:`~tranid_link`
            - Get the DefineTransformation object for tranid.


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

    from part_duplicate import PartDuplicate

Property detail
---------------

.. py:property:: ptype
   :type: str


   
   Get or set the Set to "PART" to duplicate a single part or "PSET" to duplicate a part set.
















   ..
       !! processed by numpydoc !!

.. py:property:: typeid
   :type: Optional[int]


   
   Get or set the ID of part or part set to be duplicated.
















   ..
       !! processed by numpydoc !!

.. py:property:: idpoff
   :type: int


   
   Get or set the ID offset of newly created parts
















   ..
       !! processed by numpydoc !!

.. py:property:: ideoff
   :type: int


   
   Get or set the ID offset of newly created elements.
















   ..
       !! processed by numpydoc !!

.. py:property:: idnoff
   :type: int


   
   Get or set the ID offset of newly created nodes
















   ..
       !! processed by numpydoc !!

.. py:property:: tranid
   :type: int


   
   Get or set the ID of *DEFINE_TRANSFORMATION to transform the existing nodes in a part or part set..
















   ..
       !! processed by numpydoc !!

.. py:property:: boxid
   :type: int


   
   Get or set the ID of box defining the boundary of the transformed nodal coordinates; see Remark 6.
















   ..
       !! processed by numpydoc !!

.. py:property:: zmin
   :type: float


   
   Get or set the ID of box defining the boundary of the transformed nodal coordinates; see Remark 6.
















   ..
       !! processed by numpydoc !!

.. py:property:: tranid_link
   :type: define_transformation.DefineTransformation


   
   Get the DefineTransformation object for tranid.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'PART'


.. py:attribute:: subkeyword
   :value: 'DUPLICATE'






