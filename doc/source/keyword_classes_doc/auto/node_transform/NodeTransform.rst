





:class:`NodeTransform`
======================


.. py:class:: node_transform.NodeTransform(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA NODE_TRANSFORM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: NodeTransform

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~trsid`
            - Get or set the The ID of the transformation defined under *DEFINE_TRANSFOR-MATION
          * - :py:attr:`~nsid`
            - Get or set the Node set ID of the set of nodes to be subject to the transformation.
          * - :py:attr:`~immed`
            - Get or set the Optional flag for transformation processing :
          * - :py:attr:`~trsid_link`
            - Get the DefineTransformation object for trsid.


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

    from node_transform import NodeTransform

Property detail
---------------

.. py:property:: trsid
   :type: Optional[int]


   
   Get or set the The ID of the transformation defined under *DEFINE_TRANSFOR-MATION
















   ..
       !! processed by numpydoc !!

.. py:property:: nsid
   :type: Optional[int]


   
   Get or set the Node set ID of the set of nodes to be subject to the transformation.
















   ..
       !! processed by numpydoc !!

.. py:property:: immed
   :type: int


   
   Get or set the Optional flag for transformation processing :
   EQ.0:   Node transformation is performed after all input cards are read through.It is more efficient,and the definition sequence of  NODE_TRANSFORMand its NSID is irrelevant, i.e., the referred NSID doesn�t have to be defined prior to* NODE_TRANSFORM.However, for example, if nodes in NSID are used in POS6N of* DEFINE_TRANSFORMATION, its original coordinates, not the transformed coordinates, will be used to define the transformation matrix.
   EQ.1 : Node transformation is performed immediately after * NODE_TRANSFORM is read.The referred NSID and its nodes have to be defined prior to * NODE_TRANSFORM
















   ..
       !! processed by numpydoc !!

.. py:property:: trsid_link
   :type: define_transformation.DefineTransformation


   
   Get the DefineTransformation object for trsid.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'NODE'


.. py:attribute:: subkeyword
   :value: 'TRANSFORM'






