





:class:`ConstrainedNodeInterpolation`
=====================================


.. py:class:: constrained_node_interpolation.ConstrainedNodeInterpolation(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_NODE_INTERPOLATION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedNodeInterpolation

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nid`
            - Get or set the Node ID of the interpolation node as defined in *NODE (see Remark 1).
          * - :py:attr:`~numcn`
            - Get or set the Number of nodes controling the interpolation node.
          * - :py:attr:`~cn1`
            - Get or set the Node ID of controlling node i.
          * - :py:attr:`~w1`
            - Get or set the Weighting factor of controlling node i.
          * - :py:attr:`~cn2`
            - Get or set the Node ID of controlling node i.
          * - :py:attr:`~w2`
            - Get or set the Weighting factor of controlling node i.
          * - :py:attr:`~cn3`
            - Get or set the Node ID of controlling node i.
          * - :py:attr:`~w3`
            - Get or set the Weighting factor of controlling node i.
          * - :py:attr:`~cn4`
            - Get or set the Node ID of controlling node i.
          * - :py:attr:`~w4`
            - Get or set the Weighting factor of controlling node i.


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

    from constrained_node_interpolation import ConstrainedNodeInterpolation

Property detail
---------------

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Node ID of the interpolation node as defined in *NODE (see Remark 1).
















   ..
       !! processed by numpydoc !!

.. py:property:: numcn
   :type: Optional[int]


   
   Get or set the Number of nodes controling the interpolation node.
















   ..
       !! processed by numpydoc !!

.. py:property:: cn1
   :type: Optional[int]


   
   Get or set the Node ID of controlling node i.
















   ..
       !! processed by numpydoc !!

.. py:property:: w1
   :type: Optional[float]


   
   Get or set the Weighting factor of controlling node i.
















   ..
       !! processed by numpydoc !!

.. py:property:: cn2
   :type: Optional[int]


   
   Get or set the Node ID of controlling node i.
















   ..
       !! processed by numpydoc !!

.. py:property:: w2
   :type: Optional[float]


   
   Get or set the Weighting factor of controlling node i.
















   ..
       !! processed by numpydoc !!

.. py:property:: cn3
   :type: Optional[int]


   
   Get or set the Node ID of controlling node i.
















   ..
       !! processed by numpydoc !!

.. py:property:: w3
   :type: Optional[float]


   
   Get or set the Weighting factor of controlling node i.
















   ..
       !! processed by numpydoc !!

.. py:property:: cn4
   :type: Optional[int]


   
   Get or set the Node ID of controlling node i.
















   ..
       !! processed by numpydoc !!

.. py:property:: w4
   :type: Optional[float]


   
   Get or set the Weighting factor of controlling node i.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'NODE_INTERPOLATION'






