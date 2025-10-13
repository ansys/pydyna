





:class:`ConstrainedNodeSet`
===========================


.. py:class:: constrained_node_set.ConstrainedNodeSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_NODE_SET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedNodeSet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~cnsid`
            - Get or set the Optional constrained node set ID
          * - :py:attr:`~nsid`
            - Get or set the Node set ID, see *SET_NODE.
          * - :py:attr:`~dof`
            - Get or set the Applicable degrees-of-freedom:
          * - :py:attr:`~tf`
            - Get or set the Failure time for nodal constraint set.


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

    from constrained_node_set import ConstrainedNodeSet

Property detail
---------------

.. py:property:: cnsid
   :type: Optional[int]


   
   Get or set the Optional constrained node set ID
















   ..
       !! processed by numpydoc !!

.. py:property:: nsid
   :type: Optional[int]


   
   Get or set the Node set ID, see *SET_NODE.
















   ..
       !! processed by numpydoc !!

.. py:property:: dof
   :type: int


   
   Get or set the Applicable degrees-of-freedom:
   EQ.1: x-translational degree-of-freedom,
   EQ.2: y-translational degree-of-freedom,
   EQ.3: z-translational degree-of-freedom,
   EQ.4: x and y-translational degrees-of-freedom,
   EQ.5: y and z-translational degrees-of-freedom,
   EQ.6: z and x-translational degrees-of-freedom,
   EQ.7: x, y, and z-translational degrees-of-freedom.
   EQ.8: electric potential of piezoelectric material.
















   ..
       !! processed by numpydoc !!

.. py:property:: tf
   :type: float


   
   Get or set the Failure time for nodal constraint set.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'NODE_SET'






