





:class:`SetPartTree`
====================


.. py:class:: set_part_tree.SetPartTree(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SET_PART_TREE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SetPartTree

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~brid`
            - Get or set the Branch identification. A unique number must be specified..
          * - :py:attr:`~heading`
            - Get or set the Heading for the branch.
          * - :py:attr:`~compi`
            - Get or set the Components of branch BRID:
          * - :py:attr:`~title`
            - Get or set the Additional title line


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~keyword`
            - 
          * - :py:attr:`~subkeyword`
            - 
          * - :py:attr:`~option_specs`
            - Get the card format type.






Import detail
-------------

.. code-block:: python

    from set_part_tree import SetPartTree

Property detail
---------------

.. py:property:: brid
   :type: Optional[int]


   
   Get or set the Branch identification. A unique number must be specified..
















   ..
       !! processed by numpydoc !!

.. py:property:: heading
   :type: Optional[str]


   
   Get or set the Heading for the branch.
















   ..
       !! processed by numpydoc !!

.. py:property:: compi
   :type: Optional[int]


   
   Get or set the Components of branch BRID:
   GT.0: ID of a sub-branch
   LT.0: ID of a part.
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Additional title line
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'SET'


.. py:attribute:: subkeyword
   :value: 'PART_TREE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





