





:class:`AleFsiToLoadNode`
=========================


.. py:class:: ale_fsi_to_load_node.AleFsiToLoadNode(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ALE_FSI_TO_LOAD_NODE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: AleFsiToLoadNode

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~dt`
            - Get or set the Output intervals.
          * - :py:attr:`~nsid`
            - Get or set the Node Set ID.
          * - :py:attr:`~iopt`
            - Get or set the Options to create the keyword file alefsiloadnode.k
          * - :py:attr:`~path`
            - Get or set the Path to the directory where the databases are created.


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

    from ale_fsi_to_load_node import AleFsiToLoadNode

Property detail
---------------

.. py:property:: dt
   :type: Optional[float]


   
   Get or set the Output intervals.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsid
   :type: Optional[int]


   
   Get or set the Node Set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: iopt
   :type: int


   
   Get or set the Options to create the keyword file alefsiloadnode.k
   EQ.0: The keyword is created at the end of the run by LS-DYNA.
   EQ.1: The database of coupling forces is dumped without the conversion in keyword file at the end of the run. The database is then treated by a program (alefsiloadnode.exe) to write alefsiloadnode.k.
   EQ.2: The database of coupling forces is read back from the temporary files created by IOPT = 1 to directly apply the nodal forces without using *LOAD_NODE. The parameters DT and NSID are not read.
   EQ.3:   A database of coupling accelerations is dumped at the end of the run (see Remark 3).
   EQ.4:   The database of coupling accelerations created by IOPT = 3 (see Remark 3) is read back. The structure meshes can be different.
   The accelerations are interpolated at the nodes provided by NSID. The parameters DT and NSID are read
















   ..
       !! processed by numpydoc !!

.. py:property:: path
   :type: Optional[str]


   
   Get or set the Path to the directory where the databases are created.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ALE'


.. py:attribute:: subkeyword
   :value: 'FSI_TO_LOAD_NODE'






