





:class:`IcfdControlEmbedshell`
==============================


.. py:class:: icfd_control_embedshell.IcfdControlEmbedshell(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_CONTROL_EMBEDSHELL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdControlEmbedshell

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~gtype`
            - Get or set the Gap type. Defines the criteria for selecting a distance to build the gap between the embedded nodes and the newly generated :
          * - :py:attr:`~dist`
            - Get or set the Distance value if GTYPE=1 or scale factor value if GTYPE=0
          * - :py:attr:`~tps`
            - Get or set the Triple Point Seal. Allows to control the fluid escape through triple points


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

    from icfd_control_embedshell import IcfdControlEmbedshell

Property detail
---------------

.. py:property:: gtype
   :type: int


   
   Get or set the Gap type. Defines the criteria for selecting a distance to build the gap between the embedded nodes and the newly generated :
   EQ.0:   Automatic and based on the surface mesh size multiplied by a scale factor given by DIST.Default method.
   EQ.1 : Specific gap size given by the user and defined by DIST
















   ..
       !! processed by numpydoc !!

.. py:property:: dist
   :type: float


   
   Get or set the Distance value if GTYPE=1 or scale factor value if GTYPE=0
















   ..
       !! processed by numpydoc !!

.. py:property:: tps
   :type: int


   
   Get or set the Triple Point Seal. Allows to control the fluid escape through triple points
   EQ.0:   Off
   EQ.1 : On.The triple points of embedded shells in contact to walls or among each other are sealed and no flow goes through them
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'CONTROL_EMBEDSHELL'






