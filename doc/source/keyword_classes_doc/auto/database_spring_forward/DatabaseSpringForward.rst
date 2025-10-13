





:class:`DatabaseSpringForward`
==============================


.. py:class:: database_spring_forward.DatabaseSpringForward(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DATABASE_SPRING_FORWARD keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DatabaseSpringForward

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~iflag`
            - Get or set the Output type:


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

    from database_spring_forward import DatabaseSpringForward

Property detail
---------------

.. py:property:: iflag
   :type: int


   
   Get or set the Output type:
   EQ.0: off,
   EQ.1: output element nodal force vector for deformable nodes,
   EQ.2: output element nodal force vector for materials, subset for NIKE3D interface file.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DATABASE'


.. py:attribute:: subkeyword
   :value: 'SPRING_FORWARD'






