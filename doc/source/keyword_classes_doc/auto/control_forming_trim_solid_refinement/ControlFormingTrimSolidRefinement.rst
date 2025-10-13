





:class:`ControlFormingTrimSolidRefinement`
==========================================


.. py:class:: control_forming_trim_solid_refinement.ControlFormingTrimSolidRefinement(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_FORMING_TRIM_SOLID_REFINEMENT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlFormingTrimSolidRefinement

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~irefine`
            - Get or set the A flag to activate the adaptive trimming of a multi-layer sandwiched part. Currently setting this to either 0 or 1 will turn on the adaptive trimming.
          * - :py:attr:`~ilevel`
            - Get or set the Adaptive refinement level.  Currently setting this variable to any integer other than 0 will refine the mesh one level down along the trim curve.


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

    from control_forming_trim_solid_refinement import ControlFormingTrimSolidRefinement

Property detail
---------------

.. py:property:: irefine
   :type: int


   
   Get or set the A flag to activate the adaptive trimming of a multi-layer sandwiched part. Currently setting this to either 0 or 1 will turn on the adaptive trimming.
   EQ.1:   Activate the adaptive trimming.
















   ..
       !! processed by numpydoc !!

.. py:property:: ilevel
   :type: int


   
   Get or set the Adaptive refinement level.  Currently setting this variable to any integer other than 0 will refine the mesh one level down along the trim curve.
   EQ.0:   no refinement
   EQ.1 : refine one level down.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'FORMING_TRIM_SOLID_REFINEMENT'






