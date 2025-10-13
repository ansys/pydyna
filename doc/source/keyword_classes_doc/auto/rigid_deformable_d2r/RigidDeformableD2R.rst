





:class:`RigidDeformableD2R`
===========================


.. py:class:: rigid_deformable_d2r.RigidDeformableD2R(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA RIGID_DEFORMABLE_D2R keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: RigidDeformableD2R

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID of the part which is switched to a rigid material.
          * - :py:attr:`~lrb`
            - Get or set the Part ID of the lead rigid body to which the part is merged.


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

    from rigid_deformable_d2r import RigidDeformableD2R

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID of the part which is switched to a rigid material.
















   ..
       !! processed by numpydoc !!

.. py:property:: lrb
   :type: int


   
   Get or set the Part ID of the lead rigid body to which the part is merged.
   EQ.0: The part becomes either an independent or lead rigid body.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'RIGID'


.. py:attribute:: subkeyword
   :value: 'DEFORMABLE_D2R'






