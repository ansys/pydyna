





:class:`InterfaceDeHbond`
=========================


.. py:class:: interface_de_hbond.InterfaceDeHbond(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INTERFACE_DE_HBOND keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InterfaceDeHbond

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Interface ID. All interfaces should have a unique ID.
          * - :py:attr:`~pid1`
            - Get or set the First part ID.
          * - :py:attr:`~pid2`
            - Get or set the Second part ID. PID1 and PID2 define the bonds that this fracture
          * - :py:attr:`~ptype1`
            - Get or set the First part type:
          * - :py:attr:`~ptype2`
            - Get or set the Second part type:
          * - :py:attr:`~frmdl`
            - Get or set the Fracture model. (same as FRMDL in Card2 of keyword *DEFINE_DE_HBOND.).
          * - :py:attr:`~frgk`
            - Get or set the Fracture energy release rate for volumetric deformation. (same as        FRGK in Card2 of keyword *DEFINE_DE_HBOND.).
          * - :py:attr:`~frgs`
            - Get or set the Fracture energy release rate for shear deformation. (same as FRGS in Card 2 of keyword *DEFINE_DE_HBOND.).
          * - :py:attr:`~dmg`
            - Get or set the Continuous damage development. (same as DMG in Card 2 of keyword *DEFINE_DE_HBOND.).


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

    from interface_de_hbond import InterfaceDeHbond

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Interface ID. All interfaces should have a unique ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: pid1
   :type: Optional[int]


   
   Get or set the First part ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: pid2
   :type: Optional[int]


   
   Get or set the Second part ID. PID1 and PID2 define the bonds that this fracture
   model is applied to. There are three combinations as
   Case a: PID1.EQ.0 This is the default model for all bonds, overriding the default       model defined in Card 2 of *DEFINE_DE_HBOND.
   Case b: PID1.GT.0 and PID2.EQ.0 This model is applied to the bonds within part PID1, instead of the default model.
   Case c: PID1.GT.0 and PID2.GT.0 This model is applied to the bonds between parts PID1 and
   PID2 only, but not to those within part PID1 or part PID2 (as in case b).
   Notes:
   1. The default fracture model is applied to all parts that are not specified in case b.
   2. The fracture model of the part with a smaller part id is applied to the bonds between two different parts if not specified in case c..
















   ..
       !! processed by numpydoc !!

.. py:property:: ptype1
   :type: int


   
   Get or set the First part type:
   EQ.0: DES part set
   EQ.1: DES part.
















   ..
       !! processed by numpydoc !!

.. py:property:: ptype2
   :type: int


   
   Get or set the Second part type:
   EQ.0: DES part set
   EQ.1: DES part.
















   ..
       !! processed by numpydoc !!

.. py:property:: frmdl
   :type: int


   
   Get or set the Fracture model. (same as FRMDL in Card2 of keyword *DEFINE_DE_HBOND.).
















   ..
       !! processed by numpydoc !!

.. py:property:: frgk
   :type: Optional[float]


   
   Get or set the Fracture energy release rate for volumetric deformation. (same as        FRGK in Card2 of keyword *DEFINE_DE_HBOND.).
















   ..
       !! processed by numpydoc !!

.. py:property:: frgs
   :type: Optional[float]


   
   Get or set the Fracture energy release rate for shear deformation. (same as FRGS in Card 2 of keyword *DEFINE_DE_HBOND.).
















   ..
       !! processed by numpydoc !!

.. py:property:: dmg
   :type: float


   
   Get or set the Continuous damage development. (same as DMG in Card 2 of keyword *DEFINE_DE_HBOND.).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INTERFACE'


.. py:attribute:: subkeyword
   :value: 'DE_HBOND'






