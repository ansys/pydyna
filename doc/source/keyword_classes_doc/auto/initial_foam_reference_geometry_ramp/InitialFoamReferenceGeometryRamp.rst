





:class:`InitialFoamReferenceGeometryRamp`
=========================================


.. py:class:: initial_foam_reference_geometry_ramp.InitialFoamReferenceGeometryRamp(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INITIAL_FOAM_REFERENCE_GEOMETRY_RAMP keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InitialFoamReferenceGeometryRamp

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ndtrrg`
            - Get or set the Number of time steps taken for an element to restore its reference geometry.  Definition of NDTRRG allows an element to ramp up to its reference shape in NDTRRG time steps.  Currently ls-dynauses only one NDTRRG and applies it to all foam materials with reference geometries. If more than one NDTRRG is defined, the latter defined one will replace the previously define one.
          * - :py:attr:`~nid`
            - Get or set the Node ID.
          * - :py:attr:`~x`
            - Get or set the x-coordinate.
          * - :py:attr:`~y`
            - Get or set the y-coordinate.
          * - :py:attr:`~z`
            - Get or set the z-coordinate.


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

    from initial_foam_reference_geometry_ramp import InitialFoamReferenceGeometryRamp

Property detail
---------------

.. py:property:: ndtrrg
   :type: Optional[int]


   
   Get or set the Number of time steps taken for an element to restore its reference geometry.  Definition of NDTRRG allows an element to ramp up to its reference shape in NDTRRG time steps.  Currently ls-dynauses only one NDTRRG and applies it to all foam materials with reference geometries. If more than one NDTRRG is defined, the latter defined one will replace the previously define one.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Node ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: x
   :type: float


   
   Get or set the x-coordinate.
















   ..
       !! processed by numpydoc !!

.. py:property:: y
   :type: float


   
   Get or set the y-coordinate.
















   ..
       !! processed by numpydoc !!

.. py:property:: z
   :type: float


   
   Get or set the z-coordinate.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INITIAL'


.. py:attribute:: subkeyword
   :value: 'FOAM_REFERENCE_GEOMETRY_RAMP'






