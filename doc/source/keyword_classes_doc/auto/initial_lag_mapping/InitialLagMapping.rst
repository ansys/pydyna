





:class:`InitialLagMapping`
==========================


.. py:class:: initial_lag_mapping.InitialLagMapping(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INITIAL_LAG_MAPPING keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InitialLagMapping

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~setid`
            - Get or set the part set ID
          * - :py:attr:`~xp`
            - Get or set the x-position of a point belonging to the plane from which the 3D mesh is generated
          * - :py:attr:`~yp`
            - Get or set the y-position of a point belonging to the plane from which the 3D mesh is generated
          * - :py:attr:`~zp`
            - Get or set the z-position of a point belonging to the plane from which the 3D mesh is generated
          * - :py:attr:`~vecid`
            - Get or set the ID of the symmetric axis defined by *DEFINE_VECTOR
          * - :py:attr:`~angle`
            - Get or set the Angle of rotation around an axis defined by *DEFINE_VECTOR
          * - :py:attr:`~nelangl`
            - Get or set the Mapping parameter.  See Remark 5.


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

    from initial_lag_mapping import InitialLagMapping

Property detail
---------------

.. py:property:: setid
   :type: Optional[int]


   
   Get or set the part set ID
















   ..
       !! processed by numpydoc !!

.. py:property:: xp
   :type: float


   
   Get or set the x-position of a point belonging to the plane from which the 3D mesh is generated
















   ..
       !! processed by numpydoc !!

.. py:property:: yp
   :type: float


   
   Get or set the y-position of a point belonging to the plane from which the 3D mesh is generated
















   ..
       !! processed by numpydoc !!

.. py:property:: zp
   :type: float


   
   Get or set the z-position of a point belonging to the plane from which the 3D mesh is generated
















   ..
       !! processed by numpydoc !!

.. py:property:: vecid
   :type: Optional[int]


   
   Get or set the ID of the symmetric axis defined by *DEFINE_VECTOR
















   ..
       !! processed by numpydoc !!

.. py:property:: angle
   :type: Optional[float]


   
   Get or set the Angle of rotation around an axis defined by *DEFINE_VECTOR
















   ..
       !! processed by numpydoc !!

.. py:property:: nelangl
   :type: Optional[int]


   
   Get or set the Mapping parameter.  See Remark 5.
   GT. 0:  For a 2D to 3D mapping, number of elements to create in the azimuthal direction for ANGLE
   EQ.-1:  No mesh is generated or projected.
   EQ.-2:  For a 3D to 3D mapping, ANGLE only rotates the data from the mapping file (not the current mesh).
   EQ.-3:  No mesh is generated or projected except that the boundary nodes of the current mesh are projected on the boundary faces of the previous mesh
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INITIAL'


.. py:attribute:: subkeyword
   :value: 'LAG_MAPPING'






