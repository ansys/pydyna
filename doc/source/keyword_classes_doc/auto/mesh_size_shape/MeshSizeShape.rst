





:class:`MeshSizeShape`
======================


.. py:class:: mesh_size_shape.MeshSizeShape(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MESH_SIZE_SHAPE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MeshSizeShape

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sname`
            - Get or set the Shape name. Possibilities include  box,  cylinder,  pol and  sphere
          * - :py:attr:`~force`
            - Get or set the Force to keep the mesh size criteria even after a remeshing is done.
          * - :py:attr:`~method`
            - Get or set the Force to keep the mesh size criteria even after a remeshing is done.
          * - :py:attr:`~bt`
            - Get or set the Force to keep the mesh size criteria even after a remeshing is done.
          * - :py:attr:`~dt`
            - Get or set the Force to keep the mesh size criteria even after a remeshing is done.
          * - :py:attr:`~msize`
            - Get or set the Mesh size that needs to be applied in the zone of the shape defined by Sname
          * - :py:attr:`~pminx`
            - Get or set the X,Y,Z for the point of minimum coordinates
          * - :py:attr:`~pminy`
            - Get or set the X,Y,Z for the point of minimum coordinates
          * - :py:attr:`~pminz`
            - Get or set the X,Y,Z for the point of minimum coordinates
          * - :py:attr:`~pmaxx`
            - Get or set the X,Y,Z for the point of maximum coordinates
          * - :py:attr:`~pmaxy`
            - Get or set the X,Y,Z for the point of maximum coordinates
          * - :py:attr:`~pmaxz`
            - Get or set the X,Y,Z for the point of maximum coordinates
          * - :py:attr:`~radius`
            - Get or set the Radius of the sphere if Sname is Sphere or of the cross section disk if Sname is Cylinder
          * - :py:attr:`~centerx`
            - Get or set the Coordinates of the sphere center in cases where Sname is Sphere
          * - :py:attr:`~centery`
            - Get or set the Coordinates of the sphere center in cases where Sname is Sphere
          * - :py:attr:`~centerz`
            - Get or set the Coordinates of the sphere center in cases where Sname is Sphere


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

    from mesh_size_shape import MeshSizeShape

Property detail
---------------

.. py:property:: sname
   :type: str


   
   Get or set the Shape name. Possibilities include  box,  cylinder,  pol and  sphere
















   ..
       !! processed by numpydoc !!

.. py:property:: force
   :type: int


   
   Get or set the Force to keep the mesh size criteria even after a remeshing is done.
   EQ.0: Off, mesh size shape will be lost if a remeshing occurs.
   EQ.1: On.
















   ..
       !! processed by numpydoc !!

.. py:property:: method
   :type: int


   
   Get or set the Force to keep the mesh size criteria even after a remeshing is done.
   EQ.0: Off, mesh size shape will be lost if a remeshing occurs.
   EQ.1: On.
















   ..
       !! processed by numpydoc !!

.. py:property:: bt
   :type: float


   
   Get or set the Force to keep the mesh size criteria even after a remeshing is done.
   EQ.0: Off, mesh size shape will be lost if a remeshing occurs.
   EQ.1: On.
















   ..
       !! processed by numpydoc !!

.. py:property:: dt
   :type: float


   
   Get or set the Force to keep the mesh size criteria even after a remeshing is done.
   EQ.0: Off, mesh size shape will be lost if a remeshing occurs.
   EQ.1: On.
















   ..
       !! processed by numpydoc !!

.. py:property:: msize
   :type: Optional[float]


   
   Get or set the Mesh size that needs to be applied in the zone of the shape defined by Sname
















   ..
       !! processed by numpydoc !!

.. py:property:: pminx
   :type: Optional[float]


   
   Get or set the X,Y,Z for the point of minimum coordinates
















   ..
       !! processed by numpydoc !!

.. py:property:: pminy
   :type: Optional[float]


   
   Get or set the X,Y,Z for the point of minimum coordinates
















   ..
       !! processed by numpydoc !!

.. py:property:: pminz
   :type: Optional[float]


   
   Get or set the X,Y,Z for the point of minimum coordinates
















   ..
       !! processed by numpydoc !!

.. py:property:: pmaxx
   :type: Optional[float]


   
   Get or set the X,Y,Z for the point of maximum coordinates
















   ..
       !! processed by numpydoc !!

.. py:property:: pmaxy
   :type: Optional[float]


   
   Get or set the X,Y,Z for the point of maximum coordinates
















   ..
       !! processed by numpydoc !!

.. py:property:: pmaxz
   :type: Optional[float]


   
   Get or set the X,Y,Z for the point of maximum coordinates
















   ..
       !! processed by numpydoc !!

.. py:property:: radius
   :type: Optional[float]


   
   Get or set the Radius of the sphere if Sname is Sphere or of the cross section disk if Sname is Cylinder
















   ..
       !! processed by numpydoc !!

.. py:property:: centerx
   :type: Optional[float]


   
   Get or set the Coordinates of the sphere center in cases where Sname is Sphere
















   ..
       !! processed by numpydoc !!

.. py:property:: centery
   :type: Optional[float]


   
   Get or set the Coordinates of the sphere center in cases where Sname is Sphere
















   ..
       !! processed by numpydoc !!

.. py:property:: centerz
   :type: Optional[float]


   
   Get or set the Coordinates of the sphere center in cases where Sname is Sphere
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'MESH'


.. py:attribute:: subkeyword
   :value: 'SIZE_SHAPE'






