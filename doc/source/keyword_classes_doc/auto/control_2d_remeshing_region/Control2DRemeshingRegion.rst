





:class:`Control2DRemeshingRegion`
=================================


.. py:class:: control_2d_remeshing_region.Control2DRemeshingRegion(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_2D_REMESHING_REGION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: Control2DRemeshingRegion

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ltyp`
            - Get or set the Type of regions defined by the parameters PARi:
          * - :py:attr:`~par1`
            - Get or set the Parameters defined by LTYP
          * - :py:attr:`~par2`
            - Get or set the Parameters defined by LTYP
          * - :py:attr:`~par3`
            - Get or set the Parameters defined by LTYP
          * - :py:attr:`~par4`
            - Get or set the Parameters defined by LTYP
          * - :py:attr:`~par5`
            - Get or set the Parameters defined by LTYP
          * - :py:attr:`~par6`
            - Get or set the Parameters defined by LTYP
          * - :py:attr:`~par7`
            - Get or set the Parameters defined by LTYP


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

    from control_2d_remeshing_region import Control2DRemeshingRegion

Property detail
---------------

.. py:property:: ltyp
   :type: Optional[int]


   
   Get or set the Type of regions defined by the parameters PARi:
   EQ.1:   Box.PAR1 is the ID of * DEFINE_BOX that defines the region to remesh.The other parameters are not used.
   EQ.2 : Part set.PAR1 is the ID of * SET_PART that selects the parts to remesh.The other parameters are not used.
   EQ.3 : Region around elements in contact from a specified contact.To specify the desired contact, set PAR1 to its order of appearance in the input deck.For instance, PAR1 = 5 if the desired contact is the 5th contact keyword to appear in the input deck.A box is created around each element in contact for the remeshing.PAR2 through PAR4 add padding around the box to increase the remeshing region.PAR2 > 0 is the length subtracted from the x - coordinate of the box’s lower corner.PAR3 > 0 is the length added to the x - coordinate of the box’s upper corner.PAR4 > 0 is the length subtracted from the y - coordinate of the box’s lower corner.PAR5 > 0 is the length added to the y - coordinate of the box’s upper corner.The last 2 parameters are not used.
   EQ.4:   PAR1 is the ID of * DEFINE_BOX that selects mesh boundaries (edges) of remeshing regions along which nodes added after remeshing(Hanging nodes between edge ends) keep their initial parametric positions between the boundary corner nodesedge-end constraining nodes).The other parameters are not used.By default, when nodes are added to the edges of elements in the regionand the edges are not on the mesh boundary, the positionsand velocities of the hanging nodes are interpolated from the positionsand velocities of the original constraining nodes along these edges(constraining nodes are nodes that existed before remeshing).If the edge of a remeshed element is on the mesh boundaries, the positionsand velocities of the hanging nodes on the edge are, by default, not interpolated because they are likely to be subject to boundary conditions.With LTYP = 4, the hanging nodes on the boundary of the mesh are interpolated.
   EQ.5:   PAR1 is the ID of *SET_NODE that selects nodes along shell edges. After remeshing, the node set is recreated with nodes along the same shell edges. PAR2>0 is a thickness for the shell edges to select the new nodes after remeshing. The other parameters are not used.
   EQ.6:   PAR1 is the ID of * SET_PART that selects the parts for which the total displacements are output in D3PLOT after remeshings.
   EQ.7 : PAR1 is the ID of * SET_SHELL that selects the shells to remesh.The other parameters are not used
















   ..
       !! processed by numpydoc !!

.. py:property:: par1
   :type: float


   
   Get or set the Parameters defined by LTYP
















   ..
       !! processed by numpydoc !!

.. py:property:: par2
   :type: float


   
   Get or set the Parameters defined by LTYP
















   ..
       !! processed by numpydoc !!

.. py:property:: par3
   :type: float


   
   Get or set the Parameters defined by LTYP
















   ..
       !! processed by numpydoc !!

.. py:property:: par4
   :type: float


   
   Get or set the Parameters defined by LTYP
















   ..
       !! processed by numpydoc !!

.. py:property:: par5
   :type: float


   
   Get or set the Parameters defined by LTYP
















   ..
       !! processed by numpydoc !!

.. py:property:: par6
   :type: float


   
   Get or set the Parameters defined by LTYP
















   ..
       !! processed by numpydoc !!

.. py:property:: par7
   :type: float


   
   Get or set the Parameters defined by LTYP
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: '2D_REMESHING_REGION'






