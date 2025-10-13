





:class:`Contact2DSlidingOnly`
=============================


.. py:class:: contact_2d_sliding_only.Contact2DSlidingOnly(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTACT_2D_SLIDING_ONLY keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: Contact2DSlidingOnly

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~surfa`
            - Get or set the Nodal set ID for the SURFA nodes, see *SET_‌NODE.  The surface specified with SURFA must be to the left of the surface specified with SURFB. For nonsymmetric contact, this surface is the tracked surface (all contacts in this section except PENALTY and PENALTY_FRICTION).
          * - :py:attr:`~surfb`
            - Get or set the Nodal set ID for the SURFB nodes, see *SET_‌NODE.  For nonsymmetric contact, this surface is the reference surface (all contacts in this section except PENALTY and PENALTY_FRICTION).
          * - :py:attr:`~tbirth`
            - Get or set the Birth time for contact.
          * - :py:attr:`~tdeath`
            - Get or set the Death time for contact
          * - :py:attr:`~ext_pas`
            - Get or set the Slideline extension bypass option.
          * - :py:attr:`~theta1`
            - Get or set the Angle in degrees of slideline extension at first SURFB node.
          * - :py:attr:`~theta2`
            - Get or set the Angle in degrees of slideline extension at last SURFB node.
          * - :py:attr:`~tol_ig`
            - Get or set the Tolerance for determining initial gaps. Default is set to 1.0E-03.
          * - :py:attr:`~pen`
            - Get or set the Scale factor or penalty. Default is set to 1.0E-01.
          * - :py:attr:`~toloff`
            - Get or set the Tolerance for stiffness insertion for implicit solution only. The contact stiffness is inserted when a node approaches a segment a distance equal to the segment length multiplied by TOLOFF. The stiffness is increased as the node moves closer with the full stiffness being used when the nodal point finally makes contact. default set to 0.25.
          * - :py:attr:`~frcscl`
            - Get or set the Scale factor for the interface friction.
          * - :py:attr:`~oneway`
            - Get or set the Flag for one way treatment. if set to 1.0 the nodal points on the slave surface are constrained to the master surface. This option is generally recommended if the master surface is rigid.


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

    from contact_2d_sliding_only import Contact2DSlidingOnly

Property detail
---------------

.. py:property:: surfa
   :type: Optional[int]


   
   Get or set the Nodal set ID for the SURFA nodes, see *SET_‌NODE.  The surface specified with SURFA must be to the left of the surface specified with SURFB. For nonsymmetric contact, this surface is the tracked surface (all contacts in this section except PENALTY and PENALTY_FRICTION).
















   ..
       !! processed by numpydoc !!

.. py:property:: surfb
   :type: Optional[int]


   
   Get or set the Nodal set ID for the SURFB nodes, see *SET_‌NODE.  For nonsymmetric contact, this surface is the reference surface (all contacts in this section except PENALTY and PENALTY_FRICTION).
















   ..
       !! processed by numpydoc !!

.. py:property:: tbirth
   :type: float


   
   Get or set the Birth time for contact.
















   ..
       !! processed by numpydoc !!

.. py:property:: tdeath
   :type: float


   
   Get or set the Death time for contact
















   ..
       !! processed by numpydoc !!

.. py:property:: ext_pas
   :type: int


   
   Get or set the Slideline extension bypass option.
   EQ.0: extensions are used (default),
   EQ.1: extensions are not used.
















   ..
       !! processed by numpydoc !!

.. py:property:: theta1
   :type: float


   
   Get or set the Angle in degrees of slideline extension at first SURFB node.
   EQ.0.0: extension remains tangent to first SURFB segment (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: theta2
   :type: float


   
   Get or set the Angle in degrees of slideline extension at last SURFB node.
   EQ.0.0: extension remains tangent to last DURFB segment (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: tol_ig
   :type: float


   
   Get or set the Tolerance for determining initial gaps. Default is set to 1.0E-03.
















   ..
       !! processed by numpydoc !!

.. py:property:: pen
   :type: float


   
   Get or set the Scale factor or penalty. Default is set to 1.0E-01.
















   ..
       !! processed by numpydoc !!

.. py:property:: toloff
   :type: float


   
   Get or set the Tolerance for stiffness insertion for implicit solution only. The contact stiffness is inserted when a node approaches a segment a distance equal to the segment length multiplied by TOLOFF. The stiffness is increased as the node moves closer with the full stiffness being used when the nodal point finally makes contact. default set to 0.25.
















   ..
       !! processed by numpydoc !!

.. py:property:: frcscl
   :type: float


   
   Get or set the Scale factor for the interface friction.
















   ..
       !! processed by numpydoc !!

.. py:property:: oneway
   :type: float


   
   Get or set the Flag for one way treatment. if set to 1.0 the nodal points on the slave surface are constrained to the master surface. This option is generally recommended if the master surface is rigid.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTACT'


.. py:attribute:: subkeyword
   :value: '2D_SLIDING_ONLY'






