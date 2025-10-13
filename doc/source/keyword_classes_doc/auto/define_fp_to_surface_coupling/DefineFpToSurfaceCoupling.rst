





:class:`DefineFpToSurfaceCoupling`
==================================


.. py:class:: define_fp_to_surface_coupling.DefineFpToSurfaceCoupling(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_FP_TO_SURFACE_COUPLING keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineFpToSurfaceCoupling

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~fp`
            - Get or set the Part set ID defined in the coupling on the slave side.
          * - :py:attr:`~surf`
            - Get or set the Segments set ID defined in the coupling on the master side. Currently the segments set should be generated from the 8-noded hexahedron elements.
          * - :py:attr:`~fptype`
            - Get or set the Type for SLAVE:
          * - :py:attr:`~surftype`
            - Get or set the Type for SURF:
          * - :py:attr:`~sbc`
            - Get or set the Type of boundary condition.
          * - :py:attr:`~sca`
            - Get or set the Static (equilibrium) contact angle in radian
          * - :py:attr:`~sfp`
            - Get or set the Stiffness coefficient along the normal direction of the contact interface. SFP should be less than 1.0. If SFPSFPN is too small, large penetrations can occur.
          * - :py:attr:`~title`
            - Get or set the Additional title line


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~keyword`
            - 
          * - :py:attr:`~subkeyword`
            - 
          * - :py:attr:`~option_specs`
            - Get the card format type.






Import detail
-------------

.. code-block:: python

    from define_fp_to_surface_coupling import DefineFpToSurfaceCoupling

Property detail
---------------

.. py:property:: fp
   :type: Optional[int]


   
   Get or set the Part set ID defined in the coupling on the slave side.
















   ..
       !! processed by numpydoc !!

.. py:property:: surf
   :type: Optional[int]


   
   Get or set the Segments set ID defined in the coupling on the master side. Currently the segments set should be generated from the 8-noded hexahedron elements.
















   ..
       !! processed by numpydoc !!

.. py:property:: fptype
   :type: int


   
   Get or set the Type for SLAVE:
   EQ.0: Part set ID
   EQ.1 : Part ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: surftype
   :type: int


   
   Get or set the Type for SURF:
   EQ.0:   Segment set ID
















   ..
       !! processed by numpydoc !!

.. py:property:: sbc
   :type: Optional[int]


   
   Get or set the Type of boundary condition.
   EQ.0: free-slip boundary
   EQ.1: non - slip boundary
















   ..
       !! processed by numpydoc !!

.. py:property:: sca
   :type: int


   
   Get or set the Static (equilibrium) contact angle in radian
















   ..
       !! processed by numpydoc !!

.. py:property:: sfp
   :type: int


   
   Get or set the Stiffness coefficient along the normal direction of the contact interface. SFP should be less than 1.0. If SFPSFPN is too small, large penetrations can occur.
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Additional title line
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DEFINE'


.. py:attribute:: subkeyword
   :value: 'FP_TO_SURFACE_COUPLING'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





