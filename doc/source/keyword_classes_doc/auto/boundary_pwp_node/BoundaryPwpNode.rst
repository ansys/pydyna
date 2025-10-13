





:class:`BoundaryPwpNode`
========================


.. py:class:: boundary_pwp_node.BoundaryPwpNode(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_PWP_NODE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryPwpNode

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nid`
            - Get or set the NODE ID.
          * - :py:attr:`~lc`
            - Get or set the Load curve giving pore water pressure head (length units) vs time. =0: constant pressure head assumed equal to CMULT(leave blank for TABLE option)
          * - :py:attr:`~cmult`
            - Get or set the Factor on curve or constant pressure head if LC=0
          * - :py:attr:`~lcdr`
            - Get or set the Load curve giving pore water pressure head during dynamic relaxation.
          * - :py:attr:`~tbirth`
            - Get or set the Time at which boundary condition becomes active
          * - :py:attr:`~tdeath`
            - Get or set the Time at which boundary condition becomes inactive
          * - :py:attr:`~iphre`
            - Get or set the Flag =1 for phreatic behaviour (water can be removed by the boundary condition but not added, e.g. at a sloping free surface). Not applicable to TABLE option.
          * - :py:attr:`~itotex`
            - Get or set the Flag for type of pressure boundary condition: (see notes)
          * - :py:attr:`~idrflag`
            - Get or set the Active flag:
          * - :py:attr:`~lcleak`
            - Get or set the Optional load curve ID (see *DEFINE_CURVE) applicable to IPHRE = 1 only, giving area of the hole through which pore fluid leaks to the zero pressure boundary condition. See Remark 9.
          * - :py:attr:`~cleak`
            - Get or set the Discharge coefficient, applicable when LCLEAK is nonzero
          * - :py:attr:`~lcpum`
            - Get or set the Optional load curve ID (see *DEFINE_CURVE) giving volumetric outflow rate per node. The curve x-axis is time while the y-axis is in units of volume per unit time. If defined, LCPUMP overrides all other input fields on Card 2.  See Remark 11


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

    from boundary_pwp_node import BoundaryPwpNode

Property detail
---------------

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the NODE ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: lc
   :type: Optional[float]


   
   Get or set the Load curve giving pore water pressure head (length units) vs time. =0: constant pressure head assumed equal to CMULT(leave blank for TABLE option)
















   ..
       !! processed by numpydoc !!

.. py:property:: cmult
   :type: float


   
   Get or set the Factor on curve or constant pressure head if LC=0
















   ..
       !! processed by numpydoc !!

.. py:property:: lcdr
   :type: Optional[int]


   
   Get or set the Load curve giving pore water pressure head during dynamic relaxation.
















   ..
       !! processed by numpydoc !!

.. py:property:: tbirth
   :type: float


   
   Get or set the Time at which boundary condition becomes active
















   ..
       !! processed by numpydoc !!

.. py:property:: tdeath
   :type: float


   
   Get or set the Time at which boundary condition becomes inactive
















   ..
       !! processed by numpydoc !!

.. py:property:: iphre
   :type: int


   
   Get or set the Flag =1 for phreatic behaviour (water can be removed by the boundary condition but not added, e.g. at a sloping free surface). Not applicable to TABLE option.
















   ..
       !! processed by numpydoc !!

.. py:property:: itotex
   :type: int


   
   Get or set the Flag for type of pressure boundary condition: (see notes)
   =0:     Total head
   =1:     Excess head
   =2:     Hydraulic head
   =4:     Z-coord where head=0 (piezometric level)
















   ..
       !! processed by numpydoc !!

.. py:property:: idrflag
   :type: int


   
   Get or set the Active flag:
   =0:     Active only in transient analysis
   =1:     Active only in dynamic relaxation
   =2:     Active in all analysis phases(leave blank for TABLE option)
















   ..
       !! processed by numpydoc !!

.. py:property:: lcleak
   :type: Optional[int]


   
   Get or set the Optional load curve ID (see *DEFINE_CURVE) applicable to IPHRE = 1 only, giving area of the hole through which pore fluid leaks to the zero pressure boundary condition. See Remark 9.
















   ..
       !! processed by numpydoc !!

.. py:property:: cleak
   :type: Optional[float]


   
   Get or set the Discharge coefficient, applicable when LCLEAK is nonzero
















   ..
       !! processed by numpydoc !!

.. py:property:: lcpum
   :type: Optional[int]


   
   Get or set the Optional load curve ID (see *DEFINE_CURVE) giving volumetric outflow rate per node. The curve x-axis is time while the y-axis is in units of volume per unit time. If defined, LCPUMP overrides all other input fields on Card 2.  See Remark 11
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'PWP_NODE'






