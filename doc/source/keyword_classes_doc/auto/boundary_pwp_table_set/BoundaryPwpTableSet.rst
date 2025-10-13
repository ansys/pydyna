





:class:`BoundaryPwpTableSet`
============================


.. py:class:: boundary_pwp_table_set.BoundaryPwpTableSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_PWP_TABLE_SET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryPwpTableSet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part Set ID.
          * - :py:attr:`~lc`
            - Get or set the Load curve giving pore water pressure head (length units) vs time. =0: constant pressure head assumed equal to CMULT(leave blank for TABLE option)
          * - :py:attr:`~tbirth`
            - Get or set the Time at which boundary condition becomes active
          * - :py:attr:`~tdeath`
            - Get or set the Time at which boundary condition becomes inactive
          * - :py:attr:`~itotex`
            - Get or set the Flag for type of pressure boundary condition: (see notes)
          * - :py:attr:`~table`
            - Get or set the Table ID for TABLE option only. See notes below.


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

    from boundary_pwp_table_set import BoundaryPwpTableSet

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part Set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: lc
   :type: Optional[float]


   
   Get or set the Load curve giving pore water pressure head (length units) vs time. =0: constant pressure head assumed equal to CMULT(leave blank for TABLE option)
















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

.. py:property:: itotex
   :type: int


   
   Get or set the Flag for type of pressure boundary condition: (see notes)
   =0:     Total head
   =1:     Excess head
   =2:     Hydraulic head
















   ..
       !! processed by numpydoc !!

.. py:property:: table
   :type: int


   
   Get or set the Table ID for TABLE option only. See notes below.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'PWP_TABLE_SET'






