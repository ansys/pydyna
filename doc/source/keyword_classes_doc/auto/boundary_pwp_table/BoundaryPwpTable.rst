





:class:`BoundaryPwpTable`
=========================


.. py:class:: boundary_pwp_table.BoundaryPwpTable(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_PWP_TABLE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryPwpTable

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID.
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

    from boundary_pwp_table import BoundaryPwpTable

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID.
















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
   :value: 'PWP_TABLE'






