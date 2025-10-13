





:class:`EfGrid`
===============


.. py:class:: ef_grid.EfGrid(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EF_GRID keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EfGrid

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ngx`
            - Get or set the The mathematical algorithm underlying the ray tracer, involves gridding the enclosure.  NGX specify the number of grid divisions along the x axis.  This parameter does not affect LS-DYNA’s ability to obtain a solution, but it does affect the amount of CPU time consumed to process each photon.  There is no fixed rule for picking NGX, NGY, and NGZ, however for large geometries involving 1,000 to 15,000 surfaces NGX = NGY = NGZ = 25 is often optimal.  For smaller geometries smaller values are recommended
          * - :py:attr:`~ngy`
            - Get or set the Specifies the number of grid divisions along the y-axis.
          * - :py:attr:`~ngz`
            - Get or set the Specifies the number of grid divisions along the z-axis


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

    from ef_grid import EfGrid

Property detail
---------------

.. py:property:: ngx
   :type: Optional[int]


   
   Get or set the The mathematical algorithm underlying the ray tracer, involves gridding the enclosure.  NGX specify the number of grid divisions along the x axis.  This parameter does not affect LS-DYNA’s ability to obtain a solution, but it does affect the amount of CPU time consumed to process each photon.  There is no fixed rule for picking NGX, NGY, and NGZ, however for large geometries involving 1,000 to 15,000 surfaces NGX = NGY = NGZ = 25 is often optimal.  For smaller geometries smaller values are recommended
















   ..
       !! processed by numpydoc !!

.. py:property:: ngy
   :type: Optional[int]


   
   Get or set the Specifies the number of grid divisions along the y-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: ngz
   :type: Optional[int]


   
   Get or set the Specifies the number of grid divisions along the z-axis
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EF'


.. py:attribute:: subkeyword
   :value: 'GRID'






