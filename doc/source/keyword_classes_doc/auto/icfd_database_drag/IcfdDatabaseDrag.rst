





:class:`IcfdDatabaseDrag`
=========================


.. py:class:: icfd_database_drag.IcfdDatabaseDrag(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_DATABASE_DRAG keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdDatabaseDrag

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID of the surface where the drag force will be computed
          * - :py:attr:`~cpid`
            - Get or set the Center point ID used for the calculation of the force's moment. By default the reference frame center is used (Coordinates (0,0,0)).
          * - :py:attr:`~dtout`
            - Get or set the Time interval to print the output. If DTOUT is equal to 0.0, then the ICFD timestep will be used.
          * - :py:attr:`~perout`
            - Get or set the Outputs the contribution of the different elements on the total drag in fractions of the total drag in the d3plots.
          * - :py:attr:`~divi`
            - Get or set the Number of drag divisions for PEROUT. Default is 10 which means the contributions will be grouped in 10 deciles.
          * - :py:attr:`~elout`
            - Get or set the Outputs the drag value of each element in the d3plots.
          * - :py:attr:`~ssout`
            - Get or set the Outputs the pressure loads caused by the fluid on each solid segment set in keyword format. FSI needs to be activated.


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

    from icfd_database_drag import IcfdDatabaseDrag

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID of the surface where the drag force will be computed
















   ..
       !! processed by numpydoc !!

.. py:property:: cpid
   :type: Optional[int]


   
   Get or set the Center point ID used for the calculation of the force's moment. By default the reference frame center is used (Coordinates (0,0,0)).
















   ..
       !! processed by numpydoc !!

.. py:property:: dtout
   :type: float


   
   Get or set the Time interval to print the output. If DTOUT is equal to 0.0, then the ICFD timestep will be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: perout
   :type: int


   
   Get or set the Outputs the contribution of the different elements on the total drag in fractions of the total drag in the d3plots.
















   ..
       !! processed by numpydoc !!

.. py:property:: divi
   :type: int


   
   Get or set the Number of drag divisions for PEROUT. Default is 10 which means the contributions will be grouped in 10 deciles.
















   ..
       !! processed by numpydoc !!

.. py:property:: elout
   :type: int


   
   Get or set the Outputs the drag value of each element in the d3plots.
















   ..
       !! processed by numpydoc !!

.. py:property:: ssout
   :type: int


   
   Get or set the Outputs the pressure loads caused by the fluid on each solid segment set in keyword format. FSI needs to be activated.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'DATABASE_DRAG'






