





:class:`BoundaryPzepot`
=======================


.. py:class:: boundary_pzepot.BoundaryPzepot(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_PZEPOT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryPzepot

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the ID of this boundary condition, which can be referred to by *SENSOR _CONTROL with TYPE='PZBC' or *DEFINE_CURVE_FUNCTION with FUNCTION='ECHGBC'.
          * - :py:attr:`~nsid`
            - Get or set the Node set ID, see *SET_NODE.
          * - :py:attr:`~lcid`
            - Get or set the Load curve giving prescribed electric potential as a function of time.
          * - :py:attr:`~sf`
            - Get or set the Scale factor on curve or constant electric potential if LCID = 0.


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

    from boundary_pzepot import BoundaryPzepot

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the ID of this boundary condition, which can be referred to by *SENSOR _CONTROL with TYPE='PZBC' or *DEFINE_CURVE_FUNCTION with FUNCTION='ECHGBC'.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsid
   :type: Optional[int]


   
   Get or set the Node set ID, see *SET_NODE.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve giving prescribed electric potential as a function of time.
















   ..
       !! processed by numpydoc !!

.. py:property:: sf
   :type: Optional[int]


   
   Get or set the Scale factor on curve or constant electric potential if LCID = 0.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'PZEPOT'






