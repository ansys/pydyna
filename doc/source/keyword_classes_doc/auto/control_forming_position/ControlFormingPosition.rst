





:class:`ControlFormingPosition`
===============================


.. py:class:: control_forming_position.ControlFormingPosition(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_FORMING_POSITION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlFormingPosition

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID
          * - :py:attr:`~premove`
            - Get or set the The distance to pre-move the tool in the reverse direction of this tool's movement
          * - :py:attr:`~target`
            - Get or set the Move part (PID) in the reverse direction of this tool movement, and make sure the minimum distance between PID and TARGET is defined by GAP


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

    from control_forming_position import ControlFormingPosition

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID
















   ..
       !! processed by numpydoc !!

.. py:property:: premove
   :type: Optional[float]


   
   Get or set the The distance to pre-move the tool in the reverse direction of this tool's movement
















   ..
       !! processed by numpydoc !!

.. py:property:: target
   :type: Optional[int]


   
   Get or set the Move part (PID) in the reverse direction of this tool movement, and make sure the minimum distance between PID and TARGET is defined by GAP
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'FORMING_POSITION'






