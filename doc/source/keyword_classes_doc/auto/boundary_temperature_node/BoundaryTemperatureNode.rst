





:class:`BoundaryTemperatureNode`
================================


.. py:class:: boundary_temperature_node.BoundaryTemperatureNode(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_TEMPERATURE_NODE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryTemperatureNode

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nid`
            - Get or set the Node ID.
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID for temperature versus time:
          * - :py:attr:`~cmult`
            - Get or set the Curve multiplier for temperature.
          * - :py:attr:`~loc`
            - Get or set the Application of surface for thermal shell elements, see paramter, TSHELL, in the *CONTROL_SHELL input:
          * - :py:attr:`~tdeath`
            - Get or set the Deactivation time for temperature boundary condition. At this point in time the temperature constraint is removed.
          * - :py:attr:`~tbirth`
            - Get or set the Activation time for temperature boundary condition. Before this point in time the temperature constraint is ignored


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

    from boundary_temperature_node import BoundaryTemperatureNode

Property detail
---------------

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Node ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: int


   
   Get or set the Load curve ID for temperature versus time:
   EQ.0: use the constant multiplier value given below by CMULT (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: cmult
   :type: float


   
   Get or set the Curve multiplier for temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: loc
   :type: int


   
   Get or set the Application of surface for thermal shell elements, see paramter, TSHELL, in the *CONTROL_SHELL input:
   EQ.-1: lower surface of thermal shell element,
   EQ. 1: upper surface of thermal shell element
















   ..
       !! processed by numpydoc !!

.. py:property:: tdeath
   :type: float


   
   Get or set the Deactivation time for temperature boundary condition. At this point in time the temperature constraint is removed.
















   ..
       !! processed by numpydoc !!

.. py:property:: tbirth
   :type: float


   
   Get or set the Activation time for temperature boundary condition. Before this point in time the temperature constraint is ignored
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'TEMPERATURE_NODE'






