





:class:`ControlFormingInitialThickness`
=======================================


.. py:class:: control_forming_initial_thickness.ControlFormingInitialThickness(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_FORMING_INITIAL_THICKNESS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlFormingInitialThickness

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID of the sheet blank to be defined with varying thickness, as in *PART.  Currently only 1 PID is allowed.
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID defining thickness (Y-values) vs. distance (X-values) starting from position coordinates (X0, Y0, Z0) and in the direction of a vector [VX, VY, VZ], as in *DEFINE_CURVE.
          * - :py:attr:`~x0`
            - Get or set the Starting position coordinates.
          * - :py:attr:`~y0`
            - Get or set the Starting position coordinates.
          * - :py:attr:`~z0x`
            - Get or set the Starting position coordinates.
          * - :py:attr:`~vx`
            - Get or set the Vector components defining the direction of the distance in the load curve
          * - :py:attr:`~vy`
            - Get or set the Vector components defining the direction of the distance in the load curve
          * - :py:attr:`~vz`
            - Get or set the Vector components defining the direction of the distance in the load curve


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

    from control_forming_initial_thickness import ControlFormingInitialThickness

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID of the sheet blank to be defined with varying thickness, as in *PART.  Currently only 1 PID is allowed.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID defining thickness (Y-values) vs. distance (X-values) starting from position coordinates (X0, Y0, Z0) and in the direction of a vector [VX, VY, VZ], as in *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: x0
   :type: Optional[float]


   
   Get or set the Starting position coordinates.
















   ..
       !! processed by numpydoc !!

.. py:property:: y0
   :type: Optional[float]


   
   Get or set the Starting position coordinates.
















   ..
       !! processed by numpydoc !!

.. py:property:: z0x
   :type: Optional[float]


   
   Get or set the Starting position coordinates.
















   ..
       !! processed by numpydoc !!

.. py:property:: vx
   :type: Optional[float]


   
   Get or set the Vector components defining the direction of the distance in the load curve
















   ..
       !! processed by numpydoc !!

.. py:property:: vy
   :type: Optional[float]


   
   Get or set the Vector components defining the direction of the distance in the load curve
















   ..
       !! processed by numpydoc !!

.. py:property:: vz
   :type: Optional[float]


   
   Get or set the Vector components defining the direction of the distance in the load curve
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'FORMING_INITIAL_THICKNESS'






