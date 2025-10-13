





:class:`BoundaryElementMethodFlow`
==================================


.. py:class:: boundary_element_method_flow.BoundaryElementMethodFlow(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_ELEMENT_METHOD_FLOW keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryElementMethodFlow

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ssid`
            - Get or set the Shell set ID for the set of shell elements which define the surface of the bodies of interest (see *SET_SHELL). The nodes of these shells should be ordered so that the shell normals point into the fluid.
          * - :py:attr:`~vx`
            - Get or set the x-component of the free-stream fluid velocity.
          * - :py:attr:`~vy`
            - Get or set the y-component of the free-stream fluid velocity.
          * - :py:attr:`~vz`
            - Get or set the z-component of the free-stream fluid velocity.
          * - :py:attr:`~ro`
            - Get or set the Fluid density.
          * - :py:attr:`~pstatic`
            - Get or set the Fluid static pressure.
          * - :py:attr:`~mach`
            - Get or set the Free-stream Mach number.


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

    from boundary_element_method_flow import BoundaryElementMethodFlow

Property detail
---------------

.. py:property:: ssid
   :type: Optional[int]


   
   Get or set the Shell set ID for the set of shell elements which define the surface of the bodies of interest (see *SET_SHELL). The nodes of these shells should be ordered so that the shell normals point into the fluid.
















   ..
       !! processed by numpydoc !!

.. py:property:: vx
   :type: Optional[float]


   
   Get or set the x-component of the free-stream fluid velocity.
















   ..
       !! processed by numpydoc !!

.. py:property:: vy
   :type: Optional[float]


   
   Get or set the y-component of the free-stream fluid velocity.
















   ..
       !! processed by numpydoc !!

.. py:property:: vz
   :type: Optional[float]


   
   Get or set the z-component of the free-stream fluid velocity.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Fluid density.
















   ..
       !! processed by numpydoc !!

.. py:property:: pstatic
   :type: float


   
   Get or set the Fluid static pressure.
















   ..
       !! processed by numpydoc !!

.. py:property:: mach
   :type: float


   
   Get or set the Free-stream Mach number.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'ELEMENT_METHOD_FLOW'






