





:class:`InitialAxialForceBeam`
==============================


.. py:class:: initial_axial_force_beam.InitialAxialForceBeam(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INITIAL_AXIAL_FORCE_BEAM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InitialAxialForceBeam

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~bsid`
            - Get or set the Beam set ID
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID defining preload stress versus time.  When the load curve ends or goes to zero, the initialization is assumed to be completed
          * - :py:attr:`~scale`
            - Get or set the Scale factor on load curve.
          * - :py:attr:`~kbend`
            - Get or set the Bending stiffness flag


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

    from initial_axial_force_beam import InitialAxialForceBeam

Property detail
---------------

.. py:property:: bsid
   :type: Optional[int]


   
   Get or set the Beam set ID
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID defining preload stress versus time.  When the load curve ends or goes to zero, the initialization is assumed to be completed
















   ..
       !! processed by numpydoc !!

.. py:property:: scale
   :type: float


   
   Get or set the Scale factor on load curve.
















   ..
       !! processed by numpydoc !!

.. py:property:: kbend
   :type: int


   
   Get or set the Bending stiffness flag
   EQ.0:   Bending stiffness is negligible since all integration points are assigned the same axial stress
   EQ.1:   Bending stiffness is retained by keeping the axial stress gradient
   EQ.2:   Same as 1, but also allows for lining up several beams with prescribed axial force.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INITIAL'


.. py:attribute:: subkeyword
   :value: 'AXIAL_FORCE_BEAM'






