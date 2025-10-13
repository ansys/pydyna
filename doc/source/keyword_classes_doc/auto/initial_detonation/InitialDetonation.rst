





:class:`InitialDetonation`
==========================


.. py:class:: initial_detonation.InitialDetonation(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INITIAL_DETONATION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InitialDetonation

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID of the high explosive to be lit, except in the case where the high explosive is modeled using an ALE formulation, in which case PID is the part ID of the mesh where the high explosive material to be lit initially resides.  However, two other options are available:
          * - :py:attr:`~x`
            - Get or set the x-coordinate of detonation point.
          * - :py:attr:`~y`
            - Get or set the y-coordinate of detonation point.
          * - :py:attr:`~z`
            - Get or set the z-coordinate of detonation point.
          * - :py:attr:`~lt`
            - Get or set the Lighting time for detonation point (ignored for accustic boundary).
          * - :py:attr:`~mmgset`
            - Get or set the ID of *SET_MULTI-MATERIAL_GROUP_LIST selecting the explosive ALE groups to be lit in the mesh defined by PID.
          * - :py:attr:`~peak`
            - Get or set the Peak pressure,po ,of incident pressure pulse.
          * - :py:attr:`~decay`
            - Get or set the Decay constant, tau.
          * - :py:attr:`~xs`
            - Get or set the x-coordinate of standoff point.
          * - :py:attr:`~ys`
            - Get or set the y-coordinate of standoff point.
          * - :py:attr:`~zs`
            - Get or set the z-coordinate of standoff point.
          * - :py:attr:`~nid`
            - Get or set the Reference node ID near structure.


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

    from initial_detonation import InitialDetonation

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID of the high explosive to be lit, except in the case where the high explosive is modeled using an ALE formulation, in which case PID is the part ID of the mesh where the high explosive material to be lit initially resides.  However, two other options are available:
   EQ.-1:  an acoustic boundary, also, *BOUNDARY_USA_SURFACE,
   EQ.0:   all high explosive materials are considered.
   LT.-1:    |PID| is the ID of a part set (*SET_PART)
















   ..
       !! processed by numpydoc !!

.. py:property:: x
   :type: float


   
   Get or set the x-coordinate of detonation point.
















   ..
       !! processed by numpydoc !!

.. py:property:: y
   :type: float


   
   Get or set the y-coordinate of detonation point.
















   ..
       !! processed by numpydoc !!

.. py:property:: z
   :type: float


   
   Get or set the z-coordinate of detonation point.
















   ..
       !! processed by numpydoc !!

.. py:property:: lt
   :type: float


   
   Get or set the Lighting time for detonation point (ignored for accustic boundary).
















   ..
       !! processed by numpydoc !!

.. py:property:: mmgset
   :type: Optional[int]


   
   Get or set the ID of *SET_MULTI-MATERIAL_GROUP_LIST selecting the explosive ALE groups to be lit in the mesh defined by PID.
















   ..
       !! processed by numpydoc !!

.. py:property:: peak
   :type: Optional[float]


   
   Get or set the Peak pressure,po ,of incident pressure pulse.
















   ..
       !! processed by numpydoc !!

.. py:property:: decay
   :type: Optional[float]


   
   Get or set the Decay constant, tau.
















   ..
       !! processed by numpydoc !!

.. py:property:: xs
   :type: float


   
   Get or set the x-coordinate of standoff point.
















   ..
       !! processed by numpydoc !!

.. py:property:: ys
   :type: float


   
   Get or set the y-coordinate of standoff point.
















   ..
       !! processed by numpydoc !!

.. py:property:: zs
   :type: float


   
   Get or set the z-coordinate of standoff point.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid
   :type: int


   
   Get or set the Reference node ID near structure.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INITIAL'


.. py:attribute:: subkeyword
   :value: 'DETONATION'






