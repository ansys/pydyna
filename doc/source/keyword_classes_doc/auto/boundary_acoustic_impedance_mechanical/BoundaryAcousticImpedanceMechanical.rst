





:class:`BoundaryAcousticImpedanceMechanical`
============================================


.. py:class:: boundary_acoustic_impedance_mechanical.BoundaryAcousticImpedanceMechanical(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_ACOUSTIC_IMPEDANCE_MECHANICAL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryAcousticImpedanceMechanical

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ssid`
            - Get or set the Segment set ID of an acoustic surface.
          * - :py:attr:`~mparea`
            - Get or set the Mass per unit area m.
          * - :py:attr:`~cparea`
            - Get or set the Damping per unit area c.
          * - :py:attr:`~kparea`
            - Get or set the Stiffness per unit area k.


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

    from boundary_acoustic_impedance_mechanical import BoundaryAcousticImpedanceMechanical

Property detail
---------------

.. py:property:: ssid
   :type: Optional[int]


   
   Get or set the Segment set ID of an acoustic surface.
















   ..
       !! processed by numpydoc !!

.. py:property:: mparea
   :type: float


   
   Get or set the Mass per unit area m.
















   ..
       !! processed by numpydoc !!

.. py:property:: cparea
   :type: float


   
   Get or set the Damping per unit area c.
















   ..
       !! processed by numpydoc !!

.. py:property:: kparea
   :type: int


   
   Get or set the Stiffness per unit area k.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'ACOUSTIC_IMPEDANCE_MECHANICAL'






