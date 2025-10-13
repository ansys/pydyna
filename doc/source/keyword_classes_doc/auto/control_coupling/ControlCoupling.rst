





:class:`ControlCoupling`
========================


.. py:class:: control_coupling.ControlCoupling(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_COUPLING keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlCoupling

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~unleng`
            - Get or set the Unit conversion factor for length.
          * - :py:attr:`~untime`
            - Get or set the Unit conversion factor for time.
          * - :py:attr:`~unforc`
            - Get or set the Unit conversion factor for force.
          * - :py:attr:`~timidl`
            - Get or set the Idle time during which CAL3D or MADYMO is computing and LS-DYNA3D remains inactive.
          * - :py:attr:`~flipx`
            - Get or set the Flag for flipping X-coordinate of CAL3D/MADYMO3D relative to the LS-DYNA3D model:
          * - :py:attr:`~flipy`
            - Get or set the Flag for flipping Y-coordinate of CAL3D/MADYMO3D relative to the LS-DYNA3D model:
          * - :py:attr:`~flipz`
            - Get or set the Flag for flipping Z-coordinate of CAL3D/MADYMO3D relative to the LS-DYNA3D model:
          * - :py:attr:`~subcyl`
            - Get or set the CAL3D/MADYMO3D subcycling interval (# of cycles) (default =1):


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

    from control_coupling import ControlCoupling

Property detail
---------------

.. py:property:: unleng
   :type: float


   
   Get or set the Unit conversion factor for length.
















   ..
       !! processed by numpydoc !!

.. py:property:: untime
   :type: float


   
   Get or set the Unit conversion factor for time.
















   ..
       !! processed by numpydoc !!

.. py:property:: unforc
   :type: float


   
   Get or set the Unit conversion factor for force.
















   ..
       !! processed by numpydoc !!

.. py:property:: timidl
   :type: float


   
   Get or set the Idle time during which CAL3D or MADYMO is computing and LS-DYNA3D remains inactive.
















   ..
       !! processed by numpydoc !!

.. py:property:: flipx
   :type: int


   
   Get or set the Flag for flipping X-coordinate of CAL3D/MADYMO3D relative to the LS-DYNA3D model:
   EQ.0: off,
   EQ.1: on.
















   ..
       !! processed by numpydoc !!

.. py:property:: flipy
   :type: int


   
   Get or set the Flag for flipping Y-coordinate of CAL3D/MADYMO3D relative to the LS-DYNA3D model:
   EQ.0: off,
   EQ.1: on.
















   ..
       !! processed by numpydoc !!

.. py:property:: flipz
   :type: int


   
   Get or set the Flag for flipping Z-coordinate of CAL3D/MADYMO3D relative to the LS-DYNA3D model:
   EQ.0: off,
   EQ.1: on.
















   ..
       !! processed by numpydoc !!

.. py:property:: subcyl
   :type: int


   
   Get or set the CAL3D/MADYMO3D subcycling interval (# of cycles) (default =1):
   EQ.n: number of LS-DYNA time steps between each CAL3D/MADYMO3D step.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'COUPLING'






