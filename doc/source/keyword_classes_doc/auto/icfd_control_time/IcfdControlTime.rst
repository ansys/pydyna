





:class:`IcfdControlTime`
========================


.. py:class:: icfd_control_time.IcfdControlTime(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_CONTROL_TIME keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdControlTime

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ttm`
            - Get or set the Total time of simulation for the fluid problem.
          * - :py:attr:`~dt`
            - Get or set the Time step for the fluid problem. If different than zero the time step will be set constant and equal to this value. If DT = 0 then the time step is automatically computed.
          * - :py:attr:`~cfl`
            - Get or set the Scale factor that multplies DT.
          * - :py:attr:`~lcidsf`
            - Get or set the Load Curve ID specifying the CFL number when DT = 0 as a function of time, and more generally LCIDSF specifies the time step scale factor as the function of time.
          * - :py:attr:`~dtmin`
            - Get or set the Minimum time step. When an automatic time step is used and DTMIN is defined, the time step cannot adopt a smaller value than DTMIN.A negative value will refer to a time dependent load curve.
          * - :py:attr:`~dtmax`
            - Get or set the Maximum time step. When an automatic time step is used and DTMAX is defined, the time step cannot adopt a higher value than DTMAX.. A negative value will refer to a time dependent load curve.
          * - :py:attr:`~dtinit`
            - Get or set the Initial time step. If not defined, the solver will automatically determine an initial timestep based on the flow velocity or dimensions of the problem in cases where there is no inflow.
          * - :py:attr:`~tdeath`
            - Get or set the Death time for the Navier Stokes solve. After TDEATH, the velocity and pressure will no longer be updated. But the temperature and other similar quantities still can.
          * - :py:attr:`~dtt`
            - Get or set the Thermal timestep
          * - :py:attr:`~btbl`
            - Get or set the Flag to include boundary layer elements in the automatic timestep calculation.


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

    from icfd_control_time import IcfdControlTime

Property detail
---------------

.. py:property:: ttm
   :type: float


   
   Get or set the Total time of simulation for the fluid problem.
















   ..
       !! processed by numpydoc !!

.. py:property:: dt
   :type: float


   
   Get or set the Time step for the fluid problem. If different than zero the time step will be set constant and equal to this value. If DT = 0 then the time step is automatically computed.
















   ..
       !! processed by numpydoc !!

.. py:property:: cfl
   :type: float


   
   Get or set the Scale factor that multplies DT.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidsf
   :type: Optional[int]


   
   Get or set the Load Curve ID specifying the CFL number when DT = 0 as a function of time, and more generally LCIDSF specifies the time step scale factor as the function of time.
















   ..
       !! processed by numpydoc !!

.. py:property:: dtmin
   :type: Optional[float]


   
   Get or set the Minimum time step. When an automatic time step is used and DTMIN is defined, the time step cannot adopt a smaller value than DTMIN.A negative value will refer to a time dependent load curve.
















   ..
       !! processed by numpydoc !!

.. py:property:: dtmax
   :type: Optional[float]


   
   Get or set the Maximum time step. When an automatic time step is used and DTMAX is defined, the time step cannot adopt a higher value than DTMAX.. A negative value will refer to a time dependent load curve.
















   ..
       !! processed by numpydoc !!

.. py:property:: dtinit
   :type: Optional[float]


   
   Get or set the Initial time step. If not defined, the solver will automatically determine an initial timestep based on the flow velocity or dimensions of the problem in cases where there is no inflow.
















   ..
       !! processed by numpydoc !!

.. py:property:: tdeath
   :type: float


   
   Get or set the Death time for the Navier Stokes solve. After TDEATH, the velocity and pressure will no longer be updated. But the temperature and other similar quantities still can.
















   ..
       !! processed by numpydoc !!

.. py:property:: dtt
   :type: Optional[float]


   
   Get or set the Thermal timestep
















   ..
       !! processed by numpydoc !!

.. py:property:: btbl
   :type: int


   
   Get or set the Flag to include boundary layer elements in the automatic timestep calculation.
   EQ.0:   Default.The boundary layer elements are excluded.
   EQ.1 : The boundary layer elements are included.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'CONTROL_TIME'






