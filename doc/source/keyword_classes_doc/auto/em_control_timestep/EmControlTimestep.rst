





:class:`EmControlTimestep`
==========================


.. py:class:: em_control_timestep.EmControlTimestep(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_CONTROL_TIMESTEP keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmControlTimestep

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~tstype`
            - Get or set the Time Step type
          * - :py:attr:`~dtcons`
            - Get or set the Constant value for the time step for TSTYPE=1
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID giving the time step vs time for TSTYPE=2
          * - :py:attr:`~factor`
            - Get or set the Multiplicative factor applied to the time step for TSTYPE=3
          * - :py:attr:`~tsmin`
            - Get or set the Minimum time step. When TSMIN is defined, the EM time step cannot drop below TSMIN. A negative value will refer to a time dependent load curve.
          * - :py:attr:`~tsmas`
            - Get or set the Maximum time step. When TSMAX is defined, the EM time step cannot increase beyond TSMAX. A negative value will refer to a time dependent load curve.
          * - :py:attr:`~rlcsf`
            - Get or set the RLC Circuit time step scale factor.
          * - :py:attr:`~mecats`
            - Get or set the Mechanical time step handling in cases where the EM solver time step becomes smaller (see Remark 3):


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

    from em_control_timestep import EmControlTimestep

Property detail
---------------

.. py:property:: tstype
   :type: int


   
   Get or set the Time Step type
   EQ.1 : constant time step given in DTCONST
   EQ.2 : time step as a function of time given by a load curve specified in LCID
   EQ.3 : Automatic time step computation, depending on the solver type. This time step is then multiplied by FACTOR
















   ..
       !! processed by numpydoc !!

.. py:property:: dtcons
   :type: Optional[float]


   
   Get or set the Constant value for the time step for TSTYPE=1
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID giving the time step vs time for TSTYPE=2
















   ..
       !! processed by numpydoc !!

.. py:property:: factor
   :type: float


   
   Get or set the Multiplicative factor applied to the time step for TSTYPE=3
















   ..
       !! processed by numpydoc !!

.. py:property:: tsmin
   :type: Optional[float]


   
   Get or set the Minimum time step. When TSMIN is defined, the EM time step cannot drop below TSMIN. A negative value will refer to a time dependent load curve.
















   ..
       !! processed by numpydoc !!

.. py:property:: tsmas
   :type: Optional[float]


   
   Get or set the Maximum time step. When TSMAX is defined, the EM time step cannot increase beyond TSMAX. A negative value will refer to a time dependent load curve.
















   ..
       !! processed by numpydoc !!

.. py:property:: rlcsf
   :type: int


   
   Get or set the RLC Circuit time step scale factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: mecats
   :type: int


   
   Get or set the Mechanical time step handling in cases where the EM solver time step becomes smaller (see Remark 3):
   EQ.0: Default.The EM time step will go below the solid mechanics timestep,and several EM solves will occur between two solid mechanics time steps to ensure time consistency.
   EQ.1: The solid mechanics time step will adapt and decrease to the EM time step value so that only one EM solve occurs between two solid mechanics solves.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'CONTROL_TIMESTEP'






