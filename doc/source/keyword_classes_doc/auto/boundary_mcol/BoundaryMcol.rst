





:class:`BoundaryMcol`
=====================


.. py:class:: boundary_mcol.BoundaryMcol(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_MCOL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryMcol

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nmcol`
            - Get or set the Number of ships in MCOL coupling.
          * - :py:attr:`~mxstep`
            - Get or set the Maximum of time step in MCOL calculation. If the number of MCOL time steps exceeds MXSTEP, then LS-DYNA will terminate.
          * - :py:attr:`~endtmcol`
            - Get or set the Uncoupling termination time, see Remark 2 below. EQ. 0.0: set to LS-DYNA termination time
          * - :py:attr:`~tsubc`
            - Get or set the Time interval for MCOL subcycling.
          * - :py:attr:`~prtmcol`
            - Get or set the Time interval for output of MCOL rigid body data.
          * - :py:attr:`~rbmcol`
            - Get or set the LS-DYNA rigid body material assignment for the ship.
          * - :py:attr:`~mcolfile`
            - Get or set the Filename containing MCOL input parameters for the ship.


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

    from boundary_mcol import BoundaryMcol

Property detail
---------------

.. py:property:: nmcol
   :type: int


   
   Get or set the Number of ships in MCOL coupling.
















   ..
       !! processed by numpydoc !!

.. py:property:: mxstep
   :type: Optional[int]


   
   Get or set the Maximum of time step in MCOL calculation. If the number of MCOL time steps exceeds MXSTEP, then LS-DYNA will terminate.
















   ..
       !! processed by numpydoc !!

.. py:property:: endtmcol
   :type: float


   
   Get or set the Uncoupling termination time, see Remark 2 below. EQ. 0.0: set to LS-DYNA termination time
















   ..
       !! processed by numpydoc !!

.. py:property:: tsubc
   :type: float


   
   Get or set the Time interval for MCOL subcycling.
   EQ. 0.0: no subcycling
















   ..
       !! processed by numpydoc !!

.. py:property:: prtmcol
   :type: Optional[float]


   
   Get or set the Time interval for output of MCOL rigid body data.
















   ..
       !! processed by numpydoc !!

.. py:property:: rbmcol
   :type: int


   
   Get or set the LS-DYNA rigid body material assignment for the ship.
















   ..
       !! processed by numpydoc !!

.. py:property:: mcolfile
   :type: Optional[str]


   
   Get or set the Filename containing MCOL input parameters for the ship.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'MCOL'






