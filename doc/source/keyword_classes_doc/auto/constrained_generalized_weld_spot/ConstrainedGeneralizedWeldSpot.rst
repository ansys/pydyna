





:class:`ConstrainedGeneralizedWeldSpot`
=======================================


.. py:class:: constrained_generalized_weld_spot.ConstrainedGeneralizedWeldSpot(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_GENERALIZED_WELD_SPOT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedGeneralizedWeldSpot

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~wid`
            - Get or set the Optional weld ID
          * - :py:attr:`~nsid`
            - Get or set the Node set ID, see *SET_NODE.
          * - :py:attr:`~cid`
            - Get or set the Coordinate system ID for output of data in local system, see *DEFINE_COORDINATE_OPTION. CID is not required for spotwelds if the nodes are not coincident.
          * - :py:attr:`~filter`
            - Get or set the Number of force vectors saved for filtering. This option can eliminate spurious failures due to numerical force spikes; however, memory requirements are significant since 6 force components are stored with each vector.
          * - :py:attr:`~window`
            - Get or set the Time window for filtering. This option requires the specification of the maximum number of steps which can occur within the filtering time window. If the time step decreases too far, then the filtering time window will be ignored and the simple average is used.
          * - :py:attr:`~npr`
            - Get or set the NFW, number of individual nodal pairs (only cross fillet or combined general weld).
          * - :py:attr:`~nprt`
            - Get or set the Print option in file RBDOUT.
          * - :py:attr:`~tfail`
            - Get or set the Failure time for constraint set, tf (default=1.0E+20).
          * - :py:attr:`~epsf`
            - Get or set the Effective plastic strain at failure.
          * - :py:attr:`~sn`
            - Get or set the Sn, normal force at failure, only for the brittle failure of spotwelds.
          * - :py:attr:`~ss`
            - Get or set the Ss, shear force at failure, only for the brittle failure of spotwelds.
          * - :py:attr:`~n`
            - Get or set the n, exponent for normal force, only for the brittle failure of spotwelds.
          * - :py:attr:`~m`
            - Get or set the m, exponent for shear force, only for the brittle failure of spotwelds.


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

    from constrained_generalized_weld_spot import ConstrainedGeneralizedWeldSpot

Property detail
---------------

.. py:property:: wid
   :type: Optional[int]


   
   Get or set the Optional weld ID
















   ..
       !! processed by numpydoc !!

.. py:property:: nsid
   :type: Optional[int]


   
   Get or set the Node set ID, see *SET_NODE.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: Optional[int]


   
   Get or set the Coordinate system ID for output of data in local system, see *DEFINE_COORDINATE_OPTION. CID is not required for spotwelds if the nodes are not coincident.
















   ..
       !! processed by numpydoc !!

.. py:property:: filter
   :type: Optional[int]


   
   Get or set the Number of force vectors saved for filtering. This option can eliminate spurious failures due to numerical force spikes; however, memory requirements are significant since 6 force components are stored with each vector.
   LE.1: no filtering,
   EQ.n: simple average of force components divided by n or the maximum number of force vectors that are stored for the time window option below.
















   ..
       !! processed by numpydoc !!

.. py:property:: window
   :type: float


   
   Get or set the Time window for filtering. This option requires the specification of the maximum number of steps which can occur within the filtering time window. If the time step decreases too far, then the filtering time window will be ignored and the simple average is used.
   EQ.0: time window is not used.
















   ..
       !! processed by numpydoc !!

.. py:property:: npr
   :type: Optional[int]


   
   Get or set the NFW, number of individual nodal pairs (only cross fillet or combined general weld).
















   ..
       !! processed by numpydoc !!

.. py:property:: nprt
   :type: int


   
   Get or set the Print option in file RBDOUT.
   EQ.0: default from control card is used (default),
   EQ.1: data is printed,
   EQ.2: data is not printed.
















   ..
       !! processed by numpydoc !!

.. py:property:: tfail
   :type: float


   
   Get or set the Failure time for constraint set, tf (default=1.0E+20).
















   ..
       !! processed by numpydoc !!

.. py:property:: epsf
   :type: Optional[float]


   
   Get or set the Effective plastic strain at failure.
















   ..
       !! processed by numpydoc !!

.. py:property:: sn
   :type: Optional[float]


   
   Get or set the Sn, normal force at failure, only for the brittle failure of spotwelds.
















   ..
       !! processed by numpydoc !!

.. py:property:: ss
   :type: Optional[float]


   
   Get or set the Ss, shear force at failure, only for the brittle failure of spotwelds.
















   ..
       !! processed by numpydoc !!

.. py:property:: n
   :type: Optional[float]


   
   Get or set the n, exponent for normal force, only for the brittle failure of spotwelds.
















   ..
       !! processed by numpydoc !!

.. py:property:: m
   :type: Optional[float]


   
   Get or set the m, exponent for shear force, only for the brittle failure of spotwelds.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'GENERALIZED_WELD_SPOT'






