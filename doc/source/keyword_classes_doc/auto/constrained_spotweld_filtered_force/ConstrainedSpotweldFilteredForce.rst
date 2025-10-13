





:class:`ConstrainedSpotweldFilteredForce`
=========================================


.. py:class:: constrained_spotweld_filtered_force.ConstrainedSpotweldFilteredForce(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_SPOTWELD_FILTERED_FORCE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedSpotweldFilteredForce

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~wid`
            - Get or set the Optional weld ID
          * - :py:attr:`~n1`
            - Get or set the Node ID for node 1.
          * - :py:attr:`~n2`
            - Get or set the Node ID for node 2.
          * - :py:attr:`~sn`
            - Get or set the Normal force at spotweld failure (optional).
          * - :py:attr:`~ss`
            - Get or set the Shear force at spotweld failure (optional).
          * - :py:attr:`~n`
            - Get or set the Exponent for normal spotweld force (optional).
          * - :py:attr:`~m`
            - Get or set the Exponent for shear spotweld force (optional).
          * - :py:attr:`~tf`
            - Get or set the Failure time for nodal constraint set (default=1.0E+20).
          * - :py:attr:`~ep`
            - Get or set the Effective plastic strain at failure (default=1.0E+20).
          * - :py:attr:`~nf`
            - Get or set the Number of force vectors stored for filtering.
          * - :py:attr:`~tw`
            - Get or set the Time window for filtering.


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

    from constrained_spotweld_filtered_force import ConstrainedSpotweldFilteredForce

Property detail
---------------

.. py:property:: wid
   :type: Optional[int]


   
   Get or set the Optional weld ID
















   ..
       !! processed by numpydoc !!

.. py:property:: n1
   :type: Optional[int]


   
   Get or set the Node ID for node 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: n2
   :type: Optional[int]


   
   Get or set the Node ID for node 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: sn
   :type: Optional[float]


   
   Get or set the Normal force at spotweld failure (optional).
















   ..
       !! processed by numpydoc !!

.. py:property:: ss
   :type: Optional[float]


   
   Get or set the Shear force at spotweld failure (optional).
















   ..
       !! processed by numpydoc !!

.. py:property:: n
   :type: Optional[float]


   
   Get or set the Exponent for normal spotweld force (optional).
















   ..
       !! processed by numpydoc !!

.. py:property:: m
   :type: Optional[float]


   
   Get or set the Exponent for shear spotweld force (optional).
















   ..
       !! processed by numpydoc !!

.. py:property:: tf
   :type: float


   
   Get or set the Failure time for nodal constraint set (default=1.0E+20).
















   ..
       !! processed by numpydoc !!

.. py:property:: ep
   :type: float


   
   Get or set the Effective plastic strain at failure (default=1.0E+20).
















   ..
       !! processed by numpydoc !!

.. py:property:: nf
   :type: Optional[int]


   
   Get or set the Number of force vectors stored for filtering.
















   ..
       !! processed by numpydoc !!

.. py:property:: tw
   :type: Optional[float]


   
   Get or set the Time window for filtering.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'SPOTWELD_FILTERED_FORCE'






