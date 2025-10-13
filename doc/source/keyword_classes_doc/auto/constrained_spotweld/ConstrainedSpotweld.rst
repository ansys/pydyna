





:class:`ConstrainedSpotweld`
============================


.. py:class:: constrained_spotweld.ConstrainedSpotweld(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_SPOTWELD keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedSpotweld

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
            - Get or set the Node ID of node 1.
          * - :py:attr:`~n2`
            - Get or set the Node ID of node 2.
          * - :py:attr:`~sn`
            - Get or set the Normal force at spotweld failure (optional, see Remark 2 in user's manual).
          * - :py:attr:`~ss`
            - Get or set the Shear force at spotweld failure (optional, see Remark 2 in user's manual).
          * - :py:attr:`~n`
            - Get or set the Exponent for normal spotweld force (optional, see Remark 2 in user's manual).
          * - :py:attr:`~m`
            - Get or set the Exponent for shear spotweld force (optional, see Remark 2 in user's manual).
          * - :py:attr:`~tf`
            - Get or set the Failure time for nodal constraint set (default=1.0E+20).
          * - :py:attr:`~ep`
            - Get or set the Effective plastic strain at failure (default=1.0E+20).


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

    from constrained_spotweld import ConstrainedSpotweld

Property detail
---------------

.. py:property:: wid
   :type: Optional[int]


   
   Get or set the Optional weld ID
















   ..
       !! processed by numpydoc !!

.. py:property:: n1
   :type: Optional[int]


   
   Get or set the Node ID of node 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: n2
   :type: Optional[int]


   
   Get or set the Node ID of node 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: sn
   :type: Optional[float]


   
   Get or set the Normal force at spotweld failure (optional, see Remark 2 in user's manual).
   EQ.0.0: the failure criteria is disabled
   GT.0.0: normal force at spot weld failure
   LT.0.0: curve ID which specifies the normal force at spot weld failure         as a function of the nodal temperature
















   ..
       !! processed by numpydoc !!

.. py:property:: ss
   :type: Optional[float]


   
   Get or set the Shear force at spotweld failure (optional, see Remark 2 in user's manual).
   EQ.0.0: the failure criteria is disabled
   GT.0.0: shear force at spot weld failure
   LT.0.0: curve ID which specifies the shear force at spot weld failure         as a function of the nodal temperature
















   ..
       !! processed by numpydoc !!

.. py:property:: n
   :type: Optional[float]


   
   Get or set the Exponent for normal spotweld force (optional, see Remark 2 in user's manual).
















   ..
       !! processed by numpydoc !!

.. py:property:: m
   :type: Optional[float]


   
   Get or set the Exponent for shear spotweld force (optional, see Remark 2 in user's manual).
















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



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'SPOTWELD'






