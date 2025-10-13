





:class:`LoadPyroActuator`
=========================


.. py:class:: load_pyro_actuator.LoadPyroActuator(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_PYRO_ACTUATOR keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadPyroActuator

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Unique ID for actuator.
          * - :py:attr:`~id1`
            - Get or set the GT.0:    Node ID 1
          * - :py:attr:`~id2`
            - Get or set the GT.0:    Node ID 2
          * - :py:attr:`~csa`
            - Get or set the Chamber cross section area.
          * - :py:attr:`~vol`
            - Get or set the GT.0:    Initial chamber volume
          * - :py:attr:`~prs`
            - Get or set the Ambient pressure.
          * - :py:attr:`~dens`
            - Get or set the Ambient gas density.
          * - :py:attr:`~atime`
            - Get or set the Activation time.
          * - :py:attr:`~mcid`
            - Get or set the Mass flow curve ID (mass flow as function of time).
          * - :py:attr:`~cv`
            - Get or set the Specific heat capacity at constant pressure.
          * - :py:attr:`~cp`
            - Get or set the Specific heat capacity at constant volume.
          * - :py:attr:`~temp`
            - Get or set the Gas generator temperature.


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

    from load_pyro_actuator import LoadPyroActuator

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Unique ID for actuator.
















   ..
       !! processed by numpydoc !!

.. py:property:: id1
   :type: Optional[int]


   
   Get or set the GT.0:    Node ID 1
   LT.0:   Segment set ID 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: id2
   :type: Optional[int]


   
   Get or set the GT.0:    Node ID 2
   LT.0:   Segment set ID 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: csa
   :type: Optional[float]


   
   Get or set the Chamber cross section area.
















   ..
       !! processed by numpydoc !!

.. py:property:: vol
   :type: Optional[float]


   
   Get or set the GT.0:    Initial chamber volume
   EQ.0:   Initial chamber volume given by distance between ID1 and ID2, see Remarks.
















   ..
       !! processed by numpydoc !!

.. py:property:: prs
   :type: Optional[float]


   
   Get or set the Ambient pressure.
















   ..
       !! processed by numpydoc !!

.. py:property:: dens
   :type: Optional[float]


   
   Get or set the Ambient gas density.
















   ..
       !! processed by numpydoc !!

.. py:property:: atime
   :type: Optional[float]


   
   Get or set the Activation time.
















   ..
       !! processed by numpydoc !!

.. py:property:: mcid
   :type: Optional[int]


   
   Get or set the Mass flow curve ID (mass flow as function of time).
















   ..
       !! processed by numpydoc !!

.. py:property:: cv
   :type: Optional[float]


   
   Get or set the Specific heat capacity at constant pressure.
















   ..
       !! processed by numpydoc !!

.. py:property:: cp
   :type: Optional[float]


   
   Get or set the Specific heat capacity at constant volume.
















   ..
       !! processed by numpydoc !!

.. py:property:: temp
   :type: Optional[float]


   
   Get or set the Gas generator temperature.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'PYRO_ACTUATOR'






