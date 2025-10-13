





:class:`DefineAlebagInflator`
=============================


.. py:class:: define_alebag_inflator.DefineAlebagInflator(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_ALEBAG_INFLATOR keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineAlebagInflator

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~infid`
            - Get or set the Inflator ID.
          * - :py:attr:`~ngas`
            - Get or set the Number of Gas components
          * - :py:attr:`~norif`
            - Get or set the Number of point source (define below)
          * - :py:attr:`~lcvel`
            - Get or set the Load curve Id for inlet velocity. Used only for ALE phase.
          * - :py:attr:`~lct`
            - Get or set the Load curve ID for temperature
          * - :py:attr:`~lcidm`
            - Get or set the Load curve ID for mass flow rate
          * - :py:attr:`~mwgas`
            - Get or set the Molecular weight of gas components
          * - :py:attr:`~gasa`
            - Get or set the First Coefficient of molar heat capacity at constant pressure
          * - :py:attr:`~gasb`
            - Get or set the Second Coefficient of molar heat capacity at constant pressure
          * - :py:attr:`~gasc`
            - Get or set the Third Coefficient of molar heat capacity at constant pressure
          * - :py:attr:`~nodeid`
            - Get or set the Node ID defining the point source
          * - :py:attr:`~vecid`
            - Get or set the Vector Id defining the direction of flow at the point source
          * - :py:attr:`~orifare`
            - Get or set the Orifice area at the point source
          * - :py:attr:`~title`
            - Get or set the Additional title line


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~keyword`
            - 
          * - :py:attr:`~subkeyword`
            - 
          * - :py:attr:`~option_specs`
            - Get the card format type.






Import detail
-------------

.. code-block:: python

    from define_alebag_inflator import DefineAlebagInflator

Property detail
---------------

.. py:property:: infid
   :type: Optional[int]


   
   Get or set the Inflator ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: ngas
   :type: int


   
   Get or set the Number of Gas components
















   ..
       !! processed by numpydoc !!

.. py:property:: norif
   :type: int


   
   Get or set the Number of point source (define below)
















   ..
       !! processed by numpydoc !!

.. py:property:: lcvel
   :type: Optional[int]


   
   Get or set the Load curve Id for inlet velocity. Used only for ALE phase.
















   ..
       !! processed by numpydoc !!

.. py:property:: lct
   :type: Optional[int]


   
   Get or set the Load curve ID for temperature
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidm
   :type: Optional[int]


   
   Get or set the Load curve ID for mass flow rate
















   ..
       !! processed by numpydoc !!

.. py:property:: mwgas
   :type: float


   
   Get or set the Molecular weight of gas components
















   ..
       !! processed by numpydoc !!

.. py:property:: gasa
   :type: float


   
   Get or set the First Coefficient of molar heat capacity at constant pressure
















   ..
       !! processed by numpydoc !!

.. py:property:: gasb
   :type: float


   
   Get or set the Second Coefficient of molar heat capacity at constant pressure
















   ..
       !! processed by numpydoc !!

.. py:property:: gasc
   :type: float


   
   Get or set the Third Coefficient of molar heat capacity at constant pressure
















   ..
       !! processed by numpydoc !!

.. py:property:: nodeid
   :type: int


   
   Get or set the Node ID defining the point source
















   ..
       !! processed by numpydoc !!

.. py:property:: vecid
   :type: int


   
   Get or set the Vector Id defining the direction of flow at the point source
















   ..
       !! processed by numpydoc !!

.. py:property:: orifare
   :type: float


   
   Get or set the Orifice area at the point source
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Additional title line
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DEFINE'


.. py:attribute:: subkeyword
   :value: 'ALEBAG_INFLATOR'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





