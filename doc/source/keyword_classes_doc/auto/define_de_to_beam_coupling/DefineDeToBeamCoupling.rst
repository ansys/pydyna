





:class:`DefineDeToBeamCoupling`
===============================


.. py:class:: define_de_to_beam_coupling.DefineDeToBeamCoupling(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_DE_TO_BEAM_COUPLING keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineDeToBeamCoupling

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~slave`
            - Get or set the DES nodes.
          * - :py:attr:`~master`
            - Get or set the Shell set.
          * - :py:attr:`~stype`
            - Get or set the EQ.0: Slave node set
          * - :py:attr:`~mtype`
            - Get or set the EQ.0: Part set
          * - :py:attr:`~frics`
            - Get or set the Friction coefficient.
          * - :py:attr:`~fricd`
            - Get or set the Rolling friction coefficient.
          * - :py:attr:`~damp`
            - Get or set the Damping coefficient.
          * - :py:attr:`~bsort`
            - Get or set the Number of cycle between bucket sortings. (Default = 100).
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

    from define_de_to_beam_coupling import DefineDeToBeamCoupling

Property detail
---------------

.. py:property:: slave
   :type: int


   
   Get or set the DES nodes.
















   ..
       !! processed by numpydoc !!

.. py:property:: master
   :type: int


   
   Get or set the Shell set.
















   ..
       !! processed by numpydoc !!

.. py:property:: stype
   :type: int


   
   Get or set the EQ.0: Slave node set
   EQ.1: Slave node
   EQ.2: Slave part set
   EQ.3: Slave part.
















   ..
       !! processed by numpydoc !!

.. py:property:: mtype
   :type: int


   
   Get or set the EQ.0: Part set
   EQ.1: Part.
















   ..
       !! processed by numpydoc !!

.. py:property:: frics
   :type: Optional[float]


   
   Get or set the Friction coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: fricd
   :type: float


   
   Get or set the Rolling friction coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: damp
   :type: float


   
   Get or set the Damping coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: bsort
   :type: int


   
   Get or set the Number of cycle between bucket sortings. (Default = 100).
















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
   :value: 'DE_TO_BEAM_COUPLING'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





