





:class:`ControlFormingAutoNet`
==============================


.. py:class:: control_forming_auto_net.ControlFormingAutoNet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_FORMING_AUTO_NET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlFormingAutoNet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~idnet`
            - Get or set the ID of the net; must be unique.
          * - :py:attr:`~itype`
            - Get or set the Not used at this time
          * - :py:attr:`~idv`
            - Get or set the Vector ID indicating the direction of the net to be generated. See *DEFINE_VECTOR for details. If not defined, the net will be generated along the global Z-axis
          * - :py:attr:`~idp`
            - Get or set the Part ID of the panel undergoing springback simulation.
          * - :py:attr:`~x`
            - Get or set the X-coordinate of a reference point for the net to be generated
          * - :py:attr:`~y`
            - Get or set the Y-coordinate of a reference point for the net to be generated
          * - :py:attr:`~z`
            - Get or set the Z-coordinate of a reference point for the net to be generated
          * - :py:attr:`~sx`
            - Get or set the Length of the net along X-axis.
          * - :py:attr:`~sy`
            - Get or set the Length of the net along X-axis.
          * - :py:attr:`~offset`
            - Get or set the The net will be generated at this offset distance away from the reference point.


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

    from control_forming_auto_net import ControlFormingAutoNet

Property detail
---------------

.. py:property:: idnet
   :type: Optional[int]


   
   Get or set the ID of the net; must be unique.
















   ..
       !! processed by numpydoc !!

.. py:property:: itype
   :type: Optional[str]


   
   Get or set the Not used at this time
















   ..
       !! processed by numpydoc !!

.. py:property:: idv
   :type: int


   
   Get or set the Vector ID indicating the direction of the net to be generated. See *DEFINE_VECTOR for details. If not defined, the net will be generated along the global Z-axis
















   ..
       !! processed by numpydoc !!

.. py:property:: idp
   :type: int


   
   Get or set the Part ID of the panel undergoing springback simulation.
















   ..
       !! processed by numpydoc !!

.. py:property:: x
   :type: float


   
   Get or set the X-coordinate of a reference point for the net to be generated
















   ..
       !! processed by numpydoc !!

.. py:property:: y
   :type: float


   
   Get or set the Y-coordinate of a reference point for the net to be generated
















   ..
       !! processed by numpydoc !!

.. py:property:: z
   :type: float


   
   Get or set the Z-coordinate of a reference point for the net to be generated
















   ..
       !! processed by numpydoc !!

.. py:property:: sx
   :type: float


   
   Get or set the Length of the net along X-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: sy
   :type: float


   
   Get or set the Length of the net along X-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: offset
   :type: float


   
   Get or set the The net will be generated at this offset distance away from the reference point.
   GT.0: the net will be on the global +Z side, or on the vector head side if IDV is defined.
   LT.0: the net will be on the global ¨CZ side, or on the vector tail side if IDV is defined
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'FORMING_AUTO_NET'






