





:class:`DefinePartFromLayer`
============================


.. py:class:: define_part_from_layer.DefinePartFromLayer(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_PART_FROM_LAYER keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefinePartFromLayer

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the The part ID to be created
          * - :py:attr:`~layer`
            - Get or set the The layer ID of the PID,
          * - :py:attr:`~pidsrc`
            - Get or set the The part ID of the existing blank to be copied from
          * - :py:attr:`~layold`
            - Get or set the The layer ID of the existing blank
          * - :py:attr:`~mid`
            - Get or set the The material ID of the PID
          * - :py:attr:`~thick`
            - Get or set the The thickness of the PID
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

    from define_part_from_layer import DefinePartFromLayer

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the The part ID to be created
















   ..
       !! processed by numpydoc !!

.. py:property:: layer
   :type: Optional[int]


   
   Get or set the The layer ID of the PID,
















   ..
       !! processed by numpydoc !!

.. py:property:: pidsrc
   :type: Optional[int]


   
   Get or set the The part ID of the existing blank to be copied from
















   ..
       !! processed by numpydoc !!

.. py:property:: layold
   :type: Optional[int]


   
   Get or set the The layer ID of the existing blank
















   ..
       !! processed by numpydoc !!

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the The material ID of the PID
















   ..
       !! processed by numpydoc !!

.. py:property:: thick
   :type: Optional[float]


   
   Get or set the The thickness of the PID
















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
   :value: 'PART_FROM_LAYER'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





