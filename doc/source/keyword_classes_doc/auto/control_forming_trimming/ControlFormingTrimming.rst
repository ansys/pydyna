





:class:`ControlFormingTrimming`
===============================


.. py:class:: control_forming_trimming.ControlFormingTrimming(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_FORMING_TRIMMING keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlFormingTrimming

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~psid`
            - Get or set the Part set ID for trimming, see *SET_PART.
          * - :py:attr:`~ityp`
            - Get or set the Activation flag for sandwiched parts (laminates) trimming:


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

    from control_forming_trimming import ControlFormingTrimming

Property detail
---------------

.. py:property:: psid
   :type: Optional[int]


   
   Get or set the Part set ID for trimming, see *SET_PART.
















   ..
       !! processed by numpydoc !!

.. py:property:: ityp
   :type: Optional[int]


   
   Get or set the Activation flag for sandwiched parts (laminates) trimming:
   EQ.0: Trimming for solid elements.
   EQ.1: Trimming for laminates.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'FORMING_TRIMMING'






