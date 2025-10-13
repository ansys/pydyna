





:class:`SetPartColumnCollect`
=============================


.. py:class:: set_part_column_collect.SetPartColumnCollect(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SET_PART_COLUMN_COLLECT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SetPartColumnCollect

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the Part set ID. All part sets should have a unique set ID.
          * - :py:attr:`~da1`
            - Get or set the First attribute default value is 0.0.
          * - :py:attr:`~da2`
            - Get or set the Second attribute default value is 0.0.
          * - :py:attr:`~da3`
            - Get or set the Third attribute default value is 0.0.
          * - :py:attr:`~da4`
            - Get or set the Fourth attribute default value is 0.0.
          * - :py:attr:`~solver`
            - Get or set the EQ.MECH: mechanics.
          * - :py:attr:`~pid`
            - Get or set the Part ID.
          * - :py:attr:`~a1`
            - Get or set the First part attribute
          * - :py:attr:`~a2`
            - Get or set the second part attribute.
          * - :py:attr:`~a3`
            - Get or set the third part attribute.
          * - :py:attr:`~a4`
            - Get or set the forth part attribute.
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

    from set_part_column_collect import SetPartColumnCollect

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Part set ID. All part sets should have a unique set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: da1
   :type: float


   
   Get or set the First attribute default value is 0.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: da2
   :type: float


   
   Get or set the Second attribute default value is 0.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: da3
   :type: float


   
   Get or set the Third attribute default value is 0.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: da4
   :type: float


   
   Get or set the Fourth attribute default value is 0.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: solver
   :type: str


   
   Get or set the EQ.MECH: mechanics.
   EQ.CESE: CE/SE compressible fluid flow solver.
   EQ.ICFD: Incompressible fluid flow solver.
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: Optional[float]


   
   Get or set the First part attribute
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the second part attribute.
















   ..
       !! processed by numpydoc !!

.. py:property:: a3
   :type: Optional[float]


   
   Get or set the third part attribute.
















   ..
       !! processed by numpydoc !!

.. py:property:: a4
   :type: Optional[float]


   
   Get or set the forth part attribute.
















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
   :value: 'SET'


.. py:attribute:: subkeyword
   :value: 'PART_COLUMN_COLLECT'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





