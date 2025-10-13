





:class:`SetPartList`
====================


.. py:class:: set_part_list.SetPartList(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SET_PART_LIST keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SetPartList

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
          * - :py:attr:`~parts`
            - dynamic array of part ids..
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

    from set_part_list import SetPartList

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

.. py:property:: parts
   :type: ansys.dyna.core.lib.series_card.SeriesCard


   
   dynamic array of part ids..
















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
   :value: 'PART_LIST'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





