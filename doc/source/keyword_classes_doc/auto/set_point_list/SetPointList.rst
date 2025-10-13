





:class:`SetPointList`
=====================


.. py:class:: set_point_list.SetPointList(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SET_POINT_LIST keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SetPointList

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the Set Identification. All point sets should have a unique set ID.
          * - :py:attr:`~x`
            - Get or set the X coordinate.
          * - :py:attr:`~y`
            - Get or set the Y coordinate.
          * - :py:attr:`~z`
            - Get or set the Z coordinate.
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

    from set_point_list import SetPointList

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Set Identification. All point sets should have a unique set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: x
   :type: Optional[float]


   
   Get or set the X coordinate.
















   ..
       !! processed by numpydoc !!

.. py:property:: y
   :type: Optional[float]


   
   Get or set the Y coordinate.
















   ..
       !! processed by numpydoc !!

.. py:property:: z
   :type: Optional[float]


   
   Get or set the Z coordinate.
















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
   :value: 'POINT_LIST'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





