





:class:`PartStackedElements`
============================


.. py:class:: part_stacked_elements.PartStackedElements(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA PART_STACKED_ELEMENTS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: PartStackedElements

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~title`
            - Get or set the Enter title for the datacard.
          * - :py:attr:`~pidref`
            - Get or set the Part ID of reference shell element mesh.
          * - :py:attr:`~numlay`
            - Get or set the Number of layers.
          * - :py:attr:`~adpopt`
            - Get or set the Indicates if parts are adapted.
          * - :py:attr:`~inplcmp`
            - Get or set the Option for in-plane composed parts:
          * - :py:attr:`~pidi`
            - Get or set the Part identification.
          * - :py:attr:`~sidi`
            - Get or set the Section identification for layer i defined in a *SECTION keyword.
          * - :py:attr:`~midi`
            - Get or set the Material identification for layer i defined in a *MAT keyword.
          * - :py:attr:`~hgidi`
            - Get or set the Hourglass identification for layer i defined in a *HOURGLASS keyword.
          * - :py:attr:`~tmidi`
            - Get or set the Thermal material identification for layer i defined in a *MAT_THERMAL keyword.
          * - :py:attr:`~thki`
            - Get or set the Thickness of layer i.
          * - :py:attr:`~nsldi`
            - Get or set the Number of through-thickness solid elements for layer i.


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

    from part_stacked_elements import PartStackedElements

Property detail
---------------

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Enter title for the datacard.
















   ..
       !! processed by numpydoc !!

.. py:property:: pidref
   :type: int


   
   Get or set the Part ID of reference shell element mesh.
















   ..
       !! processed by numpydoc !!

.. py:property:: numlay
   :type: int


   
   Get or set the Number of layers.
















   ..
       !! processed by numpydoc !!

.. py:property:: adpopt
   :type: int


   
   Get or set the Indicates if parts are adapted.
















   ..
       !! processed by numpydoc !!

.. py:property:: inplcmp
   :type: int


   
   Get or set the Option for in-plane composed parts:
   EQ.0:   Off
   EQ.1 : On;
















   ..
       !! processed by numpydoc !!

.. py:property:: pidi
   :type: Optional[int]


   
   Get or set the Part identification.
















   ..
       !! processed by numpydoc !!

.. py:property:: sidi
   :type: Optional[int]


   
   Get or set the Section identification for layer i defined in a *SECTION keyword.
















   ..
       !! processed by numpydoc !!

.. py:property:: midi
   :type: Optional[int]


   
   Get or set the Material identification for layer i defined in a *MAT keyword.
















   ..
       !! processed by numpydoc !!

.. py:property:: hgidi
   :type: int


   
   Get or set the Hourglass identification for layer i defined in a *HOURGLASS keyword.
















   ..
       !! processed by numpydoc !!

.. py:property:: tmidi
   :type: int


   
   Get or set the Thermal material identification for layer i defined in a *MAT_THERMAL keyword.
















   ..
       !! processed by numpydoc !!

.. py:property:: thki
   :type: Optional[float]


   
   Get or set the Thickness of layer i.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsldi
   :type: Optional[int]


   
   Get or set the Number of through-thickness solid elements for layer i.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'PART'


.. py:attribute:: subkeyword
   :value: 'STACKED_ELEMENTS'






