





:class:`DefineElementGeneralizedSolid`
======================================


.. py:class:: define_element_generalized_solid.DefineElementGeneralizedSolid(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_ELEMENT_GENERALIZED_SOLID keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineElementGeneralizedSolid

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~elform`
            - Get or set the Element Formulation ID referenced via *SECTION_SOLID to connect
          * - :py:attr:`~nip`
            - Get or set the Number of integration points.
          * - :py:attr:`~nmnp`
            - Get or set the Number of nodes for this element formulation.
          * - :py:attr:`~imass`
            - Get or set the Option for lumping of mass matrix:
          * - :py:attr:`~wi`
            - Get or set the Integration weight at integration point i.
          * - :py:attr:`~nki`
            - Get or set the Value of the shape function N k evaluated at integration point i.
          * - :py:attr:`~dnkidr`
            - Get or set the Value of the derivative of the shape function Nk with respect to the local coordinate r at the integration point i.
          * - :py:attr:`~dnkids`
            - Get or set the Value of the derivative of the shape function Nk with respect to the local coordinate s at the integration point i.
          * - :py:attr:`~dnkidt`
            - Get or set the Value of the derivative of the shape function Nk with respect to the local coordinate t at the integration point i.
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

    from define_element_generalized_solid import DefineElementGeneralizedSolid

Property detail
---------------

.. py:property:: elform
   :type: Optional[int]


   
   Get or set the Element Formulation ID referenced via *SECTION_SOLID to connect
   *ELEMENT_GENERALIZED_SOLID with the appropriate solid formulation.
   The chosen number needs to be greater or equal than 1000.
















   ..
       !! processed by numpydoc !!

.. py:property:: nip
   :type: Optional[int]


   
   Get or set the Number of integration points.
















   ..
       !! processed by numpydoc !!

.. py:property:: nmnp
   :type: Optional[int]


   
   Get or set the Number of nodes for this element formulation.
















   ..
       !! processed by numpydoc !!

.. py:property:: imass
   :type: int


   
   Get or set the Option for lumping of mass matrix:
   EQ.0: row sum
   EQ.1: diagonal weighting.
















   ..
       !! processed by numpydoc !!

.. py:property:: wi
   :type: Optional[float]


   
   Get or set the Integration weight at integration point i.
















   ..
       !! processed by numpydoc !!

.. py:property:: nki
   :type: Optional[float]


   
   Get or set the Value of the shape function N k evaluated at integration point i.
















   ..
       !! processed by numpydoc !!

.. py:property:: dnkidr
   :type: Optional[float]


   
   Get or set the Value of the derivative of the shape function Nk with respect to the local coordinate r at the integration point i.
















   ..
       !! processed by numpydoc !!

.. py:property:: dnkids
   :type: Optional[float]


   
   Get or set the Value of the derivative of the shape function Nk with respect to the local coordinate s at the integration point i.
















   ..
       !! processed by numpydoc !!

.. py:property:: dnkidt
   :type: Optional[float]


   
   Get or set the Value of the derivative of the shape function Nk with respect to the local coordinate t at the integration point i.
















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
   :value: 'ELEMENT_GENERALIZED_SOLID'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





