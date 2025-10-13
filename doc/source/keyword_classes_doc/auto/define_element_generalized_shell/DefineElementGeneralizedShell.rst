





:class:`DefineElementGeneralizedShell`
======================================


.. py:class:: define_element_generalized_shell.DefineElementGeneralizedShell(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_ELEMENT_GENERALIZED_SHELL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineElementGeneralizedShell

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~elform`
            - Get or set the Element Formulation ID referenced via *SECTION_SHELL to connect
          * - :py:attr:`~nip`
            - Get or set the Number of in-plane integration points.
          * - :py:attr:`~nmnp`
            - Get or set the Number of nodes for this element formulation.
          * - :py:attr:`~imass`
            - Get or set the Option for lumping of mass matrix:
          * - :py:attr:`~form`
            - Get or set the Shell formulation to be used
          * - :py:attr:`~wi`
            - Get or set the Integration weight at integration point i.
          * - :py:attr:`~nki`
            - Get or set the Value of the shape function N k evaluated at integration point i.
          * - :py:attr:`~dnkidr`
            - Get or set the Value of the derivative of the shape function Nk with respect to the local coordinate r at the integration point i.
          * - :py:attr:`~dnkids`
            - Get or set the Value of the derivative of the shape function Nk with respect to the local coordinate s at the integration point i.
          * - :py:attr:`~dnkldr`
            - Get or set the Value of the derivative of the shape function Nk with respect to the local coordinate r at the integration point l.
          * - :py:attr:`~dnklds`
            - Get or set the Value of the derivative of the shape function Nk with respect to the local coordinate s at the integration point l.
          * - :py:attr:`~d2nkidr2`
            - Get or set the Value of the second derivative of the shape function Nk with respect to the local coordinate r at the integration point i.
          * - :py:attr:`~d2nkidrds`
            - Get or set the Value of the second derivative of the shape function Nk with respect to the local coordinates r and s at the integration point i.
          * - :py:attr:`~d2nkids2`
            - Get or set the Value of the second derivative of the shape function Nk with respect to the local coordinate s at the integration point  i.
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

    from define_element_generalized_shell import DefineElementGeneralizedShell

Property detail
---------------

.. py:property:: elform
   :type: Optional[int]


   
   Get or set the Element Formulation ID referenced via *SECTION_SHELL to connect
   *ELEMENT_GENERALIZED_SHELL with the appropriate shell
   formulation. The chosen number needs to be greater or equal than 1000.
















   ..
       !! processed by numpydoc !!

.. py:property:: nip
   :type: Optional[int]


   
   Get or set the Number of in-plane integration points.
















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

.. py:property:: form
   :type: int


   
   Get or set the Shell formulation to be used
   EQ.0: shear deformable shell theory with rotational DOFs (shell normal evaluated at the nodes)
   EQ.1: shear deformable shell theory without rotational DOFs (shell      normal evaluated at the nodes)
   EQ.2: thin shell theory without rotational DOFs (shell normal evaluated at the integration points)
   EQ.3: thin shell theory with rotational DOFs (shell normal evaluated at the integration points).
















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

.. py:property:: dnkldr
   :type: Optional[float]


   
   Get or set the Value of the derivative of the shape function Nk with respect to the local coordinate r at the integration point l.
















   ..
       !! processed by numpydoc !!

.. py:property:: dnklds
   :type: Optional[float]


   
   Get or set the Value of the derivative of the shape function Nk with respect to the local coordinate s at the integration point l.
















   ..
       !! processed by numpydoc !!

.. py:property:: d2nkidr2
   :type: Optional[float]


   
   Get or set the Value of the second derivative of the shape function Nk with respect to the local coordinate r at the integration point i.
















   ..
       !! processed by numpydoc !!

.. py:property:: d2nkidrds
   :type: Optional[float]


   
   Get or set the Value of the second derivative of the shape function Nk with respect to the local coordinates r and s at the integration point i.
















   ..
       !! processed by numpydoc !!

.. py:property:: d2nkids2
   :type: Optional[float]


   
   Get or set the Value of the second derivative of the shape function Nk with respect to the local coordinate s at the integration point  i.
















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
   :value: 'ELEMENT_GENERALIZED_SHELL'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





