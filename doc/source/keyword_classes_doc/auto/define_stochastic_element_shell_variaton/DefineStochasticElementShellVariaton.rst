





:class:`DefineStochasticElementShellVariaton`
=============================================


.. py:class:: define_stochastic_element_shell_variaton.DefineStochasticElementShellVariaton(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_STOCHASTIC_ELEMENT_SHELL_VARIATON keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineStochasticElementShellVariaton

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ide`
            - Get or set the Element ID.
          * - :py:attr:`~varsy`
            - Get or set the The yield stress and its hardening function are scaled by 1.+VARSY.
          * - :py:attr:`~varf`
            - Get or set the The failure criterion is scaled by 1+VARF.
          * - :py:attr:`~varro`
            - Get or set the The density is scaled by 1+VARRO. This is intended to be used with topology optimization. This option is not available for shell elements.
          * - :py:attr:`~vare`
            - Get or set the The elastic moduli are scaled by 1+VARE. This is intended to be used with topology optimization.
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

    from define_stochastic_element_shell_variaton import DefineStochasticElementShellVariaton

Property detail
---------------

.. py:property:: ide
   :type: int


   
   Get or set the Element ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: varsy
   :type: float


   
   Get or set the The yield stress and its hardening function are scaled by 1.+VARSY.
















   ..
       !! processed by numpydoc !!

.. py:property:: varf
   :type: float


   
   Get or set the The failure criterion is scaled by 1+VARF.
















   ..
       !! processed by numpydoc !!

.. py:property:: varro
   :type: float


   
   Get or set the The density is scaled by 1+VARRO. This is intended to be used with topology optimization. This option is not available for shell elements.
















   ..
       !! processed by numpydoc !!

.. py:property:: vare
   :type: float


   
   Get or set the The elastic moduli are scaled by 1+VARE. This is intended to be used with topology optimization.
















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
   :value: 'STOCHASTIC_ELEMENT_SHELL_VARIATON'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





