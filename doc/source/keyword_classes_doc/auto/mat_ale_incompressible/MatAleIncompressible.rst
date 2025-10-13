





:class:`MatAleIncompressible`
=============================


.. py:class:: mat_ale_incompressible.MatAleIncompressible(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_ALE_INCOMPRESSIBLE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatAleIncompressible

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material ID. A unique number or label not exceeding 8 charaters
          * - :py:attr:`~ro`
            - Get or set the Material density.
          * - :py:attr:`~pc`
            - Get or set the Pressure cutoff (< or = 0.0).
          * - :py:attr:`~mu`
            - Get or set the Dynamic viscosity coefficient.
          * - :py:attr:`~tol`
            - Get or set the Tolerance for the convergence of the conjugate gradient.
          * - :py:attr:`~dtout`
            - Get or set the Time interval between screen outputs.
          * - :py:attr:`~ncg`
            - Get or set the Maximum number of loops in the conjugate gradient.
          * - :py:attr:`~meth`
            - Get or set the Conjugate gradient methods:
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

    from mat_ale_incompressible import MatAleIncompressible

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material ID. A unique number or label not exceeding 8 charaters
   must be specified. Material ID is referenced in the *PART card and must be unique.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Material density.
















   ..
       !! processed by numpydoc !!

.. py:property:: pc
   :type: Optional[float]


   
   Get or set the Pressure cutoff (< or = 0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: mu
   :type: Optional[float]


   
   Get or set the Dynamic viscosity coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: tol
   :type: float


   
   Get or set the Tolerance for the convergence of the conjugate gradient.
















   ..
       !! processed by numpydoc !!

.. py:property:: dtout
   :type: float


   
   Get or set the Time interval between screen outputs.
















   ..
       !! processed by numpydoc !!

.. py:property:: ncg
   :type: int


   
   Get or set the Maximum number of loops in the conjugate gradient.
















   ..
       !! processed by numpydoc !!

.. py:property:: meth
   :type: int


   
   Get or set the Conjugate gradient methods:
   EQ.-6: solves the poisson equation for the pressure
   EQ.-7: solves the poisson equation for the pressure increment.
















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
   :value: 'MAT'


.. py:attribute:: subkeyword
   :value: 'ALE_INCOMPRESSIBLE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





