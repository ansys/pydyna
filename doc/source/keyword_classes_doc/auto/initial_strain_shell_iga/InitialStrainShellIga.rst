





:class:`InitialStrainShellIga`
==============================


.. py:class:: initial_strain_shell_iga.InitialStrainShellIga(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INITIAL_STRAIN_SHELL_IGA keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InitialStrainShellIga

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~eid`
            - Get or set the iga element ID.
          * - :py:attr:`~nplane`
            - Get or set the Number of in-plane integration points being output.
          * - :py:attr:`~nthk`
            - Get or set the Flag for initialization of thicknesses at in-plane IPs:
          * - :py:attr:`~large`
            - Get or set the Large format flag:
          * - :py:attr:`~r`
            - Get or set the Parametric r-coordinate of location of in-plane integration point (with respect to iga shell definition)
          * - :py:attr:`~s`
            - Get or set the Parametric s-coordinate of location of in-plane integration point (with respect to iga shell definition)
          * - :py:attr:`~t`
            - Get or set the Parametric coordinate of through thickness integration point between -1 and 1 inclusive.
          * - :py:attr:`~epsxx`
            - Get or set the Define the xx strain component. The strains are defined in the GLOBAL Cartesian system.
          * - :py:attr:`~epsyy`
            - Get or set the Define the yy strain component.The strains are defined in the GLOBAL Cartesian system.
          * - :py:attr:`~epszz`
            - Get or set the Define the zz strain component.The strains are defined in the GLOBAL Cartesian system.
          * - :py:attr:`~epsxy`
            - Get or set the Define the xy strain component.The strains are defined in the GLOBAL Cartesian system.
          * - :py:attr:`~epsyz`
            - Get or set the Define the yz strain component.The strains are defined in the GLOBAL Cartesian system.
          * - :py:attr:`~epszx`
            - Get or set the Define the zx strain componentThe strains are defined in the GLOBAL Cartesian system.
          * - :py:attr:`~thki`
            - Get or set the The thickness value at in-plane integration point


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

    from initial_strain_shell_iga import InitialStrainShellIga

Property detail
---------------

.. py:property:: eid
   :type: Optional[int]


   
   Get or set the iga element ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: nplane
   :type: Optional[int]


   
   Get or set the Number of in-plane integration points being output.
















   ..
       !! processed by numpydoc !!

.. py:property:: nthk
   :type: int


   
   Get or set the Flag for initialization of thicknesses at in-plane IPs:
   EQ.0:   o
   ffEQ.1 : on.
















   ..
       !! processed by numpydoc !!

.. py:property:: large
   :type: int


   
   Get or set the Large format flag:
   EQ.0:   off
   EQ.1 : on.Each strain field is twice as long for higher precision.
















   ..
       !! processed by numpydoc !!

.. py:property:: r
   :type: Optional[float]


   
   Get or set the Parametric r-coordinate of location of in-plane integration point (with respect to iga shell definition)
















   ..
       !! processed by numpydoc !!

.. py:property:: s
   :type: Optional[float]


   
   Get or set the Parametric s-coordinate of location of in-plane integration point (with respect to iga shell definition)
















   ..
       !! processed by numpydoc !!

.. py:property:: t
   :type: Optional[float]


   
   Get or set the Parametric coordinate of through thickness integration point between -1 and 1 inclusive.
















   ..
       !! processed by numpydoc !!

.. py:property:: epsxx
   :type: float


   
   Get or set the Define the xx strain component. The strains are defined in the GLOBAL Cartesian system.
















   ..
       !! processed by numpydoc !!

.. py:property:: epsyy
   :type: float


   
   Get or set the Define the yy strain component.The strains are defined in the GLOBAL Cartesian system.
















   ..
       !! processed by numpydoc !!

.. py:property:: epszz
   :type: float


   
   Get or set the Define the zz strain component.The strains are defined in the GLOBAL Cartesian system.
















   ..
       !! processed by numpydoc !!

.. py:property:: epsxy
   :type: float


   
   Get or set the Define the xy strain component.The strains are defined in the GLOBAL Cartesian system.
















   ..
       !! processed by numpydoc !!

.. py:property:: epsyz
   :type: float


   
   Get or set the Define the yz strain component.The strains are defined in the GLOBAL Cartesian system.
















   ..
       !! processed by numpydoc !!

.. py:property:: epszx
   :type: float


   
   Get or set the Define the zx strain componentThe strains are defined in the GLOBAL Cartesian system.
















   ..
       !! processed by numpydoc !!

.. py:property:: thki
   :type: float


   
   Get or set the The thickness value at in-plane integration point
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INITIAL'


.. py:attribute:: subkeyword
   :value: 'STRAIN_SHELL_IGA'






