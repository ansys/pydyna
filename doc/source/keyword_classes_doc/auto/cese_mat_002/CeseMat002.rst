





:class:`CeseMat002`
===================


.. py:class:: cese_mat_002.CeseMat002(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CESE_MAT_002 keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: CeseMat002

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identifier
          * - :py:attr:`~mu`
            - Get or set the Fluid dynamic viscosity. For Air at 15  C, MU = 1.81E-5 kg/m.s.
          * - :py:attr:`~prnd`
            - Get or set the The Prandtl Number (used to determine the coefficient of thermal conductivity). It is approximately constant for most gases. For air at standard conditions PRND = 0.72.


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

    from cese_mat_002 import CeseMat002

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identifier
















   ..
       !! processed by numpydoc !!

.. py:property:: mu
   :type: Optional[float]


   
   Get or set the Fluid dynamic viscosity. For Air at 15  C, MU = 1.81E-5 kg/m.s.
















   ..
       !! processed by numpydoc !!

.. py:property:: prnd
   :type: float


   
   Get or set the The Prandtl Number (used to determine the coefficient of thermal conductivity). It is approximately constant for most gases. For air at standard conditions PRND = 0.72.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CESE'


.. py:attribute:: subkeyword
   :value: 'MAT_002'






