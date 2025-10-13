





:class:`IcfdControlPorous`
==========================


.. py:class:: icfd_control_porous.IcfdControlPorous(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_CONTROL_POROUS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdControlPorous

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pmstype`
            - Get or set the Indicates the porous media solve type.


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

    from icfd_control_porous import IcfdControlPorous

Property detail
---------------

.. py:property:: pmstype
   :type: int


   
   Get or set the Indicates the porous media solve type.
   EQ.0: Anisotropic Generalized Navier-Stokes model for porous media (See *ICFD_MODEL_POROUS) using Fractional step method.
   Anisotropic Darcy-Forcheimer model using a Monolithic approach for the solve. This method is better suited for very low Reynolds flows through porous media.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'CONTROL_POROUS'






