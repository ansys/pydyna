





:class:`EmControlEp`
====================


.. py:class:: em_control_ep.EmControlEp(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_CONTROL_EP keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmControlEp

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~solvetype`
            - Get or set the ElectroPhysiology solver sector: eq. 11: monodomain, eq. 12: bidomain, eq.13 mono+bidomain
          * - :py:attr:`~numspliti`
            - Get or set the Split ratio between the ionic cell model time step and the monodomain time step. (we will do “numplit” cell model time steps for each diffusion time step)
          * - :py:attr:`~actusigma`
            - Get or set the Time period at which the electrical conductivity is updated.


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

    from em_control_ep import EmControlEp

Property detail
---------------

.. py:property:: solvetype
   :type: Optional[int]


   
   Get or set the ElectroPhysiology solver sector: eq. 11: monodomain, eq. 12: bidomain, eq.13 mono+bidomain
















   ..
       !! processed by numpydoc !!

.. py:property:: numspliti
   :type: Optional[int]


   
   Get or set the Split ratio between the ionic cell model time step and the monodomain time step. (we will do “numplit” cell model time steps for each diffusion time step)
















   ..
       !! processed by numpydoc !!

.. py:property:: actusigma
   :type: Optional[int]


   
   Get or set the Time period at which the electrical conductivity is updated.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'CONTROL_EP'






