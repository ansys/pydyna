





:class:`ControlAdapstep`
========================


.. py:class:: control_adapstep.ControlAdapstep(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_ADAPSTEP keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlAdapstep

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~factin`
            - Get or set the Initial relaxation factor for contact force during each adaptive remesh. Unless stability problems occur in the contact, FACTIN=1.0 is recommended since this option can create some numerical noise in the resultant tooling forces. A typical value for this parameter is 0.10.
          * - :py:attr:`~dfactr`
            - Get or set the Incremental increase of FACTIN during each time step after the adaptive step. FACTIN is not allowed to exceed unity. A typical value might be 0.01 (default).


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

    from control_adapstep import ControlAdapstep

Property detail
---------------

.. py:property:: factin
   :type: float


   
   Get or set the Initial relaxation factor for contact force during each adaptive remesh. Unless stability problems occur in the contact, FACTIN=1.0 is recommended since this option can create some numerical noise in the resultant tooling forces. A typical value for this parameter is 0.10.
















   ..
       !! processed by numpydoc !!

.. py:property:: dfactr
   :type: float


   
   Get or set the Incremental increase of FACTIN during each time step after the adaptive step. FACTIN is not allowed to exceed unity. A typical value might be 0.01 (default).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'ADAPSTEP'






