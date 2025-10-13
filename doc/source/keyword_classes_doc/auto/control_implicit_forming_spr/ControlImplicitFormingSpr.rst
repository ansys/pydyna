





:class:`ControlImplicitFormingSpr`
==================================


.. py:class:: control_implicit_forming_spr.ControlImplicitFormingSpr(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_IMPLICIT_FORMING_SPR keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlImplicitFormingSpr

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ioption`
            - Get or set the Solution type:
          * - :py:attr:`~nsmin`
            - Get or set the Minimum number of implicit steps for IOPTION = 2.
          * - :py:attr:`~nsmax`
            - Get or set the Maximum number of implicit steps for IOPTION = 2
          * - :py:attr:`~birth`
            - Get or set the Birth time to activate this feature
          * - :py:attr:`~death`
            - Get or set the Death time to activate this feature
          * - :py:attr:`~penchk`
            - Get or set the Relative allowed penetration with respect to the part thickness in contact for IOPTION = 2
          * - :py:attr:`~dt0`
            - Get or set the Initial time step size that overrides the DT0 field defined in *CONTROL_IMPLICIT_GENERAL


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

    from control_implicit_forming_spr import ControlImplicitFormingSpr

Property detail
---------------

.. py:property:: ioption
   :type: int


   
   Get or set the Solution type:
   EQ.1: Gravity loading simulation, see remarks below.
   EQ.2: Binder closing and flanging simulation, see remarks below
















   ..
       !! processed by numpydoc !!

.. py:property:: nsmin
   :type: Optional[int]


   
   Get or set the Minimum number of implicit steps for IOPTION = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsmax
   :type: int


   
   Get or set the Maximum number of implicit steps for IOPTION = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: birth
   :type: float


   
   Get or set the Birth time to activate this feature
















   ..
       !! processed by numpydoc !!

.. py:property:: death
   :type: float


   
   Get or set the Death time to activate this feature
















   ..
       !! processed by numpydoc !!

.. py:property:: penchk
   :type: float


   
   Get or set the Relative allowed penetration with respect to the part thickness in contact for IOPTION = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: dt0
   :type: Optional[float]


   
   Get or set the Initial time step size that overrides the DT0 field defined in *CONTROL_IMPLICIT_GENERAL
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'IMPLICIT_FORMING_SPR'






