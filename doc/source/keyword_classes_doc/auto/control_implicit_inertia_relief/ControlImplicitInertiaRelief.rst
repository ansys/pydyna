





:class:`ControlImplicitInertiaRelief`
=====================================


.. py:class:: control_implicit_inertia_relief.ControlImplicitInertiaRelief(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_IMPLICIT_INERTIA_RELIEF keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlImplicitInertiaRelief

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~irflag`
            - Get or set the Inertia relief flag
          * - :py:attr:`~thresh`
            - Get or set the Threshold for what is a rigid body mode. the default is set to 0.001 hertz where it is assumed that the units are in seconds
          * - :py:attr:`~ircnt`
            - Get or set the The user can specify to use the lowest IRCNT modes instead of using THRESH to determine the number of modes.
          * - :py:attr:`~mode1`
            - Get or set the Ignore THRESH and IRCNT and use a specific list of modes, skipping those that should not be used
          * - :py:attr:`~mode2`
            - Get or set the Ignore THRESH and IRCNT and use a specific list of modes, skipping those that should not be used
          * - :py:attr:`~mode3`
            - Get or set the Ignore THRESH and IRCNT and use a specific list of modes, skipping those that should not be used.


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

    from control_implicit_inertia_relief import ControlImplicitInertiaRelief

Property detail
---------------

.. py:property:: irflag
   :type: int


   
   Get or set the Inertia relief flag
   EQ.0: do not perform inertia relief.
   EQ 1: do perform inertia relief and use for both implicit and explicit
   EQ.2:   do perform inertia relief but only use for implicit time steps
















   ..
       !! processed by numpydoc !!

.. py:property:: thresh
   :type: float


   
   Get or set the Threshold for what is a rigid body mode. the default is set to 0.001 hertz where it is assumed that the units are in seconds
















   ..
       !! processed by numpydoc !!

.. py:property:: ircnt
   :type: int


   
   Get or set the The user can specify to use the lowest IRCNT modes instead of using THRESH to determine the number of modes.
















   ..
       !! processed by numpydoc !!

.. py:property:: mode1
   :type: Optional[int]


   
   Get or set the Ignore THRESH and IRCNT and use a specific list of modes, skipping those that should not be used
















   ..
       !! processed by numpydoc !!

.. py:property:: mode2
   :type: Optional[int]


   
   Get or set the Ignore THRESH and IRCNT and use a specific list of modes, skipping those that should not be used
















   ..
       !! processed by numpydoc !!

.. py:property:: mode3
   :type: Optional[int]


   
   Get or set the Ignore THRESH and IRCNT and use a specific list of modes, skipping those that should not be used.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'IMPLICIT_INERTIA_RELIEF'






