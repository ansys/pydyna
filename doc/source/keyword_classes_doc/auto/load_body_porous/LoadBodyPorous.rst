





:class:`LoadBodyPorous`
=======================


.. py:class:: load_body_porous.LoadBodyPorous(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_BODY_POROUS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadBodyPorous

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the Set ID of the ALE fluid part subjected to porous flow condition
          * - :py:attr:`~sidtyp`
            - Get or set the Set ID type of the SID above.  If SIDTYP=0 (default), then the SID=PSID (part set ID).  If SIDTYP=1, then SID=PID (part ID).
          * - :py:attr:`~ax`
            - Get or set the Viscous coefficients for viscous terms in global X, Y and Z directions (please see equation below).  If   , then an isotropic viscous permeability condition is assumed for the porous medium.
          * - :py:attr:`~ay`
            - Get or set the Viscous coefficients for viscous terms in global X, Y and Z directions (please see equation below).  If   , then an isotropic viscous permeability condition is assumed for the porous medium.
          * - :py:attr:`~az`
            - Get or set the Viscous coefficients for viscous terms in global X, Y and Z directions (please see equation below).  If   , then an isotropic viscous permeability condition is assumed for the porous medium.
          * - :py:attr:`~bx`
            - Get or set the Viscous coefficients for inertia terms in global X, Y and Z directions (please see equation below).  If   , then an isotropic inertial permeability condition is assumed for the porous medium.
          * - :py:attr:`~by`
            - Get or set the Viscous coefficients for inertia terms in global X, Y and Z directions (please see equation below).  If   , then an isotropic inertial permeability condition is assumed for the porous medium.
          * - :py:attr:`~bz`
            - Get or set the Viscous coefficients for inertia terms in global X, Y and Z directions (please see equation below).  If   , then an isotropic inertial permeability condition is assumed for the porous medium.
          * - :py:attr:`~aopt`
            - Get or set the Material axis option:


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

    from load_body_porous import LoadBodyPorous

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Set ID of the ALE fluid part subjected to porous flow condition
















   ..
       !! processed by numpydoc !!

.. py:property:: sidtyp
   :type: int


   
   Get or set the Set ID type of the SID above.  If SIDTYP=0 (default), then the SID=PSID (part set ID).  If SIDTYP=1, then SID=PID (part ID).
















   ..
       !! processed by numpydoc !!

.. py:property:: ax
   :type: float


   
   Get or set the Viscous coefficients for viscous terms in global X, Y and Z directions (please see equation below).  If   , then an isotropic viscous permeability condition is assumed for the porous medium.
















   ..
       !! processed by numpydoc !!

.. py:property:: ay
   :type: float


   
   Get or set the Viscous coefficients for viscous terms in global X, Y and Z directions (please see equation below).  If   , then an isotropic viscous permeability condition is assumed for the porous medium.
















   ..
       !! processed by numpydoc !!

.. py:property:: az
   :type: float


   
   Get or set the Viscous coefficients for viscous terms in global X, Y and Z directions (please see equation below).  If   , then an isotropic viscous permeability condition is assumed for the porous medium.
















   ..
       !! processed by numpydoc !!

.. py:property:: bx
   :type: float


   
   Get or set the Viscous coefficients for inertia terms in global X, Y and Z directions (please see equation below).  If   , then an isotropic inertial permeability condition is assumed for the porous medium.
















   ..
       !! processed by numpydoc !!

.. py:property:: by
   :type: float


   
   Get or set the Viscous coefficients for inertia terms in global X, Y and Z directions (please see equation below).  If   , then an isotropic inertial permeability condition is assumed for the porous medium.
















   ..
       !! processed by numpydoc !!

.. py:property:: bz
   :type: float


   
   Get or set the Viscous coefficients for inertia terms in global X, Y and Z directions (please see equation below).  If   , then an isotropic inertial permeability condition is assumed for the porous medium.
















   ..
       !! processed by numpydoc !!

.. py:property:: aopt
   :type: int


   
   Get or set the Material axis option:
   EQ.0: inactive.
   EQ.1: The forces are applied in a local system attached to the ALE solid (see CTYPE=12 and DIREC=1 in *CONSTRAINED_LAGRANGE_IN_SOLID).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'BODY_POROUS'






