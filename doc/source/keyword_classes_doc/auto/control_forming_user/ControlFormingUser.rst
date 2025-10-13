





:class:`ControlFormingUser`
===========================


.. py:class:: control_forming_user.ControlFormingUser(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_FORMING_USER keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlFormingUser

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~blank`
            - Get or set the Blank ID
          * - :py:attr:`~type`
            - Get or set the 0: Part ID (blank)
          * - :py:attr:`~thick`
            - Get or set the Blank thickness. If the blank thickness is already defined, this parameter is ignored
          * - :py:attr:`~r00`
            - Get or set the Material anisotropic parameters
          * - :py:attr:`~r45`
            - Get or set the Material anisotropic parameters
          * - :py:attr:`~r90`
            - Get or set the Material anisotropic parameters
          * - :py:attr:`~al_fe`
            - Get or set the This parameter is used to define blank Young's Modulus and density. If this parameter is defined, E and Density will be found by using the proper unit, which is specified below.
          * - :py:attr:`~unit`
            - Get or set the Unit adopted in this simulation. This unit is used to obtain proper punch velocity, acceleration, time step, and material properties
          * - :py:attr:`~lcss`
            - Get or set the If the material for the blank has not been defined, this curve will be used to define the stress-strain relation. Otherwise, this curve is ignored.
          * - :py:attr:`~k`
            - Get or set the strength coefficient for exponential hardening. If LCSS is defined, or blank materials has already been defined, this parameter is ignored
          * - :py:attr:`~n`
            - Get or set the exponent for exponential hardening. If LCSS is defined, or blank materials has already been defined, this parameter is ignored
          * - :py:attr:`~e`
            - Get or set the Material Young's Modulus. If AL/FE is defined, E is not necessary
          * - :py:attr:`~density`
            - Get or set the Blank density. If AL/FE is defined, this parameter is not necessary
          * - :py:attr:`~pr`
            - Get or set the Possion Ratio
          * - :py:attr:`~fs`
            - Get or set the Friction coefficient. If contact is defined, this parameter is ignored
          * - :py:attr:`~mtype`
            - Get or set the Material model (Only M37 is supported)
          * - :py:attr:`~patern`
            - Get or set the Velocity profile.
          * - :py:attr:`~vmax`
            - Get or set the The maximum allowable tool velocity.
          * - :py:attr:`~amax`
            - Get or set the The maximum allowable acceleration
          * - :py:attr:`~lvlada`
            - Get or set the Level of adaptivity
          * - :py:attr:`~sizeada`
            - Get or set the Minimize for adaptivity
          * - :py:attr:`~adatims`
            - Get or set the Total number of adaptivity cycles in this process
          * - :py:attr:`~d3plot`
            - Get or set the Number of state output for d3plot file
          * - :py:attr:`~gap`
            - Get or set the minimum gap between tools


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

    from control_forming_user import ControlFormingUser

Property detail
---------------

.. py:property:: blank
   :type: Optional[int]


   
   Get or set the Blank ID
















   ..
       !! processed by numpydoc !!

.. py:property:: type
   :type: int


   
   Get or set the 0: Part ID (blank)
   1:   PART SET Ids (blank).
















   ..
       !! processed by numpydoc !!

.. py:property:: thick
   :type: Optional[float]


   
   Get or set the Blank thickness. If the blank thickness is already defined, this parameter is ignored
















   ..
       !! processed by numpydoc !!

.. py:property:: r00
   :type: float


   
   Get or set the Material anisotropic parameters
















   ..
       !! processed by numpydoc !!

.. py:property:: r45
   :type: float


   
   Get or set the Material anisotropic parameters
















   ..
       !! processed by numpydoc !!

.. py:property:: r90
   :type: float


   
   Get or set the Material anisotropic parameters
















   ..
       !! processed by numpydoc !!

.. py:property:: al_fe
   :type: str


   
   Get or set the This parameter is used to define blank Young's Modulus and density. If this parameter is defined, E and Density will be found by using the proper unit, which is specified below.
   EQ. A:  the blank is aluminum
   EQ. F:   the blank is steel (default)
















   ..
       !! processed by numpydoc !!

.. py:property:: unit
   :type: int


   
   Get or set the Unit adopted in this simulation. This unit is used to obtain proper punch velocity, acceleration, time step, and material properties
















   ..
       !! processed by numpydoc !!

.. py:property:: lcss
   :type: Optional[int]


   
   Get or set the If the material for the blank has not been defined, this curve will be used to define the stress-strain relation. Otherwise, this curve is ignored.
















   ..
       !! processed by numpydoc !!

.. py:property:: k
   :type: float


   
   Get or set the strength coefficient for exponential hardening. If LCSS is defined, or blank materials has already been defined, this parameter is ignored
















   ..
       !! processed by numpydoc !!

.. py:property:: n
   :type: Optional[float]


   
   Get or set the exponent for exponential hardening. If LCSS is defined, or blank materials has already been defined, this parameter is ignored
















   ..
       !! processed by numpydoc !!

.. py:property:: e
   :type: Optional[float]


   
   Get or set the Material Young's Modulus. If AL/FE is defined, E is not necessary
















   ..
       !! processed by numpydoc !!

.. py:property:: density
   :type: Optional[float]


   
   Get or set the Blank density. If AL/FE is defined, this parameter is not necessary
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: Optional[float]


   
   Get or set the Possion Ratio
















   ..
       !! processed by numpydoc !!

.. py:property:: fs
   :type: float


   
   Get or set the Friction coefficient. If contact is defined, this parameter is ignored
















   ..
       !! processed by numpydoc !!

.. py:property:: mtype
   :type: int


   
   Get or set the Material model (Only M37 is supported)
















   ..
       !! processed by numpydoc !!

.. py:property:: patern
   :type: int


   
   Get or set the Velocity profile.
   EQ.: 1.Ramped velocity profile
   EQ.:2. Smooth velocity curve
















   ..
       !! processed by numpydoc !!

.. py:property:: vmax
   :type: float


   
   Get or set the The maximum allowable tool velocity.
















   ..
       !! processed by numpydoc !!

.. py:property:: amax
   :type: float


   
   Get or set the The maximum allowable acceleration
















   ..
       !! processed by numpydoc !!

.. py:property:: lvlada
   :type: int


   
   Get or set the Level of adaptivity
















   ..
       !! processed by numpydoc !!

.. py:property:: sizeada
   :type: float


   
   Get or set the Minimize for adaptivity
















   ..
       !! processed by numpydoc !!

.. py:property:: adatims
   :type: int


   
   Get or set the Total number of adaptivity cycles in this process
















   ..
       !! processed by numpydoc !!

.. py:property:: d3plot
   :type: int


   
   Get or set the Number of state output for d3plot file
















   ..
       !! processed by numpydoc !!

.. py:property:: gap
   :type: float


   
   Get or set the minimum gap between tools
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'FORMING_USER'






