





:class:`ControlExplicitThermalProperties`
=========================================


.. py:class:: control_explicit_thermal_properties.ControlExplicitThermalProperties(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_EXPLICIT_THERMAL_PROPERTIES keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlExplicitThermalProperties

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~partset`
            - Get or set the Part set ID (See *SET_PART).
          * - :py:attr:`~cp`
            - Get or set the Heat capacity.
          * - :py:attr:`~cptyp`
            - Get or set the Type of CP:
          * - :py:attr:`~vecid1`
            - Get or set the *DEFINE_VECTOR IDs to define a specific coordinate system.
          * - :py:attr:`~vecid2`
            - Get or set the *DEFINE_VECTOR IDs to define a specific coordinate system.
          * - :py:attr:`~local`
            - Get or set the Flag to activate an element coordinate system:
          * - :py:attr:`~kxx`
            - Get or set the Heat conductivity matrix.
          * - :py:attr:`~kxy`
            - Get or set the Heat conductivity matrix.
          * - :py:attr:`~kxz`
            - Get or set the Heat conductivity matrix.
          * - :py:attr:`~kxxtyp`
            - Get or set the Type of Kij:
          * - :py:attr:`~kxytyp`
            - Get or set the Type of Kij:
          * - :py:attr:`~kxztyp`
            - Get or set the Type of Kij:
          * - :py:attr:`~kyx`
            - Get or set the Heat conductivity matrix.
          * - :py:attr:`~kyy`
            - Get or set the Heat conductivity matrix.
          * - :py:attr:`~kyz`
            - Get or set the Heat conductivity matrix.
          * - :py:attr:`~kyxtyp`
            - Get or set the Type of Kij:
          * - :py:attr:`~kyytyp`
            - Get or set the Type of Kij:
          * - :py:attr:`~kyztyp`
            - Get or set the Type of Kij:
          * - :py:attr:`~kzx`
            - Get or set the Heat conductivity matrix.
          * - :py:attr:`~kzy`
            - Get or set the Heat conductivity matrix.
          * - :py:attr:`~kzz`
            - Get or set the Heat conductivity matrix.
          * - :py:attr:`~kzxtyp`
            - Get or set the Type of Kij:
          * - :py:attr:`~kzytyp`
            - Get or set the Type of Kij:
          * - :py:attr:`~kzztyp`
            - Get or set the Type of Kij:


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

    from control_explicit_thermal_properties import ControlExplicitThermalProperties

Property detail
---------------

.. py:property:: partset
   :type: Optional[int]


   
   Get or set the Part set ID (See *SET_PART).
















   ..
       !! processed by numpydoc !!

.. py:property:: cp
   :type: Optional[float]


   
   Get or set the Heat capacity.
















   ..
       !! processed by numpydoc !!

.. py:property:: cptyp
   :type: int


   
   Get or set the Type of CP:
   EQ.0:   CP is a constant
   EQ.1 : CP is the ID of a * DEFINE_‌CURVE defining a table of temperature as a function of heat capacity.
















   ..
       !! processed by numpydoc !!

.. py:property:: vecid1
   :type: int


   
   Get or set the *DEFINE_VECTOR IDs to define a specific coordinate system.
   VECID1 and VECID2 give the x- and y-direction, respectively.
   The z-vector is a cross product of VECID1 and VECID2. If VECID2 is not orthogonal to VECID1,
   its direction will be corrected with a cross-product of the z- and x-vectors.
   The conductivity matrix Kij is applied in this coordinate system.
















   ..
       !! processed by numpydoc !!

.. py:property:: vecid2
   :type: int


   
   Get or set the *DEFINE_VECTOR IDs to define a specific coordinate system.
   VECID1 and VECID2 give the x- and y-direction, respectively.
   The z-vector is a cross product of VECID1 and VECID2. If VECID2 is not orthogonal to VECID1,
   its direction will be corrected with a cross-product of the z- and x-vectors.
   The conductivity matrix Kij is applied in this coordinate system..
















   ..
       !! processed by numpydoc !!

.. py:property:: local
   :type: int


   
   Get or set the Flag to activate an element coordinate system:
   EQ.0:   The vectors VECIDj are considered in a global coordinate system.
   EQ.1 : The vectors VECIDj are considered in a local system attached to the element.
   For shells and solids, the system is the same as DIREC = 1 and CTYPE = 12 in * CONSTRAINED_LAGRANGE_IN_SOLID.
   For shells, the edge centers replace the face centers.For beams, the x - direction is aligned with
   the first 2 nodes in * ELEMENT_BEAM and there should be a 3rd node for the y - direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: kxx
   :type: float


   
   Get or set the Heat conductivity matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: kxy
   :type: float


   
   Get or set the Heat conductivity matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: kxz
   :type: float


   
   Get or set the Heat conductivity matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: kxxtyp
   :type: int


   
   Get or set the Type of Kij:
   EQ.0:   Kij is a constant
   EQ.1 : Kij is the ID of a * DEFINE_‌CURVE defining a table of temperature as a function of heat conductivity.
















   ..
       !! processed by numpydoc !!

.. py:property:: kxytyp
   :type: int


   
   Get or set the Type of Kij:
   EQ.0:   Kij is a constant
   EQ.1 : Kij is the ID of a * DEFINE_‌CURVE defining a table of temperature as a function of heat conductivity.
















   ..
       !! processed by numpydoc !!

.. py:property:: kxztyp
   :type: int


   
   Get or set the Type of Kij:
   EQ.0:   Kij is a constant
   EQ.1 : Kij is the ID of a * DEFINE_‌CURVE defining a table of temperature as a function of heat conductivity.
















   ..
       !! processed by numpydoc !!

.. py:property:: kyx
   :type: float


   
   Get or set the Heat conductivity matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: kyy
   :type: float


   
   Get or set the Heat conductivity matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: kyz
   :type: float


   
   Get or set the Heat conductivity matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: kyxtyp
   :type: int


   
   Get or set the Type of Kij:
   EQ.0:   Kij is a constant
   EQ.1 : Kij is the ID of a * DEFINE_‌CURVE defining a table of temperature as a function of heat conductivity.
















   ..
       !! processed by numpydoc !!

.. py:property:: kyytyp
   :type: int


   
   Get or set the Type of Kij:
   EQ.0:   Kij is a constant
   EQ.1 : Kij is the ID of a * DEFINE_‌CURVE defining a table of temperature as a function of heat conductivity.
















   ..
       !! processed by numpydoc !!

.. py:property:: kyztyp
   :type: int


   
   Get or set the Type of Kij:
   EQ.0:   Kij is a constant
   EQ.1 : Kij is the ID of a * DEFINE_‌CURVE defining a table of temperature as a function of heat conductivity.
















   ..
       !! processed by numpydoc !!

.. py:property:: kzx
   :type: float


   
   Get or set the Heat conductivity matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: kzy
   :type: float


   
   Get or set the Heat conductivity matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: kzz
   :type: float


   
   Get or set the Heat conductivity matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: kzxtyp
   :type: int


   
   Get or set the Type of Kij:
   EQ.0:   Kij is a constant
   EQ.1 : Kij is the ID of a * DEFINE_‌CURVE defining a table of temperature as a function of heat conductivity.
















   ..
       !! processed by numpydoc !!

.. py:property:: kzytyp
   :type: int


   
   Get or set the Type of Kij:
   EQ.0:   Kij is a constant
   EQ.1 : Kij is the ID of a * DEFINE_‌CURVE defining a table of temperature as a function of heat conductivity.
















   ..
       !! processed by numpydoc !!

.. py:property:: kzztyp
   :type: int


   
   Get or set the Type of Kij:
   EQ.0:   Kij is a constant
   EQ.1 : Kij is the ID of a * DEFINE_‌CURVE defining a table of temperature as a function of heat conductivity.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'EXPLICIT_THERMAL_PROPERTIES'






