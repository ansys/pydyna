





:class:`AirbagSimpleAirbagModel`
================================


.. py:class:: airbag_simple_airbag_model.AirbagSimpleAirbagModel(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA AIRBAG_SIMPLE_AIRBAG_MODEL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: AirbagSimpleAirbagModel

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the Set ID.
          * - :py:attr:`~sidtyp`
            - Get or set the Set type:
          * - :py:attr:`~rbid`
            - Get or set the Rigid body part ID for user defined activation subroutine:
          * - :py:attr:`~vsca`
            - Get or set the Volume scale factor, V-sca (default=1.0).
          * - :py:attr:`~psca`
            - Get or set the Pressure scale factor, P-sca (default=1.0).
          * - :py:attr:`~vini`
            - Get or set the Initial filled volume, V-ini (default=0.0).
          * - :py:attr:`~mwd`
            - Get or set the Mass weighted damping factor, D (default=0.0).
          * - :py:attr:`~spsf`
            - Get or set the Stagnation pressure scale factor, 0.0 <= gamma <= 1.0.
          * - :py:attr:`~cv`
            - Get or set the Heat capacity at constant volume.
          * - :py:attr:`~cp`
            - Get or set the Heat capacity at constant pressure.
          * - :py:attr:`~t`
            - Get or set the Temperature of input gas.
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID specifying input mass flow rate. See *DEFINE_CURVE.
          * - :py:attr:`~mu`
            - Get or set the Shape factor for exit hole, mu:
          * - :py:attr:`~area`
            - Get or set the Exit area, A:
          * - :py:attr:`~pe`
            - Get or set the Ambient pressure, pe.
          * - :py:attr:`~ro`
            - Get or set the Ambient density, rho.
          * - :py:attr:`~lou`
            - Get or set the Optional load curve ID giving mass flow out versus gauge pressure in bag. See *DEFINE_CURVE.
          * - :py:attr:`~text`
            - Get or set the Ambient temperature. (Define if and only if CV=0.0).
          * - :py:attr:`~a`
            - Get or set the First heat capacity coefficient of inflator gas (e.g., Joules/mole/o K). (Define if and only if CV=0.0).
          * - :py:attr:`~b`
            - Get or set the Second heat capacity coefficient of inflator gas, (e.g., Joules/mole/o K**2 ).  (Define if and only if CV=0.0).
          * - :py:attr:`~mw`
            - Get or set the Molecular weight of inflator gas (e.g., Kg/mole). (Define if and only if CV=0.0).
          * - :py:attr:`~gasc`
            - Get or set the Universal gas constant of inflator gas (e.g., 8.314 Joules/mole/o K).  (Define if and only if CV=0.0).


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

    from airbag_simple_airbag_model import AirbagSimpleAirbagModel

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: sidtyp
   :type: int


   
   Get or set the Set type:
   EQ.0: segment,
   EQ.1: part IDs.
















   ..
       !! processed by numpydoc !!

.. py:property:: rbid
   :type: int


   
   Get or set the Rigid body part ID for user defined activation subroutine:
   EQ.-RBID: sensor subroutine flags initiates the inflator. Load curves are offset by initiation time,
   EQ.0: the control volume is active from time zero,
   EQ.RBID: user sensor subroutine flags the start of the inflation. Load curves are offset by initiation time.
















   ..
       !! processed by numpydoc !!

.. py:property:: vsca
   :type: float


   
   Get or set the Volume scale factor, V-sca (default=1.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: psca
   :type: float


   
   Get or set the Pressure scale factor, P-sca (default=1.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: vini
   :type: float


   
   Get or set the Initial filled volume, V-ini (default=0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: mwd
   :type: float


   
   Get or set the Mass weighted damping factor, D (default=0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: spsf
   :type: float


   
   Get or set the Stagnation pressure scale factor, 0.0 <= gamma <= 1.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: cv
   :type: Optional[float]


   
   Get or set the Heat capacity at constant volume.
















   ..
       !! processed by numpydoc !!

.. py:property:: cp
   :type: Optional[float]


   
   Get or set the Heat capacity at constant pressure.
















   ..
       !! processed by numpydoc !!

.. py:property:: t
   :type: Optional[float]


   
   Get or set the Temperature of input gas.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID specifying input mass flow rate. See *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: mu
   :type: Optional[float]


   
   Get or set the Shape factor for exit hole, mu:
   LT.0.0: |mu| is the load curve number defining the shape factor as a function of absolute pressure.
















   ..
       !! processed by numpydoc !!

.. py:property:: area
   :type: Optional[float]


   
   Get or set the Exit area, A:
   GE.0.0: A is the exit area and is constant in time,
   LT.0.0: |A| is the load curve number defining the exit area as a function of absolute pressure.
















   ..
       !! processed by numpydoc !!

.. py:property:: pe
   :type: Optional[float]


   
   Get or set the Ambient pressure, pe.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Ambient density, rho.
















   ..
       !! processed by numpydoc !!

.. py:property:: lou
   :type: int


   
   Get or set the Optional load curve ID giving mass flow out versus gauge pressure in bag. See *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: text
   :type: float


   
   Get or set the Ambient temperature. (Define if and only if CV=0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: a
   :type: float


   
   Get or set the First heat capacity coefficient of inflator gas (e.g., Joules/mole/o K). (Define if and only if CV=0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: b
   :type: float


   
   Get or set the Second heat capacity coefficient of inflator gas, (e.g., Joules/mole/o K**2 ).  (Define if and only if CV=0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: mw
   :type: float


   
   Get or set the Molecular weight of inflator gas (e.g., Kg/mole). (Define if and only if CV=0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: gasc
   :type: float


   
   Get or set the Universal gas constant of inflator gas (e.g., 8.314 Joules/mole/o K).  (Define if and only if CV=0.0).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'AIRBAG'


.. py:attribute:: subkeyword
   :value: 'SIMPLE_AIRBAG_MODEL'






