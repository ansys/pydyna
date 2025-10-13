





:class:`DatabaseTracerGenerate`
===============================


.. py:class:: database_tracer_generate.DatabaseTracerGenerate(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DATABASE_TRACER_GENERATE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DatabaseTracerGenerate

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~dt`
            - Get or set the Interval time between each tracer generation and position update (See Remark 1).
          * - :py:attr:`~valow`
            - Get or set the Range of values between which the isosurface is defined. VALOW is the lower bound while VALUP is the upper bound.  See Remark 2.
          * - :py:attr:`~valup`
            - Get or set the Range of values between which the isosurface is defined. VALOW is the lower bound while VALUP is the upper bound.  See Remark 2.
          * - :py:attr:`~valtype1`
            - Get or set the The variable that will be used to generate the isosurfaces.  See VALTYPE2 for enumeration of values.
          * - :py:attr:`~set`
            - Get or set the Set ID (See Remark 2)
          * - :py:attr:`~setype`
            - Get or set the Type of set (See Remark 2):
          * - :py:attr:`~mmgset`
            - Get or set the Multi-material group set (See Remark 3).
          * - :py:attr:`~updt`
            - Get or set the Time interval between tracer position update (See Remark 1).
          * - :py:attr:`~varloc`
            - Get or set the Variable location in trcrgen_binout to be replaced with the variable specified in the VALTYPE2 field:
          * - :py:attr:`~valtype2`
            - Get or set the Data to be output to the trcrgen_binout file.  The interpretation of VALTYPE1 and VALTYPE2 is enumerated in the following list:


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

    from database_tracer_generate import DatabaseTracerGenerate

Property detail
---------------

.. py:property:: dt
   :type: Optional[float]


   
   Get or set the Interval time between each tracer generation and position update (See Remark 1).
















   ..
       !! processed by numpydoc !!

.. py:property:: valow
   :type: float


   
   Get or set the Range of values between which the isosurface is defined. VALOW is the lower bound while VALUP is the upper bound.  See Remark 2.
   The value at the isosurface is 0.5(VALOW+VALUP).
   The variable with this value is defined by VALTYPE.
















   ..
       !! processed by numpydoc !!

.. py:property:: valup
   :type: float


   
   Get or set the Range of values between which the isosurface is defined. VALOW is the lower bound while VALUP is the upper bound.  See Remark 2.
   The value at the isosurface is 0.5(VALOW+VALUP).
   The variable with this value is defined by VALTYPE.
















   ..
       !! processed by numpydoc !!

.. py:property:: valtype1
   :type: Optional[int]


   
   Get or set the The variable that will be used to generate the isosurfaces.  See VALTYPE2 for enumeration of values.
















   ..
       !! processed by numpydoc !!

.. py:property:: set
   :type: Optional[int]


   
   Get or set the Set ID (See Remark 2)
















   ..
       !! processed by numpydoc !!

.. py:property:: setype
   :type: int


   
   Get or set the Type of set (See Remark 2):
   EQ.0:   solid set
   EQ.1:   segment set
   EQ.2:   node set
















   ..
       !! processed by numpydoc !!

.. py:property:: mmgset
   :type: Optional[int]


   
   Get or set the Multi-material group set (See Remark 3).
















   ..
       !! processed by numpydoc !!

.. py:property:: updt
   :type: Optional[float]


   
   Get or set the Time interval between tracer position update (See Remark 1).
















   ..
       !! processed by numpydoc !!

.. py:property:: varloc
   :type: int


   
   Get or set the Variable location in trcrgen_binout to be replaced with the variable specified in the VALTYPE2 field:
   EQ.4:   -velocity
   EQ.5:   -velocity
   EQ.6:   -velocity
   EQ.7:   -stress
   EQ.8:   -stress
   EQ.9:   -stress
   EQ.10:  -stress
   EQ.11:  -stress
   EQ.12:  -stress
   EQ.13:  plastic strain
   EQ.14:  density
   EQ.15:  relative volume
















   ..
       !! processed by numpydoc !!

.. py:property:: valtype2
   :type: int


   
   Get or set the Data to be output to the trcrgen_binout file.  The interpretation of VALTYPE1 and VALTYPE2 is enumerated in the following list:
   EQ.1:   -stress
   EQ.2:   -stress
   EQ.3:   -stress
   EQ.4:   -stress
   EQ.5:   -stress
   EQ.6:   -stress
   EQ.7:   plastic strain
   EQ.8:   internal energy
   EQ.9:   bulk viscosity
   EQ.10:  relative volume
   GE.11 and LE.19:        other auxiliary variables
   EQ.20:  pressure
   EQ.21:  density
   EQ.22:  material volume
   EQ.23:  compression ratio
   EQ.24:  element volume fraction.
   EQ.25:  nodal volume fraction
   EQ.26:  -position
   EQ.27:  -position
   EQ.28:  -position
   EQ.29:  -velocity
   EQ.30:  -velocity
   EQ.31:  -velocity
   EQ.31:  velocity
   EQ.33:  -acceleration
   EQ.34:  - acceleration
   EQ.35:  - acceleration
   EQ.36:  acceleration
   EQ.37:  nodal mass
   EQ.38:  nodal temperature.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DATABASE'


.. py:attribute:: subkeyword
   :value: 'TRACER_GENERATE'






