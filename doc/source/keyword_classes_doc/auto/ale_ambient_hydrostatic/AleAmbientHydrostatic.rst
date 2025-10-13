





:class:`AleAmbientHydrostatic`
==============================


.. py:class:: ale_ambient_hydrostatic.AleAmbientHydrostatic(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ALE_AMBIENT_HYDROSTATIC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: AleAmbientHydrostatic

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~alesid`
            - Get or set the ALESID defines the reservoir-type. ALE domain/mesh whose hydrostatic pressure field due to gravity is being initialized by this keyword. See Remark 4.
          * - :py:attr:`~stype`
            - Get or set the Set type for the SID above:  EQ.0:  SID is a part set ID ; EQ.1:  SID is a part ID.
          * - :py:attr:`~vecid`
            - Get or set the A vector ID defining the direction of gravitational acceleration.
          * - :py:attr:`~grav`
            - Get or set the Magnitude of the gravitational acceleration.
          * - :py:attr:`~pbase`
            - Get or set the The “base” pressure of each fluid layer.  This is the ambient pressure at the top of each ALE material (fluid) layer to be initialized.  Each layer must be represented by one ALE multi-material group ID (AMMG).
          * - :py:attr:`~ramptlc`
            - Get or set the This ID refers to a load curve (*DEFINE_CURVE) which defines how gravity is ramped up as a function of time.  Given the value of the gravitational acceleration, this curve, a time function, should typically vary from 0.0 to 1.0.
          * - :py:attr:`~nid`
            - Get or set the Node ID defining the top location of a material/fluid layer.
          * - :py:attr:`~mmgbl`
            - Get or set the The ALE multi-material group ID (AMMG) of the fluid occupying the space below this corresponding node (NID).


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

    from ale_ambient_hydrostatic import AleAmbientHydrostatic

Property detail
---------------

.. py:property:: alesid
   :type: Optional[int]


   
   Get or set the ALESID defines the reservoir-type. ALE domain/mesh whose hydrostatic pressure field due to gravity is being initialized by this keyword. See Remark 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: stype
   :type: int


   
   Get or set the Set type for the SID above:  EQ.0:  SID is a part set ID ; EQ.1:  SID is a part ID.
   EQ.2:Solid set ID (SSID).
















   ..
       !! processed by numpydoc !!

.. py:property:: vecid
   :type: Optional[int]


   
   Get or set the A vector ID defining the direction of gravitational acceleration.
















   ..
       !! processed by numpydoc !!

.. py:property:: grav
   :type: Optional[float]


   
   Get or set the Magnitude of the gravitational acceleration.
















   ..
       !! processed by numpydoc !!

.. py:property:: pbase
   :type: float


   
   Get or set the The “base” pressure of each fluid layer.  This is the ambient pressure at the top of each ALE material (fluid) layer to be initialized.  Each layer must be represented by one ALE multi-material group ID (AMMG).
















   ..
       !! processed by numpydoc !!

.. py:property:: ramptlc
   :type: int


   
   Get or set the This ID refers to a load curve (*DEFINE_CURVE) which defines how gravity is ramped up as a function of time.  Given the value of the gravitational acceleration, this curve, a time function, should typically vary from 0.0 to 1.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Node ID defining the top location of a material/fluid layer.
















   ..
       !! processed by numpydoc !!

.. py:property:: mmgbl
   :type: Optional[int]


   
   Get or set the The ALE multi-material group ID (AMMG) of the fluid occupying the space below this corresponding node (NID).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ALE'


.. py:attribute:: subkeyword
   :value: 'AMBIENT_HYDROSTATIC'






