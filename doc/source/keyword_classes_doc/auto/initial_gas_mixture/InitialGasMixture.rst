





:class:`InitialGasMixture`
==========================


.. py:class:: initial_gas_mixture.InitialGasMixture(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INITIAL_GAS_MIXTURE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InitialGasMixture

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the Set ID for initialization.
          * - :py:attr:`~stype`
            - Get or set the Set Type:
          * - :py:attr:`~mmgid`
            - Get or set the ALE Multi-material group ID of the material that may be present at t = 0 in the ALE mesh set defined by SID.  For general ALE, it must be AMMGID.
          * - :py:attr:`~temp`
            - Get or set the Initial temperature value
          * - :py:attr:`~ro1`
            - Get or set the Initial densities for up to eight different gas species.
          * - :py:attr:`~ro2`
            - Get or set the Initial densities for up to eight different gas species.
          * - :py:attr:`~ro3`
            - Get or set the Initial densities for up to eight different gas species.
          * - :py:attr:`~ro4`
            - Get or set the Initial densities for up to eight different gas species.
          * - :py:attr:`~ro5`
            - Get or set the Initial densities for up to eight different gas species.
          * - :py:attr:`~ro6`
            - Get or set the Initial densities for up to eight different gas species.
          * - :py:attr:`~ro7`
            - Get or set the Initial densities for up to eight different gas species.
          * - :py:attr:`~ro8`
            - Get or set the Initial densities for up to eight different gas species.


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

    from initial_gas_mixture import InitialGasMixture

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Set ID for initialization.
















   ..
       !! processed by numpydoc !!

.. py:property:: stype
   :type: int


   
   Get or set the Set Type:
   EQ.0: Set Part
   EQ.1: Part.
















   ..
       !! processed by numpydoc !!

.. py:property:: mmgid
   :type: Optional[int]


   
   Get or set the ALE Multi-material group ID of the material that may be present at t = 0 in the ALE mesh set defined by SID.  For general ALE, it must be AMMGID.
   For S - ALE, either AMMGID or AMMG name(AMMGNM) could be used here.Please refer to * ALE_STRUCTURED_MULTI - MATERIALS_GROUP for more details..
















   ..
       !! processed by numpydoc !!

.. py:property:: temp
   :type: Optional[float]


   
   Get or set the Initial temperature value
















   ..
       !! processed by numpydoc !!

.. py:property:: ro1
   :type: float


   
   Get or set the Initial densities for up to eight different gas species.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro2
   :type: float


   
   Get or set the Initial densities for up to eight different gas species.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro3
   :type: float


   
   Get or set the Initial densities for up to eight different gas species.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro4
   :type: float


   
   Get or set the Initial densities for up to eight different gas species.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro5
   :type: float


   
   Get or set the Initial densities for up to eight different gas species.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro6
   :type: float


   
   Get or set the Initial densities for up to eight different gas species.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro7
   :type: float


   
   Get or set the Initial densities for up to eight different gas species.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro8
   :type: float


   
   Get or set the Initial densities for up to eight different gas species.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INITIAL'


.. py:attribute:: subkeyword
   :value: 'GAS_MIXTURE'






