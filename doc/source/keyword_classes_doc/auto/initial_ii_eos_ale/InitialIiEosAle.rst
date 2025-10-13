





:class:`InitialIiEosAle`
========================


.. py:class:: initial_ii_eos_ale.InitialIiEosAle(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INITIAL_II_EOS_ALE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InitialIiEosAle

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Part ID or part set ID or element set ID.
          * - :py:attr:`~type`
            - Get or set the Type of “ID”:
          * - :py:attr:`~mmg`
            - Get or set the Specifies the multi-material group.GT.0: ALE multi - material group.LT.0 : Set ID of ALE multi - material groups defined in * SET_‌MULTI - MATERIAL_‌GROUP.
          * - :py:attr:`~e0`
            - Get or set the Initial internal energy per reference volume unit (as defined in *EOS).  See Remark 1
          * - :py:attr:`~v0`
            - Get or set the Initial relative volume (as defined in *EOS)
          * - :py:attr:`~p0`
            - Get or set the Initial pressure


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

    from initial_ii_eos_ale import InitialIiEosAle

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Part ID or part set ID or element set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: type
   :type: Optional[int]


   
   Get or set the Type of “ID”:
   EQ.0:   part set ID.
   EQ.1 : part ID.
   EQ.2 : element set ID(*SET_‌BEAM in 1D, *SET_‌SHELL in 2D, *SET_‌SOLID in 3D).
















   ..
       !! processed by numpydoc !!

.. py:property:: mmg
   :type: Optional[int]


   
   Get or set the Specifies the multi-material group.GT.0: ALE multi - material group.LT.0 : Set ID of ALE multi - material groups defined in * SET_‌MULTI - MATERIAL_‌GROUP.
















   ..
       !! processed by numpydoc !!

.. py:property:: e0
   :type: float


   
   Get or set the Initial internal energy per reference volume unit (as defined in *EOS).  See Remark 1
















   ..
       !! processed by numpydoc !!

.. py:property:: v0
   :type: float


   
   Get or set the Initial relative volume (as defined in *EOS)
















   ..
       !! processed by numpydoc !!

.. py:property:: p0
   :type: float


   
   Get or set the Initial pressure
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INITIAL'


.. py:attribute:: subkeyword
   :value: 'II_EOS_ALE'






