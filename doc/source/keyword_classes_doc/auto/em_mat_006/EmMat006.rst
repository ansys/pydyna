





:class:`EmMat006`
=================


.. py:class:: em_mat_006.EmMat006(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_MAT_006 keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmMat006

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material ID: refers to MID in the *PART card.
          * - :py:attr:`~mtype`
            - Get or set the Defines the electromagnetism type of the material:
          * - :py:attr:`~sigp`
            - Get or set the Conductivities of the Positive current collector materials.
          * - :py:attr:`~eosp`
            - Get or set the Optional ID of the EOS to be used for the two conductivities.
          * - :py:attr:`~sign`
            - Get or set the Conductivities of the Negative current collector materials.
          * - :py:attr:`~eosn`
            - Get or set the Optional ID of the EOS to be used for the two conductivities.
          * - :py:attr:`~deatht`
            - Get or set the Death time for the material. After DEATHT, the material will no longer be considered a conductor and removed from the EM solve


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

    from em_mat_006 import EmMat006

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material ID: refers to MID in the *PART card.
















   ..
       !! processed by numpydoc !!

.. py:property:: mtype
   :type: int


   
   Get or set the Defines the electromagnetism type of the material:
   EQ.0:Air or vacuum
   EQ.1: Insulator material:these materials have the same electromagnetism behavior as MTYPE = 0
   EQ.5:Material associated to *EM_RANDLES_BATMAC.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigp
   :type: Optional[float]


   
   Get or set the Conductivities of the Positive current collector materials.
















   ..
       !! processed by numpydoc !!

.. py:property:: eosp
   :type: Optional[int]


   
   Get or set the Optional ID of the EOS to be used for the two conductivities.
















   ..
       !! processed by numpydoc !!

.. py:property:: sign
   :type: Optional[float]


   
   Get or set the Conductivities of the Negative current collector materials.
















   ..
       !! processed by numpydoc !!

.. py:property:: eosn
   :type: Optional[int]


   
   Get or set the Optional ID of the EOS to be used for the two conductivities.
















   ..
       !! processed by numpydoc !!

.. py:property:: deatht
   :type: float


   
   Get or set the Death time for the material. After DEATHT, the material will no longer be considered a conductor and removed from the EM solve
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'MAT_006'






