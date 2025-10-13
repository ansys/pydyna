





:class:`DeformableToRigid`
==========================


.. py:class:: deformable_to_rigid.DeformableToRigid(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFORMABLE_TO_RIGID keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DeformableToRigid

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID for the part that will switched to a rigid material, also see *PART.
          * - :py:attr:`~lrb`
            - Get or set the Part ID of the lead rigid body to which the part is merged.
          * - :py:attr:`~ptype`
            - Get or set the Type of PID:


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

    from deformable_to_rigid import DeformableToRigid

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID for the part that will switched to a rigid material, also see *PART.
















   ..
       !! processed by numpydoc !!

.. py:property:: lrb
   :type: int


   
   Get or set the Part ID of the lead rigid body to which the part is merged.
   EQ.0: Part becomes either an independent or lead rigid body.
















   ..
       !! processed by numpydoc !!

.. py:property:: ptype
   :type: str


   
   Get or set the Type of PID:
   EQ."PART": PID is a part ID.
   EQ."PSET": PID is a part set ID.All parts included in part set PID will be switched to rigid at the start of the calculation.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DEFORMABLE'


.. py:attribute:: subkeyword
   :value: 'TO_RIGID'






