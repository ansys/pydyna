





:class:`EmEpCreatefiberorientation`
===================================


.. py:class:: em_ep_createfiberorientation.EmEpCreatefiberorientation(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_EP_CREATEFIBERORIENTATION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmEpCreatefiberorientation

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~parstld`
            - Get or set the Part set on which the system is solved
          * - :py:attr:`~solvelde`
            - Get or set the ID of the Laplace system that is solved in the transmural direction
          * - :py:attr:`~alpha`
            - Get or set the helical angle with respect to the counterclockwise circumferential direction in the heart when looking from the base towards the apex. If a negative value is entered, a *DEFINE_‌FUNCTION will be expected. See remark 1- for available parameters
          * - :py:attr:`~beta`
            - Get or set the angle with respect to the outward transmural axis of the heart. If a negative value is entered, a *DEFINE_‌FUNCTION will be expected. See remark 1- for available parameters
          * - :py:attr:`~w_file`
            - Get or set the Selects whether result files (ELEMENT_‌SOLID_‌ORTHO.k and vtk files) are exported:
          * - :py:attr:`~prerun`
            - Get or set the Select whether the run is stopped after creating fibers:


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

    from em_ep_createfiberorientation import EmEpCreatefiberorientation

Property detail
---------------

.. py:property:: parstld
   :type: Optional[int]


   
   Get or set the Part set on which the system is solved
















   ..
       !! processed by numpydoc !!

.. py:property:: solvelde
   :type: Optional[int]


   
   Get or set the ID of the Laplace system that is solved in the transmural direction
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha
   :type: Optional[int]


   
   Get or set the helical angle with respect to the counterclockwise circumferential direction in the heart when looking from the base towards the apex. If a negative value is entered, a *DEFINE_‌FUNCTION will be expected. See remark 1- for available parameters
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[int]


   
   Get or set the angle with respect to the outward transmural axis of the heart. If a negative value is entered, a *DEFINE_‌FUNCTION will be expected. See remark 1- for available parameters
















   ..
       !! processed by numpydoc !!

.. py:property:: w_file
   :type: Optional[int]


   
   Get or set the Selects whether result files (ELEMENT_‌SOLID_‌ORTHO.k and vtk files) are exported:
   EQ.0:   not exported
   EQ.1 : exported
















   ..
       !! processed by numpydoc !!

.. py:property:: prerun
   :type: Optional[int]


   
   Get or set the Select whether the run is stopped after creating fibers:
   EQ.0:   do not stop after fiber creation
   EQ.1 : stop after fiber creation
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'EP_CREATEFIBERORIENTATION'






