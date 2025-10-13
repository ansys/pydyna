





:class:`ControlImplicitModalDynamic`
====================================


.. py:class:: control_implicit_modal_dynamic.ControlImplicitModalDynamic(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_IMPLICIT_MODAL_DYNAMIC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlImplicitModalDynamic

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mdflag`
            - Get or set the Modal Dynamic flag
          * - :py:attr:`~zeta`
            - Get or set the Modal Dynamic damping constant.
          * - :py:attr:`~md_strs`
            - Get or set the Calculate modal dynamic stress.
          * - :py:attr:`~dtout`
            - Get or set the Modal dynamics output interval.
          * - :py:attr:`~integ`
            - Get or set the Integration method
          * - :py:attr:`~nsid`
            - Get or set the The node set ID of the nodes in the modal model that are subjected to loads. If the set is not specified, then the forces are summed over all the nodes, and that is usually much more expensive than summing over only those subjected to a load..
          * - :py:attr:`~filename`
            - Get or set the If specified the eigenmodes are read from the specified file. Otherwise
          * - :py:attr:`~filename2`
            - Get or set the If specified the eigenmodes are read from the specified file. Otherwise


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

    from control_implicit_modal_dynamic import ControlImplicitModalDynamic

Property detail
---------------

.. py:property:: mdflag
   :type: int


   
   Get or set the Modal Dynamic flag
   EQ.0: no modal dynamic analysis
   EQ.1: perform modal dynamic analysis.
   EQ.2:   perform modal dynamic analysis with prescribed motion constraints on the constraint modes input with Card 3.  See Remark 7
















   ..
       !! processed by numpydoc !!

.. py:property:: zeta
   :type: Optional[float]


   
   Get or set the Modal Dynamic damping constant.
















   ..
       !! processed by numpydoc !!

.. py:property:: md_strs
   :type: Optional[int]


   
   Get or set the Calculate modal dynamic stress.
















   ..
       !! processed by numpydoc !!

.. py:property:: dtout
   :type: Optional[float]


   
   Get or set the Modal dynamics output interval.
















   ..
       !! processed by numpydoc !!

.. py:property:: integ
   :type: int


   
   Get or set the Integration method
   EQ.0:   defaults to 1.
   EQ.1:   perform modal dynamic analysis with explicit time integration.
   EQ.2:   perform modal dynamic analysis with implicit time integration..
















   ..
       !! processed by numpydoc !!

.. py:property:: nsid
   :type: Optional[int]


   
   Get or set the The node set ID of the nodes in the modal model that are subjected to loads. If the set is not specified, then the forces are summed over all the nodes, and that is usually much more expensive than summing over only those subjected to a load..
















   ..
       !! processed by numpydoc !!

.. py:property:: filename
   :type: Optional[str]


   
   Get or set the If specified the eigenmodes are read from the specified file. Otherwise
   the eigenmodes are computed as specified on *CONTROL_IMPLICIT_  EIGENVALUE.
















   ..
       !! processed by numpydoc !!

.. py:property:: filename2
   :type: Optional[str]


   
   Get or set the If specified the eigenmodes are read from the specified file. Otherwise
   the eigenmodes are computed as specified on *CONTROL_IMPLICIT_  EIGENVALUE.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'IMPLICIT_MODAL_DYNAMIC'






