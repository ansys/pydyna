





:class:`EmEpCellmodelFentonkarma`
=================================


.. py:class:: em_ep_cellmodel_fentonkarma.EmEpCellmodelFentonkarma(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_EP_CELLMODEL_FENTONKARMA keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmEpCellmodelFentonkarma

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~matid`
            - Get or set the Material ID defined in *MAT_.
          * - :py:attr:`~taud`
            - Get or set the Time constant td described in Equation(5).
          * - :py:attr:`~taur`
            - Get or set the Time constant tr described in Equation (6).
          * - :py:attr:`~tausi`
            - Get or set the Time constant tsi described in Equation (7).
          * - :py:attr:`~tauo`
            - Get or set the Time constant t0 described in Equation(6).
          * - :py:attr:`~tauvp`
            - Get or set the Time constant tvp described in Equation(3).
          * - :py:attr:`~tauvm`
            - Get or set the Time constant tvm described in Equation(3)
          * - :py:attr:`~tauwp`
            - Get or set the Time constant twp described in Equation(4).
          * - :py:attr:`~tauwm`
            - Get or set the Time constant twm described in Equation(4).
          * - :py:attr:`~uc`
            - Get or set the Threshold potential Uc for activation of Jfi (the fast inward current) Equation (3, 4, 5, 6).
          * - :py:attr:`~ucsi`
            - Get or set the Threshold potential Ucsi for activation of Jsi (the slow inward current) in Equation (7).
          * - :py:attr:`~k`
            - Get or set the Constant k in Equation(7).
          * - :py:attr:`~u0`
            - Get or set the Initial value of U respectively.
          * - :py:attr:`~v0`
            - Get or set the Initial value of V respectively.
          * - :py:attr:`~w0`
            - Get or set the Initial value of W respectively.


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

    from em_ep_cellmodel_fentonkarma import EmEpCellmodelFentonkarma

Property detail
---------------

.. py:property:: matid
   :type: Optional[int]


   
   Get or set the Material ID defined in *MAT_.
















   ..
       !! processed by numpydoc !!

.. py:property:: taud
   :type: Optional[float]


   
   Get or set the Time constant td described in Equation(5).
















   ..
       !! processed by numpydoc !!

.. py:property:: taur
   :type: Optional[float]


   
   Get or set the Time constant tr described in Equation (6).
















   ..
       !! processed by numpydoc !!

.. py:property:: tausi
   :type: Optional[float]


   
   Get or set the Time constant tsi described in Equation (7).
















   ..
       !! processed by numpydoc !!

.. py:property:: tauo
   :type: Optional[float]


   
   Get or set the Time constant t0 described in Equation(6).
















   ..
       !! processed by numpydoc !!

.. py:property:: tauvp
   :type: Optional[float]


   
   Get or set the Time constant tvp described in Equation(3).
















   ..
       !! processed by numpydoc !!

.. py:property:: tauvm
   :type: Optional[float]


   
   Get or set the Time constant tvm described in Equation(3)
















   ..
       !! processed by numpydoc !!

.. py:property:: tauwp
   :type: Optional[float]


   
   Get or set the Time constant twp described in Equation(4).
















   ..
       !! processed by numpydoc !!

.. py:property:: tauwm
   :type: Optional[float]


   
   Get or set the Time constant twm described in Equation(4).
















   ..
       !! processed by numpydoc !!

.. py:property:: uc
   :type: Optional[float]


   
   Get or set the Threshold potential Uc for activation of Jfi (the fast inward current) Equation (3, 4, 5, 6).
















   ..
       !! processed by numpydoc !!

.. py:property:: ucsi
   :type: Optional[float]


   
   Get or set the Threshold potential Ucsi for activation of Jsi (the slow inward current) in Equation (7).
















   ..
       !! processed by numpydoc !!

.. py:property:: k
   :type: Optional[float]


   
   Get or set the Constant k in Equation(7).
















   ..
       !! processed by numpydoc !!

.. py:property:: u0
   :type: Optional[float]


   
   Get or set the Initial value of U respectively.
















   ..
       !! processed by numpydoc !!

.. py:property:: v0
   :type: Optional[float]


   
   Get or set the Initial value of V respectively.
















   ..
       !! processed by numpydoc !!

.. py:property:: w0
   :type: Optional[float]


   
   Get or set the Initial value of W respectively.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'EP_CELLMODEL_FENTONKARMA'






