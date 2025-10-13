





:class:`EmEpCellmodelTomek`
===========================


.. py:class:: em_ep_cellmodel_tomek.EmEpCellmodelTomek(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_EP_CELLMODEL_TOMEK keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmEpCellmodelTomek

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~matid`
            - Get or set the Material ID: refers to MID in the *PART card
          * - :py:attr:`~phiendmid`
            - Get or set the Value between 0 and 1 that indicates the ratio of the cardiac tissue to be considered as the endocardial version of the ToR-Ord cell model
          * - :py:attr:`~phimidepl`
            - Get or set the Value between 0 and 1 that indicates the ratio of the cardiac tissue to be considered as the myocardial version of the ToR-Ord cell model


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

    from em_ep_cellmodel_tomek import EmEpCellmodelTomek

Property detail
---------------

.. py:property:: matid
   :type: Optional[int]


   
   Get or set the Material ID: refers to MID in the *PART card
















   ..
       !! processed by numpydoc !!

.. py:property:: phiendmid
   :type: Optional[float]


   
   Get or set the Value between 0 and 1 that indicates the ratio of the cardiac tissue to be considered as the endocardial version of the ToR-Ord cell model
















   ..
       !! processed by numpydoc !!

.. py:property:: phimidepl
   :type: Optional[float]


   
   Get or set the Value between 0 and 1 that indicates the ratio of the cardiac tissue to be considered as the myocardial version of the ToR-Ord cell model
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'EP_CELLMODEL_TOMEK'






