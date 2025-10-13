





:class:`CeseInitialChemistry`
=============================


.. py:class:: cese_initial_chemistry.CeseInitialChemistry(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CESE_INITIAL_CHEMISTRY keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: CeseInitialChemistry

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~chemid`
            - Get or set the Identifier of chemistry control card to use.
          * - :py:attr:`~compid`
            - Get or set the Identifier of chemical composition to use.
          * - :py:attr:`~uic`
            - Get or set the X-component of the fluid velocity.
          * - :py:attr:`~vic`
            - Get or set the Y-component of the fluid velocity.
          * - :py:attr:`~wic`
            - Get or set the Z-component of the fluid velocity.
          * - :py:attr:`~rhoic`
            - Get or set the Initial fluid density.
          * - :py:attr:`~pic`
            - Get or set the Initial fluid pressure.
          * - :py:attr:`~tic`
            - Get or set the Initial fluid temperature.
          * - :py:attr:`~hic`
            - Get or set the Initial fluid enthalpy. However, when CHEMID refers to a ZND 1-step reaction card, this is the progressive variable (degree of combustion).


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

    from cese_initial_chemistry import CeseInitialChemistry

Property detail
---------------

.. py:property:: chemid
   :type: Optional[int]


   
   Get or set the Identifier of chemistry control card to use.
















   ..
       !! processed by numpydoc !!

.. py:property:: compid
   :type: Optional[int]


   
   Get or set the Identifier of chemical composition to use.
















   ..
       !! processed by numpydoc !!

.. py:property:: uic
   :type: Optional[float]


   
   Get or set the X-component of the fluid velocity.
















   ..
       !! processed by numpydoc !!

.. py:property:: vic
   :type: Optional[float]


   
   Get or set the Y-component of the fluid velocity.
















   ..
       !! processed by numpydoc !!

.. py:property:: wic
   :type: Optional[float]


   
   Get or set the Z-component of the fluid velocity.
















   ..
       !! processed by numpydoc !!

.. py:property:: rhoic
   :type: Optional[float]


   
   Get or set the Initial fluid density.
















   ..
       !! processed by numpydoc !!

.. py:property:: pic
   :type: Optional[float]


   
   Get or set the Initial fluid pressure.
















   ..
       !! processed by numpydoc !!

.. py:property:: tic
   :type: Optional[float]


   
   Get or set the Initial fluid temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: hic
   :type: Optional[float]


   
   Get or set the Initial fluid enthalpy. However, when CHEMID refers to a ZND 1-step reaction card, this is the progressive variable (degree of combustion).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CESE'


.. py:attribute:: subkeyword
   :value: 'INITIAL_CHEMISTRY'






