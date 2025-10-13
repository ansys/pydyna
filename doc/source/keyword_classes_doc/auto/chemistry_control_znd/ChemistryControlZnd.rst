





:class:`ChemistryControlZnd`
============================


.. py:class:: chemistry_control_znd.ChemistryControlZnd(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CHEMISTRY_CONTROL_ZND keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ChemistryControlZnd

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Identifier for this full chemistry calculation.
          * - :py:attr:`~f`
            - Get or set the Overdriven factor.
          * - :py:attr:`~eplus`
            - Get or set the EPLUS parameter of the ZND model.
          * - :py:attr:`~q0`
            - Get or set the Q0 parameter of the ZND model.
          * - :py:attr:`~gam`
            - Get or set the GAM parameter of the ZND model.
          * - :py:attr:`~xyzd`
            - Get or set the Position of the detonation front in the DETDIR direction.
          * - :py:attr:`~detdir`
            - Get or set the Detonation propagation direction (1 => X; 2 => Y; 3 => Z)


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

    from chemistry_control_znd import ChemistryControlZnd

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Identifier for this full chemistry calculation.
















   ..
       !! processed by numpydoc !!

.. py:property:: f
   :type: Optional[float]


   
   Get or set the Overdriven factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: eplus
   :type: Optional[float]


   
   Get or set the EPLUS parameter of the ZND model.
















   ..
       !! processed by numpydoc !!

.. py:property:: q0
   :type: Optional[float]


   
   Get or set the Q0 parameter of the ZND model.
















   ..
       !! processed by numpydoc !!

.. py:property:: gam
   :type: Optional[float]


   
   Get or set the GAM parameter of the ZND model.
















   ..
       !! processed by numpydoc !!

.. py:property:: xyzd
   :type: Optional[float]


   
   Get or set the Position of the detonation front in the DETDIR direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: detdir
   :type: Optional[int]


   
   Get or set the Detonation propagation direction (1 => X; 2 => Y; 3 => Z)
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CHEMISTRY'


.. py:attribute:: subkeyword
   :value: 'CONTROL_ZND'






