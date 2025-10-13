





:class:`ChemistryControlFull`
=============================


.. py:class:: chemistry_control_full.ChemistryControlFull(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CHEMISTRY_CONTROL_FULL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ChemistryControlFull

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Identifier for this full chemistry calculation.
          * - :py:attr:`~errlim`
            - Get or set the Error tolerance for the full chemistry calculation.
          * - :py:attr:`~rhomin`
            - Get or set the Minimum fluid density above which chemical reactions are computed.
          * - :py:attr:`~tmin`
            - Get or set the Minimum temperature above which chemical reactions are computed


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

    from chemistry_control_full import ChemistryControlFull

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Identifier for this full chemistry calculation.
















   ..
       !! processed by numpydoc !!

.. py:property:: errlim
   :type: Optional[float]


   
   Get or set the Error tolerance for the full chemistry calculation.
















   ..
       !! processed by numpydoc !!

.. py:property:: rhomin
   :type: Optional[float]


   
   Get or set the Minimum fluid density above which chemical reactions are computed.
















   ..
       !! processed by numpydoc !!

.. py:property:: tmin
   :type: Optional[float]


   
   Get or set the Minimum temperature above which chemical reactions are computed
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CHEMISTRY'


.. py:attribute:: subkeyword
   :value: 'CONTROL_FULL'






