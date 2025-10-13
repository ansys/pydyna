





:class:`LoadBrode`
==================


.. py:class:: load_brode.LoadBrode(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_BRODE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadBrode

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~yld`
            - Get or set the Yield (Kt, equivalent tons of TNT).
          * - :py:attr:`~bht`
            - Get or set the Height of burst.
          * - :py:attr:`~xbo`
            - Get or set the x-coordinates of Brode origin.
          * - :py:attr:`~ybo`
            - Get or set the y-coordinates of Brode origin.
          * - :py:attr:`~zbo`
            - Get or set the z-coordinates of Brode origin.
          * - :py:attr:`~tbo`
            - Get or set the Time offset of Brode origin.
          * - :py:attr:`~talc`
            - Get or set the Load curve number giving time of arrival versus range relative to Brode origin (space, time), see *DEFINE_CURVE.
          * - :py:attr:`~sflc`
            - Get or set the Load curve number giving yield scaling versus scaled time , see *DEFINE_ CURVE.
          * - :py:attr:`~cfl`
            - Get or set the Conversion factor - kft to LS-DYNA length.
          * - :py:attr:`~cft`
            - Get or set the Conversion factor - milliseconds to LS-DYNA time units.
          * - :py:attr:`~cfp`
            - Get or set the Conversion factor - psi to LS-DYNA pressure units.


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

    from load_brode import LoadBrode

Property detail
---------------

.. py:property:: yld
   :type: float


   
   Get or set the Yield (Kt, equivalent tons of TNT).
















   ..
       !! processed by numpydoc !!

.. py:property:: bht
   :type: float


   
   Get or set the Height of burst.
















   ..
       !! processed by numpydoc !!

.. py:property:: xbo
   :type: float


   
   Get or set the x-coordinates of Brode origin.
















   ..
       !! processed by numpydoc !!

.. py:property:: ybo
   :type: float


   
   Get or set the y-coordinates of Brode origin.
















   ..
       !! processed by numpydoc !!

.. py:property:: zbo
   :type: float


   
   Get or set the z-coordinates of Brode origin.
















   ..
       !! processed by numpydoc !!

.. py:property:: tbo
   :type: float


   
   Get or set the Time offset of Brode origin.
















   ..
       !! processed by numpydoc !!

.. py:property:: talc
   :type: int


   
   Get or set the Load curve number giving time of arrival versus range relative to Brode origin (space, time), see *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: sflc
   :type: int


   
   Get or set the Load curve number giving yield scaling versus scaled time , see *DEFINE_ CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: cfl
   :type: float


   
   Get or set the Conversion factor - kft to LS-DYNA length.
















   ..
       !! processed by numpydoc !!

.. py:property:: cft
   :type: float


   
   Get or set the Conversion factor - milliseconds to LS-DYNA time units.
















   ..
       !! processed by numpydoc !!

.. py:property:: cfp
   :type: float


   
   Get or set the Conversion factor - psi to LS-DYNA pressure units.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'BRODE'






