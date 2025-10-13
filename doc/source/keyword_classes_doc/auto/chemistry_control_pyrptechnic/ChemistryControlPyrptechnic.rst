





:class:`ChemistryControlPyrptechnic`
====================================


.. py:class:: chemistry_control_pyrptechnic.ChemistryControlPyrptechnic(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CHEMISTRY_CONTROL_PYRPTECHNIC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ChemistryControlPyrptechnic

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~comp1id`
            - Get or set the Chemical composition identifier of composition to use in the chamber
          * - :py:attr:`~vol1`
            - Get or set the Volume of the chamber.
          * - :py:attr:`~area1`
            - Get or set the Area of the chamber
          * - :py:attr:`~cd1`
            - Get or set the Discharge coefficient of the chamber
          * - :py:attr:`~p1`
            - Get or set the Pressure in the chamber
          * - :py:attr:`~t1`
            - Get or set the Temperature in the chamber
          * - :py:attr:`~delp1`
            - Get or set the Rupture pressure in the chamber
          * - :py:attr:`~tflame`
            - Get or set the Adiabatic flame temperature.
          * - :py:attr:`~comp2id`
            - Get or set the Chemical composition identifier of composition to use in the plenum
          * - :py:attr:`~vol2`
            - Get or set the Volume of the plenum
          * - :py:attr:`~area2`
            - Get or set the Area of the plenum
          * - :py:attr:`~cd2`
            - Get or set the Discharge coefficient of the plenum
          * - :py:attr:`~p2`
            - Get or set the Pressure in the plenum
          * - :py:attr:`~t2`
            - Get or set the Temperature in the plenum
          * - :py:attr:`~delp2`
            - Get or set the Rupture pressure in the plenum
          * - :py:attr:`~truntime`
            - Get or set the Total run time
          * - :py:attr:`~comp3id`
            - Get or set the Chemical composition identifier of composition to use in the airbag
          * - :py:attr:`~vol3`
            - Get or set the Volume of the airbag
          * - :py:attr:`~p3`
            - Get or set the Pressure in the airbag
          * - :py:attr:`~t3`
            - Get or set the Temperature in the airbag.
          * - :py:attr:`~ptime`
            - Get or set the Time interval for output of time history data to FILE.
          * - :py:attr:`~file`
            - Get or set the Name of the lsda file in which to write the results of the inflator simulation.Two load curves are written out to this file: mass flow rate and total temperature as a function of time.


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

    from chemistry_control_pyrptechnic import ChemistryControlPyrptechnic

Property detail
---------------

.. py:property:: comp1id
   :type: Optional[int]


   
   Get or set the Chemical composition identifier of composition to use in the chamber
















   ..
       !! processed by numpydoc !!

.. py:property:: vol1
   :type: Optional[float]


   
   Get or set the Volume of the chamber.
















   ..
       !! processed by numpydoc !!

.. py:property:: area1
   :type: Optional[float]


   
   Get or set the Area of the chamber
















   ..
       !! processed by numpydoc !!

.. py:property:: cd1
   :type: Optional[float]


   
   Get or set the Discharge coefficient of the chamber
















   ..
       !! processed by numpydoc !!

.. py:property:: p1
   :type: Optional[float]


   
   Get or set the Pressure in the chamber
















   ..
       !! processed by numpydoc !!

.. py:property:: t1
   :type: Optional[float]


   
   Get or set the Temperature in the chamber
















   ..
       !! processed by numpydoc !!

.. py:property:: delp1
   :type: Optional[float]


   
   Get or set the Rupture pressure in the chamber
















   ..
       !! processed by numpydoc !!

.. py:property:: tflame
   :type: Optional[float]


   
   Get or set the Adiabatic flame temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: comp2id
   :type: Optional[int]


   
   Get or set the Chemical composition identifier of composition to use in the plenum
















   ..
       !! processed by numpydoc !!

.. py:property:: vol2
   :type: Optional[float]


   
   Get or set the Volume of the plenum
















   ..
       !! processed by numpydoc !!

.. py:property:: area2
   :type: Optional[float]


   
   Get or set the Area of the plenum
















   ..
       !! processed by numpydoc !!

.. py:property:: cd2
   :type: Optional[float]


   
   Get or set the Discharge coefficient of the plenum
















   ..
       !! processed by numpydoc !!

.. py:property:: p2
   :type: Optional[float]


   
   Get or set the Pressure in the plenum
















   ..
       !! processed by numpydoc !!

.. py:property:: t2
   :type: Optional[float]


   
   Get or set the Temperature in the plenum
















   ..
       !! processed by numpydoc !!

.. py:property:: delp2
   :type: Optional[float]


   
   Get or set the Rupture pressure in the plenum
















   ..
       !! processed by numpydoc !!

.. py:property:: truntime
   :type: Optional[float]


   
   Get or set the Total run time
















   ..
       !! processed by numpydoc !!

.. py:property:: comp3id
   :type: Optional[int]


   
   Get or set the Chemical composition identifier of composition to use in the airbag
















   ..
       !! processed by numpydoc !!

.. py:property:: vol3
   :type: Optional[float]


   
   Get or set the Volume of the airbag
















   ..
       !! processed by numpydoc !!

.. py:property:: p3
   :type: Optional[float]


   
   Get or set the Pressure in the airbag
















   ..
       !! processed by numpydoc !!

.. py:property:: t3
   :type: Optional[float]


   
   Get or set the Temperature in the airbag.
















   ..
       !! processed by numpydoc !!

.. py:property:: ptime
   :type: Optional[float]


   
   Get or set the Time interval for output of time history data to FILE.
















   ..
       !! processed by numpydoc !!

.. py:property:: file
   :type: Optional[str]


   
   Get or set the Name of the lsda file in which to write the results of the inflator simulation.Two load curves are written out to this file: mass flow rate and total temperature as a function of time.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CHEMISTRY'


.. py:attribute:: subkeyword
   :value: 'CONTROL_PYRPTECHNIC'






