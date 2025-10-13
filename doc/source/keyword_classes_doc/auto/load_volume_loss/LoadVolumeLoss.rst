





:class:`LoadVolumeLoss`
=======================


.. py:class:: load_volume_loss.LoadVolumeLoss(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_VOLUME_LOSS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadVolumeLoss

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~psid`
            - Get or set the Part Set ID.
          * - :py:attr:`~coord`
            - Get or set the Coordinate System ID (default - global coordinate system).
          * - :py:attr:`~lcur`
            - Get or set the Curve ID containing volume fraction lost as a function of time.
          * - :py:attr:`~fx`
            - Get or set the Fraction of strain occurring in x-direction.
          * - :py:attr:`~fy`
            - Get or set the Fraction of strain occurring in y-direction.
          * - :py:attr:`~fz`
            - Get or set the Fraction of strain occurring in z-direction.
          * - :py:attr:`~pmin`
            - Get or set the (Leave blank).
          * - :py:attr:`~factor`
            - Get or set the Feedback factor.


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

    from load_volume_loss import LoadVolumeLoss

Property detail
---------------

.. py:property:: psid
   :type: Optional[int]


   
   Get or set the Part Set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: coord
   :type: Optional[int]


   
   Get or set the Coordinate System ID (default - global coordinate system).
















   ..
       !! processed by numpydoc !!

.. py:property:: lcur
   :type: int


   
   Get or set the Curve ID containing volume fraction lost as a function of time.
















   ..
       !! processed by numpydoc !!

.. py:property:: fx
   :type: float


   
   Get or set the Fraction of strain occurring in x-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: fy
   :type: float


   
   Get or set the Fraction of strain occurring in y-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: fz
   :type: float


   
   Get or set the Fraction of strain occurring in z-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: pmin
   :type: float


   
   Get or set the (Leave blank).
















   ..
       !! processed by numpydoc !!

.. py:property:: factor
   :type: float


   
   Get or set the Feedback factor.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'VOLUME_LOSS'






