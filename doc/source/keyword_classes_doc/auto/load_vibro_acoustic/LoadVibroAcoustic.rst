





:class:`LoadVibroAcoustic`
==========================


.. py:class:: load_vibro_acoustic.LoadVibroAcoustic(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_VIBRO_ACOUSTIC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadVibroAcoustic

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nmode`
            - Get or set the Number of normal vibration modes employed for coupling with excitation pressure field
          * - :py:attr:`~texpos`
            - Get or set the Esposure time
          * - :py:attr:`~tscale`
            - Get or set the Time scale
          * - :py:attr:`~temper`
            - Get or set the Temperature
          * - :py:attr:`~dampro`
            - Get or set the Damping ratio
          * - :py:attr:`~damptype`
            - Get or set the Type of damping (=1, broadband; =2, modal damping)
          * - :py:attr:`~spltype`
            - Get or set the Type of SPL input (=1, prs; =2, spl)
          * - :py:attr:`~lddamp`
            - Get or set the Load curve for damping ratio (if non-constant)
          * - :py:attr:`~ldspl`
            - Get or set the Load curve for PSD or SPL value vs. frequency
          * - :py:attr:`~ldvel`
            - Get or set the Load curve for phase velocity
          * - :py:attr:`~ldflw`
            - Get or set the Load curve for exponential decay for TBL in flow-wise direction
          * - :py:attr:`~ldspn`
            - Get or set the Load curve for exponential decay for TBL in span-wise direction


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

    from load_vibro_acoustic import LoadVibroAcoustic

Property detail
---------------

.. py:property:: nmode
   :type: Optional[float]


   
   Get or set the Number of normal vibration modes employed for coupling with excitation pressure field
















   ..
       !! processed by numpydoc !!

.. py:property:: texpos
   :type: float


   
   Get or set the Esposure time
















   ..
       !! processed by numpydoc !!

.. py:property:: tscale
   :type: float


   
   Get or set the Time scale
















   ..
       !! processed by numpydoc !!

.. py:property:: temper
   :type: Optional[float]


   
   Get or set the Temperature
















   ..
       !! processed by numpydoc !!

.. py:property:: dampro
   :type: Optional[float]


   
   Get or set the Damping ratio
















   ..
       !! processed by numpydoc !!

.. py:property:: damptype
   :type: Optional[float]


   
   Get or set the Type of damping (=1, broadband; =2, modal damping)
















   ..
       !! processed by numpydoc !!

.. py:property:: spltype
   :type: Optional[float]


   
   Get or set the Type of SPL input (=1, prs; =2, spl)
















   ..
       !! processed by numpydoc !!

.. py:property:: lddamp
   :type: Optional[int]


   
   Get or set the Load curve for damping ratio (if non-constant)
















   ..
       !! processed by numpydoc !!

.. py:property:: ldspl
   :type: Optional[int]


   
   Get or set the Load curve for PSD or SPL value vs. frequency
















   ..
       !! processed by numpydoc !!

.. py:property:: ldvel
   :type: Optional[int]


   
   Get or set the Load curve for phase velocity
















   ..
       !! processed by numpydoc !!

.. py:property:: ldflw
   :type: Optional[int]


   
   Get or set the Load curve for exponential decay for TBL in flow-wise direction
















   ..
       !! processed by numpydoc !!

.. py:property:: ldspn
   :type: Optional[int]


   
   Get or set the Load curve for exponential decay for TBL in span-wise direction
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'VIBRO_ACOUSTIC'






