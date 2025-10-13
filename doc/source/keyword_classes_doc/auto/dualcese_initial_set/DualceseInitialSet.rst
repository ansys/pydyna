





:class:`DualceseInitialSet`
===========================


.. py:class:: dualcese_initial_set.DualceseInitialSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DUALCESE_INITIAL_SET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DualceseInitialSet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~esid`
            - Get or set the Element set ID (see *DUALCESE_ELEMENTSET)
          * - :py:attr:`~ifunc`
            - Get or set the Option to define initial conditions using *DEFINE_FUNCTION cards:
          * - :py:attr:`~u`
            - Get or set the x-, y-, z-velocity components respectively
          * - :py:attr:`~v`
            - Get or set the x-, y-, z-velocity components respectively
          * - :py:attr:`~w`
            - Get or set the x-, y-, z-velocity components respectively
          * - :py:attr:`~rho`
            - Get or set the density
          * - :py:attr:`~p`
            - Get or set the pressure
          * - :py:attr:`~t`
            - Get or set the temperature


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

    from dualcese_initial_set import DualceseInitialSet

Property detail
---------------

.. py:property:: esid
   :type: Optional[int]


   
   Get or set the Element set ID (see *DUALCESE_ELEMENTSET)
















   ..
       !! processed by numpydoc !!

.. py:property:: ifunc
   :type: Optional[int]


   
   Get or set the Option to define initial conditions using *DEFINE_FUNCTION cards:
   EQ.0:   Not in use.
   EQ.1:All values for initial velocity, pressure, density, and temperature now refer to *DEFINE_FUNCTION IDs. In these functions, the following parameters are allowed: f(x,y,z), meaning that each variable’s initial profile is a function of position
















   ..
       !! processed by numpydoc !!

.. py:property:: u
   :type: float


   
   Get or set the x-, y-, z-velocity components respectively
















   ..
       !! processed by numpydoc !!

.. py:property:: v
   :type: float


   
   Get or set the x-, y-, z-velocity components respectively
















   ..
       !! processed by numpydoc !!

.. py:property:: w
   :type: float


   
   Get or set the x-, y-, z-velocity components respectively
















   ..
       !! processed by numpydoc !!

.. py:property:: rho
   :type: float


   
   Get or set the density
















   ..
       !! processed by numpydoc !!

.. py:property:: p
   :type: float


   
   Get or set the pressure
















   ..
       !! processed by numpydoc !!

.. py:property:: t
   :type: float


   
   Get or set the temperature
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DUALCESE'


.. py:attribute:: subkeyword
   :value: 'INITIAL_SET'






