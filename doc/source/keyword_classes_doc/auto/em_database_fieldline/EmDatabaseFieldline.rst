





:class:`EmDatabaseFieldline`
============================


.. py:class:: em_database_fieldline.EmDatabaseFieldline(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_DATABASE_FIELDLINE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmDatabaseFieldline

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~flid`
            - Get or set the Field line set ID.
          * - :py:attr:`~psid`
            - Get or set the Point Set ID associated to the field line set (See *EM_POINT_SET).
          * - :py:attr:`~dtout`
            - Get or set the Time interval to print the output. If DTOUT is equal to 0.0, then the EM timestep will be used.
          * - :py:attr:`~npoint`
            - Get or set the Number of points per field line. The points are regularly spaced.
          * - :py:attr:`~integ`
            - Get or set the Type of numerical integrator used to compute the field lines:
          * - :py:attr:`~h`
            - Get or set the The value of the step size is equal to the maximum value between /n 1/|B| (evaluated at the starting point of the field line) and the value
          * - :py:attr:`~hmin`
            - Get or set the Minimal stepsize value. Only used in the case of an integrator with adaptive stepsize.
          * - :py:attr:`~hmax`
            - Get or set the Maximal stepsize value. Only used in the case of an integrator with adaptive stepsize.
          * - :py:attr:`~tolabs`
            - Get or set the Absolute tolerance of the integrator. Only used in the case of an integrator with adaptive stepsize.
          * - :py:attr:`~tolrel`
            - Get or set the Relative tolerance of the integrator. Only used in the case of an integrator with adaptive stepsize.
          * - :py:attr:`~btype`
            - Get or set the Method to compute the magnetic field:


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

    from em_database_fieldline import EmDatabaseFieldline

Property detail
---------------

.. py:property:: flid
   :type: Optional[int]


   
   Get or set the Field line set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: psid
   :type: Optional[int]


   
   Get or set the Point Set ID associated to the field line set (See *EM_POINT_SET).
   The coordinates given by the different points will be the starting points of the field lines.
















   ..
       !! processed by numpydoc !!

.. py:property:: dtout
   :type: Optional[float]


   
   Get or set the Time interval to print the output. If DTOUT is equal to 0.0, then the EM timestep will be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: npoint
   :type: int


   
   Get or set the Number of points per field line. The points are regularly spaced.
















   ..
       !! processed by numpydoc !!

.. py:property:: integ
   :type: int


   
   Get or set the Type of numerical integrator used to compute the field lines:
   EQ.1: RK4 (Runge Kutta 4).
   EQ.2: DOP853 (Dormand Prince 8(5,3)).
















   ..
       !! processed by numpydoc !!

.. py:property:: h
   :type: float


   
   Get or set the The value of the step size is equal to the maximum value between /n 1/|B| (evaluated at the starting point of the field line) and the value
   H given by the user. In case of an integrator with adaptive stepsize,
   it is the initial value of the stepsize.
















   ..
       !! processed by numpydoc !!

.. py:property:: hmin
   :type: float


   
   Get or set the Minimal stepsize value. Only used in the case of an integrator with adaptive stepsize.
















   ..
       !! processed by numpydoc !!

.. py:property:: hmax
   :type: float


   
   Get or set the Maximal stepsize value. Only used in the case of an integrator with adaptive stepsize.
















   ..
       !! processed by numpydoc !!

.. py:property:: tolabs
   :type: float


   
   Get or set the Absolute tolerance of the integrator. Only used in the case of an integrator with adaptive stepsize.
















   ..
       !! processed by numpydoc !!

.. py:property:: tolrel
   :type: float


   
   Get or set the Relative tolerance of the integrator. Only used in the case of an integrator with adaptive stepsize.
















   ..
       !! processed by numpydoc !!

.. py:property:: btype
   :type: int


   
   Get or set the Method to compute the magnetic field:
   EQ.1: Direct method (every contribution is computed by the Biot Savart Law and summed up : very slow).
   EQ.2: Multipole method (approximation of the direct method using the multipole expansion).
   EQ.3: Multicenter method (approximation of the direct method using a weighted subset of points only in order to compute the magnetic field).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'DATABASE_FIELDLINE'






