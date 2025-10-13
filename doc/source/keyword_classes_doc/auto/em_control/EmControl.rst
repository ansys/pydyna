





:class:`EmControl`
==================


.. py:class:: em_control.EmControl(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_CONTROL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmControl

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~emsol`
            - Get or set the Electromagnetism solver selector:
          * - :py:attr:`~numls`
            - Get or set the Number of local EM steps in A whole period for EMSOL=2 If a negative value is entered, it will give the number of local EM steps as a function of the macro time
          * - :py:attr:`~macrodt`
            - Get or set the Macro timestep when EMSOL = 2.
          * - :py:attr:`~dimtype`
            - Get or set the EM dimension type:
          * - :py:attr:`~nperio`
            - Get or set the Number of periods for which the last is used to calculate the average Joule heat rate when EMSOL=2. NPERIO=2 means that two periods of NUMLS steps will be calculated. Only the last period of NPERIO is used for the average Joule heat calculation
          * - :py:attr:`~ncylfem`
            - Get or set the Number of electromagnetism cycles between the recomputation of EM-FEM matrices,If a negative value is entered, then the absolute value refers to a load curve giving the number of electromagnetism cysles as function of time.
          * - :py:attr:`~ncylbem`
            - Get or set the Number of electromagnetism cycles between the recomputation of EM-BEM matrices,If a negative value is entered, then the absolute value refers to a load curve giving the number of electomagnetism cycles as function of time.


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

    from em_control import EmControl

Property detail
---------------

.. py:property:: emsol
   :type: int


   
   Get or set the Electromagnetism solver selector:
   EQ.-1:Turns the EM solver off after reading the EM keywords.
   EQ.1:eddy current solver.
   EQ.2:induced heating solver.
   EQ.3:resistive heating solver.
   EQ.11:Electrophysiology monodomain.
   EQ.12:Electrophysiology bidomain.
   EQ.13:Electrophysiology monodmain coupled with bidomain.
















   ..
       !! processed by numpydoc !!

.. py:property:: numls
   :type: int


   
   Get or set the Number of local EM steps in A whole period for EMSOL=2 If a negative value is entered, it will give the number of local EM steps as a function of the macro time
















   ..
       !! processed by numpydoc !!

.. py:property:: macrodt
   :type: Optional[float]


   
   Get or set the Macro timestep when EMSOL = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: dimtype
   :type: int


   
   Get or set the EM dimension type:
   EQ.0:3D solve.
   EQ.1:2D planar with 4-zero thickness shell elements.
   EQ.3:2D axisymmetric (Y axis only) with zero thickness elements.
















   ..
       !! processed by numpydoc !!

.. py:property:: nperio
   :type: int


   
   Get or set the Number of periods for which the last is used to calculate the average Joule heat rate when EMSOL=2. NPERIO=2 means that two periods of NUMLS steps will be calculated. Only the last period of NPERIO is used for the average Joule heat calculation
















   ..
       !! processed by numpydoc !!

.. py:property:: ncylfem
   :type: int


   
   Get or set the Number of electromagnetism cycles between the recomputation of EM-FEM matrices,If a negative value is entered, then the absolute value refers to a load curve giving the number of electromagnetism cysles as function of time.
















   ..
       !! processed by numpydoc !!

.. py:property:: ncylbem
   :type: int


   
   Get or set the Number of electromagnetism cycles between the recomputation of EM-BEM matrices,If a negative value is entered, then the absolute value refers to a load curve giving the number of electomagnetism cycles as function of time.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'CONTROL'






