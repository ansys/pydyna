





:class:`IcfdControlOutputVar`
=============================


.. py:class:: icfd_control_output_var.IcfdControlOutputVar(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_CONTROL_OUTPUT_VAR keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdControlOutputVar

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~vel`
            - Get or set the Velocity :
          * - :py:attr:`~avgvel`
            - Get or set the average velocity :
          * - :py:attr:`~vort`
            - Get or set the vorticity :
          * - :py:attr:`~pre`
            - Get or set the pressure:
          * - :py:attr:`~preavg`
            - Get or set the average pressure, levelset, Q criterion, CFL number :
          * - :py:attr:`~lset`
            - Get or set the levelset :
          * - :py:attr:`~oc`
            - Get or set the Q criterion:
          * - :py:attr:`~cfl`
            - Get or set the CFL number :
          * - :py:attr:`~temp`
            - Get or set the Temperature :
          * - :py:attr:`~tempavg`
            - Get or set the average temperature  :
          * - :py:attr:`~kp`
            - Get or set the RANS output variables, kinetic energy, diffusion, turbulent viscosity, turbulent intensity, Cmu variable ::
          * - :py:attr:`~ep`
            - Get or set the RANS output variables, kinetic energy, diffusion, turbulent viscosity, turbulent intensity, Cmu variable ::
          * - :py:attr:`~mut`
            - Get or set the RANS output variables, kinetic energy, diffusion, turbulent viscosity, turbulent intensity, Cmu variable ::
          * - :py:attr:`~int_`
            - Get or set the RANS output variables, kinetic energy, diffusion, turbulent viscosity, turbulent intensity, Cmu variable ::
          * - :py:attr:`~cmu`
            - Get or set the RANS output variables, kinetic energy, diffusion, turbulent viscosity, turbulent intensity, Cmu variable ::


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

    from icfd_control_output_var import IcfdControlOutputVar

Property detail
---------------

.. py:property:: vel
   :type: int


   
   Get or set the Velocity :
   EQ.0:   Is output.
   EQ.1:   Is not output.
















   ..
       !! processed by numpydoc !!

.. py:property:: avgvel
   :type: int


   
   Get or set the average velocity :
   EQ.0:   Is output.
   EQ.1:   Is not output.
















   ..
       !! processed by numpydoc !!

.. py:property:: vort
   :type: int


   
   Get or set the vorticity :
   EQ.0:   Is output.
   EQ.1:   Is not output.
















   ..
       !! processed by numpydoc !!

.. py:property:: pre
   :type: int


   
   Get or set the pressure:
   EQ.0:   Is output.
   EQ.1:   Is not output
















   ..
       !! processed by numpydoc !!

.. py:property:: preavg
   :type: int


   
   Get or set the average pressure, levelset, Q criterion, CFL number :
   EQ.0:   Is output.
   EQ.1:   Is not output
















   ..
       !! processed by numpydoc !!

.. py:property:: lset
   :type: int


   
   Get or set the levelset :
   EQ.0:   Is output.
   EQ.1:   Is not output
















   ..
       !! processed by numpydoc !!

.. py:property:: oc
   :type: int


   
   Get or set the Q criterion:
   EQ.0:   Is output.
   EQ.1:   Is not output
















   ..
       !! processed by numpydoc !!

.. py:property:: cfl
   :type: int


   
   Get or set the CFL number :
   EQ.0:   Is output.
   EQ.1:   Is not output
















   ..
       !! processed by numpydoc !!

.. py:property:: temp
   :type: int


   
   Get or set the Temperature :
   EQ.0:   Is output.
   EQ.1:   Is not output
















   ..
       !! processed by numpydoc !!

.. py:property:: tempavg
   :type: int


   
   Get or set the average temperature  :
   EQ.0:   Is output.
   EQ.1:   Is not output
















   ..
       !! processed by numpydoc !!

.. py:property:: kp
   :type: int


   
   Get or set the RANS output variables, kinetic energy, diffusion, turbulent viscosity, turbulent intensity, Cmu variable ::
   EQ.0:   Is output.
   EQ.1:   Is not output
















   ..
       !! processed by numpydoc !!

.. py:property:: ep
   :type: int


   
   Get or set the RANS output variables, kinetic energy, diffusion, turbulent viscosity, turbulent intensity, Cmu variable ::
   EQ.0:   Is output.
   EQ.1:   Is not output
















   ..
       !! processed by numpydoc !!

.. py:property:: mut
   :type: int


   
   Get or set the RANS output variables, kinetic energy, diffusion, turbulent viscosity, turbulent intensity, Cmu variable ::
   EQ.0:   Is output.
   EQ.1:   Is not output
















   ..
       !! processed by numpydoc !!

.. py:property:: int_
   :type: int


   
   Get or set the RANS output variables, kinetic energy, diffusion, turbulent viscosity, turbulent intensity, Cmu variable ::
   EQ.0:   Is output.
   EQ.1:   Is not output
















   ..
       !! processed by numpydoc !!

.. py:property:: cmu
   :type: int


   
   Get or set the RANS output variables, kinetic energy, diffusion, turbulent viscosity, turbulent intensity, Cmu variable ::
   EQ.0:   Is output.
   EQ.1:   Is not output
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'CONTROL_OUTPUT_VAR'






