





:class:`ControlReferenceConfiguration`
======================================


.. py:class:: control_reference_configuration.ControlReferenceConfiguration(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_REFERENCE_CONFIGURATION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlReferenceConfiguration

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~maxiter`
            - Get or set the The maximum number of iterations in the iterative method
          * - :py:attr:`~target`
            - Get or set the File containing all nodes of the target geometry
          * - :py:attr:`~method`
            - Get or set the Iterative method
          * - :py:attr:`~step`
            - Get or set the The step size used in the iterations to update the current approximate reference geometry for Sellier’s method. It must be > 0.
          * - :py:attr:`~tol`
            - Get or set the The tolerance used to determining convergence of the iterative method.This is given in the unit of length


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

    from control_reference_configuration import ControlReferenceConfiguration

Property detail
---------------

.. py:property:: maxiter
   :type: Optional[int]


   
   Get or set the The maximum number of iterations in the iterative method
















   ..
       !! processed by numpydoc !!

.. py:property:: target
   :type: Optional[str]


   
   Get or set the File containing all nodes of the target geometry
















   ..
       !! processed by numpydoc !!

.. py:property:: method
   :type: Optional[int]


   
   Get or set the Iterative method
   EQ.0 : Sellier’s method
   EQ.1 : Rausch’s method
   EQ.3:Rausch’s method with an additional line search.
















   ..
       !! processed by numpydoc !!

.. py:property:: step
   :type: float


   
   Get or set the The step size used in the iterations to update the current approximate reference geometry for Sellier’s method. It must be > 0.
















   ..
       !! processed by numpydoc !!

.. py:property:: tol
   :type: float


   
   Get or set the The tolerance used to determining convergence of the iterative method.This is given in the unit of length
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'REFERENCE_CONFIGURATION'






