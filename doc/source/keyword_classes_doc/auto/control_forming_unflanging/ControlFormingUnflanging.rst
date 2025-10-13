





:class:`ControlFormingUnflanging`
=================================


.. py:class:: control_forming_unflanging.ControlFormingUnflanging(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_FORMING_UNFLANGING keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlFormingUnflanging

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~noption`
            - Get or set the Flag to turn on an unfolding simulation:
          * - :py:attr:`~dvid`
            - Get or set the This variable is currently not being used.
          * - :py:attr:`~nunbend`
            - Get or set the Estimated number of unbending, ranging from 10 to 100.
          * - :py:attr:`~stfbend`
            - Get or set the Unflanging stiffness, ranging from 0.1 to 10.0.
          * - :py:attr:`~stfcnt`
            - Get or set the Normal stiffness, ranging from 0.1 to 10.0.
          * - :py:attr:`~iflimit`
            - Get or set the Iteration limit for the first phase of unfolding, typically ranging from 11 to 400.
          * - :py:attr:`~dist`
            - Get or set the Distance tolerance for auto-SPC along flange root.
          * - :py:attr:`~ilinear`
            - Get or set the Unfolding algorithm selection flag:
          * - :py:attr:`~nb1`
            - Get or set the The start node ID on a flange root boundary For closed-loop flange root boundary, only this parameter needs to be defined; for open-loop flange root boundary, define this parameter as well as NB2 and NB3.
          * - :py:attr:`~nb2`
            - Get or set the The ID of a node in the middle of the flange root boundary Define this parameter for open-loop flange root boundary.
          * - :py:attr:`~nb3`
            - Get or set the The end node ID on a flange root boundary. Define this parameter for open-loop flange root boundary.
          * - :py:attr:`~charlen`
            - Get or set the Maximum flange height to limit the search region for the boundary nodes along the flange root.
          * - :py:attr:`~ndouter`
            - Get or set the A node ID on the outer flange boundary.This node helps search of nodes along the flange root, especially when holes are present in the flange area.


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

    from control_forming_unflanging import ControlFormingUnflanging

Property detail
---------------

.. py:property:: noption
   :type: Optional[int]


   
   Get or set the Flag to turn on an unfolding simulation:
   EQ.1: Activate the unfolding simulation program.
















   ..
       !! processed by numpydoc !!

.. py:property:: dvid
   :type: Optional[int]


   
   Get or set the This variable is currently not being used.
















   ..
       !! processed by numpydoc !!

.. py:property:: nunbend
   :type: Optional[int]


   
   Get or set the Estimated number of unbending, ranging from 10 to 100.
















   ..
       !! processed by numpydoc !!

.. py:property:: stfbend
   :type: Optional[float]


   
   Get or set the Unflanging stiffness, ranging from 0.1 to 10.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: stfcnt
   :type: Optional[float]


   
   Get or set the Normal stiffness, ranging from 0.1 to 10.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: iflimit
   :type: Optional[int]


   
   Get or set the Iteration limit for the first phase of unfolding, typically ranging from 11 to 400.
















   ..
       !! processed by numpydoc !!

.. py:property:: dist
   :type: Optional[float]


   
   Get or set the Distance tolerance for auto-SPC along flange root.
















   ..
       !! processed by numpydoc !!

.. py:property:: ilinear
   :type: int


   
   Get or set the Unfolding algorithm selection flag:
   EQ.0: nonlinear unfolding.
   EQ.1: linear unfolding.
   EQ.2: a new method of initial unfolding followed by nonlinear iterations (recommended).
















   ..
       !! processed by numpydoc !!

.. py:property:: nb1
   :type: Optional[int]


   
   Get or set the The start node ID on a flange root boundary For closed-loop flange root boundary, only this parameter needs to be defined; for open-loop flange root boundary, define this parameter as well as NB2 and NB3.
















   ..
       !! processed by numpydoc !!

.. py:property:: nb2
   :type: Optional[int]


   
   Get or set the The ID of a node in the middle of the flange root boundary Define this parameter for open-loop flange root boundary.
















   ..
       !! processed by numpydoc !!

.. py:property:: nb3
   :type: Optional[int]


   
   Get or set the The end node ID on a flange root boundary. Define this parameter for open-loop flange root boundary.
















   ..
       !! processed by numpydoc !!

.. py:property:: charlen
   :type: float


   
   Get or set the Maximum flange height to limit the search region for the boundary nodes along the flange root.
















   ..
       !! processed by numpydoc !!

.. py:property:: ndouter
   :type: Optional[int]


   
   Get or set the A node ID on the outer flange boundary.This node helps search of nodes along the flange root, especially when holes are present in the flange area.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'FORMING_UNFLANGING'






