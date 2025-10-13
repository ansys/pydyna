





:class:`ConstrainedSpr2`
========================


.. py:class:: constrained_spr2.ConstrainedSpr2(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_SPR2 keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedSpr2

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~upid`
            - Get or set the Upper sheet part ID
          * - :py:attr:`~lpid`
            - Get or set the Lower sheet part ID
          * - :py:attr:`~nsid`
            - Get or set the Node set ID of rivet location nodes.
          * - :py:attr:`~thick`
            - Get or set the Total thickness of upper and lower sheets.
          * - :py:attr:`~d`
            - Get or set the Rivet diameter.
          * - :py:attr:`~fn`
            - Get or set the Rivet strength in tension (pull-out):
          * - :py:attr:`~ft`
            - Get or set the Rivet strength in pure shear.
          * - :py:attr:`~dn`
            - Get or set the Failure displacement in normal direction.
          * - :py:attr:`~dt`
            - Get or set the Failure displacement in tangential direction.
          * - :py:attr:`~xin`
            - Get or set the Fraction of failure displacement at maximum normal force.
          * - :py:attr:`~xit`
            - Get or set the Fraction of failure displacement at maximum tangential force.
          * - :py:attr:`~alpha1`
            - Get or set the Dimensionless parameter scaling the effective displacement.
          * - :py:attr:`~alpha2`
            - Get or set the Dimensionless parameter scaling the effective displacement
          * - :py:attr:`~alpha3`
            - Get or set the Dimensionless parameter scaling the effective displacement.The sign of ALPHA3 can be used to choose the normal update procedure :
          * - :py:attr:`~dens`
            - Get or set the Rivet density (necessary for time step calculation).
          * - :py:attr:`~intp`
            - Get or set the Flag for interpolation.
          * - :py:attr:`~expn`
            - Get or set the Exponent value for load function in normal direction.
          * - :py:attr:`~expt`
            - Get or set the Exponent value for load function in tangential direction.
          * - :py:attr:`~pidvb`
            - Get or set the Part ID for visualization beams representing SPR2 in post-processing.
          * - :py:attr:`~xpid1`
            - Get or set the Extra part ID 1 for multi-sheet connection.
          * - :py:attr:`~xpid2`
            - Get or set the Extra part ID 2 for multi-sheet connection.
          * - :py:attr:`~xpid3`
            - Get or set the Extra part ID 3 for multi-sheet connection.
          * - :py:attr:`~xpid4`
            - Get or set the Extra part ID 4 for multi-sheet connection.


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

    from constrained_spr2 import ConstrainedSpr2

Property detail
---------------

.. py:property:: upid
   :type: Optional[int]


   
   Get or set the Upper sheet part ID
















   ..
       !! processed by numpydoc !!

.. py:property:: lpid
   :type: Optional[int]


   
   Get or set the Lower sheet part ID
















   ..
       !! processed by numpydoc !!

.. py:property:: nsid
   :type: Optional[int]


   
   Get or set the Node set ID of rivet location nodes.
















   ..
       !! processed by numpydoc !!

.. py:property:: thick
   :type: Optional[float]


   
   Get or set the Total thickness of upper and lower sheets.
















   ..
       !! processed by numpydoc !!

.. py:property:: d
   :type: Optional[float]


   
   Get or set the Rivet diameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: fn
   :type: Optional[float]


   
   Get or set the Rivet strength in tension (pull-out):
   GT.0: Constant value
   LT.0 : Material data from instantiation of * MAT_CONSTRAINED_SPR2(*MAT_265) with MID of absolutevalue | FN |
















   ..
       !! processed by numpydoc !!

.. py:property:: ft
   :type: Optional[float]


   
   Get or set the Rivet strength in pure shear.
















   ..
       !! processed by numpydoc !!

.. py:property:: dn
   :type: Optional[float]


   
   Get or set the Failure displacement in normal direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: dt
   :type: Optional[float]


   
   Get or set the Failure displacement in tangential direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: xin
   :type: Optional[float]


   
   Get or set the Fraction of failure displacement at maximum normal force.
















   ..
       !! processed by numpydoc !!

.. py:property:: xit
   :type: Optional[float]


   
   Get or set the Fraction of failure displacement at maximum tangential force.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha1
   :type: Optional[float]


   
   Get or set the Dimensionless parameter scaling the effective displacement.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha2
   :type: Optional[float]


   
   Get or set the Dimensionless parameter scaling the effective displacement
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha3
   :type: Optional[float]


   
   Get or set the Dimensionless parameter scaling the effective displacement.The sign of ALPHA3 can be used to choose the normal update procedure :
   GT.0 : Incremental update(default)
   LT.0 : Total update(recommended)
















   ..
       !! processed by numpydoc !!

.. py:property:: dens
   :type: Optional[float]


   
   Get or set the Rivet density (necessary for time step calculation).
















   ..
       !! processed by numpydoc !!

.. py:property:: intp
   :type: int


   
   Get or set the Flag for interpolation.
   EQ.0: Linear (default),
   EQ.1: Uniform,
   EQ.2 : Inverse distance weighting.
















   ..
       !! processed by numpydoc !!

.. py:property:: expn
   :type: float


   
   Get or set the Exponent value for load function in normal direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: expt
   :type: float


   
   Get or set the Exponent value for load function in tangential direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: pidvb
   :type: Optional[int]


   
   Get or set the Part ID for visualization beams representing SPR2 in post-processing.
   EQ.0:   Part id automatically set (default),
   GT.0:   PIDVB defines part id .
















   ..
       !! processed by numpydoc !!

.. py:property:: xpid1
   :type: Optional[int]


   
   Get or set the Extra part ID 1 for multi-sheet connection.
















   ..
       !! processed by numpydoc !!

.. py:property:: xpid2
   :type: Optional[int]


   
   Get or set the Extra part ID 2 for multi-sheet connection.
















   ..
       !! processed by numpydoc !!

.. py:property:: xpid3
   :type: Optional[int]


   
   Get or set the Extra part ID 3 for multi-sheet connection.
















   ..
       !! processed by numpydoc !!

.. py:property:: xpid4
   :type: Optional[int]


   
   Get or set the Extra part ID 4 for multi-sheet connection.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'SPR2'






