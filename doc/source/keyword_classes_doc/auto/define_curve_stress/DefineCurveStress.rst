





:class:`DefineCurveStress`
==========================


.. py:class:: define_curve_stress.DefineCurveStress(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_CURVE_STRESS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineCurveStress

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~lcid`
            - Get or set the Load curve ID for the stress-strain curve to be generated.
          * - :py:attr:`~itype`
            - Get or set the Type of hardening law:
          * - :py:attr:`~p1`
            - Get or set the ITYPE.EQ.1:      P1=K,P2=n,P3=e_0.
          * - :py:attr:`~p2`
            - Get or set the ITYPE.EQ.1:      P1=K,P2=n,P3=e_0.
          * - :py:attr:`~p3`
            - Get or set the ITYPE.EQ.1:      P1=K,P2=n,P3=e_0.
          * - :py:attr:`~p4`
            - Get or set the ITYPE.EQ.1:      P1=K,P2=n,P3=e_0.
          * - :py:attr:`~p5`
            - Get or set the ITYPE.EQ.1:      P1=K,P2=n,P3=e_0.
          * - :py:attr:`~p6`
            - Get or set the ITYPE.EQ.1:      P1=K,P2=n,P3=e_0.
          * - :py:attr:`~title`
            - Get or set the Additional title line


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~keyword`
            - 
          * - :py:attr:`~subkeyword`
            - 
          * - :py:attr:`~option_specs`
            - Get the card format type.






Import detail
-------------

.. code-block:: python

    from define_curve_stress import DefineCurveStress

Property detail
---------------

.. py:property:: lcid
   :type: int


   
   Get or set the Load curve ID for the stress-strain curve to be generated.
















   ..
       !! processed by numpydoc !!

.. py:property:: itype
   :type: int


   
   Get or set the Type of hardening law:
   ITYPE.EQ.1: Swift power law
   ITYPE.EQ.2: Voce law, please take a look at the manual
   ITYPE.EQ.3: Voce law in different forms, please take a look at the manual.
   ITYPE.EQ.4: Voce law, please take a look at the manual.
   ITYPE.EQ.5: Stoughton-Yoon hardening law.
   ITYPE.EQ.11: A weighted combination of ITYPE = 1 and any of the other ITYPEs.
   Check the manual for detail.
















   ..
       !! processed by numpydoc !!

.. py:property:: p1
   :type: float


   
   Get or set the ITYPE.EQ.1:      P1=K,P2=n,P3=e_0.
   ITYPE.EQ.2:     P1=Sigma_0,P2=R_sat,P3=zeta, P4: hardening curve contributing weighting factor.
   ITYPE.EQ.3:     P1=A,P2=B,P3=C,
   P4: hardening curve contributing weighting factor.
   ITYPE.EQ.4:     P1=A,P2=B,P3=C,P4=H.
   P5: hardening curve contributing weighting factor.
   ITYPE.EQ.11:    P1=K,P2=n,P3=e_0,P4: hardening curve contributing weighting factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: p2
   :type: float


   
   Get or set the ITYPE.EQ.1:      P1=K,P2=n,P3=e_0.
   ITYPE.EQ.2:     P1=Sigma_0,P2=R_sat,P3=zeta, P4: hardening curve contributing weighting factor.
   ITYPE.EQ.3:     P1=A,P2=B,P3=C,
   P4: hardening curve contributing weighting factor.
   ITYPE.EQ.4:     P1=A,P2=B,P3=C,P4=H.
   P5: hardening curve contributing weighting factor.
   ITYPE.EQ.11:    P1=K,P2=n,P3=e_0,P4: hardening curve contributing weighting factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: p3
   :type: float


   
   Get or set the ITYPE.EQ.1:      P1=K,P2=n,P3=e_0.
   ITYPE.EQ.2:     P1=Sigma_0,P2=R_sat,P3=zeta, P4: hardening curve contributing weighting factor.
   ITYPE.EQ.3:     P1=A,P2=B,P3=C,
   P4: hardening curve contributing weighting factor.
   ITYPE.EQ.4:     P1=A,P2=B,P3=C,P4=H.
   P5: hardening curve contributing weighting factor.
   ITYPE.EQ.11:    P1=K,P2=n,P3=e_0,P4: hardening curve contributing weighting factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: p4
   :type: float


   
   Get or set the ITYPE.EQ.1:      P1=K,P2=n,P3=e_0.
   ITYPE.EQ.2:     P1=Sigma_0,P2=R_sat,P3=zeta, P4: hardening curve contributing weighting factor.
   ITYPE.EQ.3:     P1=A,P2=B,P3=C,
   P4: hardening curve contributing weighting factor.
   ITYPE.EQ.4:     P1=A,P2=B,P3=C,P4=H.
   P5: hardening curve contributing weighting factor.
   ITYPE.EQ.11:    P1=K,P2=n,P3=e_0,P4: hardening curve contributing weighting factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: p5
   :type: float


   
   Get or set the ITYPE.EQ.1:      P1=K,P2=n,P3=e_0.
   ITYPE.EQ.2:     P1=Sigma_0,P2=R_sat,P3=zeta, P4: hardening curve contributing weighting factor.
   ITYPE.EQ.3:     P1=A,P2=B,P3=C,
   P4: hardening curve contributing weighting factor.
   ITYPE.EQ.4:     P1=A,P2=B,P3=C,P4=H.
   P5: hardening curve contributing weighting factor.
   ITYPE.EQ.11:    P1=K,P2=n,P3=e_0,P4: hardening curve contributing weighting factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: p6
   :type: float


   
   Get or set the ITYPE.EQ.1:      P1=K,P2=n,P3=e_0.
   ITYPE.EQ.2:     P1=Sigma_0,P2=R_sat,P3=zeta, P4: hardening curve contributing weighting factor.
   ITYPE.EQ.3:     P1=A,P2=B,P3=C,
   P4: hardening curve contributing weighting factor.
   ITYPE.EQ.4:     P1=A,P2=B,P3=C,P4=H.
   P5: hardening curve contributing weighting factor.
   ITYPE.EQ.11:    P1=K,P2=n,P3=e_0,P4: hardening curve contributing weighting factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Additional title line
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DEFINE'


.. py:attribute:: subkeyword
   :value: 'CURVE_STRESS'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





