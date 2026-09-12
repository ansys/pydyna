Node sets and bndout force histories
============================================================

Associate the node IDs in an existing input deck with the nodal-force records
in an ASCII ``bndout`` file. This example uses ``Deck.get_set_by_id()`` to
obtain explicit node lists and produces a CSV table and a three-panel plot.
It runs using Python, PyDyna, and Matplotlib, without a solver or DPF server.

Run the synthetic example
-------------------------

From a development checkout with PyDyna and Matplotlib installed::

    python examples/Bndout_Node_Sets/plot_bndout_node_sets.py --output bndout-results

The small files in ``data/`` are newly authored synthetic arithmetic fixtures
under the repository's MIT license. They are not results of a physical model.
At time 0.1, the expected component sums are:

.. list-table:: Expected synthetic results
   :header-rows: 1

   * - Node set
     - Source
     - Fx
     - Fy
     - Fz
   * - 1
     - velocity
     - 0
     - 10
     - 0
   * - 2
     - discrete
     - 0
     - -8
     - 0

All components are zero at time zero. Opposing nodal components cancel;
adding the magnitudes of the individual nodal forces would give a different
quantity. The fixture's unequal set totals do not represent a physical
equilibrium calculation.

Use existing results
--------------------

Pass both files and explicitly select a source for each input node-set ID::

    python examples/Bndout_Node_Sets/plot_bndout_node_sets.py --deck input.k --bndout bndout --set 1:velocity --set 2:discrete --output bndout-results

The source names correspond to these bndout headings:

* ``discrete``: ``discrete nodal point forces``.
* ``velocity``: ``velocity boundary condition forces/rigid body moments``;
  only its ``nd#`` nodal-force records are supported.

The ``setid`` printed in a bndout row is not interpreted as the input deck's
node-set ID. Membership is obtained from the deck and matched using ``nd#``.
Both selected sources must contain every selected node at every recorded time.
Missing nodes, incomplete sections, repeated records, invalid numbers, empty sets,
and unsupported set types raise errors rather than silently reducing coverage.
Repeated IDs within an input set count as one member.

The reader supports the demonstrated ASCII record layouts, including E and D
exponents. It ignores other section types and does not resolve generated sets
or process rigid-body moments. It preserves the output's coordinate basis and
the model's consistent unit system. Choose sections appropriate to the physics
of your model; the two curves alone are not an equilibrium check.

Outputs
-------

``node_set_forces.csv`` contains ``time, set_id, source, Fx, Fy, Fz``.
``node_set_forces.png`` compares the signed component sums. No unit conversion
is performed. With no ``--output`` argument, the example logs the location of a
new temporary output directory.

The sums use the printed nodal values, not the section's ``xtotal`` summary.
These can differ because ASCII nodal records and section totals have limited
output precision, particularly when large positive and negative values cancel.

Background
----------

This example addresses the result-association use case in
`PyDyna issue #1065 <https://github.com/ansys/pydyna/issues/1065>`_.
The original prototype and simulation files remain linked from that issue;
they are not bundled with this example. The existing set-query API was added
in `PR #1066 <https://github.com/ansys/pydyna/pull/1066>`_.
