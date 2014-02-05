package net.emaze.networks;

import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.Delegate;
import net.emaze.dysfunctional.ranges.DenseRange;
import net.emaze.dysfunctional.tuples.Pair;

public class Ipv4DenseRangeToIp implements Delegate<Pair<Ipv4, Ipv4>, DenseRange<Ipv4>> {

    @Override
    public Pair<Ipv4, Ipv4> perform(DenseRange<Ipv4> range) {
        dbc.precondition(range.iterator().hasNext(), "Range cannot be empty");
        final Ipv4 lastIp = range.end().fmap(new Ipv4PreviousIp()).orElse(Ipv4.getLastIp()); // Workaround for range.end()
        return Pair.of(range.begin(), lastIp);
    }

}
