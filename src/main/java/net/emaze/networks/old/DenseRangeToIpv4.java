package net.emaze.networks.old;

import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.Delegate;
import net.emaze.dysfunctional.ranges.DenseRange;
import net.emaze.dysfunctional.tuples.Pair;

public class DenseRangeToIpv4 implements Delegate<Pair<Ipv4, Ipv4>, DenseRange<Ipv4>> {

    @Override
    public Pair<Ipv4, Ipv4> perform(DenseRange<Ipv4> range) {
        dbc.precondition(range.iterator().hasNext(), "Range cannot be empty");
        final Ipv4 lastIp = range.end().fmap(new PreviousIpv4()).orElse(Ipv4.LAST_IP); // Workaround for range.end()
        return Pair.of(range.begin(), lastIp);
    }
}
