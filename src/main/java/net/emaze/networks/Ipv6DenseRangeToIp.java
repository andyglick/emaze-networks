package net.emaze.networks;

import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.Delegate;
import net.emaze.dysfunctional.ranges.DenseRange;
import net.emaze.dysfunctional.tuples.Pair;

public class Ipv6DenseRangeToIp implements Delegate<Pair<Ipv6, Ipv6>, DenseRange<Ipv6>> {

    @Override
    public Pair<Ipv6, Ipv6> perform(DenseRange<Ipv6> range) {
        dbc.precondition(range.iterator().hasNext(), "Range cannot be empty");
        final Ipv6 lastIp = range.end().fmap(new Ipv6PreviousIp()).orElse(Ipv6.getLastIp()); // Workaround for range.end()
        return Pair.of(range.begin(), lastIp);
    }

}
