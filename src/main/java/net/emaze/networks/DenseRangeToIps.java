package net.emaze.networks;

import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.Delegate;
import net.emaze.dysfunctional.ranges.DenseRange;
import net.emaze.dysfunctional.tuples.Pair;

public class DenseRangeToIps implements Delegate<Pair<Ip, Ip>, DenseRange<Ip>> {

    @Override
    public Pair<Ip, Ip> perform(DenseRange<Ip> range) {
        dbc.precondition(range.iterator().hasNext(), "Range cannot be empty");
        final Ip lastIp = range.end().fmap(new PreviousIpv4()).orElse(Ip.LAST_IP); // Workaround for range.end()
        return Pair.of(range.begin(), lastIp);
    }
}
