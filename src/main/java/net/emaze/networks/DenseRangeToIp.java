package net.emaze.networks;

import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.Delegate;
import net.emaze.dysfunctional.ranges.DenseRange;
import net.emaze.dysfunctional.tuples.Pair;

public class DenseRangeToIp implements Delegate<Pair<Ip, Ip>, DenseRange<Ip>> {

    @Override
    public Pair<Ip, Ip> perform(DenseRange<Ip> range) {
        dbc.precondition(range.iterator().hasNext(), "Range cannot be empty");
        final IpPolicy policy = range.begin().version();
        final Ip lastIp = range.end().fmap(new PreviousIp()).orElse(policy.getLastIp()); // Workaround for range.end()
        return Pair.of(range.begin(), lastIp);
    }
}
