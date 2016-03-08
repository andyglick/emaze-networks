package net.emaze.networks.ipv6;

import java.util.function.Function;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.ranges.DenseRange;
import net.emaze.dysfunctional.tuples.Pair;

public class Ipv6DenseRangeToIp implements Function<DenseRange<Ipv6>, Pair<Ipv6, Ipv6>> {

    @Override
    public Pair<Ipv6, Ipv6> apply(DenseRange<Ipv6> range) {
        dbc.precondition(range.iterator().hasNext(), "Range cannot be empty");
        final Ipv6 lastIp = range.end().map(new Ipv6PreviousIp()).orElse(Ipv6.getLastIp()); // Workaround for range.end()
        return Pair.of(range.begin(), lastIp);
    }

}
