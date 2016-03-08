package net.emaze.networks.ipv4;

import java.util.function.Function;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.ranges.DenseRange;
import net.emaze.dysfunctional.tuples.Pair;

public class Ipv4DenseRangeToIp implements Function<DenseRange<Ipv4>, Pair<Ipv4, Ipv4>> {

    @Override
    public Pair<Ipv4, Ipv4> apply(DenseRange<Ipv4> range) {
        dbc.precondition(range.iterator().hasNext(), "Range cannot be empty");
        final Ipv4 lastIp = range.end().map(new Ipv4PreviousIp()).orElse(Ipv4.getLastIp()); // Workaround for range.end()
        return Pair.of(range.begin(), lastIp);
    }

}
