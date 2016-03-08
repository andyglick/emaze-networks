package net.emaze.networks.ipv6;

import java.util.function.BiFunction;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.ranges.DenseRange;
import net.emaze.networks.IpRanges;

public class Ipv6ToDenseRange implements BiFunction<Ipv6, Ipv6,DenseRange<Ipv6>> {

    @Override
    public DenseRange<Ipv6> apply(Ipv6 first, Ipv6 last) {
        dbc.precondition(first != null && last != null, "boundaries must be not null");
        return (DenseRange<Ipv6>) IpRanges.RANGESV6.closed(first, last);
    }
}
