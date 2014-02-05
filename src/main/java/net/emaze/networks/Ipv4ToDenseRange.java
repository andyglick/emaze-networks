package net.emaze.networks;

import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.BinaryDelegate;
import net.emaze.dysfunctional.ranges.DenseRange;

public class Ipv4ToDenseRange implements BinaryDelegate<DenseRange<Ipv4>, Ipv4, Ipv4> {

    @Override
    public DenseRange<Ipv4> perform(Ipv4 first, Ipv4 last) {
        dbc.precondition(first != null && last != null, "boundaries must be not null");
        return (DenseRange<Ipv4>) IpRanges.RANGESV4.closed(first, last);
    }
}
