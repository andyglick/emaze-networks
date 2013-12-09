package net.emaze.networks;

import net.emaze.dysfunctional.dispatching.delegates.BinaryDelegate;
import net.emaze.dysfunctional.ranges.DenseRange;

public class Ipv4ToDenseRange implements BinaryDelegate<DenseRange<Ipv4>, Ipv4, Ipv4> {


    @Override
    public DenseRange<Ipv4> perform(Ipv4 first, Ipv4 last) {
        return (DenseRange<Ipv4>) new Ipv4Ranges().closed(first, last);
    }
}
